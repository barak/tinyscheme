/* T I N Y S C H E M E
 *   Dimitrios Souflis (dsouflis@altera.gr, dsouflis@acm.org)
 *   Based on MiniScheme (original credits follow)
 *  Akira KIDA's new address is Akira.Kida@nifty.ne.jp
 */
/*                  O R I G I N A L   C R E D I T S
 *      ---------- Mini-Scheme Interpreter Version 0.85 ----------
 *
 *                coded by Atsushi Moriwaki (11/5/1989)
 *
 *            E-MAIL :  moriwaki@kurims.kurims.kyoto-u.ac.jp
 *
 *               THIS SOFTWARE IS IN THE PUBLIC DOMAIN
 *               ------------------------------------
 * This software is completely free to copy, modify and/or re-distribute.
 * But I would appreciate it if you left my name on the code as the author.
 *
 *  This version has been modified by R.C. Secrist.
 *
 *  Mini-Scheme is now maintained by Akira KIDA.
 *
 *  This is a revised and modified version by Akira KIDA.
 *   current version is 0.85k4 (15 May 1994)
 *
 *  Please send suggestions, bug reports and/or requests to:
 *        <SDI00379@niftyserve.or.jp>
 */
#include "scheme.h"
#if USE_MATH
#include <math.h>
#endif
#include <limits.h>
#include <ctype.h>

#if USE_STRCASECMP
#include <strings.h>
#define stricmp strcasecmp
#endif

#if STDIO_ADDS_CR
#define CRNL "\n"
#else
#define CRNL "\r\n"
#endif

#define TOK_EOF     (-1)
#define TOK_LPAREN  0
#define TOK_RPAREN  1
#define TOK_DOT     2
#define TOK_ATOM    3
#define TOK_QUOTE   4
#define TOK_COMMENT 5
#define TOK_DQUOTE  6
#define TOK_BQUOTE  7
#define TOK_COMMA   8
#define TOK_ATMARK  9
#define TOK_SHARP   10
#define TOK_SHARP_CONST 11
#if USE_VERBATIM
# define TOK_VERBATIM 12
#endif
#define TOK_VEC     13

# define BACKQUOTE '`'

/*
 *  Basic memory allocation units
 */

#define banner "TinyScheme 1.12"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#ifdef _T
# undef _T
#endif

#ifndef prompt
# define prompt "> "
#endif

#ifndef InitFile
# define InitFile "init.scm"
#endif

#define FIRST_CELLSEGS 3

#define OUTF (sc->ports[sc->outport].rep.file)

enum {
 T_STRING=1,
 T_NUMBER,
 T_SYNTAX,
 T_PROC,
 T_PAIR,
 T_CLOSURE,
 T_CONTINUATION,
 T_FOREIGN,
 T_CHARACTER,
 T_PORT,
 T_VECTOR
};

#define T_MASKTYPE      31    /* 0000000000011111 */
#define T_ENVIRONMENT  512    /* 0000001000000000 */
#define T_SYMBOL      1024    /* 0000010000000000 */
#define T_MACRO       2048    /* 0000100000000000 */
#define T_PROMISE     4096    /* 0001000000000000 */
#define T_IMMUTABLE   8192    /* 0010000000000000 */
#define T_ATOM       16384    /* 0100000000000000 */   /* only for gc */
#define CLRATOM      49151    /* 1011111111111111 */   /* only for gc */
#define MARK         32768    /* 1000000000000000 */
#define UNMARK       32767    /* 0111111111111111 */

static num num_add(num a, num b);
static num num_mul(num a, num b);
static num num_div(num a, num b);
static num num_intdiv(num a, num b);
static num num_sub(num a, num b);
static num num_rem(num a, num b);
static num num_mod(num a, num b);
static int num_eq(num a, num b);
static int num_ne(num a, num b);
static int num_gt(num a, num b);
static int num_ge(num a, num b);
static int num_lt(num a, num b);
static int num_le(num a, num b);

static double round_per_R5RS(double x);

static num num_zero;
static num num_one;

/* macros for cell operations */
#define typeflag(p)      ((p)->_flag)
#define type(p)          (typeflag(p)&T_MASKTYPE)

int is_string(pointer p)      { return (type(p)==T_STRING); }
#define strvalue(p)      ((p)->_object._string._svalue)
#define keynum(p)        ((p)->_object._string._keynum)
char *string_value(pointer p) { return strvalue(p); }

int is_vector(pointer p)      { return (type(p)==T_VECTOR); }

int is_number(pointer p)      { return (type(p)==T_NUMBER); }
num nvalue(pointer p)        { return ((p)->_object._number); }
long ivalue(pointer p)       { return (is_integer(p)?(p)->_object._number.value.ivalue:(long)(p)->_object._number.value.rvalue); }
double rvalue(pointer p)     { return (!is_integer(p)?(p)->_object._number.value.rvalue:(double)(p)->_object._number.value.ivalue); }
#define ivalue_unchecked(p)       ((p)->_object._number.value.ivalue)
#define rvalue_unchecked(p)       ((p)->_object._number.value.rvalue)
#define set_integer(p)   (p)->_object._number.is_fixnum=1;
#define set_real(p)      (p)->_object._number.is_fixnum=0;
int is_integer(pointer p)     { return ((p)->_object._number.is_fixnum); }
int is_real(pointer p)        { return (!(p)->_object._number.is_fixnum); }

int is_character(pointer p)   { return (type(p)==T_CHARACTER); }
long charvalue(pointer p)    { return ivalue_unchecked(p); }

int is_port(pointer p)        { return (type(p)==T_PORT); }
#define is_inport(p)      (type(p)==T_PORT && sc->ports[ivalue(p)].kind&port_input)
#define is_outport(p)     (type(p)==T_PORT && sc->ports[ivalue(p)].kind&port_output)

int is_pair(pointer p)        { return (type(p)==T_PAIR); }
#define car(p)           ((p)->_object._cons._car)
#define cdr(p)           ((p)->_object._cons._cdr)
pointer pair_car(pointer p)   { return car(p); }
pointer pair_cdr(pointer p)   { return cdr(p); }
pointer set_car(pointer p, pointer q) { return car(p)=q; }
pointer set_cdr(pointer p, pointer q) { return cdr(p)=q; }

int is_symbol(pointer p)      { return (typeflag(p)&T_SYMBOL); }
char *symname(pointer p)     { return strvalue(car(p)); }
int hasprop(pointer p)       { return (type(p)&T_SYMBOL); }
#define symprop(p)       cdr(p)

int is_syntax(pointer p)      { return (type(p)==T_SYNTAX); }
int is_proc(pointer p)        { return (type(p)==T_PROC); }
int is_foreign(pointer p)     { return (type(p)==T_FOREIGN); }
char *syntaxname(pointer p)  { return strvalue(car(p)); }
#define syntaxnum(p)     keynum(car(p))
#define procnum(p)       ivalue(p)
static const char *procname(pointer x);

int is_closure(pointer p)     { return (type(p)==T_CLOSURE); }
int is_macro(pointer p)       { return (typeflag(p)&T_MACRO); }
pointer closure_code(pointer p)   { return car(p); }
pointer closure_env(pointer p)    { return cdr(p); }

int is_continuation(pointer p)     { return (type(p)==T_CONTINUATION); }
#define cont_dump(p)     cdr(p)

/* To do: promise should be forced ONCE only */
int is_promise(pointer p)     { return (typeflag(p)&T_PROMISE); }
#define setpromise(p)    typeflag(p) |= T_PROMISE

int is_environment(pointer p) { return (typeflag(p)&T_ENVIRONMENT); }
#define setenvironment(p)    typeflag(p) |= T_ENVIRONMENT

#define is_atom(p)        (typeflag(p)&T_ATOM)
#define setatom(p)       typeflag(p) |= T_ATOM
#define clratom(p)       typeflag(p) &= CLRATOM

#define is_mark(p)        (typeflag(p)&MARK)
#define setmark(p)       typeflag(p) |= MARK
#define clrmark(p)       typeflag(p) &= UNMARK

int is_immutable(pointer p)   { return (typeflag(p)&T_IMMUTABLE); }
#define setimmutable(p)  typeflag(p) |= T_IMMUTABLE

#define caar(p)          car(car(p))
#define cadr(p)          car(cdr(p))
#define cdar(p)          cdr(car(p))
#define cddr(p)          cdr(cdr(p))
#define cadar(p)         car(cdr(car(p)))
#define caddr(p)         car(cdr(cdr(p)))
#define cadaar(p)        car(cdr(car(car(p))))
#define cadddr(p)        car(cdr(cdr(cdr(p))))
#define cddddr(p)        cdr(cdr(cdr(cdr(p))))

#if USE_CHAR_CLASSIFIERS
static int Cisalpha(int c) { return isascii(c) && isalpha(c); }
static int Cisdigit(int c) { return isascii(c) && isdigit(c); }
static int Cisspace(int c) { return isascii(c) && isspace(c); }
static int Cisupper(int c) { return isascii(c) && isupper(c); }
static int Cislower(int c) { return isascii(c) && islower(c); }
#endif

static int file_push(scheme *sc, const char *fname);
static void file_pop(scheme *sc);
static int file_interactive(scheme *sc);
static int is_one_of(char *s, int c);
static int alloc_cellseg(scheme *sc, int n);
static long binary_decode(const char *s);
static pointer get_cell(scheme *sc, pointer a, pointer b);
static pointer get_consecutive_cells(scheme *sc, int n);
static pointer find_consecutive_cells(scheme *sc, int n);
static void reclaim_cell(scheme *sc, pointer a);
static void finalize_cell(scheme *sc, pointer a);
static int count_consecutive_cells(pointer x, int needed);
static pointer find_slot_in_env(scheme *sc, pointer env, pointer sym);
static pointer mk_number(scheme *sc, num n);
static pointer mk_empty_string(scheme *sc, int len, char fill);
static char   *store_string(scheme *sc, int len, const char *str, char fill);
static pointer mk_vector(scheme *sc, int len);
static void fill_vector(pointer vec, pointer obj);
static pointer vector_elem(pointer vec, int ielem);
static pointer set_vector_elem(pointer vec, int ielem, pointer a);
static pointer mk_atom(scheme *sc, char *q);
static pointer mk_sharp_const(scheme *sc, char *name);
static pointer mk_port(scheme *sc, int portnum);
static int port_find_free(scheme *sc);
static int port_from_file(scheme *sc, const char *fn, int prop);
static int port_from_string(scheme *sc, char *start, char *past_the_end, int prop);
static void port_close(scheme *sc, int portnum, int flag);
static void mark(pointer a);
static void gc(scheme *sc, pointer a, pointer b);
static int inchar(scheme *sc);
static void backchar(scheme *sc, int c);
static void putstr(scheme *sc, const char *s);
static char   *readstr_upto(scheme *sc, char *delim);
static char   *readstrexp(scheme *sc);
static void skipspace(scheme *sc);
static int token(scheme *sc);
static void strunquote(char *p, char *s);
static int printatom(scheme *sc, pointer l, int f);
static pointer mk_proc(scheme *sc, unsigned int op);
static pointer mk_closure(scheme *sc, pointer c, pointer e);
static pointer mk_continuation(scheme *sc, pointer d);
static pointer reverse(scheme *sc, pointer a);
static pointer reverse_in_place(scheme *sc, pointer term, pointer list);
static pointer reverse_in_place(scheme *sc, pointer term, pointer list);
static pointer append(scheme *sc, pointer a, pointer b);
static int list_length(scheme *sc, pointer a);
static int eqv(pointer a, pointer b);
static pointer opexe_0(scheme *sc, int op);
static pointer opexe_1(scheme *sc, int op);
static pointer opexe_2(scheme *sc, int op);
static pointer opexe_3(scheme *sc, int op);
static pointer opexe_4(scheme *sc, int op);
static pointer opexe_5(scheme *sc, int op);
static pointer opexe_6(scheme *sc, int op);
static void Eval_Cycle(scheme *sc, int op);
static void assign_syntax(scheme *sc, unsigned int op, char *name);
static void assign_proc(scheme *sc, unsigned int op, char *name);
static void init_vars_global(scheme *sc);
static void init_syntax(scheme *sc);
static void init_procs(scheme *sc);
static void init_procs(scheme *sc);
static void init_globals(scheme *sc);

#define num_ivalue(n)       (n.is_fixnum?(n).value.ivalue:(long)(n).value.rvalue)
#define num_rvalue(n)       (!n.is_fixnum?(n).value.rvalue:(double)(n).value.ivalue)

static num num_add(num a, num b) {
 num ret;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 if(ret.is_fixnum) {
     ret.value.ivalue= a.value.ivalue+b.value.ivalue;
 } else {
     ret.value.rvalue=num_rvalue(a)+num_rvalue(b);
 }
 return ret;
}

static num num_mul(num a, num b) {
 num ret;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 if(ret.is_fixnum) {
     ret.value.ivalue= a.value.ivalue*b.value.ivalue;
 } else {
     ret.value.rvalue=num_rvalue(a)*num_rvalue(b);
 }
 return ret;
}

static num num_div(num a, num b) {
 num ret;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum && a.value.ivalue%b.value.ivalue==0;
 if(ret.is_fixnum) {
     ret.value.ivalue= a.value.ivalue/b.value.ivalue;
 } else {
     ret.value.rvalue=num_rvalue(a)/num_rvalue(b);
 }
 return ret;
}

static num num_intdiv(num a, num b) {
 num ret;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 if(ret.is_fixnum) {
     ret.value.ivalue= a.value.ivalue/b.value.ivalue;
 } else {
     ret.value.rvalue=num_rvalue(a)/num_rvalue(b);
 }
 return ret;
}

static num num_sub(num a, num b) {
 num ret;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 if(ret.is_fixnum) {
     ret.value.ivalue= a.value.ivalue-b.value.ivalue;
 } else {
     ret.value.rvalue=num_rvalue(a)-num_rvalue(b);
 }
 return ret;
}

static num num_rem(num a, num b) {
 num ret;
 long e1, e2, res;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 e1=num_ivalue(a);
 e2=num_ivalue(b);
 res=e1%e2;
 if(res*e1<0) {    /* remainder should have same sign as first operand */
     e2=labs(e2);
     if(res>0) {
          res-=e2;
     } else {
          res+=e2;
     }
 }
 ret.value.ivalue=res;
 return ret;
}

static num num_mod(num a, num b) {
 num ret;
 long e1, e2, res;
 ret.is_fixnum=a.is_fixnum && b.is_fixnum;
 e1=num_ivalue(a);
 e2=num_ivalue(b);
 res=e1%e2;
 if(res*e2<0) {    /* modulo should have same sign as second operand */
     e2=labs(e2);
     if(res>0) {
          res-=e2;
     } else {
          res+=e2;
     }
 }
 ret.value.ivalue=res;
 return ret;
}

static int num_eq(num a, num b) {
 int ret;
 int is_fixnum=a.is_fixnum && b.is_fixnum;
 if(is_fixnum) {
     ret= a.value.ivalue==b.value.ivalue;
 } else {
     ret=num_rvalue(a)==num_rvalue(b);
 }
 return ret;
}

static int num_ne(num a, num b) {
 return !num_eq(a,b);
}

static int num_gt(num a, num b) {
 int ret;
 int is_fixnum=a.is_fixnum && b.is_fixnum;
 if(is_fixnum) {
     ret= a.value.ivalue>b.value.ivalue;
 } else {
     ret=num_rvalue(a)>num_rvalue(b);
 }
 return ret;
}

static int num_ge(num a, num b) {
 return !num_lt(a,b);
}

static int num_lt(num a, num b) {
 int ret;
 int is_fixnum=a.is_fixnum && b.is_fixnum;
 if(is_fixnum) {
     ret= a.value.ivalue<b.value.ivalue;
 } else {
     ret=num_rvalue(a)<num_rvalue(b);
 }
 return ret;
}

static int num_le(num a, num b) {
 return !num_gt(a,b);
}

/* Round to nearest. Round to even if midway */
static double round_per_R5RS(double x) {
 double fl=floor(x);
 double ce=ceil(x);
 double dfl=x-fl;
 double dce=ce-x;
 if(dfl>dce) {
     return ce;
 } else if(dfl<dce) {
     return fl;
 } else {
     if(fmod(fl,2.0)==0.0) {       /* I imagine this holds */
          return fl;
     } else {
          return ce;
     }
 }
}

static long binary_decode(const char *s) {
 long x=0;

 while(*s!=0 && (*s=='1' || *s=='0')) {
     x<<=1;
     x+=*s-'0';
     s++;
 }

 return x;
}

/* allocate new cell segment */
static int alloc_cellseg(scheme *sc, int n) {
     pointer p;
     long i;
     int k;

     for (k = 0; k < n; k++) {
          if (sc->last_cell_seg >= CELL_NSEGMENT - 1)
               return k;
          p = (pointer) sc->malloc(CELL_SEGSIZE * sizeof(struct cell));
          if (p == (pointer) 0)
               return k;
          sc->cell_seg[++sc->last_cell_seg] = p;
          sc->fcells += CELL_SEGSIZE;
          for (i = 0; i < CELL_SEGSIZE - 1; i++, p++) {
               typeflag(p) = 0;
               car(p) = sc->NIL;
               cdr(p) = p + 1;
          }
          typeflag(p) = 0;
          car(p) = sc->NIL;
          cdr(p) = sc->free_cell;
          sc->free_cell = sc->cell_seg[sc->last_cell_seg];
     }
     return n;
}

/* get new cell.  parameter a, b is marked by gc. */
static pointer get_cell(scheme *sc, pointer a, pointer b) {
     pointer x;

     if(sc->no_memory) {
          return sc->sink;
     }

     if (sc->free_cell == sc->NIL) {
          gc(sc,a, b);
          if (sc->free_cell == sc->NIL) {
               if (!alloc_cellseg(sc,1)) {
                    sc->no_memory=1;
                    return sc->sink;
               }
          }
     }
     x = sc->free_cell;
     sc->free_cell = cdr(x);
     --sc->fcells;
     return (x);
}

static pointer get_consecutive_cells(scheme *sc, int n) {
     pointer x;

     if(sc->no_memory) {
          return sc->sink;
     }

     /* Are there any cells available? */
     x=find_consecutive_cells(sc,n);
     if (x == sc->NIL) {
          /* If not, try gc'ing some */
          gc(sc, sc->NIL, sc->NIL);
          x=find_consecutive_cells(sc,n);
          if (x == sc->NIL) {
               /* If there still aren't, try getting more heap */
               if (!alloc_cellseg(sc,1)) {
                    sc->no_memory=1;
                    return sc->sink;
               }
          }
          x=find_consecutive_cells(sc,n);
          if (x == sc->NIL) {
                    /* If all fail, report failure */
                    sc->no_memory=1;
                    return sc->sink;
          }
     }
     return (x);
}

static int count_consecutive_cells(pointer x, int needed) {
 int n=1;
 while(cdr(x)==x+1) {
     x=cdr(x);
     n++;
     if(n>needed) return n;
 }
 return n;
}

static pointer find_consecutive_cells(scheme *sc, int n) {
     pointer *pp;
     int cnt;

     pp=&sc->free_cell;
     while(*pp!=sc->NIL) {
          cnt=count_consecutive_cells(*pp,n);
          if(cnt>=n) {
               pointer x=*pp;
               *pp=cdr(*pp+n-1);
               sc->fcells--;
               return x;
          }
          pp=&cdr(*pp+cnt-1);
     }
     return sc->NIL;
}

/* get new cons cell */
pointer _cons(scheme *sc, pointer a, pointer b, int immutable) {
     pointer x = get_cell(sc,a, b);

     typeflag(x) = T_PAIR;
     if(immutable) {
          setimmutable(x);
     }
     car(x) = a;
     cdr(x) = b;
     return (x);
}

static pointer mk_port(scheme *sc, int portnum) {
     pointer x = get_cell(sc, sc->NIL, sc->NIL);

     typeflag(x) = T_PORT|T_ATOM;
     ivalue_unchecked(x)=portnum;
     set_integer(x);
     return (x);
}

pointer mk_character(scheme *sc, int c) {
     pointer x = get_cell(sc,sc->NIL, sc->NIL);

     typeflag(x) = (T_CHARACTER | T_ATOM);
     ivalue_unchecked(x)= c;
     set_integer(x);
     return (x);
}

/* get number atom (integer) */
pointer mk_integer(scheme *sc, long num) {
     pointer x = get_cell(sc,sc->NIL, sc->NIL);

     typeflag(x) = (T_NUMBER | T_ATOM);
     ivalue_unchecked(x)= num;
     set_integer(x);
     return (x);
}

pointer mk_real(scheme *sc, double n) {
     pointer x = get_cell(sc,sc->NIL, sc->NIL);

     typeflag(x) = (T_NUMBER | T_ATOM);
     rvalue_unchecked(x)= n;
     set_real(x);
     return (x);
}

static pointer mk_number(scheme *sc, num n) {
 if(n.is_fixnum) {
     return mk_integer(sc,n.value.ivalue);
 } else {
     return mk_real(sc,n.value.rvalue);
 }
}

/* allocate name to string area */
static char   *store_string(scheme *sc, int len_str, const char *str, char fill) {
     char *q;
     
     q=sc->malloc(len_str+1);
     if(q==0) {
          sc->no_memory=1;
          return sc->strbuff;
     }
     if(str!=0) {
          strcpy(q, str);
     } else {
          memset(q, fill, len_str);
          q[len_str]=0;
     }
     return (q);
}

/* get new string */
pointer mk_string(scheme *sc, const char *str) {
     pointer x = get_cell(sc, sc->NIL, sc->NIL);

     strvalue(x) = store_string(sc,strlen(str),str,0);
     typeflag(x) = (T_STRING | T_ATOM);
     keynum(x) = (int) (-1);
     return (x);
}

static pointer mk_empty_string(scheme *sc, int len, char fill) {
     pointer x = get_cell(sc, sc->NIL, sc->NIL);

     strvalue(x) = store_string(sc,len,0,fill);
     typeflag(x) = (T_STRING | T_ATOM);
     keynum(x) = (int) (-1);
     return (x);
}

static pointer mk_vector(scheme *sc, int len) {
     pointer x=get_consecutive_cells(sc,len/2+len%2+1);
     typeflag(x) = (T_VECTOR | T_ATOM);
     ivalue_unchecked(x)=len;
     set_integer(x);
     fill_vector(x,sc->NIL);
     return x;
}

static void fill_vector(pointer vec, pointer obj) {
     int i;
     int num=ivalue(vec)/2+ivalue(vec)%2;
     for(i=0; i<num; i++) {
          typeflag(vec+1+i) = T_PAIR;
          setimmutable(vec+1+i);
          car(vec+1+i)=obj;
          cdr(vec+1+i)=obj;
     }
}

static pointer vector_elem(pointer vec, int ielem) {
     int n=ielem/2;
     if(ielem%2==0) {
          return car(vec+1+n);
     } else {
          return cdr(vec+1+n);
     }
}

static pointer set_vector_elem(pointer vec, int ielem, pointer a) {
     int n=ielem/2;
     if(ielem%2==0) {
          return car(vec+1+n)=a;
     } else {
          return cdr(vec+1+n)=a;
     }
}

/* get new symbol */
pointer mk_symbol(scheme *sc, const char *name) {
     pointer x;

     /* first check oblist */
     for (x = sc->oblist; x != sc->NIL; x = cdr(x)) {
          if (!stricmp(name, symname(car(x)))) { /* case-insens. R5RS 6.3.3 */
               break;
          }
     }

     if (x != sc->NIL) {
          return (car(x));
     } else {
          x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
          typeflag(x) = T_SYMBOL;
          setimmutable(x);
          setimmutable(car(x));
          sc->oblist = immutable_cons(sc, x, sc->oblist);
          return (x);
     }
}

pointer gensym(scheme *sc) {
     pointer x;
     char name[40];

     for(; sc->gensym_cnt<LONG_MAX; sc->gensym_cnt++) {
          sprintf(name,"gensym-%ld",sc->gensym_cnt);

          /* first check oblist */
          for (x = sc->oblist; x != sc->NIL; x = cdr(x)) {
               if (!stricmp(name, symname(car(x)))) { /* case-insens. R5RS 6.3.3 */
                    break;
               }
          }

          if (x != sc->NIL) {
               continue;
          } else {
               x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
               typeflag(x) = T_SYMBOL;
               setimmutable(x);
               setimmutable(car(x));
               sc->oblist = immutable_cons(sc, x, sc->oblist);
               return (x);
          }
     }

     return sc->NIL;
}

/* make symbol or number atom from string */
static pointer mk_atom(scheme *sc, char *q) {
     char    c, *p;
     int has_dec_point=0;

#if USE_COLON_HOOK
     if((p=strstr(q,"::"))!=0) {
          *p=0;
          return cons(sc, mk_symbol(sc,"*colon-hook*"),
                              cons(sc,
                                   cons(sc,
                                        mk_symbol(sc,"quote"),
                                        cons(sc, mk_atom(sc,p+2), sc->NIL)),
                                   cons(sc, mk_symbol(sc,q), sc->NIL)));
     }
#endif

     p = q;
     if (!isdigit(c = *p++)) {
          if ((c != '+' && c != '-') || !isdigit(*p)) {
               return (mk_symbol(sc, q));
          }
     }
     for ( ; (c = *p) != 0; ++p) {
          if (!isdigit(c)) {
               if(c=='.') {
                    if(!has_dec_point) {
                         has_dec_point=1;
                         continue;
                    }
               }
               return (mk_symbol(sc, q));
          }
     }
     if(has_dec_point) {
          return mk_real(sc,atof(q));
     }
     return (mk_integer(sc, atol(q)));
}

/* make constant */
static pointer mk_sharp_const(scheme *sc, char *name) {
     long    x;
     char    tmp[256];

     if (!strcmp(name, "t"))
          return (sc->T);
     else if (!strcmp(name, "f"))
          return (sc->F);
     else if (*name == 'o') {/* #o (octal) */
          sprintf(tmp, "0%s", name+1);
          sscanf(tmp, "%lo", &x);
          return (mk_integer(sc, x));
     } else if (*name == 'd') {    /* #d (decimal) */
          sscanf(name+1, "%ld", &x);
          return (mk_integer(sc, x));
     } else if (*name == 'x') {    /* #x (hex) */
          sprintf(tmp, "0x%s", name+1);
          sscanf(tmp, "%lx", &x);
          return (mk_integer(sc, x));
     } else if (*name == 'b') {    /* #b (binary) */
          x = binary_decode(name+1);
          return (mk_integer(sc, x));
     } else if (*name == '\\') { /* #\w (character) */
          int c=0;
          if(stricmp(name+1,"space")==0) {
               c=' ';
          } else if(stricmp(name+1,"newline")==0) {
               c='\n';
          } else if(stricmp(name+1,"return")==0) {
               c='\r';
          } else if(stricmp(name+1,"tab")==0) {
               c='\t';
          } else if(name[2]==0) {
               c=name[1];
          } else {
               return sc->NIL;
          }
          return mk_character(sc,c);
     } else
          return (sc->NIL);
}

/* ========== garbage collector ========== */

/*--
 *  We use algorithm E (Knuth, The Art of Computer Programming Vol.1,
 *  sec.3.5) for marking.
 */
static void mark(pointer a) {
     pointer t, q, p;

     t = (pointer) 0;
     p = a;
E2:  setmark(p);
     if(is_vector(p)) {
          int i;
          int num=ivalue_unchecked(p)/2;
          for(i=0; i<num; i++) {
               /* Vector cells will be treated like ordinary cells */
               mark(p+1+i);
          }
     }
     if (is_atom(p))
          goto E6;
     q = car(p);
     if (q && !is_mark(q)) {
          setatom(p);
          car(p) = t;
          t = p;
          p = q;
          goto E2;
     }
E5:  q = cdr(p);
     if (q && !is_mark(q)) {
          cdr(p) = t;
          t = p;
          p = q;
          goto E2;
     }
E6:  if (!t)
          return;
     q = t;
     if (is_atom(q)) {
          clratom(q);
          t = car(q);
          car(q) = p;
          p = q;
          goto E5;
     } else {
          t = cdr(q);
          cdr(q) = p;
          p = q;
          goto E6;
     }
}

/* garbage collection. parameter a, b is marked. */
static void gc(scheme *sc, pointer a, pointer b) {
     pointer p;
     int i;
     long j;

     if (sc->gc_verbose) {
          fprintf(OUTF,"gc...");
     }

     /* mark system globals */
     mark(sc->oblist);
     mark(sc->global_env);

     /* mark current registers */
     mark(sc->args);
     mark(sc->envir);
     mark(sc->code);
     mark(sc->dump);

     /* mark variables a, b */
     mark(a);
     mark(b);

     /* garbage collect */
     clrmark(sc->NIL);
     sc->fcells = 0;
     sc->free_cell = sc->NIL;
     for (i = 0; i <= sc->last_cell_seg; i++) {
          for (j = 0, p = sc->cell_seg[i]; j < CELL_SEGSIZE; j++, p++) {
               if (is_mark(p)) {
                    clrmark(p);
               } else {
                    reclaim_cell(sc,p);
               }
          }
     }

     if (sc->gc_verbose) {
          fprintf(OUTF," done %ld cells are recovered.\n", sc->fcells);
     }
}

/*   Retain a free-list sorted by address, so as to maintain consecutive
     ranges, if possible, for use with vectors */
static void reclaim_cell(scheme *sc, pointer p) {
 finalize_cell(sc, p);
 typeflag(p) = 0;
 if(sc->free_cell==sc->NIL || p<sc->free_cell) {
     cdr(p) = sc->free_cell;
     car(p) = sc->NIL;
     sc->free_cell = p;
 } else {
     pointer where=sc->free_cell;
     while(cdr(where)!=sc->NIL && p<cdr(where)) {
          where=cdr(where);
     }
     cdr(p) = cdr(where);
     car(p) = sc->NIL;
     cdr(where) = p;
 }
 ++sc->fcells;
}

static void finalize_cell(scheme *sc, pointer a) {
 if(is_string(a)) {
     sc->free(strvalue(a));
 }
}

/* ========== Routines for Reading ========== */

static int file_push(scheme *sc, const char *fname) {
 FILE *fin=fopen(fname,"r");
 if(fin!=0) {
     sc->file_i++;
     sc->file_stack[sc->file_i]=fin;
     sc->nesting_stack[sc->file_i]=0;
 }
 return fin!=0;
}

static void file_pop(scheme *sc) {
 sc->nesting=sc->nesting_stack[sc->file_i];
 if(sc->file_i!=0) {
     fclose(sc->file_stack[sc->file_i]);
     sc->file_i--;
     if(file_interactive(sc)) {
          fprintf(OUTF,prompt);
     }
 }
}

static int file_interactive(scheme *sc) {
 return sc->file_i==0 && sc->file_stack[0]==stdin
     && sc->ports[sc->outport].kind&port_file;
}

static int port_find_free(scheme *sc) {
     int i;
     for(i=2; i<FILENUM; i++) {
          port *pt=sc->ports+i;
          if(pt->kind==port_free) {
               return i;
          }
     }
     return -1;
}

static int port_from_file(scheme *sc, const char *fn, int prop) {
     FILE *f;
     char *rw;
     port *pt;
     int i=port_find_free(sc);
     if(i==-1) {
          return -1;
     }
     if(prop==(port_input|port_output)) {
          rw="a+";
     } else if(prop==port_output) {
          rw="w";
     } else {
          rw="r";
     }
     f=fopen(fn,rw);
     if(f==0) {
          return -1;
     }
     pt=sc->ports+i;
     pt->kind=port_file|prop;
     pt->rep.file=f;
     return i;
}

static int port_from_string(scheme *sc, char *start, char *past_the_end, int prop) {
     port *pt;
     int i=port_find_free(sc);
     if(i==-1) {
          return -1;
     }
     pt=sc->ports+i;
     pt->kind=port_string|prop;
     pt->rep.string.start=start;
     pt->rep.string.curr=start;
     pt->rep.string.past_the_end=past_the_end;
     return i;
}

static void port_close(scheme *sc, int portnum, int flag) {
     port *pt=sc->ports+portnum;
     pt->kind&=~flag;
     if((pt->kind & (port_input|port_output))==0) {
          if(pt->kind&port_file) {
               fclose(pt->rep.file);
          }
          pt->kind=port_free;
     }
}

/* get new character from input file */
static int inchar(scheme *sc) {
     int c;
     if(sc->inport==-1) {
          while((c=fgetc(sc->file_stack[sc->file_i]))==EOF && sc->file_i!=0) {
               file_pop(sc);
               if(sc->nesting!=0) {
                    return EOF;
               }
          }
          return c;
     } else {
          port *pt=sc->ports+sc->inport;
          if(pt->kind&port_file) {
               return fgetc(pt->rep.file);
          } else {
               if(*pt->rep.string.curr==0
               || pt->rep.string.curr==pt->rep.string.past_the_end) {
                    return EOF;
               } else {
                    return *pt->rep.string.curr++;
               }
          }
     }
}

/* back character to input buffer */
static void backchar(scheme *sc, int c) {
     if(c==EOF) return;
     if(sc->inport==-1) {
          ungetc(c,sc->file_stack[sc->file_i]);
     } else {
          port *pt=sc->ports+sc->inport;
          if(pt->kind&port_file) {
               ungetc(c,pt->rep.file);
          } else {
               if(pt->rep.string.curr!=pt->rep.string.start) {
                    *--pt->rep.string.curr=c;
               }
          }
     }
}

static void putstr(scheme *sc, const char *s) {
     port *pt=sc->ports+sc->outport;
     if(pt->kind&port_file) {
          fputs(s,pt->rep.file);
     } else {
          for(;*s;s++) {
               if(pt->rep.string.curr!=pt->rep.string.past_the_end) {
                    *pt->rep.string.curr++=*s;
               }
          }
     }
}

/* read chacters up to delimiter, but cater to character constants */
static char   *readstr_upto(scheme *sc, char *delim) {
     char   *p = sc->strbuff;

     while (!is_one_of(delim, (*p++ = inchar(sc))));
     if(p==sc->strbuff+2 && p[-2]=='\\') {
          *p=0;
     } else {
          backchar(sc,p[-1]);
          *--p = '\0';
     }
     return (sc->strbuff);
}

/* read string expression "xxx...xxx" */
static char   *readstrexp(scheme *sc) {
     char *p = sc->strbuff;
     int c;

     for (;;) {
          c=inchar(sc);
          if (c != '"') {
               if((c=='n' || c=='t' || c=='r') && p > sc->strbuff && *(p-1)=='\\') {
                    if(c=='n') {
                         *(p-1)='\n';
                    } else if(c=='r') {
                    } else {
                         *(p-1)='\t';
                    }
               } else {
                    *p++ = c;
               }
          } else if (p > sc->strbuff && *(p - 1) == '\\') {
               *(p - 1) = '"';
          } else {
               *p = '\0';
               return (sc->strbuff);
          }
     }
}

/* check c is in chars */
static int is_one_of(char *s, int c) {
     if(c==EOF) return 1;
     while (*s)
          if (*s++ == c)
               return (1);
     return (0);
}

/* skip white characters */
static void skipspace(scheme *sc) {
     int c;
     while (isspace(c=inchar(sc)))
          ;
     if(c!=EOF) {
          backchar(sc,c);
     }
}

/* get token */
static int token(scheme *sc) {
     int c;
     skipspace(sc);
     switch (c=inchar(sc)) {
     case EOF:
          return (TOK_EOF);
     case '(':
          return (TOK_LPAREN);
     case ')':
          return (TOK_RPAREN);
     case '.':
          c=inchar(sc);
          if(is_one_of(" \n\t",c)) {
               return (TOK_DOT);
          } else {
               backchar(sc,c);
               return TOK_ATOM;
          }
     case '\'':
          return (TOK_QUOTE);
     case ';':
          return (TOK_COMMENT);
     case '"':
          return (TOK_DQUOTE);
     case BACKQUOTE:
          return (TOK_BQUOTE);
     case ',':
          if ((c=inchar(sc)) == '@')
               return (TOK_ATMARK);
          else {
               backchar(sc,c);
               return (TOK_COMMA);
          }
     case '#':
          c=inchar(sc);
          if (c == '(') {
               return (TOK_VEC);
          } else if(c == '!') {
               return TOK_COMMENT;
          } else {
               backchar(sc,c);
               if(is_one_of(" tfodxb\\",c)) {
                    return TOK_SHARP_CONST;
               } else {
                    return (TOK_SHARP);
               }
          }
#if USE_VERBATIM
     case '|':
          return (TOK_VERBATIM);
#endif
     default:
          backchar(sc,c);
          return (TOK_ATOM);
     }
}

/* ========== Routines for Printing ========== */
#define   ok_abbrev(x)   (is_pair(x) && cdr(x) == sc->NIL)

static void strunquote(char *p, char *s) {
     *p++ = '"';
     for ( ; *s; ++s) {
          if (*s == '"') {
               *p++ = '\\';
               *p++ = '"';
          } else if (*s == '\n') {
               *p++ = '\\';
               *p++ = 'n';
          } else if (*s == '\t') {
               *p++ = '\\';
               *p++ = 't';
          } else if (*s == '\r') {
               *p++ = '\\';
               *p++ = 'r';
          } else
               *p++ = *s;
     }
     *p++ = '"';
     *p = '\0';
}

/* print atoms */
static int printatom(scheme *sc, pointer l, int f) {
     char *p;

     if (l == sc->NIL) {
          p = "()";
     } else if (l == sc->T) {
          p = "#t";
     } else if (l == sc->F) {
          p = "#f";
     } else if (l == sc->EOF_OBJ) {
          p = "#<EOF>";
     } else if (is_port(l)) {
          p = sc->strbuff;
          sprintf(p, "#<PORT %ld>", ivalue(l));
     } else if (is_number(l)) {
          p = sc->strbuff;
          if(is_integer(l)) {
               sprintf(p, "%ld", ivalue_unchecked(l));
          } else {
               sprintf(p, "%g", rvalue_unchecked(l));
          }
     } else if (is_string(l)) {
          if (!f) {
               p = strvalue(l);
          } else {
               p = sc->strbuff;
               strunquote(p, strvalue(l));
          }
     } else if (is_character(l)) {
          int c=charvalue(l);
          p = sc->strbuff;
          if (!f) {
               p[0]=c;
               p[1]=0;
          } else {
               switch(c) {
               case ' ':
                    sprintf(p,"#\\space"); break;
               case '\n':
                    sprintf(p,"#\\newline"); break;
               case '\r':
                    sprintf(p,"#\\return"); break;
               case '\t':
                    sprintf(p,"#\\tab"); break;
               default:
                    sprintf(p,"#\\%c",c); break;
               }
          }
     } else if (is_symbol(l)) {
          p = symname(l);
     } else if (is_proc(l)) {
          p = sc->strbuff;
          sprintf(p, "#<%s PROCEDURE %ld>", procname(l),procnum(l));
     } else if (is_macro(l)) {
          p = "#<MACRO>";
     } else if (is_closure(l)) {
          p = "#<CLOSURE>";
     } else if (is_foreign(l)) {
          p = sc->strbuff;
          sprintf(p, "#<FOREIGN PROCEDURE %ld>", procnum(l));
     } else if (is_continuation(l)) {
          p = "#<CONTINUATION>";
     } else {
          p = "#<ERROR>";
     }
     if (f < 0) {
          return strlen(p);
     }
     putstr(sc,p);
     return 0;
}

/* ========== Routines for Evaluation Cycle ========== */

/* make closure. c is code. e is environment */
static pointer mk_closure(scheme *sc, pointer c, pointer e) {
     pointer x = get_cell(sc, c, e);

     typeflag(x) = T_CLOSURE;
     car(x) = c;
     cdr(x) = e;
     return (x);
}

/* make continuation. */
static pointer mk_continuation(scheme *sc, pointer d) {
     pointer x = get_cell(sc, sc->NIL, d);

     typeflag(x) = T_CONTINUATION;
     cont_dump(x) = d;
     return (x);
}

/* reverse list -- produce new list */
static pointer reverse(scheme *sc, pointer a) {
/* a must be checked by gc */
     pointer p = sc->NIL;

     for ( ; is_pair(a); a = cdr(a)) {
          p = cons(sc, car(a), p);
     }
     return (p);
}

/* reverse list --- in-place */
static pointer reverse_in_place(scheme *sc, pointer term, pointer list) {
     pointer p = list, result = term, q;

     while (p != sc->NIL) {
          q = cdr(p);
          cdr(p) = result;
          result = p;
          p = q;
     }
     return (result);
}

/* append list -- produce new list */
static pointer append(scheme *sc, pointer a, pointer b) {
     pointer p = b, q;

     if (a != sc->NIL) {
          a = reverse(sc, a);
          while (a != sc->NIL) {
               q = cdr(a);
               cdr(a) = p;
               p = a;
               a = q;
          }
     }
     return (p);
}

/* equivalence of atoms */
static int eqv(pointer a, pointer b) {
     if (is_string(a)) {
          if (is_string(b))
               return (strvalue(a) == strvalue(b));
          else
               return (0);
     } else if (is_number(a)) {
          if (is_number(b))
               return num_eq(nvalue(a),nvalue(b));
          else
               return (0);
     } else if (is_character(a)) {
          if (is_character(b))
               return charvalue(a)==charvalue(b);
          else
               return (0);
     } else if (is_port(a)) {
          if (is_port(b))
               return ivalue(a)==ivalue(b);
          else
               return (0);
     } else if (is_proc(a)) {
          if (is_proc(b))
               return procnum(a)==procnum(b);
          else
               return (0);
     } else {
          return (a == b);
     }
}

/* true or false value macro */
/* () is #t in R5RS */
#define is_true(p)       ((p) != sc->F)
#define is_false(p)      ((p) == sc->F)

/* ========== Evaluation Cycle ========== */

/* operator code */
enum {
   OP_LOAD,
   OP_T0LVL,
   OP_T1LVL,
   OP_READ_INTERNAL,
   OP_GENSYM,
   OP_VALUEPRINT,
   OP_EVAL,
   OP_E0ARGS,
   OP_E1ARGS,
   OP_APPLY,
   OP_DOMACRO,

   OP_LAMBDA,
   OP_QUOTE,
   OP_DEF0,
   OP_DEF1,
   OP_DEFP,
   OP_BEGIN,
   OP_IF0,
   OP_IF1,
   OP_SET0,
   OP_SET1,
   OP_LET0,
   OP_LET1,
   OP_LET2,
   OP_LET0AST,
   OP_LET1AST,
   OP_LET2AST,
   OP_LET0REC,
   OP_LET1REC,
   OP_LET2REC,
   OP_COND0,
   OP_COND1,
   OP_DELAY,
   OP_AND0,
   OP_AND1,
   OP_OR0,
   OP_OR1,
   OP_C0STREAM,
   OP_C1STREAM,
   OP_MACRO0,
   OP_MACRO1,
   OP_CASE0,
   OP_CASE1,
   OP_CASE2,

   OP_PEVAL,
   OP_PAPPLY,
   OP_CONTINUATION,
#if USE_MATH
   OP_INEX2EX,
   OP_EXP,
   OP_LOG,
   OP_SIN,
   OP_COS,
   OP_TAN,
   OP_ASIN,
   OP_ACOS,
   OP_ATAN,
   OP_SQRT,
   OP_EXPT,
   OP_FLOOR,
   OP_CEILING,
   OP_TRUNCATE,
   OP_ROUND,
#endif
   OP_ADD,
   OP_SUB,
   OP_MUL,
   OP_DIV,
   OP_INTDIV,
   OP_REM,
   OP_MOD,
   OP_CAR,
   OP_CDR,
   OP_CONS,
   OP_SETCAR,
   OP_SETCDR,
   OP_CHAR2INT,
   OP_INT2CHAR,
   OP_CHARUPCASE,
   OP_CHARDNCASE,
   OP_SYM2STR,
   OP_STR2SYM,
   OP_MKSTRING,
   OP_STRLEN,
   OP_STRREF,
   OP_STRSET,
   OP_SUBSTR,
   OP_VECTOR,
   OP_MKVECTOR,
   OP_VECLEN,
   OP_VECREF,
   OP_VECSET,
   OP_NOT,
   OP_BOOLP,
   OP_EOFOBJP,
   OP_NULLP,
   OP_NUMEQ,
   OP_LESS,
   OP_GRE,
   OP_LEQ,
   OP_GEQ,
   OP_SYMBOLP,
   OP_NUMBERP,
   OP_STRINGP,
   OP_INTEGERP,
   OP_REALP,
   OP_CHARP,
#if USE_CHAR_CLASSIFIERS
   OP_CHARAP,
   OP_CHARNP,
   OP_CHARWP,
   OP_CHARUP,
   OP_CHARLP,
#endif
   OP_PORTP,
   OP_INPORTP,
   OP_OUTPORTP,
   OP_PROCP,
   OP_PAIRP,
   OP_LISTP,
   OP_ENVP,
   OP_VECTORP,
   OP_EQ,
   OP_EQV,
   OP_FORCE,
   OP_SAVE_FORCED,
   OP_WRITE,
   OP_WRITE_CHAR,
   OP_DISPLAY,
   OP_NEWLINE,
   OP_ERR0,
   OP_ERR1,
   OP_REVERSE,
   OP_APPEND,
   OP_PUT,
   OP_GET,
   OP_QUIT,
   OP_GC,
   OP_GCVERB,
   OP_NEWSEGMENT,
   OP_OBLIST,
   OP_CURR_INPORT,
   OP_CURR_OUTPORT,
   OP_OPEN_INFILE,
   OP_OPEN_OUTFILE,
   OP_OPEN_INOUTFILE,
#if USE_STRING_PORTS
   OP_OPEN_INSTRING,
   OP_OPEN_OUTSTRING,
   OP_OPEN_INOUTSTRING,
#endif
   OP_CLOSE_INPORT,
   OP_CLOSE_OUTPORT,
   OP_INT_ENV,
   OP_CURR_ENV,

   OP_READ,
   OP_READ_CHAR,
   OP_PEEK_CHAR,
   OP_CHAR_READY,
   OP_SET_INPORT,
   OP_SET_OUTPORT,
   OP_RDSEXPR,
   OP_RDLIST,
   OP_RDDOT,
   OP_RDQUOTE,
   OP_RDQQUOTE,
   OP_RDUNQUOTE,
   OP_RDUQTSP,
   OP_RDVEC,

   OP_P0LIST,
   OP_P1LIST,
   OP_PVECFROM,

   OP_LIST_LENGTH,
   OP_ASSQ,
   OP_PRINT_WIDTH,
   OP_P0_WIDTH,
   OP_P1_WIDTH,
   OP_GET_CLOSURE,
   OP_CLOSUREP,
   OP_MACROP
};

static pointer find_slot_in_env(scheme *sc, pointer env, pointer hdl) {
    pointer x,y;
    for (x = env; x != sc->NIL; x = cdr(x)) {
         for (y = car(x); y != sc->NIL; y = cdr(y)) {
              if (caar(y) == hdl) {
                   break;
              }
         }
         if (y != sc->NIL) {
              break;
         }
    }
    if (x != sc->NIL) {
          return car(y);
    }
    return sc->NIL;
}

static pointer _Error_1(scheme *sc, const char *s, pointer a) {
#if USE_ERROR_HOOK
     pointer x;
     pointer hdl=mk_symbol(sc, "*error-hook*");

     x=find_slot_in_env(sc,sc->envir,hdl);
    if (x != sc->NIL) {
         if(a!=0) {
               sc->code = cons(sc, cons(sc, mk_symbol(sc,"quote"), cons(sc,(a), sc->NIL)), sc->NIL);
         } else {
               sc->code = sc->NIL;
         }
         sc->code = cons(sc, mk_string(sc, (s)), sc->code);
         setimmutable(car(sc->code));
         sc->code = cons(sc, cdr(x), sc->code);
         sc->op = (int)OP_EVAL;
         return sc->T;
    }
#endif

    if(a!=0) {
          sc->args = cons(sc, (a), sc->NIL);
    } else {
          sc->args = sc->NIL;
    }
    sc->args = cons(sc, mk_string(sc, (s)), sc->args);
    setimmutable(car(sc->args));
    sc->op = (int)OP_ERR0;
    return sc->T;
}
#define Error_1(sc,s, a) return _Error_1(sc,s,a)
#define Error_0(sc,s)    return _Error_1(sc,s,0)

/* Too small to turn into function */
# define  BEGIN     do {
# define  END  } while (0)
#define s_goto(sc,a) BEGIN                                  \
    sc->op = (int)(a);                                      \
    return sc->T; END

static pointer _s_return(scheme *sc, pointer a) {
    sc->value = (a);
    if(sc->dump==sc->NIL) return sc->NIL;
    sc->op = ivalue(car(sc->dump));
    sc->args = cadr(sc->dump);
    sc->envir = caddr(sc->dump);
    sc->code = cadddr(sc->dump);
    sc->dump = cddddr(sc->dump);
    return sc->T;
}
#define s_return(sc,a) return _s_return(sc,a)

static void s_save(scheme *sc, int op, pointer args, pointer code) {
    sc->dump = cons(sc, sc->envir, cons(sc, (code), sc->dump));
    sc->dump = cons(sc, (args), sc->dump);
    sc->dump = cons(sc, mk_integer(sc, (long)(op)), sc->dump);
}

#define s_retbool(tf)    s_return(sc,(tf) ? sc->T : sc->F)

static pointer opexe_0(scheme *sc, int op) {
     pointer x, y;

     switch (op) {
     case OP_LOAD:       /* load */
          if (!is_string(car(sc->args))) {
               Error_0(sc,"load -- argument is not string");
          }
          if(file_interactive(sc)) {
               fprintf(OUTF, "Loading %s\n", strvalue(car(sc->args)));
          }
          if (!file_push(sc,strvalue(car(sc->args)))) {
               Error_1(sc,"Unable to open", car(sc->args));
          }
          s_goto(sc,OP_T0LVL);

     case OP_T0LVL: /* top level */
          if(file_interactive(sc)) {
               fprintf(OUTF, "\n");
          }
          sc->nesting=0;
          sc->dump = sc->NIL;
          sc->envir = sc->global_env;
          if(sc->inport!=-1) {
               sc->save_inport=sc->inport;
          }
          sc->inport = -1;
          s_save(sc,OP_VALUEPRINT, sc->NIL, sc->NIL);
          s_save(sc,OP_T1LVL, sc->NIL, sc->NIL);
          if (file_interactive(sc)) {
              fprintf(OUTF,prompt);
          }
          s_goto(sc,OP_READ_INTERNAL);

     case OP_T1LVL: /* top level */
          sc->code = sc->value;
          sc->inport=sc->save_inport;
          s_goto(sc,OP_EVAL);

     case OP_READ_INTERNAL:       /* internal read */
          sc->tok = token(sc);
          if(sc->tok==TOK_EOF) {
               if(sc->inport==-1) {
                    sc->args=sc->NIL;
                    s_goto(sc,OP_QUIT);
               } else {
                    s_return(sc,sc->EOF_OBJ);
               }
          }
          s_goto(sc,OP_RDSEXPR);

     case OP_GENSYM:
          s_return(sc, gensym(sc));

     case OP_VALUEPRINT: /* print evalution result */
          /* OP_VALUEPRINT is always pushed, because when changing from
             non-interactive to interactive mode, it needs to be
             already on the stack */
          if(file_interactive(sc)) {
               sc->print_flag = 1;
               sc->args = sc->value;
               s_save(sc,OP_T0LVL, sc->NIL, sc->NIL);
               s_goto(sc,OP_P0LIST);
          } else {
               s_goto(sc,OP_T0LVL);
          }

     case OP_EVAL:       /* main part of evalution */
          if (is_symbol(sc->code)) {    /* symbol */
               x=find_slot_in_env(sc,sc->envir,sc->code);
               if (x != sc->NIL) {
                    s_return(sc,cdr(x));
               } else {
                    Error_1(sc,"Unbounded variable", sc->code);
               }
          } else if (is_pair(sc->code)) {
               if (is_syntax(x = car(sc->code))) {     /* SYNTAX */
                    sc->code = cdr(sc->code);
                    s_goto(sc,syntaxnum(x));
               } else {/* first, eval top element and eval arguments */
                    s_save(sc,OP_E0ARGS, sc->NIL, sc->code);
                    /* If no macros => s_save(sc,OP_E1ARGS, sc->NIL, cdr(sc->code));*/
                    sc->code = car(sc->code);
                    s_goto(sc,OP_EVAL);
               }
          } else {
               s_return(sc,sc->code);
          }

     case OP_E0ARGS:     /* eval arguments */
          if (is_macro(sc->value)) {    /* macro expansion */
               s_save(sc,OP_DOMACRO, sc->NIL, sc->NIL);
               sc->args = cons(sc,sc->code, sc->NIL);
               sc->code = sc->value;
               s_goto(sc,OP_APPLY);
          } else {
               sc->code = cdr(sc->code);
               s_goto(sc,OP_E1ARGS);
          }

     case OP_E1ARGS:     /* eval arguments */
          sc->args = cons(sc, sc->value, sc->args);
          if (is_pair(sc->code)) { /* continue */
               s_save(sc,OP_E1ARGS, sc->args, cdr(sc->code));
               sc->code = car(sc->code);
               sc->args = sc->NIL;
               s_goto(sc,OP_EVAL);
          } else {  /* end */
               sc->args = reverse(sc, sc->args);
               sc->code = car(sc->args);
               sc->args = cdr(sc->args);
               s_goto(sc,OP_APPLY);
          }

     case OP_APPLY:      /* apply 'code' to 'args' */
          if (is_proc(sc->code)) {
               s_goto(sc,procnum(sc->code));   /* PROCEDURE */
          } else if (is_foreign(sc->code)) {
               x=sc->ff[ivalue(sc->code)](sc,sc->args);
               s_return(sc,x);
          } else if (is_closure(sc->code)) { /* CLOSURE */
               /* make environment */
               sc->envir = immutable_cons(sc, sc->NIL, closure_env(sc->code));
               setenvironment(sc->envir);
               for (x = car(closure_code(sc->code)), y = sc->args;
                    is_pair(x); x = cdr(x), y = cdr(y)) {
                    if (y == sc->NIL) {
                         Error_0(sc,"Few arguments");
                    } else {
                         car(sc->envir) = immutable_cons(sc, immutable_cons(sc, car(x), car(y)), car(sc->envir));
                    }
               }
               if (x == sc->NIL) {
                    /*--
                     * if (y != sc->NIL) {
                     *   Error_0(sc,"Many arguments");
                     * }
                     */
               } else if (is_symbol(x))
                    car(sc->envir) = immutable_cons(sc, immutable_cons(sc, x, y), car(sc->envir));
               else {
                    Error_0(sc,"Syntax error in closure");
               }
               sc->code = cdr(closure_code(sc->code));
               sc->args = sc->NIL;
               s_goto(sc,OP_BEGIN);
          } else if (is_continuation(sc->code)) { /* CONTINUATION */
               sc->dump = cont_dump(sc->code);
               s_return(sc,sc->args != sc->NIL ? car(sc->args) : sc->NIL);
          } else {
               Error_0(sc,"Illegal function");
          }

     case OP_DOMACRO:    /* do macro */
          sc->code = sc->value;
          s_goto(sc,OP_EVAL);

     case OP_LAMBDA:     /* lambda */
          s_return(sc,mk_closure(sc, sc->code, sc->envir));

     case OP_QUOTE:      /* quote */
          x=car(sc->code);
          if(is_pair(x) && is_proc(car(x)) && procnum(car(x))==OP_VECTOR) {
               sc->args=cdr(x);
               s_goto(sc,OP_VECTOR);
          }
          s_return(sc,car(sc->code));

     case OP_DEF0:  /* define */
          if (is_pair(car(sc->code))) {
               x = caar(sc->code);
               sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
          } else {
               x = car(sc->code);
               sc->code = cadr(sc->code);
          }
          if (!is_symbol(x)) {
               Error_0(sc,"Variable is not symbol");
          }
          s_save(sc,OP_DEF1, sc->NIL, x);
          s_goto(sc,OP_EVAL);

     case OP_DEF1:  /* define */
          for (x = car(sc->envir); x != sc->NIL; x = cdr(x)) {
               if (caar(x) == sc->code) {
                    break;
               }
          }
          if (x != sc->NIL) {
               cdar(x) = sc->value;
          } else {
               car(sc->envir) = immutable_cons(sc, immutable_cons(sc, sc->code, sc->value), car(sc->envir));
          }
          s_return(sc,sc->code);

     case OP_DEFP:  /* defined? */
          x=sc->envir;
          if (!is_symbol(car(sc->args))) {
               Error_1(sc,"defined? : not a symbol", car(sc->args));
          }
          if(cdr(sc->args)!=sc->NIL) {
               if(!is_environment(cadr(sc->args))) {
                    Error_0(sc, "defined?: 2nd argument must be environment");
               }
               x=cadr(sc->args);
          }
          s_retbool(find_slot_in_env(sc,x,car(sc->args))!=sc->NIL);

     case OP_SET0:       /* set! */
          s_save(sc,OP_SET1, sc->NIL, car(sc->code));
          sc->code = cadr(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_SET1:       /* set! */
          for (x = sc->envir; x != sc->NIL; x = cdr(x)) {
               for (y = car(x); y != sc->NIL; y = cdr(y)) {
                    if (caar(y) == sc->code) {
                         break;
                    }
               }
               if (y != sc->NIL) {
                    break;
               }
          }
          if (x != sc->NIL) {
               cdar(y) = sc->value;
               s_return(sc,sc->value);
          } else {
               Error_1(sc,"Unbounded variable", sc->code);
          }

     case OP_BEGIN:      /* begin */
          if (!is_pair(sc->code)) {
               s_return(sc,sc->code);
          }
          if (cdr(sc->code) != sc->NIL) {
               s_save(sc,OP_BEGIN, sc->NIL, cdr(sc->code));
          }
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_IF0:        /* if */
          s_save(sc,OP_IF1, sc->NIL, cdr(sc->code));
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_IF1:        /* if */
          if (is_true(sc->value))
               sc->code = car(sc->code);
          else
               sc->code = cadr(sc->code);  /* (if #f 1) ==> () because
                               * car(sc->NIL) = sc->NIL */
          s_goto(sc,OP_EVAL);

     case OP_LET0:       /* let */
          sc->args = sc->NIL;
          sc->value = sc->code;
          sc->code = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code);
          s_goto(sc,OP_LET1);

     case OP_LET1:       /* let (caluculate parameters) */
          sc->args = cons(sc, sc->value, sc->args);
          if (is_pair(sc->code)) { /* continue */
               s_save(sc,OP_LET1, sc->args, cdr(sc->code));
               sc->code = cadar(sc->code);
               sc->args = sc->NIL;
               s_goto(sc,OP_EVAL);
          } else {  /* end */
               sc->args = reverse(sc, sc->args);
               sc->code = car(sc->args);
               sc->args = cdr(sc->args);
               s_goto(sc,OP_LET2);
          }

     case OP_LET2:       /* let */
          sc->envir = immutable_cons(sc, sc->NIL, sc->envir);
          setenvironment(sc->envir);
          for (x = is_symbol(car(sc->code)) ? cadr(sc->code) : car(sc->code), y = sc->args;
               y != sc->NIL; x = cdr(x), y = cdr(y)) {
               car(sc->envir) = immutable_cons(sc, immutable_cons(sc, caar(x), car(y)), car(sc->envir));
          }
          if (is_symbol(car(sc->code))) {    /* named let */
               for (x = cadr(sc->code), sc->args = sc->NIL; x != sc->NIL; x = cdr(x)) {
                    sc->args = cons(sc, caar(x), sc->args);
               }
               x = mk_closure(sc, cons(sc, reverse(sc, sc->args), cddr(sc->code)), sc->envir);
               car(sc->envir) = immutable_cons(sc, immutable_cons(sc, car(sc->code), x), car(sc->envir));
               sc->code = cddr(sc->code);
               sc->args = sc->NIL;
          } else {
               sc->code = cdr(sc->code);
               sc->args = sc->NIL;
          }
          s_goto(sc,OP_BEGIN);

     case OP_LET0AST:    /* let* */
          if (car(sc->code) == sc->NIL) {
               sc->envir = immutable_cons(sc, sc->NIL, sc->envir);
               setenvironment(sc->envir);
               sc->code = cdr(sc->code);
               s_goto(sc,OP_BEGIN);
          }
          s_save(sc,OP_LET1AST, cdr(sc->code), car(sc->code));
          sc->code = cadaar(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_LET1AST:    /* let* (make new frame) */
          sc->envir = immutable_cons(sc, sc->NIL, sc->envir);
          setenvironment(sc->envir);
          s_goto(sc,OP_LET2AST);

     case OP_LET2AST:    /* let* (caluculate parameters) */
          car(sc->envir) = immutable_cons(sc, immutable_cons(sc, caar(sc->code), sc->value), car(sc->envir));
          sc->code = cdr(sc->code);
          if (is_pair(sc->code)) { /* continue */
               s_save(sc,OP_LET2AST, sc->args, sc->code);
               sc->code = cadar(sc->code);
               sc->args = sc->NIL;
               s_goto(sc,OP_EVAL);
          } else {  /* end */
               sc->code = sc->args;
               sc->args = sc->NIL;
               s_goto(sc,OP_BEGIN);
          }
     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);
     }
     return sc->T;
}

static pointer opexe_1(scheme *sc, int op) {
     pointer x, y;

     switch (op) {
     case OP_LET0REC:    /* letrec */
          sc->envir = immutable_cons(sc, sc->NIL, sc->envir);
          setenvironment(sc->envir);
          sc->args = sc->NIL;
          sc->value = sc->code;
          sc->code = car(sc->code);
          s_goto(sc,OP_LET1REC);

     case OP_LET1REC:    /* letrec (caluculate parameters) */
          sc->args = cons(sc, sc->value, sc->args);
          if (is_pair(sc->code)) { /* continue */
               s_save(sc,OP_LET1REC, sc->args, cdr(sc->code));
               sc->code = cadar(sc->code);
               sc->args = sc->NIL;
               s_goto(sc,OP_EVAL);
          } else {  /* end */
               sc->args = reverse(sc, sc->args);
               sc->code = car(sc->args);
               sc->args = cdr(sc->args);
               s_goto(sc,OP_LET2REC);
          }

     case OP_LET2REC:    /* letrec */
          for (x = car(sc->code), y = sc->args; y != sc->NIL; x = cdr(x), y = cdr(y)) {
               car(sc->envir) = immutable_cons(sc, immutable_cons(sc, caar(x), car(y)), car(sc->envir));
          }
          sc->code = cdr(sc->code);
          sc->args = sc->NIL;
          s_goto(sc,OP_BEGIN);

     case OP_COND0:      /* cond */
          if (!is_pair(sc->code)) {
               Error_0(sc,"Syntax error in cond");
          }
          s_save(sc,OP_COND1, sc->NIL, sc->code);
          sc->code = caar(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_COND1:      /* cond */
          if (is_true(sc->value)) {
               if ((sc->code = cdar(sc->code)) == sc->NIL) {
                    s_return(sc,sc->value);
               }
               if(car(sc->code)==sc->FEED_TO) {
                    if(!is_pair(cdr(sc->code))) {
                         Error_0(sc,"Syntax error in cond");
                    }
                    x=cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL));
                    sc->code=cons(sc,cadr(sc->code),cons(sc,x,sc->NIL));
                    s_goto(sc,OP_EVAL);
               }
               s_goto(sc,OP_BEGIN);
          } else {
               if ((sc->code = cdr(sc->code)) == sc->NIL) {
                    s_return(sc,sc->NIL);
               } else {
                    s_save(sc,OP_COND1, sc->NIL, sc->code);
                    sc->code = caar(sc->code);
                    s_goto(sc,OP_EVAL);
               }
          }

     case OP_DELAY:      /* delay */
          x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
          setpromise(x);
          s_return(sc,x);

     case OP_AND0:       /* and */
          if (sc->code == sc->NIL) {
               s_return(sc,sc->T);
          }
          s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_AND1:       /* and */
          if (is_false(sc->value)) {
               s_return(sc,sc->value);
          } else if (sc->code == sc->NIL) {
               s_return(sc,sc->value);
          } else {
               s_save(sc,OP_AND1, sc->NIL, cdr(sc->code));
               sc->code = car(sc->code);
               s_goto(sc,OP_EVAL);
          }

     case OP_OR0:        /* or */
          if (sc->code == sc->NIL) {
               s_return(sc,sc->F);
          }
          s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_OR1:        /* or */
          if (is_true(sc->value)) {
               s_return(sc,sc->value);
          } else if (sc->code == sc->NIL) {
               s_return(sc,sc->value);
          } else {
               s_save(sc,OP_OR1, sc->NIL, cdr(sc->code));
               sc->code = car(sc->code);
               s_goto(sc,OP_EVAL);
          }

     case OP_C0STREAM:   /* cons-stream */
          s_save(sc,OP_C1STREAM, sc->NIL, cdr(sc->code));
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_C1STREAM:   /* cons-stream */
          sc->args = sc->value;  /* save sc->value to register sc->args for gc */
          x = mk_closure(sc, cons(sc, sc->NIL, sc->code), sc->envir);
          setpromise(x);
          s_return(sc,cons(sc, sc->args, x));

     case OP_MACRO0:     /* macro */
          if (is_pair(car(sc->code))) {
               x = caar(sc->code);
               sc->code = cons(sc, sc->LAMBDA, cons(sc, cdar(sc->code), cdr(sc->code)));
          } else {
               x = car(sc->code);
               sc->code = cadr(sc->code);
          }
          if (!is_symbol(x)) {
               Error_0(sc,"Variable is not symbol");
          }
          s_save(sc,OP_MACRO1, sc->NIL, x);
          s_goto(sc,OP_EVAL);

     case OP_MACRO1:     /* macro */
          typeflag(sc->value) |= T_MACRO;
          for (x = car(sc->envir); x != sc->NIL; x = cdr(x)) {
               if (caar(x) == sc->code) {
                    break;
               }
          }
          if (x != sc->NIL) {
               cdar(x) = sc->value;
          } else {
               car(sc->envir) = immutable_cons(sc, immutable_cons(sc, sc->code, sc->value), car(sc->envir));
          }
          s_return(sc,sc->code);

     case OP_CASE0:      /* case */
          s_save(sc,OP_CASE1, sc->NIL, cdr(sc->code));
          sc->code = car(sc->code);
          s_goto(sc,OP_EVAL);

     case OP_CASE1:      /* case */
          for (x = sc->code; x != sc->NIL; x = cdr(x)) {
               if (!is_pair(y = caar(x))) {
                    break;
               }
               for ( ; y != sc->NIL; y = cdr(y)) {
                    if (eqv(car(y), sc->value)) {
                         break;
                    }
               }
               if (y != sc->NIL) {
                    break;
               }
          }
          if (x != sc->NIL) {
               if (is_pair(caar(x))) {
                    sc->code = cdar(x);
                    s_goto(sc,OP_BEGIN);
               } else {/* else */
                    s_save(sc,OP_CASE2, sc->NIL, cdar(x));
                    sc->code = caar(x);
                    s_goto(sc,OP_EVAL);
               }
          } else {
               s_return(sc,sc->NIL);
          }

     case OP_CASE2:      /* case */
          if (is_true(sc->value)) {
               s_goto(sc,OP_BEGIN);
          } else {
               s_return(sc,sc->NIL);
          }

     case OP_PAPPLY:     /* apply */
          sc->code = car(sc->args);
          sc->args = cadr(sc->args);
          s_goto(sc,OP_APPLY);

     case OP_PEVAL: /* eval */
          if(cdr(sc->args)!=sc->NIL) {
               if(!is_environment(cadr(sc->args))) {
                    Error_0(sc, "eval: 2nd argument must be environment");
               }
               sc->envir=cadr(sc->args);
          }
          sc->code = car(sc->args);
          s_goto(sc,OP_EVAL);

     case OP_CONTINUATION:    /* call-with-current-continuation */
          sc->code = car(sc->args);
          sc->args = cons(sc, mk_continuation(sc, sc->dump), sc->NIL);
          s_goto(sc,OP_APPLY);

     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);
     }
     return sc->T;
}

static pointer opexe_2(scheme *sc, int op) {
     pointer x;
     num v;
     double dd;

     switch (op) {
#if USE_MATH
     case OP_INEX2EX:    /* inexact->exact */
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"inexact->exact : not a number:",x);
          }
          if(is_integer(x)) {
               s_return(sc,x);
          } else if(modf(rvalue_unchecked(x),&dd)==0.0) {
               s_return(sc,mk_integer(sc,ivalue(x)));
          } else {
               Error_1(sc,"inexact->exact : not integral :",x);
          }

     case OP_EXP:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"exp : not a number:",x);
          }
          s_return(sc, mk_real(sc, exp(rvalue(x))));

     case OP_LOG:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"log : not a number:",x);
          }
          s_return(sc, mk_real(sc, log(rvalue(x))));

     case OP_SIN:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"sin : not a number:",x);
          }
          s_return(sc, mk_real(sc, sin(rvalue(x))));

     case OP_COS:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"cos : not a number:",x);
          }
          s_return(sc, mk_real(sc, cos(rvalue(x))));

     case OP_TAN:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"tan : not a number:",x);
          }
          s_return(sc, mk_real(sc, tan(rvalue(x))));

     case OP_ASIN:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"asin : not a number:",x);
          }
          s_return(sc, mk_real(sc, asin(rvalue(x))));

     case OP_ACOS:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"acos : not a number:",x);
          }
          s_return(sc, mk_real(sc, acos(rvalue(x))));

     case OP_ATAN:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"atan : not a number:",x);
          }
          if(cdr(sc->args)==sc->NIL) {
               s_return(sc, mk_real(sc, atan(rvalue(x))));
          } else {
               pointer y=cadr(sc->args);
               if(!is_number(y)) {
                    Error_1(sc,"atan : not a number:",y);
               }
               s_return(sc, mk_real(sc, atan2(rvalue(x),rvalue(y))));
          }

     case OP_SQRT:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"sqrt : not a number:",x);
          }
          s_return(sc, mk_real(sc, sqrt(rvalue(x))));

     case OP_EXPT:
          x=car(sc->args);
          if(!is_number(x)) {
               Error_1(sc,"expt : not a number:",x);
          }
          if(cdr(sc->args)==sc->NIL) {
               Error_0(sc,"expt : needs two arguments");
          } else {
               pointer y=cadr(sc->args);
               if(!is_number(y)) {
                    Error_1(sc,"expt : not a number:",y);
               }
               s_return(sc, mk_real(sc, pow(rvalue(x),rvalue(y))));
          }

     case OP_FLOOR:
          x=car(sc->args);
          if(is_number(x)) {
               s_return(sc, mk_real(sc, floor(rvalue(x))));
          } else {
               Error_1(sc,"floor : not a number:",x);
          }

     case OP_CEILING:
          x=car(sc->args);
          if(is_number(x)) {
               s_return(sc, mk_real(sc, ceil(rvalue(x))));
          } else {
               Error_1(sc,"ceiling : not a number:",x);
          }

     case OP_TRUNCATE :
          x=car(sc->args);
          if(is_number(x)) {
               double rvalue_of_x ;
               rvalue_of_x = rvalue(x) ;
               if (rvalue_of_x > 0) {
                    s_return(sc, mk_real(sc, floor(rvalue_of_x)));
               } else {
                    s_return(sc, mk_real(sc, ceil(rvalue_of_x)));
               }
          } else {
               Error_1(sc,"truncate : not a number:",x);
          }

     case OP_ROUND:
          x=car(sc->args);
          if(is_number(x)) {
               s_return(sc, mk_real(sc, round_per_R5RS(rvalue(x))));
          } else {
               Error_1(sc,"round : not a number:",x);
          }
#endif

     case OP_ADD:        /* + */
          v=num_zero;
          for (x = sc->args; x != sc->NIL; x = cdr(x)) {
               if(!is_number(car(x))) {
                    Error_1(sc,"+ : not a number:",car(x));
               }
               v=num_add(v,nvalue(car(x)));
          }
          s_return(sc,mk_number(sc, v));

     case OP_MUL:        /* * */
          v=num_one;
          for (x = sc->args; x != sc->NIL; x = cdr(x)) {
               if(!is_number(car(x))) {
                    Error_1(sc,"* : not a number:",car(x));
               }
               v=num_mul(v,nvalue(car(x)));
          }
          s_return(sc,mk_number(sc, v));

     case OP_SUB:        /* - */
          if(sc->args==sc->NIL) {
               Error_0(sc,"- : no arguments");
          }
          if(cdr(sc->args)==sc->NIL) {
               x=sc->args;
               v=num_zero;
          } else {
               if(!is_number(car(sc->args))) {
                    Error_1(sc,"- : not a number:",car(sc->args));
               }
               x = cdr(sc->args);
               v = nvalue(car(sc->args));
          }
          for (; x != sc->NIL; x = cdr(x)) {
               if(!is_number(car(x))) {
                    Error_1(sc,"- : not a number:",car(x));
               }
               v=num_sub(v,nvalue(car(x)));
          }
          s_return(sc,mk_number(sc, v));

     case OP_DIV:        /* / */
          if(cdr(sc->args)==sc->NIL) {
               x=sc->args;
               v=num_one;
          } else {
               if(!is_number(car(sc->args))) {
                    Error_1(sc,"/ : not a number:",car(sc->args));
               }
               x = cdr(sc->args);
               v = nvalue(car(sc->args));
          }
          for (; x != sc->NIL; x = cdr(x)) {
               if(!is_number(car(x))) {
                    Error_1(sc,"/ : not a number:",car(x));
               }
               if (ivalue(car(x)) != 0)
                    v=num_div(v,nvalue(car(x)));
               else {
                    Error_0(sc,"/ : Divided by zero");
               }
          }
          s_return(sc,mk_number(sc, v));

     case OP_INTDIV:        /* quotient */
          if(cdr(sc->args)==sc->NIL) {
               x=sc->args;
               v=num_one;
          } else {
               if(!is_integer(car(sc->args))) {
                    Error_1(sc,"quotient : not an integer:",car(sc->args));
               }
               x = cdr(sc->args);
               v = nvalue(car(sc->args));
          }
          for (; x != sc->NIL; x = cdr(x)) {
               if(!is_integer(car(x))) {
                    Error_1(sc,"quotient : not an integer:",car(x));
               }
               if (ivalue(car(x)) != 0)
                    v=num_intdiv(v,nvalue(car(x)));
               else {
                    Error_0(sc,"quotient : Divided by zero");
               }
          }
          s_return(sc,mk_number(sc, v));

     case OP_REM:        /* remainder */
          if(!is_integer(car(sc->args))) {
               Error_1(sc,"remainder : not an integer:",car(sc->args));
          }
          if(!is_integer(cadr(sc->args))) {
               Error_1(sc,"remainder : not an integer:",cadr(sc->args));
          }
          v = nvalue(car(sc->args));
          if (ivalue(cadr(sc->args)) != 0)
               v=num_rem(v,nvalue(cadr(sc->args)));
          else {
               Error_0(sc,"remainder : Divided by zero");
          }
          s_return(sc,mk_number(sc, v));

     case OP_MOD:        /* modulo */
          if(!is_integer(car(sc->args))) {
               Error_1(sc,"modulo : not an integer:",car(sc->args));
          }
          if(!is_integer(cadr(sc->args))) {
               Error_1(sc,"modulo : not an integer:",cadr(sc->args));
          }
          v = nvalue(car(sc->args));
          if (ivalue(cadr(sc->args)) != 0)
               v=num_mod(v,nvalue(cadr(sc->args)));
          else {
               Error_0(sc,"modulo : Divided by zero");
          }
          s_return(sc,mk_number(sc, v));

     case OP_CAR:        /* car */
          if (is_pair(car(sc->args))) {
               s_return(sc,caar(sc->args));
          } else {
               Error_1(sc,"car : not a pair ", car(sc->args));
          }

     case OP_CDR:        /* cdr */
          if (is_pair(car(sc->args))) {
               s_return(sc,cdar(sc->args));
          } else {
               Error_1(sc,"cdr : not a pair ", car(sc->args));
          }

     case OP_CONS:       /* cons */
          cdr(sc->args) = cadr(sc->args);
          s_return(sc,sc->args);

     case OP_SETCAR:     /* set-car! */
          if (is_pair(car(sc->args))) {
               if(!is_immutable(car(sc->args))) {
                    caar(sc->args) = cadr(sc->args);
                    s_return(sc,car(sc->args));
               } else {
                    Error_0(sc,"set-car!: Unable to alter immutable pair");
               }
          } else {
               Error_1(sc,"set-car!: not a pair ", car(sc->args));
          }

     case OP_SETCDR:     /* set-cdr! */
          if (is_pair(car(sc->args))) {
               if(!is_immutable(car(sc->args))) {
                    cdar(sc->args) = cadr(sc->args);
                    s_return(sc,car(sc->args));
               } else {
                    Error_0(sc,"set-cdr!: Unable to alter immutable pair");
               }
          } else {
               Error_1(sc,"set-cdr!: not a pair ", car(sc->args));
          }

     case OP_CHAR2INT: { /* char->integer */
          char c;
          if(!is_character(car(sc->args))) {
               Error_1(sc,"char->integer : not a char",car(sc->args));
          }
          c=(char)ivalue(car(sc->args));
          s_return(sc,mk_integer(sc,(unsigned char)c));
     }

     case OP_INT2CHAR: { /* integer->char */
          unsigned char c;
          if(!is_integer(car(sc->args))) {
               Error_1(sc,"char->integer : not an integer",car(sc->args));
          }
          c=(unsigned char)ivalue(car(sc->args));
          s_return(sc,mk_character(sc,(char)c));
     }

     case OP_CHARUPCASE: {
          unsigned char c;
          if(!is_character(car(sc->args))) {
               Error_1(sc,"char-upcase : not an character",car(sc->args));
          }
          c=(unsigned char)ivalue(car(sc->args));
          c=toupper(c);
          s_return(sc,mk_character(sc,(char)c));
     }

     case OP_CHARDNCASE: {
          unsigned char c;
          if(!is_character(car(sc->args))) {
               Error_1(sc,"char-upcase : not an character",car(sc->args));
          }
          c=(unsigned char)ivalue(car(sc->args));
          c=tolower(c);
          s_return(sc,mk_character(sc,(char)c));
     }

     case OP_STR2SYM:  /* string->symbol */
          if(!is_string(car(sc->args))) {
               Error_1(sc,"string->symbol : not a string",car(sc->args));
          }
          s_return(sc,mk_symbol(sc,strvalue(car(sc->args))));

     case OP_SYM2STR: /* symbol->string */
          if(!is_symbol(car(sc->args))) {
               Error_1(sc,"symbol->string : not a symbol",car(sc->args));
          }
          x=mk_string(sc,symname(car(sc->args)));
          setimmutable(x);
          s_return(sc,x);

     case OP_MKSTRING: { /* make-string */
          int fill=' ';
          int len;

          if(!is_number(car(sc->args))) {
               Error_1(sc,"make-string: not a number:",car(sc->args));
          }
          len=ivalue(car(sc->args));
          if(len<0) {
               Error_1(sc,"make-string: not positive:",car(sc->args));
          }

          if(cdr(sc->args)!=sc->NIL) {
               if(!is_character(cadr(sc->args))) {
                    Error_1(sc,"make-string: not a character:",cadr(sc->args));
               }
               fill=charvalue(cadr(sc->args));
          }
          s_return(sc,mk_empty_string(sc,len,(char)fill));
     }

     case OP_STRLEN:  /* string-length */
          if(!is_string(car(sc->args))) {
               Error_1(sc,"string-length: not a string:",car(sc->args));
          }
          s_return(sc,mk_integer(sc,strlen(strvalue(car(sc->args)))));

     case OP_STRREF: { /* string-ref */
          char *str;
          int index;

          if(!is_string(car(sc->args))) {
               Error_1(sc,"string-ref: not a string:",car(sc->args));
          }
          str=strvalue(car(sc->args));

          if(cdr(sc->args)==sc->NIL) {
               Error_0(sc,"string-ref: needs two arguments");
          }
          if(!is_number(cadr(sc->args))) {
               Error_1(sc,"string-ref: not a number:",cadr(sc->args));
          }
          index=ivalue(cadr(sc->args));

          if(index<0 || (size_t)index>=strlen(str)) {
               Error_1(sc,"string-ref: out of bounds:",cadr(sc->args));
          }

          s_return(sc,mk_character(sc,str[index]));
     }

     case OP_STRSET: { /* string-set! */
          char *str;
          int index;
          int c;

          if(!is_string(car(sc->args))) {
               Error_1(sc,"string-set!: not a string:",car(sc->args));
          }
          if(is_immutable(car(sc->args))) {
               Error_1(sc,"string-set!: unable to alter immutable string:",car(sc->args));
          }
          str=strvalue(car(sc->args));

          if(!is_number(cadr(sc->args))) {
               Error_1(sc,"string-set!: not a number:",cadr(sc->args));
          }
          index=ivalue(cadr(sc->args));
          if(index<0 || (size_t)index>=strlen(str)) {
               Error_1(sc,"string-set!: out of bounds:",cadr(sc->args));
          }

          if(!is_character(caddr(sc->args))) {
               Error_1(sc,"string-set!: not a character:",caddr(sc->args));
          }
          c=charvalue(caddr(sc->args));

          str[index]=(char)c;
          s_return(sc,car(sc->args));
     }

     case OP_SUBSTR: { /* substring */
          char *str;
          int index0;
          int index1;
          int len;

          if(!is_string(car(sc->args))) {
               Error_1(sc,"substring: not a string:",car(sc->args));
          }
          str=strvalue(car(sc->args));

          if(!is_number(cadr(sc->args))) {
               Error_1(sc,"substring: not a number:",cadr(sc->args));
          }
          index0=ivalue(cadr(sc->args));

          if(index0<0 || (size_t)index0>=strlen(str)) {
               Error_1(sc,"substring: out of bounds:",cadr(sc->args));
          }

          if(cddr(sc->args)!=sc->NIL) {
               if(!is_number(caddr(sc->args))) {
                    Error_1(sc,"substring: not a number:",caddr(sc->args));
               }
               index1=ivalue(caddr(sc->args));
               if(index1<index0 || (size_t)index1>strlen(str)) {
                    Error_1(sc,"substring: out of bounds:",caddr(sc->args));
               }
          } else {
               index1=strlen(str);
          }

          len=index1-index0;
          x=mk_empty_string(sc,len,' ');
          memcpy(strvalue(x),str+index0,len);
          strvalue(x)[len]=0;

          s_return(sc,x);
     }

     case OP_VECTOR: {   /* vector */
          int i;
          pointer vec;
          int len=list_length(sc,sc->args);
          if(len<0) {
               Error_1(sc,"vector : not a proper list :",sc->args);
          }
          vec=mk_vector(sc,len);
          for (x = sc->args, i = 0; is_pair(x); x = cdr(x), i++) {
               set_vector_elem(vec,i,car(x));
          }
          s_return(sc,vec);
     }

     case OP_MKVECTOR: { /* make-vector */
          pointer fill=sc->NIL;
          int len;
          pointer vec;

          if(!is_number(car(sc->args))) {
               Error_1(sc,"make-vector: not a number:",car(sc->args));
          }
          len=ivalue(car(sc->args));
          if(len<0) {
               Error_1(sc,"make-vector: not positive:",car(sc->args));
          }

          if(cdr(sc->args)!=sc->NIL) {
               fill=cadr(sc->args);
          }
          vec=mk_vector(sc,len);
          if(fill!=sc->NIL) {
               fill_vector(vec,fill);
          }
          s_return(sc,vec);
     }

     case OP_VECLEN:  /* vector-length */
          if(!is_vector(car(sc->args))) {
               Error_1(sc,"vector-length: not a vector:",car(sc->args));
          }
          s_return(sc,mk_integer(sc,ivalue(car(sc->args))));

     case OP_VECREF: { /* vector-ref */
          int index;

          if(!is_vector(car(sc->args))) {
               Error_1(sc,"vector-ref: not a vector:",car(sc->args));
          }

          if(!is_number(cadr(sc->args))) {
               Error_1(sc,"vector-ref: not a number:",cadr(sc->args));
          }
          index=ivalue(cadr(sc->args));

          if(index<0 || index>=ivalue(car(sc->args))) {
               Error_1(sc,"vector-ref: out of bounds:",cadr(sc->args));
          }

          s_return(sc,vector_elem(car(sc->args),index));
     }

     case OP_VECSET: {   /* vector-set! */
          int index;

          if(!is_vector(car(sc->args))) {
               Error_1(sc,"vector-set!: not a vector:",car(sc->args));
          }
          if(is_immutable(car(sc->args))) {
               Error_1(sc,"vector-set!: unable to alter immutable vector:",car(sc->args));
          }

          if(!is_number(cadr(sc->args))) {
               Error_1(sc,"string-set!: not a number:",cadr(sc->args));
          }
          index=ivalue(cadr(sc->args));
          if(index<0 || index>=ivalue(car(sc->args))) {
               Error_1(sc,"vector-set!: out of bounds:",cadr(sc->args));
          }

          set_vector_elem(car(sc->args),index,caddr(sc->args));
          s_return(sc,car(sc->args));
     }

     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);
     }
     return sc->T;
}

static int list_length(scheme *sc, pointer a) {
     int v=0;
     pointer x;
     for (x = a, v = 0; is_pair(x); x = cdr(x)) {
          ++v;
     }
     if(x==sc->NIL) {
          return v;
     }
     return -1;
}

static pointer opexe_3(scheme *sc, int op) {
     pointer x;
     num v;
     int (*comp_func)(num,num);
     const char *msg;

     switch (op) {
     case OP_NOT:        /* not */
          s_retbool(is_false(car(sc->args)));
     case OP_BOOLP:       /* boolean? */
          s_retbool(car(sc->args) == sc->F || car(sc->args) == sc->T);
     case OP_EOFOBJP:       /* boolean? */
          s_retbool(car(sc->args) == sc->EOF_OBJ);
     case OP_NULLP:       /* null? */
          s_retbool(car(sc->args) == sc->NIL);
     case OP_NUMEQ:      /* = */
     case OP_LESS:       /* < */
     case OP_GRE:        /* > */
     case OP_LEQ:        /* <= */
     case OP_GEQ:        /* >= */
          switch(op) {
               case OP_NUMEQ: comp_func=num_eq; msg="= : Not a number:"; break;
               case OP_LESS:  comp_func=num_lt; msg="< : Not a number:"; break;
               case OP_GRE:   comp_func=num_gt; msg="> : Not a number:"; break;
               case OP_LEQ:   comp_func=num_le; msg="<= : Not a number:"; break;
               case OP_GEQ:   comp_func=num_ge; msg=">= : Not a number:"; break;
          }
          x=sc->args;
          if(!is_number(car(x))) {
               Error_1(sc,msg,car(x));
          }
          v=nvalue(car(x));
          x=cdr(x);
          if(!is_number(car(x))) {
               Error_1(sc,msg,car(x));
          }
          if(!comp_func(v,nvalue(car(x)))) {
               s_retbool(0);
          }
          v=nvalue(car(x));
          x=cdr(x);

          for (; x != sc->NIL; x = cdr(x)) {
               if(!is_number(car(x))) {
                    Error_1(sc,msg,car(x));
               }
               if(!comp_func(v,nvalue(car(x)))) {
                    s_retbool(0);
               }
          }
          s_retbool(1);
     case OP_SYMBOLP:     /* symbol? */
          s_retbool(is_symbol(car(sc->args)));
     case OP_NUMBERP:     /* number? */
          s_retbool(is_number(car(sc->args)));
     case OP_STRINGP:     /* string? */
          s_retbool(is_string(car(sc->args)));
     case OP_INTEGERP:     /* integer? */
          s_retbool(is_integer(car(sc->args)));
     case OP_REALP:     /* real? */
          s_retbool(is_number(car(sc->args))); /* All numbers are real */
     case OP_CHARP:     /* char? */
          s_retbool(is_character(car(sc->args)));
#if USE_CHAR_CLASSIFIERS
     case OP_CHARAP:     /* char-alphabetic? */
          s_retbool(is_character(car(sc->args)) && Cisalpha(ivalue(car(sc->args))));
     case OP_CHARNP:     /* char-numeric? */
          s_retbool(is_character(car(sc->args)) && Cisdigit(ivalue(car(sc->args))));
     case OP_CHARWP:     /* char-whitespace? */
          s_retbool(is_character(car(sc->args)) && Cisspace(ivalue(car(sc->args))));
     case OP_CHARUP:     /* char-upper-case? */
          s_retbool(is_character(car(sc->args)) && Cisupper(ivalue(car(sc->args))));
     case OP_CHARLP:     /* char-lower-case? */
          s_retbool(is_character(car(sc->args)) && Cislower(ivalue(car(sc->args))));
#endif
     case OP_PORTP:     /* port? */
          s_retbool(is_port(car(sc->args)));
     case OP_INPORTP:     /* input-port? */
          s_retbool(is_inport(car(sc->args)));
     case OP_OUTPORTP:     /* output-port? */
          s_retbool(is_outport(car(sc->args)));
     case OP_PROCP:       /* procedure? */
          /*--
              * continuation should be procedure by the example
              * (call-with-current-continuation procedure?) ==> #t
                 * in R^3 report sec. 6.9
              */
          s_retbool(is_proc(car(sc->args)) || is_closure(car(sc->args))
                 || is_continuation(car(sc->args)) || is_foreign(car(sc->args)));
     case OP_PAIRP:       /* pair? */
          s_retbool(is_pair(car(sc->args)));
     case OP_LISTP: {     /* list? */
          x=car(sc->args);
          if(!is_pair(x)) {
               s_retbool(0);
          }
          for(x=cdr(x); is_pair(x); x=cdr(x)) {
               if(x==car(sc->args)) break;
          }
          s_retbool(x==sc->NIL);
     }
     case OP_ENVP:        /* environment? */
          s_retbool(is_environment(car(sc->args)));
     case OP_VECTORP:     /* vector? */
          s_retbool(is_vector(car(sc->args)));
     case OP_EQ:         /* eq? */
          s_retbool(car(sc->args) == cadr(sc->args));
     case OP_EQV:        /* eqv? */
          s_retbool(eqv(car(sc->args), cadr(sc->args)));
     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);
     }
     return sc->T;
}

static pointer opexe_4(scheme *sc, int op) {
     pointer x, y;

     switch (op) {
     case OP_FORCE:      /* force */
          sc->code = car(sc->args);
          if (is_promise(sc->code)) {
               s_save(sc, OP_SAVE_FORCED, sc->NIL, sc->code);
               sc->args = sc->NIL;
               s_goto(sc,OP_APPLY);
          } else {
               s_return(sc,sc->code);
          }

     case OP_SAVE_FORCED:     /* Save forced value replacing promise */
          memcpy(sc->code,sc->value,sizeof(struct cell));
          s_return(sc,sc->value);

     case OP_WRITE:      /* write */
     case OP_DISPLAY:    /* display */
     case OP_WRITE_CHAR: /* write-char */
          if(is_pair(cdr(sc->args))) {
               if(!is_outport(cadr(sc->args))) {
                    Error_1(sc,"write/display: not an output port : ",cadr(sc->args));
               }
               if(ivalue(cadr(sc->args))!=sc->outport) {
                    x=mk_port(sc,sc->outport);
                    x=cons(sc,x,sc->NIL);
                    s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
                    sc->outport=ivalue(cadr(sc->args));
               }
          }
          sc->args = car(sc->args);
          if(op==OP_WRITE) {
               sc->print_flag = 1;
          } else {
               sc->print_flag = 0;
          }
          s_goto(sc,OP_P0LIST);

     case OP_NEWLINE:    /* newline */
          if(is_pair(sc->args)) {
               if(!is_outport(car(sc->args))) {
                    Error_1(sc,"newline: not an output port : ",car(sc->args));
               }
               if(ivalue(car(sc->args))!=sc->outport) {
                    x=mk_port(sc,sc->outport);
                    x=cons(sc,x,sc->NIL);
                    s_save(sc,OP_SET_OUTPORT, x, sc->NIL);
                    sc->outport=ivalue(car(sc->args));
               }
          }
          putstr(sc, "\n");
          s_return(sc,sc->T);

     case OP_ERR0:  /* error */
          sc->retcode=-1;
          if (!is_string(car(sc->args))) {
               sc->args=cons(sc,mk_string(sc," -- "),sc->args);
               setimmutable(car(sc->args));
          }
          putstr(sc, "Error: ");
          putstr(sc, strvalue(car(sc->args)));
          sc->args = cdr(sc->args);
          s_goto(sc,OP_ERR1);

     case OP_ERR1:  /* error */
          putstr(sc, " ");
          if (sc->args != sc->NIL) {
               s_save(sc,OP_ERR1, cdr(sc->args), sc->NIL);
               sc->args = car(sc->args);
               sc->print_flag = 1;
               s_goto(sc,OP_P0LIST);
          } else {
               putstr(sc, "\n");
               if(sc->interactive_repl) {
                    s_goto(sc,OP_T0LVL);
               } else {
                    return sc->NIL;
               }
          }

     case OP_REVERSE:    /* reverse */
          s_return(sc,reverse(sc, car(sc->args)));

     case OP_APPEND:     /* append */
          if(sc->args==sc->NIL) {
               s_return(sc,sc->NIL);
          }
          x=car(sc->args);
          for (y = cdr(sc->args); y != sc->NIL; y = cdr(y)) {
               x=append(sc,x,car(y));
          }
          s_return(sc,x);

     case OP_PUT:        /* put */
          if (!hasprop(car(sc->args)) || !hasprop(cadr(sc->args))) {
               Error_0(sc,"Illegal use of put");
          }
          for (x = symprop(car(sc->args)), y = cadr(sc->args); x != sc->NIL; x = cdr(x)) {
               if (caar(x) == y) {
                    break;
               }
          }
          if (x != sc->NIL)
               cdar(x) = caddr(sc->args);
          else
               symprop(car(sc->args)) = cons(sc, cons(sc, y, caddr(sc->args)),
                                symprop(car(sc->args)));
          s_return(sc,sc->T);

     case OP_GET:        /* get */
          if (!hasprop(car(sc->args)) || !hasprop(cadr(sc->args))) {
               Error_0(sc,"Illegal use of get");
          }
          for (x = symprop(car(sc->args)), y = cadr(sc->args); x != sc->NIL; x = cdr(x)) {
               if (caar(x) == y) {
                    break;
               }
          }
          if (x != sc->NIL) {
               s_return(sc,cdar(x));
          } else {
               s_return(sc,sc->NIL);
          }

     case OP_QUIT:       /* quit */
          if(is_pair(sc->args) && is_number(car(sc->args))) {
               sc->retcode=ivalue(car(sc->args));
          }
          return (sc->NIL);

     case OP_GC:         /* gc */
          gc(sc, sc->NIL, sc->NIL);
          s_return(sc,sc->T);

     case OP_GCVERB:          /* gc-verbose */
     {    int  was = sc->gc_verbose;
          
          sc->gc_verbose = (car(sc->args) != sc->F);
          s_retbool(was);
     }

     case OP_NEWSEGMENT: /* new-segment */
          if (!is_pair(sc->args) || !is_number(car(sc->args))) {
               Error_0(sc,"new-segment : argument must be number");
          }
          fprintf(OUTF, "allocate %d new segments\n",
               alloc_cellseg(sc, (int) ivalue(car(sc->args))));
          s_return(sc,sc->T);

     case OP_OBLIST: /* oblist */
          s_return(sc,sc->oblist);

     case OP_CURR_INPORT: /* current-input-port */
          s_return(sc,mk_port(sc,sc->inport));

     case OP_CURR_OUTPORT: /* current-output-port */
          s_return(sc,mk_port(sc,sc->outport));

     case OP_OPEN_INFILE: /* open-input-file */
     case OP_OPEN_OUTFILE: /* open-output-file */
     case OP_OPEN_INOUTFILE: /* open-input-output-file */ {
          int prop;
          int i;
          if (!is_string(car(sc->args))) {
               Error_1(sc,"open-*-file : not a string",car(sc->args));
          }
          switch(op) {
               case OP_OPEN_INFILE:     prop=port_input; break;
               case OP_OPEN_OUTFILE:    prop=port_output; break;
               case OP_OPEN_INOUTFILE: prop=port_input|port_output; break;
          }
          i=port_from_file(sc,strvalue(car(sc->args)),prop);
          if(i==-1) {
               s_return(sc,sc->F);
          }
          s_return(sc,mk_port(sc,i));
     }
     
#if USE_STRING_PORTS
     case OP_OPEN_INSTRING: /* open-input-string */
     case OP_OPEN_OUTSTRING: /* open-output-string */
     case OP_OPEN_INOUTSTRING: /* open-input-output-string */ {
          int prop;
          int i;
          if (!is_string(car(sc->args))) {
               Error_1(sc,"open-*-string : not a string",car(sc->args));
          }
          switch(op) {
               case OP_OPEN_INSTRING:     prop=port_input; break;
               case OP_OPEN_OUTSTRING:    prop=port_output; break;
               case OP_OPEN_INOUTSTRING:  prop=port_input|port_output; break;
          }
          i=port_from_string(sc,strvalue(car(sc->args)),0,prop);
          if(i==-1) {
               s_return(sc,sc->F);
          }
          s_return(sc,mk_port(sc,i));
     }
#endif

     case OP_CLOSE_INPORT: /* close-input-port */
          if(!is_inport(car(sc->args))) {
               Error_1(sc,"close-input-port : not an input port : ",car(sc->args));
          }
          port_close(sc,ivalue(car(sc->args)),port_input);
          s_return(sc,sc->T);

     case OP_CLOSE_OUTPORT: /* close-output-port */
          if(!is_outport(car(sc->args))) {
               Error_1(sc,"close-output-port : not an input port : ",car(sc->args));
          }
          port_close(sc,ivalue(car(sc->args)),port_output);
          s_return(sc,sc->T);

     case OP_INT_ENV: /* interaction-environment */
          s_return(sc,sc->global_env);

     case OP_CURR_ENV: /* current-environment */
          s_return(sc,sc->envir);

     }
}

static pointer opexe_5(scheme *sc, int op) {
     pointer x;

     if(sc->nesting!=0) {
          int n=sc->nesting;
          sc->nesting=0;
          sc->retcode=-1;
          Error_1(sc,"Unmatched parentheses : ",mk_integer(sc,n));
     }

     switch (op) {
     /* ========== reading part ========== */
     case OP_READ:
          if(!is_pair(sc->args)) {
               s_goto(sc,OP_READ_INTERNAL);
          }
          if(!is_inport(car(sc->args))) {
               Error_1(sc,"read : not an input port :",car(sc->args));
          }
          if(ivalue(car(sc->args))==sc->inport) {
               s_goto(sc,OP_READ_INTERNAL);
          }
          x=mk_port(sc,sc->inport);
          sc->inport=ivalue(car(sc->args));
          x=cons(sc,x,sc->NIL);
          s_save(sc,OP_SET_INPORT, x, sc->NIL);
          s_goto(sc,OP_READ_INTERNAL);

     case OP_READ_CHAR: /* read-char */
     case OP_PEEK_CHAR: /* peek-char */ {
          int c;
          if(is_pair(sc->args)) {
               if(!is_inport(car(sc->args))) {
                    Error_1(sc,"read-char/peek-char : not an input port : ",car(sc->args));
               }
               if(ivalue(car(sc->args))!=sc->inport) {
                    x=mk_port(sc,sc->inport);
                    x=cons(sc,x,sc->NIL);
                    s_save(sc,OP_SET_INPORT, x, sc->NIL);
                    sc->inport=ivalue(car(sc->args));
               }
          }
          c=inchar(sc);
          if(c==EOF) {
               s_return(sc,sc->EOF_OBJ);
          }
          if(sc->op==OP_PEEK_CHAR) {
               backchar(sc,c);
          }
          s_return(sc,mk_character(sc,c));
     }

     case OP_CHAR_READY: /* char-ready? */ {
          int iport=sc->inport;
          int res;
          if(is_pair(sc->args)) {
               if(!is_inport(car(sc->args))) {
                    Error_1(sc,"read-char/peek-char : not an input port : ",car(sc->args));
               }
               iport=ivalue(car(sc->args));
          }
          res=sc->ports[iport].kind&port_string;
          s_retbool(res);
     }

     case OP_SET_INPORT: /* set-input-port */
          if(!is_inport(car(sc->args))) {
               Error_1(sc,"set-input-port : not an input port : ",car(sc->args));
          }
          sc->inport=ivalue(car(sc->args));
          s_return(sc,sc->value);

     case OP_SET_OUTPORT: /* set-output-port */
          if(!is_outport(car(sc->args))) {
               Error_1(sc,"set-output-port : not an output port : ",car(sc->args));
          }
          sc->outport=ivalue(car(sc->args));
          s_return(sc,sc->value);

     case OP_RDSEXPR:
          switch (sc->tok) {
          case TOK_EOF:
               if(sc->inport==-1) {
                    sc->args=sc->NIL;
                    s_goto(sc,OP_QUIT);
               } else {
                    s_return(sc,sc->EOF_OBJ);
               }
          case TOK_COMMENT: {
               int c;
               while ((c=inchar(sc)) != '\n' && c!=EOF)
                    ;
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          }
#if USE_VERBATIM
          case TOK_VERBATIM: {
               int c;
               char x[2]="?";
               while ((c=inchar(sc)) != '\n') {
                    x[0]=c;
                    putstr(sc,x);
               }
               putstr(sc,CRNL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          }
#endif
          case TOK_VEC:
               s_save(sc,OP_RDVEC,sc->NIL,sc->NIL);
               /* fallthrough */
          case TOK_LPAREN:
               sc->tok = token(sc);
               if (sc->tok == TOK_RPAREN) {
                    s_return(sc,sc->NIL);
               } else if (sc->tok == TOK_DOT) {
                    Error_0(sc,"syntax error -- illegal dot expression");
               } else {
                    sc->nesting_stack[sc->file_i]++;
                    s_save(sc,OP_RDLIST, sc->NIL, sc->NIL);
                    s_goto(sc,OP_RDSEXPR);
               }
          case TOK_QUOTE:
               s_save(sc,OP_RDQUOTE, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_BQUOTE:
               s_save(sc,OP_RDQQUOTE, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_COMMA:
               s_save(sc,OP_RDUNQUOTE, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_ATMARK:
               s_save(sc,OP_RDUQTSP, sc->NIL, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          case TOK_ATOM:
               s_return(sc,mk_atom(sc, readstr_upto(sc, "();\t\n ")));
          case TOK_DQUOTE:
               x=mk_string(sc, readstrexp(sc));
               setimmutable(x);
               s_return(sc,x);
          case TOK_SHARP: {
               pointer hdl=mk_symbol(sc, "*sharp-hook*");
               pointer f=find_slot_in_env(sc,sc->envir,hdl);
               if(f==sc->NIL) {
                    Error_0(sc,"Undefined sharp expression");
               } else {
                    sc->code=cons(sc,cdr(f),sc->NIL);
                    s_goto(sc,OP_EVAL);
               }
          }
          case TOK_SHARP_CONST:
               if ((x = mk_sharp_const(sc, readstr_upto(sc, "();\t\n "))) == sc->NIL) {
                    Error_0(sc,"Undefined sharp expression");
               } else {
                    s_return(sc,x);
               }
          default:
               Error_0(sc,"syntax error -- illegal token");
          }
          break;

     case OP_RDLIST: {
          int c;
          sc->args = cons(sc, sc->value, sc->args);
          sc->tok = token(sc);
          if (sc->tok == TOK_COMMENT) {
               int c;
               while ((c=inchar(sc)) != '\n' && c!=EOF)
                    ;
               sc->tok = token(sc);
          }
#if USE_VERBATIM
          if (sc->tok == TOK_VERBATIM) {
               int c;
               char x[2]="?";
               while ((c=inchar(sc)) != '\n') {
                    x[0]=c;
                    putstr(sc,x);
               }
               putstr(sc,CRNL);
               sc->tok = token(sc);
          }
#endif
          if (sc->tok == TOK_RPAREN) {
               sc->nesting_stack[sc->file_i]--;
               s_return(sc,reverse_in_place(sc, sc->NIL, sc->args));
          } else if (sc->tok == TOK_DOT) {
               s_save(sc,OP_RDDOT, sc->args, sc->NIL);
               sc->tok = token(sc);
               s_goto(sc,OP_RDSEXPR);
          } else {
               s_save(sc,OP_RDLIST, sc->args, sc->NIL);;
               s_goto(sc,OP_RDSEXPR);
          }
     }

     case OP_RDDOT:
          if (token(sc) != TOK_RPAREN) {
               Error_0(sc,"syntax error -- illegal dot expression");
          } else {
               sc->nesting_stack[sc->file_i]--;
               s_return(sc,reverse_in_place(sc, sc->value, sc->args));
          }

     case OP_RDQUOTE:
          s_return(sc,cons(sc, sc->QUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDQQUOTE:
          s_return(sc,cons(sc, sc->QQUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDUNQUOTE:
          s_return(sc,cons(sc, sc->UNQUOTE, cons(sc, sc->value, sc->NIL)));

     case OP_RDUQTSP:
          s_return(sc,cons(sc, sc->UNQUOTESP, cons(sc, sc->value, sc->NIL)));

     case OP_RDVEC:
          /*sc->code=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
          s_goto(sc,OP_EVAL); Cannot be quoted*/
          x=cons(sc,mk_proc(sc,OP_VECTOR),sc->value);
          s_return(sc,x);

     /* ========== printing part ========== */
     case OP_P0LIST:
          if(is_vector(sc->args)) {
               putstr(sc,"#(");
               sc->args=cons(sc,sc->args,mk_integer(sc,0));
               s_goto(sc,OP_PVECFROM);
          } else if(is_environment(sc->args)) {
               putstr(sc,"#<ENVIRONMENT>");
               s_return(sc,sc->T);
          } else if (!is_pair(sc->args)) {
               printatom(sc, sc->args, sc->print_flag);
               s_return(sc,sc->T);
          } else if (car(sc->args) == sc->QUOTE && ok_abbrev(cdr(sc->args))) {
               putstr(sc, "'");
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0LIST);
          } else if (car(sc->args) == sc->QQUOTE && ok_abbrev(cdr(sc->args))) {
               putstr(sc, "`");
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0LIST);
          } else if (car(sc->args) == sc->UNQUOTE && ok_abbrev(cdr(sc->args))) {
               putstr(sc, ",");
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0LIST);
          } else if (car(sc->args) == sc->UNQUOTESP && ok_abbrev(cdr(sc->args))) {
               putstr(sc, ",@");
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0LIST);
          } else {
               putstr(sc, "(");
               s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
               sc->args = car(sc->args);
               s_goto(sc,OP_P0LIST);
          }

     case OP_P1LIST:
          if (is_pair(sc->args)) {
               s_save(sc,OP_P1LIST, cdr(sc->args), sc->NIL);
               putstr(sc, " ");
               sc->args = car(sc->args);
               s_goto(sc,OP_P0LIST);
          } else {
               if (sc->args != sc->NIL) {
                    putstr(sc, " . ");
                    printatom(sc, sc->args, sc->print_flag);
               }
               putstr(sc, ")");
               s_return(sc,sc->T);
          }
     case OP_PVECFROM: {
          int i=ivalue_unchecked(cdr(sc->args));
          pointer vec=car(sc->args);
          int len=ivalue_unchecked(vec);
          if(i==len) {
               putstr(sc,")");
               s_return(sc,sc->T);
          } else {
               pointer elem=vector_elem(vec,i);
               ivalue_unchecked(cdr(sc->args))=i+1;
               s_save(sc,OP_PVECFROM, sc->args, sc->NIL);
               sc->args=elem;
               putstr(sc," ");
               s_goto(sc,OP_P0LIST);
          }
     }

     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);

     }
     return sc->T;
}

static pointer opexe_6(scheme *sc, int op) {
     pointer x, y;
     long v;

     switch (op) {
     case OP_LIST_LENGTH:     /* length */   /* a.k */
          v=list_length(sc,car(sc->args));
          if(v<0) {
               Error_1(sc,"length : not a list ",car(sc->args));
          }
          s_return(sc,mk_integer(sc, v));

     case OP_ASSQ:       /* assq */     /* a.k */
          x = car(sc->args);
          for (y = cadr(sc->args); is_pair(y); y = cdr(y)) {
               if (!is_pair(car(y))) {
                    Error_0(sc,"Unable to handle non pair element");
               }
               if (x == caar(y))
                    break;
          }
          if (is_pair(y)) {
               s_return(sc,car(y));
          } else {
               s_return(sc,sc->F);
          }
          
     case OP_PRINT_WIDTH:     /* print-width */   /* a.k */
          sc->w = 0;
          sc->args = car(sc->args);
          sc->print_flag = -1;
          s_goto(sc,OP_P0_WIDTH);
          
     case OP_P0_WIDTH:
          if (!is_pair(sc->args)) {
               sc->w += printatom(sc, sc->args, sc->print_flag);
               s_return(sc,mk_integer(sc, sc->w));
          } else if (car(sc->args) == sc->QUOTE
                  && ok_abbrev(cdr(sc->args))) {
               ++sc->w;
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          } else if (car(sc->args) == sc->QQUOTE
                  && ok_abbrev(cdr(sc->args))) {
               ++sc->w;
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          } else if (car(sc->args) == sc->UNQUOTE
                  && ok_abbrev(cdr(sc->args))) {
               ++sc->w;
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          } else if (car(sc->args) == sc->UNQUOTESP
                  && ok_abbrev(cdr(sc->args))) {
               sc->w += 2;
               sc->args = cadr(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          } else {
               ++sc->w;
               s_save(sc,OP_P1_WIDTH, cdr(sc->args), sc->NIL);
               sc->args = car(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          }
          
     case OP_P1_WIDTH:
          if (is_pair(sc->args)) {
               s_save(sc,OP_P1_WIDTH, cdr(sc->args), sc->NIL);
               ++sc->w;
               sc->args = car(sc->args);
               s_goto(sc,OP_P0_WIDTH);
          } else {
               if (sc->args != sc->NIL)
                    sc->w += 3 + printatom(sc, sc->args, sc->print_flag);
               ++sc->w;
               s_return(sc,mk_integer(sc, sc->w));
          }
          
     case OP_GET_CLOSURE:     /* get-closure-code */   /* a.k */
          sc->args = car(sc->args);
          if (sc->args == sc->NIL) {
               s_return(sc,sc->F);
          } else if (is_closure(sc->args)) {
               s_return(sc,cons(sc, sc->LAMBDA, closure_code(sc->value)));
          } else if (is_macro(sc->args)) {
               s_return(sc,cons(sc, sc->LAMBDA, closure_code(sc->value)));
          } else {
               s_return(sc,sc->F);
          }
     case OP_CLOSUREP:        /* closure? */
          /*
           * Note, macro object is also a closure.
           * Therefore, (closure? <#MACRO>) ==> #t
           */
          s_retbool(is_closure(car(sc->args)));
     case OP_MACROP:          /* macro? */
          s_retbool(is_macro(car(sc->args)));
     default:
          sprintf(sc->strbuff, "%d is illegal operator", sc->op);
          Error_0(sc,sc->strbuff);
     }
     return sc->T; /* NOTREACHED */
}

typedef pointer (*dispatch_func)(scheme *, int);

typedef struct {
 dispatch_func func;
 char *name;
 int min_arity;
 int max_arity;
} op_code_info;

#define INF_ARG 0xffff

static op_code_info dispatch_table[]= {
     { opexe_0, "load", 1, 1}, /* OP_LOAD = 0, */
     { opexe_0, 0, 0, 0}, /* OP_T0LVL, */
     { opexe_0, 0, 0, 0}, /* OP_T1LVL, */
     { opexe_0, 0, 0, 0}, /* OP_READ_INTERNAL, */
     { opexe_0, "gensym", 0, 0}, /* OP_GENSYM, */
     { opexe_0, 0, 0, 0}, /* OP_VALUEPRINT, */
     { opexe_0, 0, 0, 0}, /* OP_EVAL, */
     { opexe_0, 0, 0, 0}, /* OP_E0ARGS, */
     { opexe_0, 0, 0, 0}, /* OP_E1ARGS, */
     { opexe_0, 0, 0, 0}, /* OP_APPLY, */
     { opexe_0, 0, 0, 0}, /* OP_DOMACRO, */
     { opexe_0, 0, 0, 0}, /* OP_LAMBDA, */
     { opexe_0, 0, 0, 0}, /* OP_QUOTE, */
     { opexe_0, 0, 0, 0}, /* OP_DEF0, */
     { opexe_0, 0, 0, 0}, /* OP_DEF1, */
     { opexe_0, "defined?", 1, 2}, /* OP_DEFP, */
     { opexe_0, 0, 0, 0}, /* OP_BEGIN, */
     { opexe_0, 0, 0, 0}, /* OP_IF0, */
     { opexe_0, 0, 0, 0}, /* OP_IF1, */
     { opexe_0, 0, 0, 0}, /* OP_SET0, */
     { opexe_0, 0, 0, 0}, /* OP_SET1, */
     { opexe_0, 0, 0, 0}, /* OP_LET0, */
     { opexe_0, 0, 0, 0}, /* OP_LET1, */
     { opexe_0, 0, 0, 0}, /* OP_LET2, */
     { opexe_0, 0, 0, 0}, /* OP_LET0AST, */
     { opexe_0, 0, 0, 0}, /* OP_LET1AST, */
     { opexe_0, 0, 0, 0}, /* OP_LET2AST, */
     { opexe_1, 0, 0, 0}, /* OP_LET0REC, */
     { opexe_1, 0, 0, 0}, /* OP_LET1REC, */
     { opexe_1, 0, 0, 0}, /* OP_LETREC2, */
     { opexe_1, 0, 0, 0}, /* OP_COND0, */
     { opexe_1, 0, 0, 0}, /* OP_COND1, */
     { opexe_1, 0, 0, 0}, /* OP_DELAY, */
     { opexe_1, 0, 0, 0}, /* OP_AND0, */
     { opexe_1, 0, 0, 0}, /* OP_AND1, */
     { opexe_1, 0, 0, 0}, /* OP_OR0, */
     { opexe_1, 0, 0, 0}, /* OP_OR1, */
     { opexe_1, 0, 0, 0}, /* OP_C0STREAM, */
     { opexe_1, 0, 0, 0}, /* OP_C1STREAM, */
     { opexe_1, 0, 0, 0}, /* OP_MACRO0, */
     { opexe_1, 0, 0, 0}, /* OP_MACRO1, */
     { opexe_1, 0, 0, 0}, /* OP_CASE0, */
     { opexe_1, 0, 0, 0}, /* OP_CASE1, */
     { opexe_1, 0, 0, 0}, /* OP_CASE2, */
     { opexe_1, "eval", 1, 2}, /* OP_PEVAL, */
     { opexe_1, "apply", 1, 2}, /* OP_PAPPLY, */
     { opexe_1, "call-with-current-continuation", 1, 1}, /* OP_CONTINUATION, */
#if USE_MATH
     { opexe_2, "inexact->exact", 1, 1}, /* OP_INEX2EX, */
     { opexe_2, "exp", 1, 1}, /* OP_EXP, */
     { opexe_2, "log", 1, 1}, /* OP_LOG, */
     { opexe_2, "sin", 1, 1}, /* OP_SIN, */
     { opexe_2, "cos", 1, 1}, /* OP_COS, */
     { opexe_2, "tan", 1, 1}, /* OP_TAN, */
     { opexe_2, "asin", 1, 1}, /* OP_ASIN, */
     { opexe_2, "acos", 1, 1}, /* OP_ACOS, */
     { opexe_2, "atan", 1, 2}, /* OP_ATAN, */
     { opexe_2, "sqrt", 1, 1}, /* OP_SQRT, */
     { opexe_2, "expt", 2, 2}, /* OP_EXPT, */
     { opexe_2, "floor", 1, 1}, /* OP_FLOOR, */
     { opexe_2, "ceiling", 1, 1}, /* OP_CEILING, */
     { opexe_2, "truncate", 1, 1}, /* OP_TRUNCATE, */
     { opexe_2, "round", 1, 1}, /* OP_ROUND, */
#endif
     { opexe_2, "+", 0, INF_ARG}, /* OP_ADD, */
     { opexe_2, "-", 1, INF_ARG}, /* OP_SUB, */
     { opexe_2, "*", 0, INF_ARG}, /* OP_MUL, */
     { opexe_2, "/", 1, INF_ARG}, /* OP_DIV, */
     { opexe_2, "quotient", 1, INF_ARG}, /* OP_INTDIV, */
     { opexe_2, "remainder", 2, 2}, /* OP_REM, */
     { opexe_2, "modulo", 2, 2}, /* OP_MOD, */
     { opexe_2, "car", 1, 1}, /* OP_CAR, */
     { opexe_2, "cdr", 1, 1}, /* OP_CDR, */
     { opexe_2, "cons", 2, 2}, /* OP_CONS, */
     { opexe_2, "set-car!", 2, 2}, /* OP_SETCAR, */
     { opexe_2, "set-cdr!", 2, 2}, /* OP_SETCDR, */
     { opexe_2, "char->integer", 1, 1}, /* OP_CHAR2INT, */
     { opexe_2, "integer->char", 1, 1}, /* OP_INT2CHAR, */
     { opexe_2, "char-upcase", 1, 1}, /* OP_CHARUPCASE, */
     { opexe_2, "char-downcase", 1, 1}, /* OP_CHARDNCASE, */
     { opexe_2, "symbol->string", 1, 1}, /* OP_STR2SYM, */
     { opexe_2, "string->symbol", 1, 1}, /* OP_SYM2STR, */
     { opexe_2, "make-string", 1, 2}, /* OP_MKSTRING, */
     { opexe_2, "string-length", 1, 1}, /* OP_STRLEN */
     { opexe_2, "string-ref", 2, 2}, /* OP_STRREF */
     { opexe_2, "string-set!", 3, 3}, /* OP_STRSET */
     { opexe_2, "substring", 2, 3}, /* OP_SUBSTR */
     { opexe_2, "vector", 0, INF_ARG}, /* OP_VECTOR */
     { opexe_2, "make-vector", 1, 2}, /* OP_MKVECTOR */
     { opexe_2, "vector-length", 1, 1}, /* OP_VECLEN */
     { opexe_2, "vector-ref", 2, 2}, /* OP_VECREF */
     { opexe_2, "vector-set!", 3, 3}, /* OP_VECSET */
     { opexe_3, "not", 1, 1}, /* OP_NOT, */
     { opexe_3, "boolean?", 1, 1}, /* OP_BOOLP, */
     { opexe_3, "eof-object?", 1, 1}, /* OP_EOFOBJP, */
     { opexe_3, "null?", 1, 1}, /* OP_NULLP, */
     { opexe_3, "=", 2, INF_ARG}, /* OP_NUMEQ, */
     { opexe_3, "<", 2, INF_ARG}, /* OP_LESS, */
     { opexe_3, ">", 2, INF_ARG}, /* OP_GRE, */
     { opexe_3, "<=", 2, INF_ARG}, /* OP_LEQ, */
     { opexe_3, ">=", 2, INF_ARG}, /* OP_GEQ, */
     { opexe_3, "symbol?", 1, 1}, /* OP_SYMBOLP, */
     { opexe_3, "number?", 1, 1}, /* OP_NUMBERP, */
     { opexe_3, "string?", 1, 1}, /* OP_STRINGP, */
     { opexe_3, "integer?", 1, 1}, /* OP_INTEGERP, */
     { opexe_3, "real?", 1, 1}, /* OP_REALP, */
     { opexe_3, "char?", 1, 1}, /* OP_CHARP */
#if USE_CHAR_CLASSIFIERS
     { opexe_3, "char-alphabetic?", 1, 1}, /* OP_CHARAP */
     { opexe_3, "char-numeric?", 1, 1}, /* OP_CHARNP */
     { opexe_3, "char-whitespace?", 1, 1}, /* OP_CHARWP */
     { opexe_3, "char-upper-case?", 1, 1}, /* OP_CHARUP */
     { opexe_3, "char-lower-case?", 1, 1}, /* OP_CHARLP */
#endif
     { opexe_3, "port?", 1, 1}, /* OP_PORTP */
     { opexe_3, "input-port?", 1, 1}, /* OP_INPORTP */
     { opexe_3, "output-port?", 1, 1}, /* OP_OUTPORTP */
     { opexe_3, "procedure?", 1, 1}, /* OP_PROCP, */
     { opexe_3, "pair?", 1, 1}, /* OP_PAIRP, */
     { opexe_3, "list?", 1, 1}, /* OP_LISTP, */
     { opexe_3, "environment?", 1, 1}, /* OP_ENVP, */
     { opexe_3, "vector?", 1, 1}, /* OP_VECTORP, */
     { opexe_3, "eq?", 2, 2}, /* OP_EQ, */
     { opexe_3, "eqv?", 2, 2}, /* OP_EQV, */
     { opexe_4, "force", 1, 1}, /* OP_FORCE, */
     { opexe_4, 0, 0, 0}, /* OP_SAVE_FORCED, */
     { opexe_4, "write", 1, 2}, /* OP_WRITE, */
     { opexe_4, "write-char", 1, 2}, /* OP_WRITE_CHAR, */
     { opexe_4, "display", 1, 2}, /* OP_DISPLAY, */
     { opexe_4, "newline", 0, 1}, /* OP_NEWLINE, */
     { opexe_4, "error", 1, INF_ARG}, /* OP_ERR0, */
     { opexe_4, 0, 0, 0}, /* OP_ERR1, */
     { opexe_4, "reverse", 1, 1}, /* OP_REVERSE, */
     { opexe_4, "append", 0, INF_ARG}, /* OP_APPEND, */
     { opexe_4, "put", 3, 3}, /* OP_PUT, */
     { opexe_4, "get", 2, 2}, /* OP_GET, */
     { opexe_4, "quit", 0, 1}, /* OP_QUIT, */
     { opexe_4, "gc", 0, 0}, /* OP_GC, */
     { opexe_4, "gc-verbose", 0, 1}, /* OP_GCVERB, */
     { opexe_4, "new-segment", 0, 1}, /* OP_NEWSEGMENT, */
     { opexe_4, "oblist", 0, 0}, /* OP_OBLIST, */
     { opexe_4, "current-input-port", 0, 0}, /* OP_CURR_INPORT, */
     { opexe_4, "current-output-port", 0, 0}, /* OP_CURR_OUTPORT, */
     { opexe_4, "open-input-file", 1, 1}, /* OP_OPEN_INFILE, */
     { opexe_4, "open-output-file", 1, 1}, /* OP_OPEN_OUTFILE, */
     { opexe_4, "open-input-output-file", 1, 1}, /* OP_OPEN_INOUTFILE, */
#if USE_STRING_PORTS
     { opexe_4, "open-input-string", 1, 1}, /* OP_OPEN_INSTRING, */
     { opexe_4, "open-output-string", 1, 1}, /* OP_OPEN_OUTSTRING, */
     { opexe_4, "open-input-output-string", 1, 1}, /* OP_OPEN_INOUTSTRING, */
#endif
     { opexe_4, "close-input-port", 1, 1}, /* OP_CLOSE_INPORT, */
     { opexe_4, "close-output-port", 1, 1}, /* OP_CLOSE_OUTPORT, */
     { opexe_4, "interaction-environment", 0, 0}, /* OP_INT_ENV, */
     { opexe_4, "current-environment", 0, 0}, /* OP_CURR_ENV, */
     { opexe_5, "read", 0, 1}, /* OP_READ, */
     { opexe_5, "read-char", 0, 1}, /* OP_READ_CHAR, */
     { opexe_5, "peek-char", 0, 1}, /* OP_PEEK_CHAR, */
     { opexe_5, "char-ready?", 0, 1}, /* OP_CHAR_READY, */
     { opexe_5, "set-input-port", 1, 1}, /* OP_SET_INPORT, */
     { opexe_5, "set-output-port", 1, 1}, /* OP_SET_OUTPORT, */
     { opexe_5, 0, 0, 0}, /* OP_RDSEXPR, */
     { opexe_5, 0, 0, 0}, /* OP_RDLIST, */
     { opexe_5, 0, 0, 0}, /* OP_RDDOT, */
     { opexe_5, 0, 0, 0}, /* OP_RDQUOTE, */
     { opexe_5, 0, 0, 0}, /* OP_RDQQUOTE, */
     { opexe_5, 0, 0, 0}, /* OP_RDUNQUOTE, */
     { opexe_5, 0, 0, 0}, /* OP_RDUQTSP, */
     { opexe_5, 0, 0, 0}, /* OP_RDVEC, */
     { opexe_5, 0, 0, 0}, /* OP_P0LIST, */
     { opexe_5, 0, 0, 0}, /* OP_P1LIST, */
     { opexe_5, 0, 0, 0}, /* OP_PVECFROM, */
     { opexe_6, "length", 1, 1}, /* OP_LIST_LENGTH, */
     { opexe_6, "assq", 2, 2}, /* OP_ASSQ, */
     { opexe_6, "print-width", 1, 1}, /* OP_PRINT_WIDTH, */
     { opexe_6, 0, 0, 0}, /* OP_P0_WIDTH, */
     { opexe_6, 0, 0, 0}, /* OP_P1_WIDTH, */
     { opexe_6, "get-closure-code", 1, 1}, /* OP_GET_CLOSURE, */
     { opexe_6, "closure?", 1, 1}, /* OP_CLOSUREP, */
     { opexe_6, "macro?", 1, 1} /* OP_MACROP, */
};

static const char *procname(pointer x) {
 int n=procnum(x);
 const char *name=dispatch_table[n].name;
 if(name==0) {
     name="ILLEGAL!";
 }
 return name;
}

/* kernel of this intepreter */
static void Eval_Cycle(scheme *sc, int op) {
     int count=0;

     sc->op = op;
     for (;;) {
          op_code_info *pcd=dispatch_table+sc->op;
          if (pcd->name!=0) { /* built-in function */
               char msg[512];
               int ok=1;
               int n=list_length(sc,sc->args);

               /* Check number of arguments */
               if(n<pcd->min_arity) {
                    ok=0;
                    sprintf(msg,"%s : needs%s %d arguments",
                         pcd->name,
                         pcd->min_arity==pcd->max_arity?"":" at least",
                         pcd->min_arity);
               }
               if(n>pcd->max_arity) {
                    ok=0;
                    sprintf(msg,"%s : needs%s %d arguments",
                         pcd->name,
                         pcd->min_arity==pcd->max_arity?"":" at most",
                         pcd->max_arity);
               }
               if(!ok) {
                    if(_Error_1(sc,msg,0)==sc->NIL) {
                         return;
                    }
                    pcd=dispatch_table+sc->op;
               }
          }
          if (pcd->func(sc, sc->op) == sc->NIL)
               return;
          if(sc->no_memory) {
               fprintf(stderr,"No memory!\n");
               return;
          }
          count++;
     }
}

/* ========== Initialization of internal keywords ========== */

static void assign_syntax(scheme *sc, unsigned int op, char *name) {
     pointer x;

     x = immutable_cons(sc, mk_string(sc, name), sc->NIL);
     typeflag(x) = (T_SYNTAX | T_SYMBOL);
     syntaxnum(x) = op;
     setimmutable(x);
     setimmutable(car(x));
     sc->oblist = immutable_cons(sc, x, sc->oblist);
}

static void assign_proc(scheme *sc, unsigned int op, char *name) {
     pointer x, y;

     x = mk_symbol(sc, name);
     y = mk_proc(sc,op);
     car(sc->global_env) = immutable_cons(sc, immutable_cons(sc, x, y), car(sc->global_env));
}

static pointer mk_proc(scheme *sc, unsigned int op) {
     pointer y;

     y = get_cell(sc, sc->NIL, sc->NIL);
     typeflag(y) = (T_PROC | T_ATOM);
     ivalue_unchecked(y) = (long) op;
     set_integer(y);
     return y;
}

static void init_vars_global(scheme *sc) {
     pointer x;

     /* init sc->NIL */
     typeflag(sc->NIL) = (T_ATOM | MARK);
     car(sc->NIL) = cdr(sc->NIL) = sc->NIL;
     /* init T */
     typeflag(sc->T) = (T_ATOM | MARK);
     car(sc->T) = cdr(sc->T) = sc->T;
     /* init F */
     typeflag(sc->F) = (T_ATOM | MARK);
     car(sc->F) = cdr(sc->F) = sc->F;
     /* init global_env */
     sc->global_env = immutable_cons(sc, sc->NIL, sc->NIL);
     setenvironment(sc->global_env);
     /* init else */
     x = mk_symbol(sc,"else");
     car(sc->global_env) = immutable_cons(sc, immutable_cons(sc, x, sc->T), car(sc->global_env));
}

static void init_syntax(scheme *sc) {
     assign_syntax(sc, OP_LAMBDA, "lambda");
     assign_syntax(sc, OP_QUOTE, "quote");
     assign_syntax(sc, OP_DEF0, "define");
     assign_syntax(sc, OP_IF0, "if");
     assign_syntax(sc, OP_BEGIN, "begin");
     assign_syntax(sc, OP_SET0, "set!");
     assign_syntax(sc, OP_LET0, "let");
     assign_syntax(sc, OP_LET0AST, "let*");
     assign_syntax(sc, OP_LET0REC, "letrec");
     assign_syntax(sc, OP_COND0, "cond");
     assign_syntax(sc, OP_DELAY, "delay");
     assign_syntax(sc, OP_AND0, "and");
     assign_syntax(sc, OP_OR0, "or");
     assign_syntax(sc, OP_C0STREAM, "cons-stream");
     assign_syntax(sc, OP_MACRO0, "macro");
     assign_syntax(sc, OP_CASE0, "case");
}

static void init_procs(scheme *sc) {
     int i, n=sizeof(dispatch_table)/sizeof(dispatch_table[0]);
     for(i=0; i<n; i++) {
          if(dispatch_table[i].name!=0) {
               assign_proc(sc, i, dispatch_table[i].name);
          }
     }
}

/* initialize several globals */
static void init_globals(scheme *sc) {
     init_vars_global(sc);
     init_syntax(sc);
     init_procs(sc);
     /* intialization of global pointers to special symbols */
     sc->LAMBDA = mk_symbol(sc, "lambda");
     sc->QUOTE = mk_symbol(sc, "quote");
     sc->QQUOTE = mk_symbol(sc, "quasiquote");
     sc->UNQUOTE = mk_symbol(sc, "unquote");
     sc->UNQUOTESP = mk_symbol(sc, "unquote-splicing");
     sc->FEED_TO = mk_symbol(sc, "=>");
}

/* initialization of Mini-Scheme */
int scheme_init(scheme *sc) {
     int i;
     num_zero.is_fixnum=1;
     num_zero.value.ivalue=0;
     num_one.is_fixnum=1;
     num_one.value.ivalue=1;

     sc->gensym_cnt=0;
     sc->malloc=malloc;
     sc->free=free;
     sc->last_cell_seg = -1;
     sc->sink = &sc->_sink;
     sc->NIL = &sc->_NIL;
     sc->T = &sc->_T;
     sc->F = &sc->_F;
     sc->EOF_OBJ=&sc->_EOF_OBJ;
     sc->oblist = &sc->_NIL;
     sc->free_cell = &sc->_NIL;
     sc->fcells = 0;
     sc->no_memory=0;
     sc->last_ff=-1;
     sc->inport=0;
     sc->outport=1;
     for(i=0; i<FILENUM; i++) {
          sc->ports[i].kind=port_free;
     }
     sc->nesting=0;
     sc->interactive_repl=0;

     if (alloc_cellseg(sc,FIRST_CELLSEGS) != FIRST_CELLSEGS) {
          sc->no_memory=1;
          return 0;
     }
#if USE_VERBOSE_GC
     sc->gc_verbose = 1;
#else
     sc->gc_verbose = 0;
#endif
     init_globals(sc);
     sc->dump = sc->NIL;
     sc->envir = sc->global_env;
     sc->code = sc->NIL;
     return !sc->no_memory;
}

void scheme_set_input_port_file(scheme *sc, FILE *fin) {
     sc->ports[sc->inport].kind=port_file|port_input;
     sc->ports[sc->inport].rep.file=fin;
}

void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end) {
     sc->ports[sc->inport].kind=port_string|port_input;
     sc->ports[sc->inport].rep.string.start=start;
     sc->ports[sc->inport].rep.string.curr=start;
     sc->ports[sc->inport].rep.string.past_the_end=past_the_end;
}

void scheme_set_output_port_file(scheme *sc, FILE *fin) {
     sc->ports[sc->outport].kind=port_file|port_output;
     sc->ports[sc->outport].rep.file=fin;
}

void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end) {
     sc->ports[sc->outport].kind=port_string|port_output;
     sc->ports[sc->outport].rep.string.start=start;
     sc->ports[sc->outport].rep.string.curr=start;
     sc->ports[sc->outport].rep.string.past_the_end=past_the_end;
}

void scheme_set_external_data(scheme *sc, void *p) {
 sc->ext_data=p;
}

void scheme_deinit(scheme *sc) {
 int i;

 sc->dump=sc->NIL;
 sc->envir=sc->NIL;
 sc->code=sc->NIL;
 sc->args=sc->NIL;
 sc->value=sc->NIL;
 gc(sc,sc->NIL,sc->NIL);

 for(i=0; i<=sc->last_cell_seg; i++) {
     sc->free(sc->cell_seg[i]);
 }

}

void scheme_custom_alloc(scheme *sc, func_alloc malloc, func_dealloc free) {
     sc->malloc=malloc;
     sc->free=free;
}

void scheme_load(scheme *sc, FILE *fin) {
     sc->dump = sc->NIL;
     sc->envir = sc->global_env;
     sc->file_i=0;
     sc->file_stack[0]=fin;
     sc->retcode=0;
     if(fin==stdin) {
          sc->interactive_repl=1;
     }
     Eval_Cycle(sc, OP_T0LVL);
     if(sc->retcode==0) {
          sc->retcode=sc->nesting!=0;
     }
}

void scheme_define(scheme *sc, pointer symbol, pointer value) {
     pointer x;

     pointer envir= sc->global_env;

     for (x = car(envir); x != sc->NIL; x = cdr(x)) {
          if (caar(x) == symbol) {
               break;
          }
     }
     if (x != sc->NIL) {
          cdar(x) = value;
     } else {
          car(envir) = immutable_cons(sc, immutable_cons(sc, symbol, value), car(envir));
     }
}

#if !STANDALONE
void assign_foreign(scheme *sc, foreign_func func, char *name) {
     pointer x, y;

     if(sc->last_ff==FFNUM-1) {
          return;
     }
     sc->last_ff++;
     sc->ff[sc->last_ff]=func;

     x = mk_symbol(sc, name);
     y = get_cell(sc, sc->NIL, sc->NIL);
     typeflag(y) = (T_FOREIGN | T_ATOM);
     ivalue_unchecked(y) = (long) sc->last_ff;
     set_integer(y);
     car(sc->global_env) = immutable_cons(sc, immutable_cons(sc, x, y), car(sc->global_env));
}

void scheme_apply0(scheme *sc, const char *procname) {
     pointer carx=mk_symbol(sc,procname);
     pointer cdrx=sc->NIL;

     sc->dump = sc->NIL;
     sc->envir = sc->global_env;
     sc->code = cons(sc,carx,cdrx);
     sc->interactive_repl=0;
     sc->retcode=0;
     Eval_Cycle(sc,OP_EVAL);
}

#endif

/* ========== Main ========== */

#if STANDALONE

int main(int argc, char **argv)
{
     scheme sc;
     FILE *fin;
     char *file_name=InitFile;
     int retcode;

     if(argc==1) {
          printf(banner);
     }
     if(argc==2 && strcmp(argv[1],"-?")==0) {
          printf("Usage: %s [-? | <file1> <file2> ... | -1 <file> <arg1> <arg2> ...]\n\tUse - as filename for stdin.\n",argv[0]);
          return 1;
     }
     if(!scheme_init(&sc)) {
          fprintf(stderr,"Could not initialize!\n");
          return 2;
     }
     scheme_set_input_port_file(&sc, stdin);
     scheme_set_output_port_file(&sc, stdout);
     argv++;
     do {
          if(strcmp(file_name,"-")==0) {
               fin=stdin;
          } else if(strcmp(file_name,"-1")==0) {
               pointer args=sc.NIL;
               file_name=*argv++;
               if(strcmp(file_name,"-")==0) {
                    fin=stdin;
               } else {
                    fin=fopen(file_name,"r");
               }
               for(;*argv;argv++) {
                    pointer value=mk_string(&sc,*argv);
                    args=cons(&sc,value,args);
               }
               args=reverse_in_place(&sc,sc.NIL,args);
               scheme_define(&sc,mk_symbol(&sc,"*args*"),args);
          } else {
               fin=fopen(file_name,"r");
          }
          if(fin==0) {
               fprintf(stderr,"Could not open file %s\n",file_name);
          } else {
               scheme_load(&sc,fin);
               if(fin!=stdin) {
                    if(sc.retcode!=0) {
                         printf("Errors encountered reading %s\n",file_name);
                    }
                    fclose(fin);
               }
          }
          file_name=*argv++;
     } while(file_name!=0);
     if(argc==1) {
          scheme_load(&sc,stdin);
     }
     retcode=sc.retcode;
     scheme_deinit(&sc);

     return retcode;
}

#endif
