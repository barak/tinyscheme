/* SCHEME.H */

#ifndef _SCHEME_H
#define _SCHEME_H

#include <stdio.h>

/*
 * Default values for #define'd symbols
 */
#ifndef STANDALONE       /* If used as standalone interpreter */
# define STANDALONE 0
#endif

#ifndef _MSC_VER 
# define USE_STRCASECMP 1 
# define USE_STRLWR 1 
#else 
# define USE_STRCASECMP 0 
# define USE_STRLWR 0 
#endif

#if USE_NO_FEATURES
# define USE_MATH 0
# define USE_CHAR_CLASSIFIERS 0
# define USE_ASCII_NAMES 0
# define USE_STRING_PORTS 0
# define USE_ERROR_HOOK 0
# define USE_TRACING 0
# define USE_COLON_HOOK 0
# define USE_DL 0
#endif

#ifndef USE_MATH         /* If math support is needed */
# define USE_MATH 1
#endif

#ifndef USE_CHAR_CLASSIFIERS  /* If char classifiers are needed */
# define USE_CHAR_CLASSIFIERS 1
#endif

#ifndef USE_ASCII_NAMES  /* If extended escaped characters are needed */
# define USE_ASCII_NAMES 1
#endif

#ifndef USE_STRING_PORTS      /* Enable string ports */
# define USE_STRING_PORTS 1
#endif

#ifndef USE_TRACING
# define USE_TRACING 1
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
# define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier */
# define USE_COLON_HOOK 1
#endif

#ifndef USE_STRCASECMP   /* stricmp for Unix */
# define USE_STRCASECMP 0
#endif

#ifndef USE_STRLWR
# define USE_STRLWR 1
#endif

#ifndef STDIO_ADDS_CR    /* Define if DOS/Windows */
# define STDIO_ADDS_CR 0
#endif

#ifndef INLINE
# define INLINE
#endif

typedef struct scheme scheme;
typedef struct cell *pointer;

typedef void * (*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

int scheme_init(scheme *sc);
int scheme_init_custom_alloc(scheme *sc, func_alloc, func_dealloc);
void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_load_file(scheme *sc, FILE *fin);
void scheme_load_string(scheme *sc, char *cmd);
void scheme_apply0(scheme *sc, const char *procname);
void scheme_set_external_data(scheme *sc, void *p);
void scheme_define(scheme *sc, pointer env, pointer symbol, pointer value);

/*------------------ Ugly internals -----------------------------------*/
/*------------------ Of interest only to FFI users --------------------*/

/* num, for generic arithmetic */
typedef struct num {
     char is_fixnum;
     union {
          long ivalue;
          double rvalue;
     } value;
} num;

enum { port_free=0, port_file=1, port_string=2, port_input=16, port_output=32 };

typedef struct port {
  unsigned char kind;
  union {
    struct {
      FILE *file;
      int closeit;
    } stdio;
    struct {
      char *start;
      char *past_the_end;
      char *curr;
    } string;
  } rep;
} port;

typedef pointer (*foreign_func)(scheme *, pointer);

/* cell structure */
struct cell {
  unsigned int _flag;
  union {
    struct {
      char   *_svalue;
      int   _length;
    } _string;
    num _number;
    port *_port;
    foreign_func _ff;
    struct {
      struct cell *_car;
      struct cell *_cdr;
    } _cons;
  } _object;
};

struct scheme {
/* arrays for segments */
func_alloc malloc;
func_dealloc free;

/* return code */
int retcode;
int tracing;

#define CELL_SEGSIZE    5000  /* # of cells in one segment */
#define CELL_NSEGMENT   10    /* # of segments for cells */
char *alloc_seg[CELL_NSEGMENT];
pointer cell_seg[CELL_NSEGMENT];
int     last_cell_seg;

/* We use 4 registers. */
pointer args;            /* register for arguments of function */
pointer envir;           /* stack register for current environment */
pointer code;            /* register for current code */
pointer dump;            /* stack register for next evaluation */

int interactive_repl;    /* are we in an interactive REPL? */

struct cell _sink;
pointer sink;            /* when mem. alloc. fails */
struct cell _NIL;
pointer NIL;             /* special cell representing empty cell */
struct cell _T;
pointer T;               /* special cell representing #t */
struct cell _F;
pointer F;               /* special cell representing #f */
struct cell _EOF_OBJ;
pointer EOF_OBJ;         /* special cell representing end-of-file object */
pointer oblist;          /* pointer to symbol table */
pointer global_env;      /* pointer to global environment */

/* global pointers to special symbols */
pointer LAMBDA;               /* pointer to syntax lambda */
pointer QUOTE;           /* pointer to syntax quote */

pointer QQUOTE;               /* pointer to symbol quasiquote */
pointer UNQUOTE;         /* pointer to symbol unquote */
pointer UNQUOTESP;       /* pointer to symbol unquote-splicing */
pointer FEED_TO;         /* => */
pointer COLON_HOOK;      /* *colon-hook* */
pointer ERROR_HOOK;      /* *error-hook* */
pointer SHARP_HOOK;  /* *sharp-hook* */

pointer free_cell;       /* pointer to top of free cells */
long    fcells;          /* # of free cells */

pointer inport;
pointer outport;
pointer save_inport;
pointer loadport;

#define MAXFIL 64
port load_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
int nesting_stack[MAXFIL];
int file_i;
int nesting;

char    gc_verbose;      /* if gc_verbose is not zero, print gc status */
char    no_memory;       /* Whether mem. alloc. has failed */

#define LINESIZE 1024
char    linebuff[LINESIZE];
char    strbuff[256];

FILE *tmpfp;
int tok;
int print_flag;
pointer value;
int op;

void *ext_data;     /* For the benefit of foreign functions */
long gensym_cnt;

struct scheme_interface *interface;
};

pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
#define cons(sc,a,b) _cons(sc,a,b,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)
pointer mk_integer(scheme *sc, long num);
pointer mk_real(scheme *sc, double num);
pointer mk_symbol(scheme *sc, const char *name);
pointer gensym(scheme *sc);
pointer mk_string(scheme *sc, const char *str);
pointer mk_counted_string(scheme *sc, const char *str, int len);
pointer mk_character(scheme *sc, int c);
pointer mk_foreign_func(scheme *sc, foreign_func f);
void putstr(scheme *sc, const char *s);

int is_string(pointer p);
char *string_value(pointer p);
int is_number(pointer p);
num nvalue(pointer p);
long ivalue(pointer p);
double rvalue(pointer p);
int is_integer(pointer p);
int is_real(pointer p);
int is_character(pointer p);
long charvalue(pointer p);
int is_vector(pointer p);

int is_port(pointer p);

int is_pair(pointer p);
pointer pair_car(pointer p);
pointer pair_cdr(pointer p);
pointer set_car(pointer p, pointer q);
pointer set_cdr(pointer p, pointer q);

int is_symbol(pointer p);
char *symname(pointer p);
int hasprop(pointer p);

int is_syntax(pointer p);
int is_proc(pointer p);
int is_foreign(pointer p);
char *syntaxname(pointer p);
int is_closure(pointer p);
#ifdef USE_MACRO
int is_macro(pointer p);
#endif
pointer closure_code(pointer p);
pointer closure_env(pointer p);

int is_continuation(pointer p);
int is_promise(pointer p);
int is_environment(pointer p);
int is_immutable(pointer p);
void setimmutable(pointer p);

#if USE_DL
struct scheme_interface {
void (*scheme_define)(scheme *sc, pointer env, pointer symbol, pointer value);
pointer (*cons)(scheme *sc, pointer a, pointer b);
pointer (*immutable_cons)(scheme *sc, pointer a, pointer b);
pointer (*mk_integer)(scheme *sc, long num);
pointer (*mk_real)(scheme *sc, double num);
pointer (*mk_symbol)(scheme *sc, const char *name);
pointer (*gensym)(scheme *sc);
pointer (*mk_string)(scheme *sc, const char *str);
pointer (*mk_counted_string)(scheme *sc, const char *str, int len);
pointer (*mk_character)(scheme *sc, int c);
pointer (*mk_vector)(scheme *sc, int len);
pointer (*mk_foreign_func)(scheme *sc, foreign_func f);
void (*putstr)(scheme *sc, const char *s);
void (*putcharacter)(scheme *sc, int c);

int (*is_string)(pointer p);
char *(*string_value)(pointer p);
int (*is_number)(pointer p);
num (*nvalue)(pointer p);
long (*ivalue)(pointer p);
double (*rvalue)(pointer p);
int (*is_integer)(pointer p);
int (*is_real)(pointer p);
int (*is_character)(pointer p);
long (*charvalue)(pointer p);
int (*is_vector)(pointer p);
long (*vector_length)(pointer vec);
void (*fill_vector)(pointer vec, pointer elem);
pointer (*vector_elem)(pointer vec, int ielem);
pointer (*set_vector_elem)(pointer vec, int ielem, pointer newel);
int (*is_port)(pointer p);

int (*is_pair)(pointer p);
pointer (*pair_car)(pointer p);
pointer (*pair_cdr)(pointer p);
pointer (*set_car)(pointer p, pointer q);
pointer (*set_cdr)(pointer p, pointer q);

int (*is_symbol)(pointer p);
char *(*symname)(pointer p);
int (*hasprop)(pointer p);

int (*is_syntax)(pointer p);
int (*is_proc)(pointer p);
int (*is_foreign)(pointer p);
char *(*syntaxname)(pointer p);
int (*is_closure)(pointer p);
int (*is_macro)(pointer p);
pointer (*closure_code)(pointer p);
pointer (*closure_env)(pointer p);

int (*is_continuation)(pointer p);
int (*is_promise)(pointer p);
int (*is_environment)(pointer p);
int (*is_immutable)(pointer p);
void (*setimmutable)(pointer p);
};
#endif

#endif

