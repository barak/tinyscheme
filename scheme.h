/* SCHEME.H */

#ifndef _SCHEME_H
#define _SCHEME_H

#include <stdio.h>

#ifdef _T
# undef _T
#endif

/*
 * Default values for #define'd symbols
 */
#ifndef USE_VERBOSE_GC   /* 1 you want verbose GC */
# define USE_VERBOSE_GC 0
#endif

#ifndef STANDALONE       /* If used as standalone interpreter */
# define STANDALONE 0
#endif

#ifndef USE_MATH         /* If math support is needed */
# define USE_MATH 1
#endif

/* To force system errors through user-defined error handling (see *error-hook*) */
#ifndef USE_ERROR_HOOK
# define USE_ERROR_HOOK 1
#endif

#ifndef USE_COLON_HOOK   /* Enable qualified qualifier */
# define USE_COLON_HOOK 1
#endif

#ifndef USE_VERBATIM     /* Enable verbatim comments (for HTML) */
# define USE_VERBATIM 1
#endif

#ifndef USE_STRCASECMP   /* stricmp for Unix */
# define USE_STRCASECMP 0
#endif

#ifndef STDIO_ADDS_CR    /* Define if DOS/Windows */
# define STDIO_ADDS_CR 0
#endif

typedef struct scheme scheme;
typedef struct cell *pointer;

typedef void * (*func_alloc)(size_t);
typedef void (*func_dealloc)(void *);

int scheme_init(scheme *sc);
void scheme_custom_alloc(scheme *sc, func_alloc, func_dealloc);
void scheme_deinit(scheme *sc);
void scheme_set_input_port_file(scheme *sc, FILE *fin);
void scheme_set_input_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_set_output_port_file(scheme *sc, FILE *fin);
void scheme_set_output_port_string(scheme *sc, char *start, char *past_the_end);
void scheme_load(scheme *sc, FILE *fin);
void scheme_apply0(scheme *sc, const char *procname);
void scheme_set_external_data(scheme *sc, void *p);
void scheme_define(scheme *sc, pointer symbol, pointer value);

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

typedef struct {
     unsigned char kind;
     union {
          FILE *file;
          struct {
               char *start;
               char *past_the_end;
               char *curr;
          } string;
     } rep;
} port;

/* cell structure */
struct cell {
     unsigned int _flag;
     union {
          struct {
               char   *_svalue;
               int   _keynum;
          } _string;
          num _number;
          struct {
               struct cell *_car;
               struct cell *_cdr;
          } _cons;
     } _object;
};

typedef pointer (*foreign_func)(scheme *, pointer);

struct scheme {
/* arrays for segments */
func_alloc malloc;
func_dealloc free;

/* return code */
int retcode;

#define CELL_SEGSIZE    5000  /* # of cells in one segment */
#define CELL_NSEGMENT   100   /* # of segments for cells */
pointer cell_seg[CELL_NSEGMENT];
int     last_cell_seg;

#define FFNUM 200             /* Number of foreign functions */
foreign_func ff[FFNUM];
int     last_ff;

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

pointer free_cell;       /* pointer to top of free cells */
long    fcells;          /* # of free cells */

int inport;
int outport;
int save_inport;

#define MAXFIL 64
FILE *file_stack[MAXFIL];     /* Stack of open files for port -1 (LOADing) */
int nesting_stack[MAXFIL];
int file_i;
int nesting;

#define FILENUM 32
port ports[FILENUM];          /* Open files */

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

long w;             /* Used for printing */
};

pointer _cons(scheme *sc, pointer a, pointer b, int immutable);
#define cons(sc,a,b) _cons(sc,a,b,0)
#define immutable_cons(sc,a,b) _cons(sc,a,b,1)
pointer mk_integer(scheme *sc, long num);
pointer mk_real(scheme *sc, double num);
pointer mk_symbol(scheme *sc, const char *name);
pointer gensym(scheme *sc);
pointer mk_string(scheme *sc, const char *str);
pointer mk_character(scheme *sc, int c);
void assign_foreign(scheme *sc, foreign_func, char *name);

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

#define pair_caar(p)          pair_car(pair_car(p))
#define pair_cadr(p)          pair_car(pair_cdr(p))
#define pair_cdar(p)          pair_cdr(pair_car(p))
#define pair_cddr(p)          pair_cdr(pair_cdr(p))

#endif
