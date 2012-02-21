#ifndef SCHEME_FFI_HEADER
#define SCHEME_FFI_HEADER 1

#include "scheme.h"
#include "scheme-private.h"

#define dbgmsg(msg)					\
  do {							\
    fprintf(stderr,__FILE__ ":%d: " msg, __LINE__);	\
    fflush(stderr);					\
  } while(0)

#define getarg(a,type,conv)					\
  do{								\
    pointer p;							\
    if(args == sc->NIL){					\
      dbgmsg("Missing argument of type "#type"!\n");		\
      return sc->F;						\
    }								\
    p = pair_car(args);						\
    args = pair_cdr(args);					\
    if(!is_##type(p)){						\
      dbgmsg("Argument should be of type "#type"!\n");		\
      return sc->F;						\
    }								\
    (a) = conv(p);						\
  }while(0)

#define getnarg(a)                                              \
  do{                                                           \
    pointer p;                                                  \
    if(args == sc->NIL){                                        \
      dbgmsg("Missing argument of type number!");               \
      return sc->F;                                             \
    }                                                           \
    p = pair_car(args);                                         \
    args = pair_cdr(args);                                      \
    if(!is_number(p)){                                          \
      dbgmsg("Argument should be of type number!");             \
      return sc->F;                                             \
    } else if(is_integer(p)){                                   \
      (a) = ivalue(p);                                          \
    } else if(is_real(p)){                                      \
      (a) = rvalue(p);                                          \
    }                                                           \
  }while(0)

#define narg(a) getnarg(a)
#define iarg(a) getarg(a,integer,ivalue)
#define rarg(a) getarg(a,real,rvalue)
#define sarg(a) getarg(a,string,string_value)
#define parg(a) getarg(a,foreignptr,fpvalue)
#define symarg(a) getarg(a,symbol,symname)

#define internfun(symbol,fptr)			\
  scheme_define(sc				\
		,sc->global_env			\
		,mk_symbol(sc,symbol)		\
		,mk_foreign_func(sc,fptr))

#define internval(symbol,pval)			\
  scheme_define(sc				\
		,sc->global_env			\
		,mk_symbol(sc,symbol)		\
		,pval)

#define internconst(symbol,pval)		\
  do{						\
    pointer s = mk_symbol(sc,symbol);		\
    pointer v = pval;				\
    setimmutable(s);				\
    setimmutable(v);				\
    scheme_define(sc,sc->global_env,s,v);	\
  }while(0)

#define defun(name) pointer name(scheme* sc, pointer args)

#endif/* ! SCHEME_FFI_HEADER */
