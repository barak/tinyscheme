#ifndef SCHEME_FFI_HEADER
#define SCHEME_FFI_HEADER 1

#include "scheme.h"
#include "scheme-private.h"

#define getarg(a,type,conv)					\
  do{								\
    pointer p;							\
    if(args == sc->NIL){					\
      fprintf(stderr,"Missing argument of type "#type"!");	\
      fflush(stderr);						\
      return sc->F;						\
    }								\
    p = pair_car(args);						\
    args = pair_cdr(args);					\
    if(!is_##type(p)){						\
      fprintf(stderr,"Argument should be of type "#type"!");	\
      fflush(stderr);						\
      return sc->F;						\
    }								\
    (a) = conv(p);						\
  }while(0)

#define iarg(a) getarg(a,integer,ivalue)
#define rarg(a) getarg(a,real,rvalue)
#define sarg(a) getarg(a,string,string_value)
#define parg(a) getarg(a,foreignptr,fpvalue)

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
