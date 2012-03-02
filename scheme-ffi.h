#ifndef SCHEME_FFI_HEADER
#define SCHEME_FFI_HEADER 1

#include "scheme.h"
#include "scheme-private.h"

#include <stdio.h>
#define dbgmsg(msg)                                    \
  do {                                                 \
    fprintf(stderr,__FILE__ ":%d: " msg, __LINE__);    \
    fflush(stderr);                                    \
  } while(0)

/* Convenience macros to manage (foreign_ptr*) */
#define hastag(fp,tag) ((fp)->_tag == (tag))
#define hasptr(fp,tag) ((fp)->_ptr && hastag(fp,tag))
#define getptr(fp) ((fp)->_ptr)
#define clrptr(fp) do{(fp)->_ptr=NULL;(fp)->_fin=NULL;(fp)->_tag=0;}while(0)

/* Convenience macros to handle foreign function arguments. */
#define getarg(a,type,conv)                                     \
  do{                                                           \
    pointer p;                                                  \
    if(args == sc->NIL){                                        \
      dbgmsg("Missing argument of type "#type"!\n");            \
      return sc->F;                                             \
    }                                                           \
    p = pair_car(args);                                         \
    args = pair_cdr(args);                                      \
    if(!is_##type(p)){                                          \
      dbgmsg("Argument should be of type "#type"!\n");          \
      return sc->F;                                             \
    }                                                           \
    (a) = conv(p);                                              \
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

/* Convenience macros to internalize objects into Scheme. */

#define internfun(symbol,fptr)                  \
  scheme_define(sc                              \
                ,sc->global_env                 \
                ,mk_symbol(sc,symbol)           \
                ,mk_foreign_func(sc,fptr))

#define internval(symbol,pval)                  \
  scheme_define(sc                              \
                ,sc->global_env                 \
                ,mk_symbol(sc,symbol)           \
                ,pval)

#define internconst(symbol,pval)                \
  do{                                           \
    pointer s = mk_symbol(sc,symbol);           \
    pointer v = pval;                           \
    setimmutable(s);                            \
    setimmutable(v);                            \
    scheme_define(sc,sc->global_env,s,v);       \
  }while(0)


/* FFI finalizer function. */
typedef void(*ffi_finalizer)(scheme*sc);

#define FFI_FINALIZER_TAG 0xD1EFF10B
#define internfinalizer(symbol,finptr)          \
  do{                                           \
    foreign_ptr fp;                             \
    fp._ptr = sc;                               \
    fp._fin = (finalizer)finptr;                \
    fp._tag = FFI_FINALIZER_TAG;                \
    internconst(symbol,mk_foreign_ptr(sc,&fp)); \
  }while(0)

/* Convenience macros to define foreign functions. */
#define defun(name) pointer name(scheme* sc, pointer args)

/* Convenience functions to manage vectors. */
#define newvec(size) sc->vptr->mk_vector(sc,(size))
#define setvec(v,i,e) sc->vptr->set_vector_elem((v),(i),(e))

#endif/* ! SCHEME_FFI_HEADER */
