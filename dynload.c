/* dynload.c Dynamic Loader for TinyScheme */
/* Original Copyright (c) 1999 Alexander Shendi     */
/* Modifications for NT and dl_* interface, scm_load_ext: D. Souflis */

#include "scheme.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef MAXPATHLEN
# define MAXPATHLEN 1024
#endif

static void make_filename(const char *name, char *filename); 
static void make_init_fn(const char *name, char *init_fn); 

#ifdef WIN32
# include <windows.h>
#else
typedef void *HMODULE;
typedef void (*FARPROC)();
#include <dlfcn.h>
#endif

#ifdef WIN32

#define PREFIX ""
#define SUFFIX ".dll"

static HMODULE dl_attach(const char *module) {
 return LoadLibrary(module);
}

static FARPROC dl_proc(HMODULE mo, const char *proc) {
 return GetProcAddress(mo,proc);
}

static void dl_detach(HMODULE mo) {
 (void)FreeLibrary(mo);
}

#elif defined(SUN_DL)

#include <dlfcn.h>

#define PREFIX "lib"
#define SUFFIX ".so"

static HMODULE dl_attach(const char *module) {
 return dlopen(module,RTLD_LAZY);
}

static FARPROC dl_proc(HMODULE mo, const char *proc) {
 const char *errmsg;
 FARPROC fp=dlsym(mo,proc);
 if ((errmsg = dlerror()) == 0) {
     return fp;
 }
 return 0;
}

static void dl_detach(HMODULE mo) {
 (void)dlclose(mo);
}
#endif

pointer scm_load_ext(scheme *sc, pointer args)
{
   pointer first_arg;
   pointer retval;
   char filename[MAXPATHLEN], init_fn[MAXPATHLEN+6];
   char *name, *errmsg;
   HMODULE dll_handle;
   void (*module_init)(scheme *sc, assign_foreign_func func);
   
   if ((args != sc->NIL) && is_string((first_arg = pair_car(args)))) {
      name = string_value(first_arg);
      make_filename(name,filename);     
      make_init_fn(name,init_fn);     
      dll_handle = dl_attach(filename);
      if (dll_handle == 0) {
         fprintf(stderr, "Error loading extension: %s \n", dlerror());
         fflush(stderr);
         retval = sc -> F;
      }
      else {
         module_init = dl_proc(dll_handle, init_fn);
         if (module_init != 0) {
            (*module_init)(sc,assign_foreign);
            retval = sc -> T;
         }
         else {
            fprintf(stderr, "Error initializing module: %s \n", dlerror());
            fflush(stderr);
            retval = sc->F;
         }
      }
   }
   else {
      retval = sc -> F;
   }
   
  return(retval);
}

static void make_filename(const char *name, char *filename) {
 strcpy(filename,name);
 strcat(filename,SUFFIX);
}         

static void make_init_fn(const char *name, char *init_fn) {
 const char *p=strrchr(name,'/');
 if(p==0) {
     p=name;
 } else {
     p++;
 }
 strcpy(init_fn,"init_");
 strcat(init_fn,p);
}
