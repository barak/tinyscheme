# Makefile for TinyScheme 
# Time-stamp: <2002-06-24 14:13:27 gildea> 
  
# Windows/2000 
#CC = cl -nologo 
#DEBUG= -W3 -Z7 -MD 
#DL_FLAGS= -DWIN32 
#SYS_LIBS= 
#Osuf=obj 
#SOsuf=dll 
#EXE_EXT=.exe 
#LD = link -nologo 
#LDFLAGS = -debug -map -dll -incremental:no 
#LIBPREFIX = 
#OUT = -out:$@ 

# Unix, generally 
CC = gcc -fpic 
DEBUG=-g -Wall -Wno-char-subscripts -O 
Osuf=o 
SOsuf=so 
LIBsuf=a
EXE_EXT=
LIBPREFIX=lib
OUT = -o $@ 
 
# Linux 
LD = gcc 
LDFLAGS = -shared 
DEBUG=-g -Wno-char-subscripts -O
SYS_LIBS= -ldl
PLATFORM_FEATURES= -DSUN_DL=1

 
# Solaris 
#SYS_LIBS= -ldl -lc 
#Osuf=o 
#SOsuf=so 
#EXE_EXT= 
#LD = ld 
#LDFLAGS = -G -Bsymbolic -z text 
#LIBPREFIX = lib 
#OUT = -o $@ 
 
FEATURES = $(PLATFORM_FEATURES) -DUSE_DL=1 -DUSE_MATH=0 -DUSE_ASCII_NAMES=0 
 
OBJS = scheme.$(Osuf) dynload.$(Osuf) 
 
LIBTARGET = $(LIBPREFIX)tinyscheme.$(SOsuf) 
STATICLIBTARGET = $(LIBPREFIX)tinyscheme.$(LIBsuf)

all: $(LIBTARGET) $(STATICLIBTARGET) scheme$(EXE_EXT)

%.$(Osuf): %.c 
	$(CC) -I. -c $(DEBUG) $(FEATURES) $(DL_FLAGS) $< 

$(LIBTARGET): $(OBJS) 
	$(LD) $(LDFLAGS) $(OUT) $(OBJS) $(SYS_LIBS) 

scheme$(EXE_EXT): $(OBJS) 
	$(CC) -o $@ $(DEBUG) $(OBJS) $(SYS_LIBS) 

$(STATICLIBTARGET): $(OBJS)
	ar cr $@ $(OBJS)

$(OBJS): scheme.h 
dynload.$(Osuf): dynload.h 

clean: 
	-rm -f $(OBJS) $(LIBTARGET) scheme$(EXE_EXT) 
	-rm -f tinyscheme.dll tinyscheme.ilk tinyscheme.map tinyscheme.pdb 

tags: TAGS 
TAGS: scheme.h scheme.c dynload.h dynload.c 
	etags $^ 
