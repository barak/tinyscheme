CPPFLAGS += -DSUN_DL=1 -DUSE_DL=1 -DUSE_MATH=1 -DUSE_ASCII_NAMES=0
CPPFLAGS += -DUSE_PLIST=1
CPPFLAGS += -DINLINE=inline
CPPFLAGS += -DInitFile=\"/usr/lib/tinyscheme/init.scm\" 

CPPFLAGS += -Wall -Wno-switch

CFLAGS += -O2

LOADLIBES += -ldl -lm

obj = scheme.o dynload.o

scheme: $(obj)

clean:
	-rm -f $(obj) scheme

.PHONY: all clean
