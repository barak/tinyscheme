DEBUG=-g
#DEBUG=

all: scheme

.c.o:
	gcc -I . -c $(DEBUG) -DUSE_DL=1 -DSUN_DL $+

scheme.o: scheme.c
	gcc -c $(DEBUG) -DSTANDALONE=1 -DUSE_DL=1 scheme.c

scheme: scheme.o dynload.o
	gcc -o scheme $(DEBUG) -lm $+ -ldl

