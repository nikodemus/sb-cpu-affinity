all: sb-sched.so

sb-sched.so: sb-sched.o
	ld -shared -o sb-sched.so sb-sched.o

sb-sched.o: sb-sched.c
	gcc -c sb-sched.c -o sb-sched.o -fPIC
