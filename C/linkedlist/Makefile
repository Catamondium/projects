intList.o: intList.c intList.h
	gcc -Wall -g -c intList.c

libList.a: intList.o
	ar rcs libList.a intList.o

a.out: libList.a
	gcc -Wall -g intList.o -lList -o a.out
