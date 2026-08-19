CC ?= gcc
AR ?= ar
prefix ?= /usr/local

all: libgreet.a

greet.o: greet.c greet.h
	$(CC) -c greet.c -o greet.o

libgreet.a: greet.o
	$(AR) rcs libgreet.a greet.o

install: all
	mkdir -p $(prefix)/include $(prefix)/lib
	cp greet.h $(prefix)/include/
	cp libgreet.a $(prefix)/lib/

.PHONY: all install
