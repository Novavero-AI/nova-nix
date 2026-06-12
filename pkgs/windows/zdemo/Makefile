CC ?= gcc
prefix ?= /usr/local

all: zdemo.exe

zdemo.exe: main.c
	$(CC) $(CPPFLAGS) main.c $(LDFLAGS) -lz -o zdemo.exe

install: all
	mkdir -p $(prefix)/bin
	cp zdemo.exe $(prefix)/bin/

.PHONY: all install
