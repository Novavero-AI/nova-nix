CC ?= gcc
prefix ?= /usr/local

all: greeter.exe

greeter.exe: main.c
	$(CC) $(CPPFLAGS) main.c $(LDFLAGS) -lgreet -o greeter.exe

install: all
	mkdir -p $(prefix)/bin
	cp greeter.exe $(prefix)/bin/

.PHONY: all install
