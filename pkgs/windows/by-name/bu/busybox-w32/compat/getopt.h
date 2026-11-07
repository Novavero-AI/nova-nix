/*
 * MinGW-w64's getopt.h (mingw-w64-headers/crt/getopt.h), for getopt.c from
 * mingw-w64-crt/misc, which the package compiles: TinyCC has neither.
 */
#ifndef __GETOPT_H__
#define __GETOPT_H__

extern int optind;
extern int optopt;
extern int opterr;
extern char *optarg;

extern int getopt(int nargc, char * const *nargv, const char *options);

#endif

#if !defined(__UNISTD_H_SOURCED__) && !defined(__GETOPT_LONG_H__)
#define __GETOPT_LONG_H__

struct option
{
  const char *name;
  int has_arg;
  int *flag;
  int val;
};

enum
{
  no_argument = 0,
  required_argument,
  optional_argument
};

extern int getopt_long(int nargc, char * const *nargv, const char *options,
    const struct option *long_options, int *idx);
extern int getopt_long_only(int nargc, char * const *nargv, const char *options,
    const struct option *long_options, int *idx);

#endif
