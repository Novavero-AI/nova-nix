/* Prints the standard it is compiled as: C's __STDC_VERSION__, or C++'s
   __cplusplus, since a C++ driver compiles a .c file as C++.  */
#include <stdio.h>

int
main (void)
{
#ifdef __cplusplus
  printf ("C++ %ld\n", (long) __cplusplus);
#else
  printf ("C %ld\n", (long) __STDC_VERSION__);
#endif
  return 0;
}
