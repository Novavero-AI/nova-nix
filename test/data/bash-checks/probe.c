/* probe: what a program bash starts receives.  `probe args STATUS ARG...'
   writes each ARG between brackets on a line of its own, as the C runtime
   parsed the command line bash built, and exits with STATUS; `probe cat'
   copies its standard input to its standard output. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main (int argc, char **argv)
{
  int c, i;

  if (argc >= 2 && strcmp (argv[1], "cat") == 0)
    {
      while ((c = getchar ()) != EOF)
	putchar (c);
      return 0;
    }
  if (argc < 3 || strcmp (argv[1], "args") != 0)
    {
      fprintf (stderr, "usage: probe args STATUS ARG... | probe cat\n");
      return 2;
    }
  for (i = 3; i < argc; i++)
    printf ("[%s]\n", argv[i]);
  return atoi (argv[2]);
}
