/* pwd.h -- a password database of one entry: the user the process runs
   as, whose home is $HOME or the profile directory (proc.c says how). */

#ifndef BASH_W32_PWD_H
#define BASH_W32_PWD_H

#include <sys/types.h>

struct passwd
{
  char *pw_name;
  char *pw_passwd;
  uid_t pw_uid;
  gid_t pw_gid;
  char *pw_gecos;
  char *pw_dir;
  char *pw_shell;
};

extern struct passwd *getpwnam (const char *);
extern struct passwd *getpwuid (uid_t);
extern struct passwd *getpwent (void);
extern void setpwent (void);
extern void endpwent (void);

#endif /* BASH_W32_PWD_H */
