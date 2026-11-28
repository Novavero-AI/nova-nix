/* termios.h -- the terminal interface, which a Windows console does not
   have: tcgetattr and tcsetattr fail with ENOTTY, so bash, which builds
   here without job control or line editing, leaves the console as it is.
   The flags are those bash's tty code names. */

#ifndef BASH_W32_TERMIOS_H
#define BASH_W32_TERMIOS_H

typedef unsigned int tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int speed_t;

#define NCCS	32

struct termios
{
  tcflag_t c_iflag;
  tcflag_t c_oflag;
  tcflag_t c_cflag;
  tcflag_t c_lflag;
  cc_t c_cc[NCCS];
};

#define VTIME	5
#define VMIN	6

#define ISTRIP	0x0020
#define INLCR	0x0040
#define ICRNL	0x0100
#define IXON	0x0400

#define OPOST	0x0001
#define ONLCR	0x0004
#define OCRNL	0x0008
#define ONOCR	0x0010
#define ONLRET	0x0020

#define CSIZE	0x0030
#define CS8	0x0030
#define PARENB	0x0100

#define ISIG	0x0001
#define ICANON	0x0002
#define ECHO	0x0008
#define ECHOE	0x0010
#define ECHOK	0x0020
#define ECHONL	0x0040
#define IEXTEN	0x8000

#define TCSANOW		0
#define TCSADRAIN	1
#define TCSAFLUSH	2

extern int tcgetattr (int, struct termios *);
extern int tcsetattr (int, int, const struct termios *);

#endif /* BASH_W32_TERMIOS_H */
