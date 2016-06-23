/*
 * Fake cygwin pty   --  fakecygpty --
 *
 *        Copyright (C) 2005 Kyotaro Horiguchi <horiguti@meaodwy.org>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either versions 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with fakecygpty, see the file COPYING.  If not, write to the
 * Free Software Foundation Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 */

/*
 * HISTORY
 * -------
 *
 *    09 Jun, 2005 : Version 1.0.0 - first release.
 *    15 Jun, 2005 : Version 1.0.1 - bug fix and change coding style.
 *    15 May, 2009 : Version 1.0.2 - Work around for Windows 7 RC
 *    25 Jan, 2014 : Version 1.1.0 - fix tty attribute like emacs tty.
 *                                   accept SIGWINCH signal for resize.
 *                                   transport some signals to child pid.
 */

/*
 * COMPILATION
 * -------
 * gcc -D_GNU_SOURCE -o fakecygpty.exe fakecygpty.c
 * Note: You must compile it in Cygwin environment. NOT in MinGW32!
 *
 */

/*
 * HOW TO RESIZE TTY WINDOW SIZE?
 * -------
 * Send SIGWINCH signal by sigqueue() with sigval stored window size.
 * window size format: high-16bit => rows, low-16bit => cols
 *
 * ex) set window size cols=140 rows=32 by C code.
 *
 *   union sigval sigval;
 *   pid_t pid = <fakecygpty's pid>;
 *   int cols = 140, rows = 32;
 *
 *   sigval.sival_int = rows << 16 + 0xFFFF & cols;
 *   sigqueue(pid, SIGWINCH, sigval);
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <termios.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <pty.h>

#define BUFSIZE		 4096	/* size of communication buffer */
#define COMMAND_PREFIX	 "f_"
#define MY_NAME "fakecygpty"

/* prototypes */
int open_master_pty(void);

pid_t exec_target(int master_fd, char* argv[], int pty_alloc_only);

void setup_tty_attributes(int tty_fd);
void set_tty_echo_on(int tty_fd);
int resize_tty_window(int tty_fd, int window_size_info);

char *real_command_name(char* my_name);

ssize_t safe_read(int fd, void *buf, size_t count);
ssize_t safe_write_full(int fd, void *buf, size_t count);
ssize_t safe_write_full_checking_eof(int fd, void *buf, size_t length);

void setup_signal_handlers();
void signal_pass_handler(int signum, siginfo_t *info, void *unused);
void sigwinch_handler(int signum, siginfo_t *info, void *unused);

BOOL WINAPI ctrl_handler(DWORD e);

/* signal trapping descriptor */
struct sigtrap_desc {
  int signum;
  void  (*action)(int, siginfo_t *, void *);
};

/* global variables */
int child_pid = -1;	/* pid of child proces  */
int master_fd = -1;	/* fd of pty served to child process */


volatile sig_atomic_t sig_winch_caught = FALSE; /* flag for SIGWINCH caught */
volatile sig_atomic_t sig_window_size = -1;     /* window size info */

/* signals trap required */
struct sigtrap_desc sigtrap_descs[] = {
  { SIGHUP,   signal_pass_handler },
  { SIGINT,   signal_pass_handler },
  { SIGQUIT,  signal_pass_handler },
  { SIGALRM,  signal_pass_handler },
  { SIGTERM,  signal_pass_handler },
  { SIGWINCH, sigwinch_handler    },
  { SIGUSR1,  signal_pass_handler },
  { SIGUSR2,  signal_pass_handler },
  { SIGTSTP,  signal_pass_handler },
  { SIGCONT,  signal_pass_handler }
};

#define SIGTRAP_COUNT (sizeof(sigtrap_descs)/sizeof(struct sigtrap_desc))

/* codes */

int main(int argc, char* argv[])
{
  fd_set sel, sel0;
  int status;
  int pty_alloc_only = FALSE;
  char* newarg0;

  /* SIGINT and SIGBREAK are indistinctive under cygwin environment. */
  /* Using Win32API to handle SIGINT.                               */
  SetConsoleCtrlHandler(ctrl_handler, TRUE);

  if (argc < 1) {
    fputs("Unable to get arg[0].", stderr);
    exit(EXIT_FAILURE);
  }

  newarg0 = real_command_name(argv[0]);

  if (newarg0)
    argv[0] = newarg0;
  else if (argc >=2)
    argv++;
  else
    pty_alloc_only = TRUE;

  if (isatty(STDIN_FILENO) && !pty_alloc_only) {
    execvp(argv[0], argv);
    fprintf(stderr, "Failed to execute \"%s\": %s\n", argv[0], strerror(errno));
    exit(EXIT_FAILURE);
  }

  master_fd = open_master_pty();

  child_pid = exec_target(master_fd, argv, pty_alloc_only);

  setup_signal_handlers();

  FD_ZERO(&sel0);
  FD_SET(master_fd, &sel0);
  FD_SET(STDIN_FILENO, &sel0);

  /* communication loop */
  while (1) {
    char buf[BUFSIZE];
    int ret;

    if (sig_winch_caught == TRUE) {
      sig_winch_caught = FALSE;
      if (child_pid != -1 && resize_tty_window(master_fd, sig_window_size) == 0)
	kill(child_pid, SIGWINCH);
    }

    sel = sel0;
    if (select (FD_SETSIZE, &sel, NULL, NULL, NULL) <= 0) {
      if(errno == EINTR)
	continue;
      else
	break;
    }

    if (FD_ISSET(master_fd, &sel)) {
      ret = safe_read(master_fd, buf, BUFSIZE);
      if (ret > 0) {
	if (safe_write_full(STDOUT_FILENO, buf, ret) < 0)
	  break;
      }
      else
	break;
    }
    else if (FD_ISSET(STDIN_FILENO, &sel)) {
      ret = safe_read(STDIN_FILENO, buf, BUFSIZE);
      if (ret > 0) {
	if (safe_write_full_checking_eof(master_fd, buf, ret) < 0)
	  break;
      } else {
	FD_CLR(STDIN_FILENO, &sel0);
	close(master_fd);
      }
    }
  }

  if (pty_alloc_only) {
    kill(child_pid, SIGKILL);
    status = 0;
  } else {
    while(waitpid(child_pid, &status, 0) < 0 && errno == EINTR)
      ;

    if (WIFEXITED(status))
      status = WEXITSTATUS(status);
    else if(WIFSIGNALED(status)) /* ntemacs cannot distinct killed by signal */
      status = 0x80 +  WTERMSIG(status);
  }

  return status;
}

int open_master_pty(void)
{
  int fd;

  fd = open("/dev/ptmx", O_RDWR);
  if (fd < 0) {
    perror("Failed to open /dev/ptmx");
    return fd;
  }

  setup_tty_attributes(fd);

  return fd;
}

/* Create pty and fork/exec target process */
pid_t exec_target(int master_fd, char* argv[], int pty_alloc_only)
{
  pid_t pid;

  pid = fork();

  if (pid < 0) {
    perror("Failed to fork");
    exit(EXIT_FAILURE);
  }

  if (pid == 0) {
    int slave_fd;
    int i;

    /* new session for obtain ctty */
    if (setsid() < 0) {
      perror("Failed to setsid");
      exit(EXIT_FAILURE);
    }

    slave_fd = open(ptsname(master_fd), O_RDWR);
    close(master_fd);

    if (slave_fd < 0) {
      perror("Failed to open slave pty");
      exit(EXIT_FAILURE);
    }

    if (pty_alloc_only)
      set_tty_echo_on(slave_fd);

    for (i = 0; i < 3; i++) {
      if (slave_fd != i) {
	dup2(slave_fd, i);
	fcntl(i, F_SETFD, 0);
      }
    }
    if (slave_fd > 2)
      close(slave_fd);

    /* make new process group and make it foreground */
    if (setpgid(0, 0) < 0)
      perror("Failed to setpgid");
    if (tcsetpgrp(0, getpgid(getpid())) < 0)
      perror("Failed to change foreground pgid");

    if (pty_alloc_only)
      /* do nothing. wait for kill */
      select(0, NULL, NULL, NULL, NULL);
    else
      execvp(argv[0], argv);

    fprintf(stderr, "Failed to execute \"%s\": %s\n", argv[0], strerror(errno));
    exit(EXIT_FAILURE);
  }

  return pid;
}

void setup_tty_attributes (int tty_fd)
{
  struct termios tm;

  /* set up tty attribute */
  if (tcgetattr(tty_fd, &tm) < 0)
    perror("Faild to tcgetattr");
  else {
    /* setup values from child_setup_tty() in emacs/src/sysdep.c */
    tm.c_iflag &= ~(IUCLC | ISTRIP);
    tm.c_iflag |= IGNCR;
    tm.c_oflag &= ~(ONLCR | OLCUC | TAB3);
    tm.c_oflag |= OPOST;
    tm.c_lflag &= ~ECHO;
    tm.c_lflag |= ISIG | ICANON;
    tm.c_cc[VERASE] = _POSIX_VDISABLE;
    tm.c_cc[VKILL] = _POSIX_VDISABLE;
    tm.c_cc[VEOF] = CTRL('D');

    if (tcsetattr(tty_fd, TCSANOW, &tm) < 0)
      perror("Failed to tcsetattr");
  }
}

void set_tty_echo_on(int tty_fd)
{
  struct termios tm;

  /* set up tty attribute */
  if (tcgetattr(tty_fd, &tm) < 0)
    perror("Faild to tcgetattr");
  else {
    tm.c_lflag |= ECHO;
    if (tcsetattr(tty_fd, TCSANOW, &tm) < 0)
      perror("Failed to tcsetattr");
  }
}


int resize_tty_window(int fd, int window_size_info)
{
  struct winsize w;
  int ret;

  if (window_size_info >= 0) {
    /* size info: high-16bit => rows, low-16bit => cols */
    w.ws_row = window_size_info >> 16;
    w.ws_col = window_size_info & 0xFFFF;

    do {
      ret = ioctl(fd, TIOCSWINSZ, &w);
    } while (ret < 0 && errno == EINTR);
  }

  return ret;
}

char *real_command_name(char* my_name)
{
  char *p;

  /* Assume mutlbyte characters do not occur here */
  p = strrchr(my_name, '/');
  if (p == NULL)
    p = strrchr(my_name, '\\');

  if (p == NULL)
    p = my_name;
  else
    p++;

  if (strcmp(p, MY_NAME) == 0)
    return NULL;    /* I am invoked as explicit wrapper program */

  if (strncmp(p, COMMAND_PREFIX, strlen (COMMAND_PREFIX)) != 0) {
    fprintf(stderr, "Illegal program name format. \"%s\"\n", my_name);
    exit(1);
  }

  return p + strlen(COMMAND_PREFIX);
}

ssize_t safe_read(int fd, void *buf, size_t count)
{
  ssize_t ret;

  do {
    ret = read(fd, buf, count);
  } while(ret < 0 && errno == EINTR);

  return ret;
}

ssize_t safe_write_full(int fd, void *buf, size_t count)
{
  ssize_t ret;

  do {
    ret = write(fd, buf, count);
    if (ret > 0) {
      buf += ret;
      count -= ret;
    }
  } while(count > 0 && (ret >= 0 || (ret < 0 && errno == EINTR)));

  return ret;
}

/*
 * Workaround for cygwin's 'behavior of EOF detection.
 * On a linux system, write("\n[EOF]somthing") cause EOF, but cygwin is not.
 * so need to recognize indivisually before and after EOF.
 */
ssize_t safe_write_full_checking_eof(int fd, void *buf, size_t length)
{
  struct termios tm;
  char eof_char;
  size_t rest;
  ssize_t ret;
  void *next_eof;

  /* retrieve EOF char on this pty */
  if (tcgetattr(fd, &tm) == 0)
    eof_char = tm.c_cc[VEOF];
  else
    eof_char = _POSIX_VDISABLE;

  if (eof_char == _POSIX_VDISABLE)
    return safe_write_full(fd, buf, length);

  rest = length;

  while (rest > 0 && (next_eof = memchr(buf, eof_char, rest)) != NULL) {
    ret = safe_write_full(fd, buf, next_eof - buf);
    if (ret < 0) return ret;

    /* workaround for flushing input buffer.. */
    /* It seems continuous write(2) calls are combined, so insert sleep. */
    usleep(1);
    ret = safe_write_full(fd, &eof_char, 1);
    if (ret < 0) return ret;

    /* workaround for flushing input buffer.. */
    usleep(1);
    rest = rest - (next_eof - buf + 1);
    buf = next_eof + 1;
  }

  if (rest > 0) {
    ret = safe_write_full(fd, buf, rest);
    if (ret < 0) return ret;
  }

  return length;
}

void setup_signal_handlers()
{
  struct sigaction newsig;
  int i;

  memset(&newsig, 0, sizeof(newsig));
  newsig.sa_flags = SA_SIGINFO;
  sigemptyset(&newsig.sa_mask);

  for (i = 0; i < SIGTRAP_COUNT; i++) {
    newsig.sa_sigaction = sigtrap_descs[i].action;
    if (sigaction(sigtrap_descs[i].signum, &newsig, NULL) < 0)
      fprintf(stderr, "Failed to sigaction on %d: %s\n",
	      sigtrap_descs[i].signum, strerror(errno));
  }
}

/* pass signals to child */
void signal_pass_handler(int signum, siginfo_t *info, void *unused)
{
  union sigval sigval;
  int saved_errno;

  if (child_pid == -1)
    return;

  saved_errno = errno;
  if (info->si_code == SI_QUEUE) {
    sigval = info->si_value;
    sigqueue(child_pid, signum, sigval);
      
  } else {
    kill(child_pid, signum);
  }
  errno = saved_errno;
}

void sigwinch_handler(int signum, siginfo_t *info, void *unused)
{
  if (child_pid == -1)
    return;

  sig_winch_caught = TRUE;
  if (info->si_code == SI_QUEUE)
    sig_window_size = info->si_value.sival_int;
  else
    sig_window_size = -1;
}

/* Signal handler for convert SIGINT into ^C on pty */
/* This seems not able to be done within cygwin POSIX framework */
BOOL WINAPI ctrl_handler(DWORD e)
{
  switch (e) {
  case CTRL_C_EVENT:
    if (master_fd != -1) {
      write(master_fd, "\003", 1);
      return TRUE;
    }
    break;

  case CTRL_CLOSE_EVENT:
    if (child_pid != -1) {
      kill(child_pid, SIGKILL);
      return FALSE;
    }
  }
  return FALSE;
}
