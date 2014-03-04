/*
 * Send signal by sigqueue(3)   --  qkill --
 *
 *        Copyright (C) 2014 Daisuke Kobayashi <d5884jp@gmail.com>
 *
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * COMPILATION
 * -------
 * gcc -o qkill.exe qkill.c
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <errno.h>
#include <termios.h>
#include <sys/cygwin.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

#define PROGNAME "qkill"

typedef enum { TRUE = 1, FALSE = 0 } bool_t;

struct parsed_argv {
  bool_t pid_is_winpid; /* using pid as windows pid */
  bool_t verbose;       /* output more verbosely */
  bool_t use_sigqueue;  /* using sigqueue instead of kill */
  char *tty_name;       /* try to send tty's foreground pgrp */
  bool_t excepting;     /* use except pgid */
  pid_t except_pgid;    /* don't send signal when tty's foreground pgrp's pgid is this */
  int signum;           /* signal number */
  int sigval;           /* signal value  */
};

bool_t parse_argv(int argc, char*argv[], struct parsed_argv *result, int *next_index);
void usage();

pid_t get_foreground_pgrp(char *tty_name);

bool_t string_to_integer(const char *str, int *ret);
bool_t string_to_signum(const char *str, int *ret);
bool_t signame_to_signum(const char*signame, int *ret);

int main(int argc, char *argv[])
{
  struct parsed_argv params;
  bool_t succeeded = FALSE;
  int i;


  if (!parse_argv(argc, argv, &params, &i))
    exit(EXIT_FAILURE);

  /* tty's foreground pgid kill mode */
  if (params.tty_name != NULL) {
    pid_t pgid = get_foreground_pgrp(params.tty_name);
    if (pgid <= 0) {
      fprintf(stderr, "%s: Cannot obtain foreground process group on %s - %s\n",
	      PROGNAME, params.tty_name, strerror(errno));
      exit(EXIT_FAILURE);
    }

    if (params.excepting && params.except_pgid == pgid) {
      if (params.verbose)
	fprintf(stderr, "info:tty's pgid = except pgid. dont kill.\n");
    } else {
      if (params.verbose)
	fprintf(stderr, "invoke:kill(pid=%d(%s),signum=%d)\n",
		-pgid, params.tty_name, params.signum);

      if (kill(-pgid, params.signum) != 0) {
	fprintf(stderr, "%s:(%d on %s) %s\n", PROGNAME, pgid, params.tty_name, strerror(errno));
	exit(EXIT_FAILURE);
      }
    }
    exit(EXIT_SUCCESS);
  }

  /* normal kill mode */
  for (; i < argc; i++) {
    pid_t pid;
    union sigval sigval;

    if (!string_to_integer(argv[i], &pid)) {
      fprintf(stderr, "%s: Invalid pid - %s\n", PROGNAME, argv[i]);
      continue;
    }
    if (params.pid_is_winpid) {
      pid_t cyg_pid = cygwin_winpid_to_pid(abs(pid));
      if (cyg_pid < 0) {
	fprintf(stderr, "%s: Not a cygwin process's pid - %s\n", PROGNAME, argv[i]);
	continue;
      }

      pid = pid < 0 ? -cyg_pid : cyg_pid;

      if (params.verbose)
	fprintf(stderr, "convert:win_pid=%s => cyg_pid=%d\n", argv[i], pid);
    }

    if (params.use_sigqueue) {
      if (params.verbose)
	fprintf(stderr, "invoke:sigqueue(pid=%d,signum=%d,sigval=%#08x)\n",
		pid, params.signum, params.sigval);
      sigval.sival_int = params.sigval;
      if (sigqueue(pid, params.signum, sigval) != 0) {
	fprintf(stderr, "%s:(%d) %s\n", PROGNAME, pid, strerror(errno));
      }
    } else {
      if (params.verbose)
	fprintf(stderr, "invoke:kill(pid=%d,signum=%d)\n", pid, params.signum);
      if (kill(pid, params.signum) != 0) {
	fprintf(stderr, "%s:(%d) %s\n", PROGNAME, pid, strerror(errno));
      }
    }

    succeeded = TRUE;
  }

  exit(succeeded ? EXIT_SUCCESS : EXIT_FAILURE);
}

bool_t parse_argv(int argc, char*argv[], struct parsed_argv *result, int *next_index)
{
  bool_t flag_pid_found = FALSE;
  int opt;


  memset(result, 0, sizeof(struct parsed_argv));
  result->signum = SIGTERM;

  opterr = 0; /* suppress auto error */
  while(!flag_pid_found && (opt = getopt(argc, argv, "+vws:S:i:t:e:h")) != -1) {
    switch(opt) {
    case 'w':
      result->pid_is_winpid = TRUE;
      break;
      
    case 's':
    case 'S': /*  for -S[IGSOME] to some */
      if (!string_to_signum(optarg,  &result->signum)) {
	fprintf(stderr, "%s: Unknown signal: %s\n", PROGNAME, optarg);
	return FALSE;
      }
      break;

    case 'i':
      if (!string_to_integer(optarg, &result->sigval)) {
	fprintf(stderr, "%s: Invalid sigval: %s\n", PROGNAME, optarg);
	return FALSE;
      }
      result->use_sigqueue = TRUE;
      break;

    case 't':
      result->tty_name = strdup(optarg);
      break;

    case 'e':
      if (!string_to_integer(optarg, &result->except_pgid)) {
	fprintf(stderr, "%s: Invalid excepting pgid: %s\n", PROGNAME, optarg);
	return FALSE;
      }
      result->excepting = TRUE;
      break;

    case 'v':
      result->verbose = TRUE;
      break;

    case 'h':
      usage();
      return FALSE;

    case '?':
      if (isdigit(optopt)) {
	flag_pid_found = TRUE;
      } else {
	fprintf(stderr, "%s: Invalid option: -%c\n", PROGNAME, optopt);
	return FALSE;
      }
    }
  }

  if (optind >= argc && result->tty_name == NULL) {
    usage();
    return FALSE;
  } else if (optind < argc && result->tty_name != NULL) {
    fprintf(stderr, "%s: Can't specify tty and pid at same time.\n", PROGNAME);
    usage();
    return FALSE;
  }

  *next_index = optind;

  return TRUE;
}

void usage()
{
  fprintf(stderr, "usage: %s [-wv] [-s sigcode] [-i sigval] pid [pids...]\n"
                  "       %s -t <tty-path> [-e pgid]\n\n"
	  "Send signal by sigqueue(3).\n\n"
	  " -w          pid is windows pid\n"
	  " -v          verbose output\n"
	  " -s sigcode  send signal with sigcode (default: SIGTERM)\n"
	  " -i sigval   send signal with sigval\n"
	  " -t tty-path send signal to tty_name's foreground process group\n"
	  " -e pgid     don't send signal when tty's foreground pgid is this pgid\n"
	  " -h          show this help\n\n",
	  PROGNAME,
	  PROGNAME);
}

pid_t get_foreground_pgrp(char *tty_name)
{
  int pty_fd;
  int pipe_fd[2];
  int ret;
  int status;

  pipe(pipe_fd);

  pid_t pid = fork();
  if (pid < 0) {
    return -1;
  } else if (pid == 0) {
    /* discard current ctty */
    if (setsid() < 0) {
      int saved_errno = errno;
      write(pipe_fd[1], &saved_errno, sizeof(saved_errno));
      exit(EXIT_FAILURE);
    }

    /* open target's tty */
    pty_fd = open(tty_name, O_RDWR);
    if (pty_fd < 0) {
      int saved_errno = errno;
      write(pipe_fd[1], &saved_errno, sizeof(saved_errno));
      exit(EXIT_FAILURE);
    }

    /* get foreground pgrp */
    ret = tcgetpgrp(pty_fd);
    if (ret < 0) {
      int saved_errno = errno;
      write(pipe_fd[1], &saved_errno, sizeof(saved_errno));
      exit(EXIT_FAILURE);
    }

    write(pipe_fd[1], &ret, sizeof(ret));

    exit(EXIT_SUCCESS);
  }
  read(pipe_fd[0], &ret, sizeof(ret));
  waitpid(pid, &status, 0);

  if (WIFEXITED(status) && WEXITSTATUS(status) != EXIT_SUCCESS) {
    errno = ret;
    return -1;
  }

  return ret;
}

bool_t string_to_integer(const char *str, int *ret)
{
  char *endptr;

  *ret = strtol(str, &endptr, 10);
  if (endptr == str)
    return FALSE;
  else
    return TRUE;
}

bool_t string_to_signum(const char *str, int *ret)
{
  if (signame_to_signum(str, ret))
    return TRUE;
  
  return string_to_integer(str, ret);
}

bool_t signame_to_signum(const char*signame, int *ret)
{
  char *upname;
  int signum;
  int search_base = 0;
  
  if ((upname = strdup(signame)) == NULL)
    return FALSE;
  strupr(upname);

  if (strstr(upname, "IG") == upname) /* for -s[igsome] to some */
    search_base = 1;
  else if (strstr(upname, "SIG") != upname)
    search_base = 3;
  
  for (signum = 0; signum < NSIG; signum++) {
    if (sys_sigabbrev[signum] != NULL) {
      if (strcmp(upname, sys_sigabbrev[signum] + search_base) == 0) {
	*ret = signum;

	free(upname);
	return TRUE;
      }
    }
  }

  free(upname);
  return FALSE;
}
