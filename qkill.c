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
#include <sys/cygwin.h>

#define PROGNAME "qkill"

typedef enum { TRUE = 1, FALSE = 0 } bool_t;
struct parsed_argv {
  bool_t pid_is_winpid;
  bool_t verbose;
  bool_t use_sigqueue;
  int signum;
  int sigval;
};

bool_t parse_argv(int argc, char*argv[], struct parsed_argv *result, int *next_index);
void usage();

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

  for (; i < argc; i++) {
    pid_t pid;
    union sigval sigval;

    if (!string_to_integer(argv[i], &pid)) {
      fprintf(stderr, "%s: Invalid pid - %s\n", PROGNAME, argv[i]);
      continue;
    }
    if (params.pid_is_winpid && (pid = cygwin_winpid_to_pid(pid)) < 0) {
      fprintf(stderr, "%s: Not a cygwin process - %s\n", PROGNAME, argv[i]);
      continue;
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

  
  /* init */
  result->pid_is_winpid = FALSE;
  result->use_sigqueue = FALSE;
  result->verbose = FALSE;
  result->signum = SIGTERM;
  result->sigval = 0;

  opterr = 0; /* suppress auto error */
  while(!flag_pid_found && (opt = getopt(argc, argv, "+vws:i:h")) != -1) {
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

  if (optind >= argc) {
    usage();
    return FALSE;
  }

  *next_index = optind;

  return TRUE;
}

void usage()
{
  fprintf(stderr, "usage: %s [-w] [-s signal]] [-i sigval] pid [pids...]\n",
	  PROGNAME);
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
