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

bool_t string_to_integer(const char *str, int *ret);
bool_t string_to_signum(const char *str, int *ret);
bool_t signame_to_signum(const char*signame, int *ret);

void usage();

int main(int argc, char *argv[])
{
  bool_t flag_winpid = FALSE;
  bool_t flag_signal_specified = FALSE;
  bool_t flag_pid_found = FALSE;
  bool_t flag_verbose = FALSE;
  int signum = 0;
  int sigval_int = 0;
  int opt;
  int i;
  
  opterr = 0; /* suppress auto error */
  while(!flag_pid_found && (opt = getopt(argc, argv, "+vws:i:h")) != -1) {
    switch(opt) {
    case 'w':
      flag_winpid = TRUE;
      break;
      
    case 's':
      if (!string_to_signum(optarg,  &signum)) {
	fprintf(stderr, "%s: Unknown signal: %s\n", PROGNAME, optarg);
	exit(EXIT_FAILURE);
      }
      flag_signal_specified = TRUE;
      break;

    case 'i':
      if (!string_to_integer(optarg, &sigval_int)) {
	fprintf(stderr, "%s: Invalid sigval: %s\n", PROGNAME, optarg);
	exit(EXIT_FAILURE);
      }
      break;

    case 'v':
      flag_verbose = TRUE;
      break;

    case 'h':
      usage();
      exit(EXIT_SUCCESS);

    case '?':
      if (isdigit(optopt)) {
	flag_pid_found = TRUE;
      } else {
	fprintf(stderr, "%s: Invalid option: -%c\n", PROGNAME, optopt);
	exit(EXIT_FAILURE);
      }
    }
  }

  if (!flag_signal_specified)
    signum = SIGTERM;

  if (optind >= argc) {
    usage();
    exit(EXIT_FAILURE);
  }

  for (i = optind; i < argc; i++) {
    pid_t pid;
    union sigval sigval;

    if (!string_to_integer(argv[i], &pid)) {
      fprintf(stderr, "%s: Invalid pid - %s\n", PROGNAME, argv[i]);
    }
    if (flag_winpid && (pid = cygwin_winpid_to_pid(pid)) < 0) {
      fprintf(stderr, "%s: Not a cygwin process - %s\n", PROGNAME, argv[i]);
      exit(EXIT_FAILURE);
    }
    
    if (flag_verbose) {
      fprintf(stderr, "invoke:sigqueue(%d,%d,%d)\n", pid, signum, sigval_int);
    }

    sigval.sival_int = sigval_int;
    if (sigqueue(pid, signum, sigval) != 0) {
      fprintf(stderr, "%s: %s\n", PROGNAME, strerror(errno));
      exit(EXIT_FAILURE);
    }
  }

  exit(EXIT_SUCCESS);
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

  if (strstr(upname, "IG") == upname) /* for -s[igsome] to some*/
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
