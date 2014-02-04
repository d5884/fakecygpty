#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <unistd.h>
#include <sys/cygwin.h>

typedef enum { TRUE = 1, FALSE = 0 } bool_t;

void start_interactive_mode();
bool_t sigqueue_for_winpid(int win_pid, int signum, int info);
void usage();

bool_t string_to_integer(const char *str, int *ret);
bool_t string_to_signum(const char *str, int *ret);

int str2sig(const char*signame, int *ret);

int main(int argc, char *argv[])
{
  if (argc == 4) {
    int win_pid;
    int signum;
    int info;

    if (!string_to_integer(argv[1], &win_pid) ||
	!string_to_signum(argv[2], &signum) ||
	!string_to_integer(argv[3], &info)){
      fputs("Invalid arguments.\n", stderr);
      usage();
      return 1;
    } else {
      if (sigqueue_for_winpid(win_pid, signum, info) == FALSE)
	return 1;
    }
  } else {
    usage();
  }
  
  return 0;
}

void usage()
{
  fputs("usage: sigqueue <windows pid> <signal number or name> <signal value>\n", stderr);
}

bool_t string_to_signum(const char *str, int *ret)
{
  if (str2sig(str, ret) == 0)
    return TRUE;
  
  return string_to_integer(str, ret);
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

bool_t sigqueue_for_winpid(int win_pid, int signum, int info)
{
  union sigval sigval;
  pid_t cyg_pid = cygwin_winpid_to_pid(win_pid);

  if (cyg_pid < 0)
    {
      fputs("Not a cygwin process.\n", stderr);
      return FALSE;
    }
    
  printf("sigqueue([winpid:%d=>cygpid:%d],%d,%08x)\n", win_pid, cyg_pid, signum, info);

  sigval.sival_int = info;
  if (sigqueue(cyg_pid, signum, sigval) != 0){
    perror("Faild to sigqueue");

    return FALSE;
  }

  return TRUE;
}

int str2sig(const char*signame, int *ret)
{
  char *upname;
  int signum;
  int search_base = 0;
  
  if ((upname = strdup(signame)) == NULL)
    return 1;
  strupr(upname);

  if (strstr(upname, "SIG") != upname)
    search_base = 3;

  for (signum = 0; signum < NSIG; signum++) {
    if (sys_sigabbrev[signum] != NULL) {
      if (strcmp(upname, sys_sigabbrev[signum] + search_base) == 0) {
	*ret = signum;
	free(upname);
	
	return 0;
      }
    }
  }

  free(upname);
  return 1;
}
