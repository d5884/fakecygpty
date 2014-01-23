#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/cygwin.h>

typedef enum { TRUE = 1, FALSE = 0 } bool_t;


void start_interactive_mode();
bool_t sigqueue_for_winpid(int win_pid, int signum, int info);
void usage();

int string_to_integer(const char *str, int *ret);

int main(int argc, char *argv[])
{
  if (argc == 4) {
    int win_pid;
    int signum;
    int info;

    if (!string_to_integer(argv[1], &win_pid) ||
	!string_to_integer(argv[2], &signum) ||
	!string_to_integer(argv[3], &info)){
      fputs("Invalid arguments.\n", stderr);
      usage();
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
  fputs("usage: sighelpr [windows pid] [signal number] [signal value]\n", stderr);
}

int string_to_integer(const char *str, int *ret)
{
  char *endptr;

  *ret = strtol(str, &endptr, 10);
  if (endptr == str)
    return 0;
  else
    return 1;
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
