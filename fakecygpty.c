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
 */

/*
 * COMPILATION
 * -------
 * gcc -o fakecygpty.exe fakecygpty.c
 *
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <termios.h>

#define BUFSIZE		 1024	/* size of communication buffer */
#define COMMAND_PREFIX	 "f_"
#define MY_NAME "fakecygpty"

/* global variables */
int child_pid;		/* pid of child proces  */
int masterfd;		/* fd of pty served to child process */


/* Console window of child process is made visible on Windows 7 RC, 
   Forcefully make it as workaround... */
BOOL CALLBACK
enum_windows_proc (HWND hWnd, LPARAM pid) {
  DWORD procid;
  GetWindowThreadProcessId (hWnd, &procid);
  
  if (pid == procid)
    ShowWindow (hWnd, SW_HIDE);
}

void
hide_console_window (void)
{
  EnumWindows (enum_windows_proc, GetCurrentProcessId ());
}


/* Create pty and fork/exec target process */
/* This function sets child_pid and masterfd */
void
exec_target(char* argv[])
{
  int fd;
  int pid;

  masterfd = open ("/dev/ptmx", O_RDWR);
  if (masterfd < 0)
    {
      perror("Cannot open pseudo tty");
      exit (1);
    }

  pid = fork ();
  if (pid < 0)
    {
      perror ("Failed to fork");
      return;
    }

  if (pid == 0)
    {
      int slave;

      setsid();

      /* this open raises console window on Windows 7 RC,
	 hide_console_window () hides it. */
      slave = open (ptsname (masterfd), O_RDWR);
      hide_console_window ();

      if (slave < 0)
	{
	  perror ("Failed to open slave fd");
	  exit (1);
	}

    for (fd = 0 ; fd < 3 ; fd++)
      {
	if (slave != fd)
	  {
	    if (dup2 (slave, fd) < 0)
	      {
		perror ("Failed to dup2");
		exit (1);
	      }
	  }
	fcntl (fd, F_SETFD, 0);
      }

    if (slave > 2) close (slave);

    execvp (argv[0], argv);

    fprintf (stderr, "Failed to execute \"%s\".", argv[0]);
    perror ("execvp");
    exit (1);
  }

  child_pid = pid;

  return;
}


struct termios oldtm;

void
setup_tty_attributes (void)
{
  struct termios tm;

  if (tcgetattr (masterfd, &tm) == 0)
    {
      /* Inhibit echo when executed under emacs/windows environment */
      if (! isatty (0))
	{
	  tm.c_iflag |= IGNCR;
	  tm.c_lflag &= ~ECHO;
	}
      tcsetattr (masterfd, TCSANOW, &tm);
    }

  if (tcgetattr (0, &oldtm) == 0)
    {
      tm = oldtm;
      tm.c_iflag &= ~(ICRNL | IXON | IXOFF);
      tm.c_iflag |= IGNBRK;
      tm.c_lflag &= ~(ICANON | ECHO | ISIG | ECHOE);
      tcsetattr (0, TCSANOW, &tm);
    }
}

void
restore_tty_attributes (void)
{
  tcsetattr (0, TCSANOW, &oldtm);
}

char *
real_command_name (char* my_name)
{
  char *p;

  /* Assume mutlbyte characters do not occur here */
  p = strrchr (my_name, '/');
  if (p == NULL)
    {
      p = strrchr (my_name, '\\');

      if (p == NULL)
	p = my_name;
      else
	p++;
    }
  else
    p++;

  if (strcmp (p, MY_NAME) == 0)
    {
      return NULL;    /* I am invoked as explicit wrapper program */
    }

  if (strncmp (p, COMMAND_PREFIX, strlen (COMMAND_PREFIX)) != 0)
    {
      fprintf (stderr, "Illegal program name format. \'%s\'\n", my_name);
      exit (1);
    }

  return p + strlen (COMMAND_PREFIX);
}

/* Signal handler for convert SIGINT into ^C on pty */
/* This seems not able to be done within cygwin POSIX framework */
BOOL WINAPI
ctrl_handler(DWORD e)
{
  switch (e)
    {
    case CTRL_C_EVENT:
      write (masterfd, "\003", 1);
      return TRUE;

    case CTRL_CLOSE_EVENT:
      kill (child_pid, SIGKILL);
      return FALSE;
    }
  return FALSE;
}

int
main (int argc, char* argv[])
{
  struct termios oldtm;
  fd_set sel, sel0;
  int status;
  char* newarg0;

  /* SIGINT and SIGBREAK are indistinctive under cygwin environment. */
  /* Using Win32API to handle SIGINT.                              */
  SetConsoleCtrlHandler (ctrl_handler, TRUE);

  if (argc < 1)
    {
      fprintf (stderr, "Unable to get arg[0].");
      exit (1);
    }

  newarg0 = real_command_name (argv[0]);
  if (newarg0)
    {
      argv[0] = newarg0;
      exec_target (argv);     /* This sets globals masterfd, child_pid */
    }
  else
    exec_target (argv + 1); /* This sets globals masterfd, child_pid */

  setup_tty_attributes ();

  FD_ZERO (&sel0);
  FD_SET (masterfd, &sel0);
  FD_SET (0, &sel0);

  /* communication loop */
  while (1)
    {
      char buf[BUFSIZE];
      int ret;

      sel = sel0;
      if (select (FD_SETSIZE, &sel, NULL, NULL, NULL) <= 0)
	break;

      if (FD_ISSET (masterfd, &sel))
	{
	  ret = read (masterfd, buf, BUFSIZE);
	  if (ret > 0)
	      write (1, buf, ret);
	  else
	    break;
	}
      else if (FD_ISSET (0, &sel))
	{
	  ret = read (0, buf, BUFSIZE);
	  if (ret > 0)
	      write (masterfd, buf, ret);
	  else
	    {
	      FD_CLR (0, &sel0);
	      close (masterfd);
	    }
	}
    }

  restore_tty_attributes ();

  kill (child_pid, SIGKILL);
  waitpid (child_pid, &status, 0);
  return status;
}

