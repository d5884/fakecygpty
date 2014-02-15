;;; fakecygpty.el --- execute cygwin pty commands using pipe.

;; Copyright (C) 2014  Daisuke Kobayashi

;; Author: Daisuke Kobayashi <d5884jp@gmail.com>
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup fakecygpty nil
  "Execute cygwin pty commands using pipe."
  :group 'processes)

(defcustom fakecygpty-program (executable-find "fakecygpty")
  "Program file name of fakecygpty."
  :group 'fakecygpty
  :type 'file)

(defcustom fakecygpty-sigqueue-program (executable-find "sigqueue")
  "Program file name of sigqueue."
  :group 'fakecygpty
  :type 'file)

(defvar fakecygpty-enabled t
  "If non-nil, `start-process' invokes program by fakecygpty."

(defvar fakecygpty--activated nil
  "If non-nil, fakecygpty is already activated.")

(defun fakecygpty-process-p (process)
  "Non-nil when PROCESS was invoked by fakecygpty."
  (and (processp process)
       (process-get process :fakecygpty-p)))

(defun fakecygpty-sigqueue (process sigcode &optional sigval)
  "Send PROCESS the signal with code SIGCODE by `fakecygpty-sigqueue-program'.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.
SIGVAL may be integer. if it's nil, 0 will be used."
  (let ((pid (cond
	      ((integerp process)
	       process)
	      ((processp process)
	       (process-id process))
	      ((stringp process)
	       (ignore-errors (process-id (get-process process))))
	      ((null process)
	       (ignore-errors (process-id (get-buffer-process
					   (current-buffer)))))
	      (t nil))))
    (when pid
      (= 0 (call-process fakecygpty-sigqueue-program nil nil nil
			 (number-to-string pid)
			 (prin1-to-string signum t)
			 (number-to-string (or sigval "0")))))))

(defun fakecygpty-activate ()
  "Activate fakecygpty features."
  (if (or fakecygpty--activated
	  (not (memq system-type '(ms-dos windows-nt)))
	  (not (and fakecygpty-program
		    (executable-find fakecygpty-program)
		    fakecygpty-sigqueue-program
		    (executable-find fakecygpty-sigqueue-program))))
      nil
    (defadvice start-process (around fakecygpty-start-process last activate)
      "If `process-connection-type' is non-nil, invoke PROGRAM by `fakecygpty-program'."
      (if (and
	   fakecygpty-enabled
	   process-connection-type	; if non-nil, required pty.
	   (ad-get-arg 2))
	  (progn
	    (ad-set-args 3 (cons (ad-get-arg 2) (ad-get-args 3)))
	    (ad-set-arg 2 fakecygpty-program)
	    ad-do-it
	    (when (processp ad-return-value)
	      (process-put ad-return-value :fakecygpty-p t)))
	ad-do-it))

    (defadvice process-command (after fakecygpty-process-command activate)
      "Return real command name if PROCESS was invoked by fakecygpty."
      (when (fakecygpty-process-p (ad-get-arg 0))
	(setq ad-return-value (cdr ad-return-value))))

    (defadvice process-tty-name (after fakecygpty-process-tty-name activate)
      "Return tty name if PROCESS was invoked by fakecygpty."
      (when (fakecygpty-process-p (ad-get-arg 0))
	(setq ad-return-value
	      (with-temp-buffer
		(if (\= 0 (call-process
			   "sh" nil (current-buffer) nil
			   "-c"
			   (format 
			    (concat
			     "X=`ls /proc/*/ppid | xargs grep -l \"^%s$\" 2>/dev/null` ; "
			     "X=`dirname $X 2>/dev/null` && cat `echo -n \"$X/ctty\"`")
			    (process-id (ad-get-arg 0)))))
		    (replace-regexp-in-string "\r?\n" "" (buffer-string))
		  "?")))))

    (defadvice process-send-eof (around fakecygpty-send-process-eof activate)
      "Send raw C-d code if PROCESS was invoked by fakecygpty."
      (let ((target (if (ad-get-arg 0)
			(get-process (ad-get-arg 0))
		      (get-buffer-process (current-buffer)))))
	(if (fakecygpty-process-p target)
	    (send-string target "\C-d")
	  ad-do-it)))

    (defadvice signal-process (around fakecygpty-signal-process activate)
      "Send signal by `signal-process' for cygwin process.
So it's able to send any type signal.
For windows process, emacs native `signal-process' will be invoked."
      (if (fakecygpty-sigqueue (ad-get-arg 0) (ad-get-arg 1))
	  (setq ad-return-value 0)
	ad-do-it
	))

    (defadvice interrupt-process (around fakecygpty-interrupt-process activate)
      "Send SIGINT signal by `signal-process'."
      (if (/= 0 (signal-process (ad-get-arg 0) 'SIGINT))
	  ad-do-it))

    (defadvice quit-process (around fakecygpty-quit-process activate)
      "Send SIGQUIT signal by `signal-process'."
      (if (/= 0 (signal-process (ad-get-arg 0) 'SIGQUIT))
	  ad-do-it))

    (defadvice stop-process (around fakecygpty-stop-process activate)
      "Send SIGTSTP signal by `signal-process'."
      (if (/= 0 (signal-process (ad-get-arg 0) 'SIGTSTP))
	  ad-do-it))

    (defadvice continue-process (around fakecygpty-continue-process activate)
      "Send SIGCONT signal by `signal-process'."
      (if (/= 0 (signal-process (ad-get-arg 0) 'SIGCONT))
	  ad-do-it))


    (defadvice set-process-window-size (around fakecygpty-set-process-window-size activate)
      "fakecygpty 経由の場合は siqgueue にて WINCH シグナルを送信する."
      (if (and (fakecygpty-process-p (ad-get-arg 0))
	       (= 0 (fakecygpty-sigqueue (ad-get-arg 0)
					 'SIGWINCH
					 (+ (* 65536 (ad-get-arg 1))
					    (ad-get-arg 2)))))
	  (setq ad-return-value t)
	ad-do-it))
    )
  (setq fakecygpty--activated t))
