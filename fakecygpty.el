;;; fakecygpty.el --- Execute cygwin pty commands using pipe.

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

(defcustom fakecygpty-qkill-program (executable-find "qkill")
  "Program file name of qkill."
  :group 'fakecygpty
  :type 'file)

(defcustom fakecygpty-ignored-program-regexps
  '("[cC][mM][dD]"
    "[cC][mM][dD][pP][rR][oO][xX][yY]")
  "Regexp list for program that run without fakecygpty."
  :group 'fakecygpty
  :type '(repeat regexp))

(defvar fakecygpty--advice-defined nil
  "If t, advices for fakecygpty are defined.")

(defvar fakecygpty--activated nil
  "If t, fakecygpty is activated.")

(defconst fakecygpty--advice-regexp "^fakecygpty--"
  "Regexp for advice name of fakecygpty package.")

(defun fakecygpty-activate ()
  "Activate fakecygpty features."
  (interactive)
  (when (and (not fakecygpty--activated)
	     (memq system-type '(ms-dos windows-nt))
	     fakecygpty-program
	     (executable-find fakecygpty-program)
	     fakecygpty-qkill-program
	     (executable-find fakecygpty-qkill-program))
    (unless fakecygpty--advice-defined
      (fakecygpty--make-advice)
      (setq fakecygpty--advice-defined t))
    
    (ad-enable-regexp fakecygpty--advice-regexp)
    (ad-activate-regexp fakecygpty--advice-regexp)
    (setq fakecygpty--activated t)))

(defun fakecygpty-deactivate ()
  "Deactivate fakecygpty features."
  (interactive)
  (when fakecygpty--activated
    (ad-disable-regexp fakecygpty--advice-regexp)
    (ad-activate-regexp fakecygpty--advice-regexp)
    (setq fakecygpty--activated nil)))

(defun fakecygpty-process-p (process)
  "Non-nil when PROCESS was invoked by fakecygpty."
  (and (processp process)
       (process-get process :fakecygpty-p)))

(defun fakecygpty-qkill (process sigcode &optional sigval)
  "Send PROCESS the signal with code SIGCODE by `fakecygpty-qkill-program'.
PROCESS may also be a number specifying the process id of the
process to signal; in this case, the process need not be a child of
this Emacs.
SIGCODE may be an integer, or a symbol whose name is a signal name.
SIGVAL may be integer.  if it's nil, 0 will be used."
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
      (zerop (apply 'call-process fakecygpty-qkill-program nil nil nil
		    (delq nil `(,@(when (>= pid 0)
				    (list "-w"))
				"-s" ,(prin1-to-string sigcode t)
				,@(when (integerp sigval)
				    (list "-i" (number-to-string sigval)))
				,(number-to-string pid)))
			   )))))

(defun fakecygpty-real-process-id (process &optional as-winpid)
  "Return subprocess's process-id if PROCESS was invoked by fakecygpty."
  (if (eq (fakecygpty-process-p process) t) ;; not pty only mode
      (with-temp-buffer
	(when (zerop
	       (call-process
		"sh" nil (current-buffer) nil
		"-c"
		(format (if as-winpid
			    "cat `dirname \\`grep -l %s /proc/*/ppid 2>/dev/null\\``/winpid"
			  "basename `dirname \\`grep -l %s /proc/*/ppid 2>/dev/null\\``")
			(process-id process))))
	  (ignore-errors
	    (save-match-data
	      (string-to-number (replace-regexp-in-string "\r?\n" ""
							  (buffer-string))))))
	)
    (process-id process)))

(defun fakecygpty--ignored-program (program)
  "Return non-nil if PROGRAM is run without fakecygpty on `start-process'.
An ignored pattern is used from `fakecygpty-ignored-program-regexps'"
  (let ((program (file-name-nondirectory program)))
    (delq nil (mapcar (lambda (p)
			(string-match-p p program))
		      fakecygpty-ignored-program-regexps))))

(defun fakecygpty--normalize-process-arg (target)
  "Return process object of TARGET.
TARGET may be a process, a buffer, or the name of process or buffer.
nil means current buffer's process."
  (cond
   ((processp target)
    target)
   ((or (bufferp target)
	(null target))
    (or (get-buffer-process (or target (current-buffer)))
	(error "Buffer %s has no process" (buffer-name target))))
   ((stringp target)
    (fakecygpty--normalize-process-arg (or (get-process target)
					   (get-buffer target)
					   (error "Process %s does not exist." target))))
   (t
    (signal 'wrong-type-argument (list 'processp target)))))

(defun fakecygpty--get-tty-special-char (process type)
  "Return special char of TYPE from PROCESS's tty."
  (let ((tty (process-tty-name process)))
    (when tty
      (with-temp-buffer
	(when (zerop (call-process "stty" nil (current-buffer) nil "-a" "-F" tty))
	  (save-match-data
	    (goto-char (point-min))
	    (when (re-search-forward (format "%s = \\(\\^?\\)\\([^;]+\\);" type) nil t)
	      (unless (equal (match-string 2) "<undef>")
		(if (equal (match-string 1) "^")
		    (logand (aref (match-string 2) 0) #o037)
		  (aref (match-string 2) 0))))))))))

(defun fakecygpty--make-advice ()
  "Make advices for fakecygpty and qkill."
  (defadvice start-process (around fakecygpty--start-process last activate)
    "If `process-connection-type' is non-nil, invoke PROGRAM by `fakecygpty-program'."
    (if (and process-connection-type	; if non-nil, required pty.
	     ;; (ad-get-arg 2)
	     (or (not (ad-get-arg 2))
		 (not (fakecygpty--ignored-program (ad-get-arg 2)))))
	(progn
	  ;; insert fakecygpty at program file name position.
	  (when (ad-get-arg 2)
	    (ad-set-args 3 (cons (ad-get-arg 2) (ad-get-args 3))))
	  (ad-set-arg 2 fakecygpty-program)
	  ad-do-it
	  (when (processp ad-return-value)
	    (process-put ad-return-value :fakecygpty-p
			 (if (ad-get-arg 3) t 'pty))))
      ad-do-it))

  (defadvice process-command (after fakecygpty--process-command activate)
    "Return real command name if PROCESS was invoked by fakecygpty."
    (when (fakecygpty-process-p (ad-get-arg 0))
      (setq ad-return-value (cdr ad-return-value))))

  (defadvice process-tty-name (after fakecygpty--process-tty-name activate)
    "Return tty name if PROCESS was invoked by fakecygpty."
    (when (fakecygpty-process-p (ad-get-arg 0))
      (setq ad-return-value
	    (with-temp-buffer
	      ;; NTEmacs cannot see cygwin's `/proc' file-system, so using cygwin program.
	      ;; Finding fakecygpty's tty-name.
	      (if (zerop (call-process
			  "cat" nil (current-buffer) nil
			  (format "/proc/%s/ctty" (process-id (ad-get-arg 0)))))
		  (replace-regexp-in-string "\r?\n" "" (buffer-string))
		"?")))))

  (defadvice process-status (after fakecygpty--process-status activate)
    "Change return value 'exit to 'failed for pty allocation only mode."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0))))
      (when (and (eq (fakecygpty-process-p proc) 'pty)
		 (memq ad-return-value '(exit signal)))
	(setq ad-return-value 'failed))))

  (defadvice process-send-eof (around fakecygpty--send-process-eof activate)
    "Send raw C-d code if PROCESS was invoked by fakecygpty."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0))))
      (if (fakecygpty-process-p proc)
	  (let ((eof-char (fakecygpty--get-tty-special-char proc "eof")))
	    (when eof-char (send-string proc (char-to-string eof-char)))
	    (setq ad-return-value proc))
	ad-do-it)))

  (defadvice signal-process (around fakecygpty--signal-process activate)
    "Send signal by `fakecygpty-qkill' for cygwin process.
So it's able to send any type signal.
For windows process, Emacs native `signal-process' will be invoked."
    (if (fakecygpty-qkill (ad-get-arg 0) (ad-get-arg 1))
	(setq ad-return-value 0)
      ad-do-it
      ))

  (defadvice interrupt-process (around fakecygpty--interrupt-process activate)
    "Send SIGINT signal by `signal-process'."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0)))
	  (current-grp (ad-get-arg 1))
	  special-char)
      (if (and current-grp
	       (fakecygpty-process-p proc)
	       (setq special-char (fakecygpty--get-tty-special-char proc "intr")))
	  (send-string proc (char-to-string special-char))
	(unless (and (fakecygpty-qkill (- (fakecygpty-real-process-id proc)) 'SIGINT)
		     (setq ad-return-value proc))
	  ad-do-it))))

  (defadvice quit-process (around fakecygpty--quit-process activate)
    "Send SIGQUIT signal by `signal-process'."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0)))
	  (current-grp (ad-get-arg 1))
	  special-char)
      (if (and current-grp
	       (fakecygpty-process-p proc)
	       (setq special-char (fakecygpty--get-tty-special-char proc "quit")))
	  (send-string proc (char-to-string special-char))
	(unless (and (fakecygpty-qkill (- (fakecygpty-real-process-id proc)) 'SIGQUIT)
		     (setq ad-return-value proc))
	  ad-do-it))))

  (defadvice stop-process (around fakecygpty--stop-process activate)
    "Send SIGTSTP signal by `signal-process'."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0)))
	  (current-grp (ad-get-arg 1))
	  special-char)
      (if (and current-grp
	       (fakecygpty-process-p proc)
	       (setq special-char (fakecygpty--get-tty-special-char proc "susp")))
	  (send-string proc (char-to-string special-char))
	(unless (and (fakecygpty-qkill (- (fakecygpty-real-process-id proc)) 'SIGTSTP)
		     (setq ad-return-value proc))
	  ad-do-it))))

  (defadvice continue-process (around fakecygpty--continue-process activate)
    "Send SIGCONT signal by `signal-process'."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0)))
	  (current-grp (ad-get-arg 1)))
      (unless (and (fakecygpty-qkill (- (fakecygpty-real-process-id proc)) 'SIGCONT)
		   (setq ad-return-value proc))
	ad-do-it)))

  (defadvice kill-process (around fakecygpty--kill-process activate)
    "Don't kill if PROCESS is invoked by fakecygpty and pty allocation only mode."
    (let ((proc (fakecygpty--normalize-process-arg (ad-get-arg 0))))
      (if (eq (fakecygpty-process-p proc) 'pty)
	  (setq ad-return-value proc)
	ad-do-it)))

  (defadvice set-process-window-size (around fakecygpty--set-process-window-size activate)
    "Send SIGWINCH signal with a window size information when process is invoked by `fakecygpty'.
The window size information is caluclated by lines * 65536 + columns."
    (if (and (fakecygpty-process-p (ad-get-arg 0))
	     (fakecygpty-qkill (ad-get-arg 0) 'SIGWINCH
			       (+ (* 65536 (ad-get-arg 1))
				  (ad-get-arg 2))))
	(setq ad-return-value t)
      ad-do-it))

  (eval-after-load "gdb-mi"
    '(defadvice gdb-io-interrupt (around fakecygpty--gdb-io-interrupt-workaround activate)
	 "Workaround for gdb-io-interrupt.  This function needs real CTRL-C singal."
	 (if (not fakecygpty--activated)
	     ad-do-it
	   (ad-disable-advice 'interrupt-process 'around 'fakecygpty--interrupt-process)
	   (ad-activate 'interrupt-process)
	   ad-do-it
	   (ad-enable-advice 'interrupt-process 'around 'fakecygpty--interrupt-process)
	   (ad-activate 'interrupt-process))))
  )

(provide 'fakecygpty)

;;; fakecygpty.el ends here
