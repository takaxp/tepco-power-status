;;; tepco-power-status.el --- get tepco power status
;;
;; Copyright (C) 2011 Takaaki ISHIKAWA
;;
;; Author: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Maintainer: Takaaki ISHIKAWA <takaxp at ieee dot org>
;; Twitter: @takaxp
;; Website: http://takaxp.com/
;; Repository: https://github.com/takaxp/tepco-power-status
;; Document: https://github.com/takaxp/tepco-power-status/blob/master/README
;; Keywords: status report, mini buffer, tepco
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;;; External data:
;;    http://www.tepco.co.jp/forecast/html/images/juyo-j.csv
;;
;;; Require:
;;    curl, perl
;;
;;; History:
;;    v1.0.1 (2011-04-07@15:11) # Publish tepco-power-status.el on GitHub
;;    v0.4.3 (2011-03-28@13:42) # Add a user variable related to PROXY server
;;    v0.4.1 (2011-03-28@01:51) # No longer use a cache directory and wget
;;    v0.3.6 (2011-03-26@18:51) # Add two user variables related to commands
;;    v0.3.4 (2011-03-26@15:48) # Add time information of status data
;;    v0.3.3 (2011-03-26@04:24) # Use a single csv file provided by tepco
;;    v0.3.1 (2011-03-25@20:11) # [FIX] Set user variable correctly
;;    v0.2.9 (2011-03-25@15:15) # Add (tepco-power-status-csv) to show change
;;    v0.2.1 (2011-03-25@00:59) # Show old data when source is unavailable
;;    v0.1.5 (2011-03-24@15:59) # Compatible with an initial format
;;    v0.1.4 (2011-03-24@14:43) # Compatible with a new format
;;    v0.1.3 (2011-03-24@01:43) # Verify existence of a downloaded file
;;    v0.1.2 (2011-03-23@22:26) # Set user variable
;;    v0.1.1 (2011-03-23@21:37) # Test under linux env.
;;    v0.1.0 (2011-03-23@20:54) # The initial release
;;
;;; Tested:
;;    MacOSX (10.6.7), Emacs (nextstep 23.3.1, local build with inline-patch)
;;    openSUSE (11.4), Emacs (23.2.1 with GTK+ 2.22.1)
;;    Ubuntu  (10.10), Emacs (23.1.1 with GTK+ 2.22.0)
;;
;;; Usage:
;;    1. Put this elisp into your load-path
;;    2. Add (require 'tepco-power-status) in your .emacs
;;    3. M-x tepco-power-status, the current status will be shown in echo area
;;    4. (tepco-power-status) run automatically every hour as default
;;    X. for auto-install user, download form
;;       http://dl.dropbox.com/u/2440/2011/tepco-power-status.el
;;
;;; Note:
;;    - There is no warranty for this free software
;;    - 100 man [kW] (in Japanese) = 1.00[GW]
;;    - This elisp no longer depends on API(http://denki.cuppat.net/)
;;    - If you have used a program v0.3.6 or the previous versions,
;;      please remove a cache directory "~/.tepco-power"
;;    - After booting up of Emacs, (tepco-power-status) will run within 5[s]

(defconst tepco-power-status-version "1.0.1"
  "The version number of the file tepco-power-status.el")

(defvar tepco-power-status-interval 3600
  "Specify an interval to run (tepco-power-status), 3600=1hour")

(defvar tepco-power-display-change-interval 3
  "Specify an interval between display of a usage and that of change value")

(defvar tepco-power-display-blink 2
  "Specify a number of times to blink status")

(defvar tepco-power-command-curl "curl"
  "Specify a name or full name to the wget command. /usr/bin/curl is default.
   For MacPort user, /opt/local/bin/curl is the full name")

(defvar tepco-power-command-perl "perl"
  "Specify a name or full name to the wget command. /usr/bin/perl is default.
   For MacPort user, /opt/local/bin/perl is the full name")

(defvar tepco-power-proxy nil
  "Specify a proxy server and port to get the data over your firewall.
  nil or http://your.proxy.server:8080/ as a string")

(add-hook 'after-init-hook
	  '(lambda ()
	     (run-at-time 5 tepco-power-status-interval 'tepco-power-status)))

(defun tepco-power-status ()
  "Show usage rate of the current tepco power generation"
  (interactive)
  (cond ((eq nil (tepco-power-status-parse))
	 (message "ERROR: Fail to get the current data from TEPCO server."))
	(t
	 (tepco-power-status-display))))

(defun tepco-power-status-parse ()
  "Read the status data and set some variables"
  (setq today-result "0")
  (setq tepco-power-status-time nil)
  (let ((data-list (tepco-power-get-data-list)))
    (cond ((eq nil data-list) nil)
	  (t
	   (let ((current-index 2)
		 (yesterday-result "0"))	
	     (setq tepco-power-capacity
		   (string-to-number (nth 0 data-list)))
	     (while (and (not (string= "0" (nth current-index data-list)))
			 (< current-index (length data-list)))
	       (setq tepco-power-status-time
		     (nth (- current-index 1) data-list))
	       (setq today-result (nth current-index data-list))
	       (setq yesterday-result (nth (+ current-index 1) data-list))
	       (setq current-index (+ current-index 3)))
	     (setq used-change
		   (/ (float (- (string-to-number today-result)
				(string-to-number yesterday-result))) 100)))))))

(defun tepco-power-status-display ()
  "Display status information in echo area"
  (let
      ((display-count 0))
    (while (< display-count tepco-power-display-blink)
      (message
       (concat " << Tepco Power Status >>                "
	       "Used: %.1f[%s] (%.2f/%.2f[GW]) @%s")
       (/ (* 100 (float (string-to-number today-result))) tepco-power-capacity)
       "%" (/ (float (string-to-number today-result)) 100)
       (/ (float tepco-power-capacity) 100) tepco-power-status-time)
      (sit-for tepco-power-display-change-interval)
      (message 
       (concat " << Tepco Power Status >>                "
	       (if (< 0 used-change)
		   "Status: OVER (Change +"
		 "Status: GOOD (Change ")
	       (format "%.2f" used-change) "[GW]) @%s") tepco-power-status-time)
      (sit-for tepco-power-display-change-interval)
      (setq display-count (1+ display-count)))
    (message nil)))

(defun tepco-power-get-data-list ()
  "Download the power status data from the TEPCO server"
  (let ((data-url "http://www.tepco.co.jp/forecast/html/images/juyo-j.csv"))
    (split-string
     (shell-command-to-string
      (concat tepco-power-command-curl
	      " -s " data-url
 	      (unless (eq nil tepco-power-proxy)
		(concat " -x " tepco-power-proxy))
	      " --connect-timeout 2 "
	      " | perl -ne 'if\(/^\(\\d+\),.+:.+$/\){ print \"$1 \"}; "
	      "if\(/,\(.+\),\(\\d+\),\(\\d+\)/\){ print \"$1 $2 $3 \"};' ")))))


(provide 'tepco-power-status)

;;; tepco-power-status.el ends here
