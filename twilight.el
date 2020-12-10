;;; twilight.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Valentin Herrmann
;;
;; Author: Valentin Herrmann <http://github/vherrmann>
;; Maintainer: Valentin Herrmann <herr.valentin.mann@gmail.com>
;; Created: December 05, 2020
;; Modified: December 05, 2020
;; Version: 0.1.0
;; Keywords: day, night, time, function
;; Homepage: https://github.com/vherrmann/twilight
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Twilight provides automated calling of functions, based on the daytime. It's a fork of circadian.el
;;
;;  Example usage (with `use-package') - make sure to properly set your
;;  latitude & longitude:
;;
;; (defun sunrise-hook ()
;;   (custom-load-theme (cdr doom-themes))
;;
;;   ;;gtk
;;   (call-process-shell-command "xsettingsd -c ~/.config/xsettings/light &"))
;;
;; (defun sunset-hook ()
;;   (custom-load-theme (car doom-themes))
;;
;;   ;;gtk
;;   (call-process-shell-command "xsettingsd -c ~/.config/xsettings/dark &"))
;;
;; (use-package twilight
;;   :config
;;   (twilight-setup)
;;   :custom
;;   (twilight-funs `((:sunrise . sunrise-hook)
;;                    (:sunset  . sunset-hook)
;;                    ("7:00"  . ,(lambda ()
;;                                   (message "Breakfast!"))))))

;;; Code:
(require 'cl-lib)
(require 'solar)

(defcustom twilight-funs '((:sunrise lambda nil 0)
                           (:sunset lambda nil 0))
  "List of functions mapped to the time they should be loaded."
  :type 'alist
  :group 'twilight)

(defun twilight-fun-max (funs)
  "Get the function FUNS, which has the largest time."
  (and funs
       (cl-reduce #'(lambda (e rest)
                      (if (< (car e)
                             (car rest))
                          rest
                        e))
                  funs)))

(defun twilight-fun-min (funs)
  "Get the function FUNS, which has the smallest time."
  (and funs
       (cl-reduce #'(lambda (e rest)
                      (if (> (car e)
                             (car rest))
                          rest
                        e))
                  funs)))

(defun twilight-funs-parse (now)
  "Parse `twilight-funs' and sort by time and shift by NOW."
  (sort
   (mapcar
    (lambda (entry)
      (cons (- (twilight-convert-funs (car entry))
               now)
            (cdr entry)))
    twilight-funs)
   (lambda (a b)
     (< (car a)
        (car b)))))


;;; --- Time
(defun twilight-now-time ()
  "Return the current daytime in seconds."
  (twilight--convert-hh-mm-ss->s (reverse (cl-subseq (decode-time) 0 3))))

(defun twilight--frac-to-seconds (f)
  "Convert fractional time F to (HH MM)."
  (floor (* (/ f 24)
               86400)))

(defun twilight--convert-hh-mm-ss->s (time)
  "Convert the TIME, which is a list in format (HH MM SS) to its representation in seconds."
  (+ (nth 2 time)
     (* 60
        (+
         (nth 1 time)
         (* 60
            (nth 0 time))))))

(defun twilight--string-to-seconds (time)
  "Convert TIME from hh:mm format to seconds."
  (twilight--convert-hh-mm-ss->s (append (seq-map #'string-to-number (split-string time ":"))
                                         '(0))))

;;; --- Sunset-sunrise
(defun twilight-sunrise ()
  "Get clean sunrise time string from Emacs' `sunset-sunrise'`."
  (twilight--frac-to-seconds (caar (solar-sunrise-sunset (calendar-current-date)))))

(defun twilight-sunset ()
  "Get clean sunset time string from Emacs' `sunset-sunrise'`."
  (twilight--frac-to-seconds (caadr (solar-sunrise-sunset (calendar-current-date)))))

(defun twilight-convert-funs (time)
  "Match TIME to a case for setting up timers."
  (cond ((cl-equalp time :sunrise)
         (twilight-sunrise))
        ((cl-equalp time :sunset)
         (twilight-sunset))
        ((stringp time)
         (twilight--string-to-seconds time))))

;;; --- Main
(defun twilight-time-of-next-activation (funs now)
  "Get the seconds to wait, until the next function from FUNS should be activated. NOW is the current time."
  (+ (or (car (twilight-fun-min (seq-remove (lambda (e)
                                  (< (car e)
                                     0))
                                funs)))
         (+ (car (twilight-fun-min funs))
            86400))
     1))

(defun twilight-get-active-fun (funs)
  "Get active function from FUNS."
  (cdr (or (twilight-fun-max (seq-remove (lambda (e)
                                  (> (car e)
                                     0))
                                funs))
           (twilight-fun-max funs))))

(defun twilight-run-active-fun ()
  "Find the function from FUNS, which should be activated and restart the timer for the next."
  (let* ((now (twilight-now-time))
         (funs (twilight-funs-parse now))
         (fun (twilight-get-active-fun funs))
         (next-time (twilight-time-of-next-activation funs now)))
    (and fun (funcall fun))
    (cancel-function-timers #'twilight-run-active-fun)
    (run-at-time next-time nil #'twilight-run-active-fun)))

;;;###autoload
(defun twilight-setup ()
  "Setup twilight based on `circadian-themes'."
  (twilight-run-active-fun))

(provide 'twilight)
;;; twilight.el ends here
