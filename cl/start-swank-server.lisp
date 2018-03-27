;; Copyright (c) 2018 Conrad Indiono
;;
;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program (see file COPYING). If not, see <http://www.gnu.org/licenses/>.
;;; -*- mode: lisp ; syntax: ansi-common-lisp -*-

;; standard quicklisp init file, since we will be launching ecl without ~/.eclrc
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(when (probe-file "/tmp/slime.2565")
  (delete-file "/tmp/slime.2565"))

(load
  "~/quicklisp/dists/quicklisp/software/slime-v2.18/swank-loader.lisp"
  :verbose t)

(funcall (read-from-string "swank-loader:init"))
(funcall (read-from-string "swank:start-server")
         "/tmp/slime.2565")
