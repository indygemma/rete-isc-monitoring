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

(defvar *bindings-library* nil)
(defvar *root-dir* (uiop:getcwd))

(defun reload-binding-library ()
  (progn
    (if (not (null *bindings-library*))
      (cffi:close-foreign-library *bindings-library*))
    (setf *bindings-library* (cffi:load-foreign-library (merge-pathnames "libbindings.so" *root-dir*)))
    (cffi:defcfun ("init_extlib" init-extlib) :void)
    (init-extlib)))

(reload-binding-library)
