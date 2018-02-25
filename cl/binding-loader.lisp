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
