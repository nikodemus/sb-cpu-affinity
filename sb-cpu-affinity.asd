#-(and linux sbcl)
(error "SB-CPU-AFFINITY is SBCL/Linux only.")

(defpackage :sb-cpu-affinity-system
  (:use :cl :asdf))

(in-package :sb-cpu-affinity-system)

(defclass c-so-source-file (c-source-file) ())

(defvar *gcc* "/usr/bin/gcc")
(defvar *gcc-options* '("-shared" "-fPIC"))

(defmethod output-files ((o compile-op) (c c-so-source-file))
  (list (make-pathname :type "so"
                       :defaults (component-pathname c))))

(defmethod perform ((o load-op) (c c-so-source-file))
  (destructuring-bind (so) (input-files o c)
    (sb-alien:load-shared-object so)))

(defmethod perform ((o compile-op) (c c-so-source-file))
  (destructuring-bind (so) (output-files o c)
    (unless (zerop (run-shell-command 
                    "~A ~A ~{~A ~}-o ~A"
                    *gcc*
                    (sb-ext:native-namestring (component-pathname c) :as-file t)
                    *gcc-options*
                    (sb-ext:native-namestring so :as-file t)))
      (error 'operation-error :operation o :component c))))

(defsystem :sb-cpu-affinity
    :components
  ((c-so-source-file "cpu-affinity-wrapper")
   (:file "package")
   (:file "cpu-affinity" :depends-on ("package" "cpu-affinity-wrapper"))))
