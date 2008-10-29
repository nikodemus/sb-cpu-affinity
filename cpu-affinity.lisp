;; Copyright (c) 2008 Nikodemus Siivola <nikodemus@random-state.net>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :sb-cpu-affinity)

;;;; Alien definitions

;; This is defined as a #DEFINE constant, but the .so stuff the value
;; into a variable for us.
(eval-when (:compile-toplevel :load-toplevel)
  (define-alien-variable cpu-setsize int))

;; This is sizeof(cpu_set_t)
(define-alien-variable cpu-mask-size int)

;; These are convenience wrappers around sched_set/getaffinity.
(define-alien-routine ("get_cpu_affinity_mask" %get-cpu-affinity-mask) int
  (mask (* (unsigned 8))))
(define-alien-routine ("set_cpu_affinity_mask" %set-cpu-affinity-mask) int
  (mask (* (unsigned 8))))

;; These are wrappers around libc macros that manipulate the mask. Why
;; the hell can't C people provide functions in addition to macros
;; like this? Or just use inline functions?
(define-alien-routine zero-cpu-affinity-mask void
  (mask (* (unsigned 8))))
(define-alien-routine ("cpu_affinity_p" %cpu-affinity-p) int
  (cpu int)
  (mask (* (unsigned 8))))
(define-alien-routine set-cpu-affinity  void
  (cpu int)
  (mask (* (unsigned 8))))
(define-alien-routine clear-cpu-affinity void
  (cpu int)
  (mask (* (unsigned 8))))

;;;; Nice lispy interface

(defconstant +cpu-limit+ cpu-setsize
  "Upper exclusive limit on the number of CPUs. Based on CPU_SETSIZE
from sched.h.")

(defvar *cpu-count* nil)

(defun forget-cpu-count ()
  (setf *cpu-count* nil))

(pushnew 'forget-cpu-count sb-ext:*save-hooks*)

(defun cpu-count ()
  "Number of CPUs available in the system. Based on /proc/cpuinfo."
  (or *cpu-count*
      (setf *cpu-count*
            (let* ((key "processor")
                   (len (length key)))
              (with-open-file (f "/proc/cpuinfo")
                (loop for line = (read-line f nil nil)
                      while line
                      count (when (> (length line) len)
                              (string= key line :end2 len))))))))

(defstruct cpu-affinity-mask
  "CPU affinity mask."
  %mask)

(defmethod print-object ((mask cpu-affinity-mask) stream)
  (print-unreadable-object (mask stream :type t)
    ;; Print the locally interesting part of the mask.
    (dotimes (i (cpu-count))
      (if (cpu-affinity-p i mask)
          (write-char #\1 stream)
          (write-char #\0 stream)))))

(defun make-cpu-mask ()
  (make-alien (unsigned 8) cpu-mask-size))

(defun get-cpu-affinity-mask ()
  "Returns the CPU affinity mask of the current thread. The mask can
be inspected and mutated using CPU-AFFINITY-P, \(SETF CPU-AFFINITY-P),
and CLEAR-CPU-AFFINITY-MASK. To make any changes take effect, the mask
must be saved using SET-CPU-AFFINITY-MASK.

Using WITH-CPU-AFFINITY-MASK instead is recommended."
  ;; FIXME: Malloc'ed mask is nasty, but libc doesn't seem to like
  ;; stack allocated ones, nor does it seem to like freeing masks that
  ;; have been used. So we never do. Gah.
  (let ((mask (make-cpu-mask)))
    (unless (zerop (%get-cpu-affinity-mask mask))
      (error "Could not read CPU affinity mask: ~A" (sb-int:strerror)))
    (make-cpu-affinity-mask :%mask mask)))

(defun set-cpu-affinity-mask (mask)
  "Sets the CPU affinity mask for the current thread.

Using WITH-CPU-AFFINITY-MASK :SAVE T instead is recommended."
  (unless (zerop (%set-cpu-affinity-mask (cpu-affinity-mask-%mask mask)))
    (error "Coud not write CPU affinity mask: ~A" (sb-int:strerror))))

(defun cpu-affinity-p (cpu mask)
  "Returns T if the CPU \(a numeric indentifier between 0 and
 +CPU-LIMIT+) is part of the MASK."
  (plusp (%cpu-affinity-p cpu (cpu-affinity-mask-%mask mask))))

(defun (setf cpu-affinity-p) (bool cpu mask)
  "Toggles presence of the CPU \(a numeric identifier between 0 and +CPU-LIMIT+)
in the MASK."
  (let ((%mask (cpu-affinity-mask-%mask mask)))
    (if bool
        (set-cpu-affinity cpu %mask)
        (clear-cpu-affinity cpu %mask)))
  bool)

(defun clear-cpu-affinity-mask (mask)
  "Removes all CPUs from the MASK."
  (zero-cpu-affinity-mask (cpu-affinity-mask-%mask mask))
  mask)

(defmacro with-cpu-affinity-mask ((mask &key save) &body body)
  "Reads the CPU affinity mask of the the current thread and binds it
to MASK. The mask can be inspected and mutated using CPU-AFFINITY-P,
\(SETF CPU-AFFINITY-P, and CLEAR-CPU-AFFINITY-MASK. Any changes take
effect only if SAVE is true (default is NIL)."
  (let ((ok-n (gensym "OK")))
    `(let (,mask ,ok-n)
       (unwind-protect
            (progn
              (setf ,mask (get-cpu-affinity-mask))
              (multiple-value-prog1 (progn ,@body)
                (setf ,ok-n t)))
         (when (and ,ok-n ,save)
           (set-cpu-affinity-mask ,mask))))))

;;;; Usage examples
#+nil
(progn
  
  (with-cpu-affinity-mask (mask)
    (print mask))

  (with-cpu-affinity-mask (mask :save t)
    ;; Remove all
    (clear-cpu-affinity-mask mask)
    ;; Set CPU 0.
    (setf (cpu-affinity-p 0 mask) t))
 
  (with-cpu-affinity-mask (mask)
    (print mask))

  (with-cpu-affinity-mask (mask :save t)
    ;; Only odd CPUs in mask.
    (dotimes (cpu (cpu-count))
      (setf (cpu-affinity-p cpu mask) (oddp cpu))))
  
  (with-cpu-affinity-mask (mask)
    (print mask)))
