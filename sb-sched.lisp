(load-shared-object "/home/nikodemus/Desktop/sb-sched.so")

(define-alien-variable cpu-setsize int)

(define-alien-variable cpu-masksize int)

(defun make-cpu-mask ()
  (make-alien (unsigned 8) cpu-masksize))

(define-alien-routine get-cpu-affinity-mask int (mask (* (unsigned 8))))

(define-alien-routine set-cpu-affinity-mask int (mask (* (unsigned 8))))

(define-alien-routine clear-cpu-affinity-mask void (mask (* (unsigned 8))))

(define-alien-routine cpu-affinity-p int (cpu int) (mask (* (unsigned 8))))

(define-alien-routine set-cpu-affinity void (cpu int) (mask (* (unsigned 8))))

(define-alien-routine clear-cpu-affinity void (cpu int) (mask (* (unsigned 8))))

(defmacro with-cpu-affinity-mask ((mask &key save) &body body)
  `(let (,mask)
     (unwind-protect
          (progn
            (setf ,mask (make-cpu-mask))
            (unless (zerop (get-cpu-affinity-mask ,mask))
              (error "Could not read CPU affinity mask."))
            ,@body)
       (when ,mask
         (when ,save           
           (unless (zerop (set-cpu-affinity-mask ,mask))
             (error "Could not set CPU affinity mask.")))
         ;; FIXME: This leaks 128 bytes per call, but glibc complains about
         ;; double free if we free this! Not sure what is going on.
         #+nil
         (free-alien ,mask)))))

;; Usage examples
#+nil
(progn
  
 (with-cpu-affinity-mask (mask)
   (dotimes (i cpu-setsize)
     (when (plusp (cpu-affinity-p i mask))
       (print (list :cpu i)))))

 (with-cpu-affinity-mask (mask :save t)
   (clear-cpu-affinity 0 mask))

  (with-cpu-affinity-mask (mask)
   (dotimes (i cpu-setsize)
     (when (plusp (cpu-affinity-p i mask))
       (print (list :cpu i))))))
