(defpackage :sb-cpu-affinity
  (:use :sb-alien :cl)
  (:export
   "WITH-CPU-AFFINITY-MASK"
   "CLEAR-CPU-AFFINITY-MASK"
   "GET-CPU-AFFINITY-MASK"
   "SET-CPU-AFFINITY-MASK"
   "CPU-AFFINITY-P"
   "CPU-COUNT"
   "+CPU-LIMIT+"))
