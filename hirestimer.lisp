(in-package :lispboy)

#+(or darwin macos)
(progn
  ;; On macOS, the library is in a different location
  (cffi:define-foreign-library libc
    (t (:framework "CoreServices")))  ; macOS system library

  (cffi:load-foreign-library 'libc)

  ;; mach_absolute_time is the preferred high-resolution timer on macOS
  (cffi:defcfun ("mach_absolute_time" mach-absolute-time) :uint64)
  
  ;; We also need the timebase info to convert to nanoseconds
  (cffi:defcstruct mach-timebase-info
    (numer :uint32)
    (denom :uint32))

  (cffi:defcfun ("mach_timebase_info" %mach-timebase-info) :int
    (info (:pointer (:struct mach-timebase-info))))

  ;; Cache the timebase info
  (defvar *timebase-info*
    (cffi:with-foreign-object (info '(:struct mach-timebase-info))
      (%mach-timebase-info info)
      (cons (cffi:foreign-slot-value info '(:struct mach-timebase-info) 'numer)
            (cffi:foreign-slot-value info '(:struct mach-timebase-info) 'denom))))

  (defun get-nanoseconds ()
    "Get current time in nanoseconds using mach_absolute_time"
    (let ((mt (mach-absolute-time)))
      ;; Convert to nanoseconds using timebase info
      (round (* mt (car *timebase-info*)) (cdr *timebase-info*)))))


#+(and unix (not (or darwin macos)))
(progn
  (cffi:define-foreign-library libc
    (:unix (:or "libc.so.6" "libc.so")))

  (cffi:load-foreign-library 'libc)

  (cffi:defcstruct timespec
    (tv-sec :long)
    (tv-nsec :long))

  (cffi:defcfun ("clock_gettime" %clock-gettime) :int
    (clk-id :int)
    (tp (:pointer (:struct timespec))))

  (defconstant CLOCK_MONOTONIC 1)

  (defun get-nanoseconds ()
    "Get current time in nanoseconds"
    (cffi:with-foreign-object (tp '(:struct timespec))
      (%clock-gettime CLOCK_MONOTONIC tp)
      (+ (* 1000000000 (cffi:foreign-slot-value tp '(:struct timespec) 'tv-sec))
         (cffi:foreign-slot-value tp '(:struct timespec) 'tv-nsec)))))

#+windows
(progn
  (cffi:define-foreign-library kernel32
    (:windows "Kernel32.dll"))

  (cffi:load-foreign-library 'kernel32)

  (cffi:defcfun ("QueryPerformanceCounter" %query-performance-counter) :int
    (counter (:pointer :uint64)))

  (cffi:defcfun ("QueryPerformanceFrequency" %query-performance-frequency) :int
    (frequency (:pointer :uint64)))

  ;; Cache the frequency
  (defvar *qpc-frequency*
    (cffi:with-foreign-object (freq :uint64)
      (%query-performance-frequency freq)
      (cffi:mem-ref freq :uint64)))

  (defun get-nanoseconds ()
    "Get current time in nanoseconds"
    (cffi:with-foreign-object (counter :uint64)
      (%query-performance-counter counter)
      (let ((count (cffi:mem-ref counter :uint64)))
        (round (* count 1000000000) *qpc-frequency*)))))

;; Platform-independent timer interface
(defstruct timer
  (start (get-nanoseconds) :type integer))

(defun timer-elapsed-ns (timer)
  "Get elapsed time in nanoseconds"
  (- (get-nanoseconds) (timer-start timer)))
