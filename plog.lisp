
(defclass tdate () ((tdatk 
                    :accessor tdate 
                    :initarg :tdate)))

(defvar pfx-date "([0-9][0-9][0-9][0-9]\-[0-9][0-9]\-[0-9][0-9])")

(defmethod make-date (year month day) (make-instance 'tdate :tdate (+ (* 10000 year) (* 100 month) day)))
(defmethod date-of-strs (year month day) (make-date (parse-integer year) (parse-integer month) (parse-integer day)))
(defmethod date-of-string (ymd) (date-of-strs (subseq ymd 0 4) (subseq ymd 5 7) (subseq ymd 8 10)))

(defmethod year ((d tdate)) (floor (/ (tdate d) 10000)))
(defmethod month ((d tdate)) (mod (floor (/ (tdate d) 100)) 100))
(defmethod day ((d tdate)) (mod (tdate d) 100))

(defmethod now-date ()
    (multiple-value-bind (a b c mday mon yr d e f) 
        (get-decoded-time) 
        (make-date yr mon mday)))

(defun four-digit-string (s) 
  (let ((pfx (if (< s 10) "000" (if (< s 100) "00" (if (< s 1000) "0" ""))))) 
    (format nil "~A~A" pfx s)))
(defun two-digit-string (s) 
  (let ((pfx (if (< s 10 ) "0" "")))
    (format nil "~A~A" pfx s)))
(defmethod to-string ((d tdate)) 
  (format t "~A-~A-~A" (four-digit-string (year d)) (two-digit-string (month d)) (two-digit-string (day d))))
  
(defclass ttime () ((ttime 
                    :accessor ttime 
                    :initarg :ttime)))

(defvar pfx-time "([0-9][0-9]:[0-9][0-9]:[0-9][0-9]),([0-9]+)")

(defmethod make-time (hr mn sc) (make-instance 'ttime :ttime (+ (* 3600 hr) (* 60 mn) sc)))
(defmethod time-of-strs (hr mn sc) (make-time (parse-integer hr) (parse-integer mn) (parse-integer sc)))
(defmethod time-of-string (hms) (time-of-strs (subseq hms 0 2) (subseq hms 3 5) (subseq hms 6 8)))

(defmethod thour ((tm ttime)) (floor (/ (ttime tm) 3600)))
(defmethod tminute ((tm ttime)) (mod (floor (/ (ttime tm) 60)) 60))
(defmethod tsecond ((tm ttime)) (mod (ttime tm) 60))

(defmethod to-string ((tm ttime))
    (format t "~A:~A:~A" (two-digit-string (thour tm)) (two-digit-string (tminute tm)) (two-digit-string (tsecond tm))))

(defmethod now-time ()
    (multiple-value-bind (sc mn hr a b c d e f)
        (get-decoded-time)
        (make-time hr mn sc)))

(defclass datetime (date ttime) ())

(defmethod to-string ((dt datetime)) 
    (format t "~A-~A-~A ~A:~A:~A"  
        (four-digit-string (year dt)) (two-digit-string (month dt)) (two-digit-string (day dt))
        (two-digit-string (thour dt)) (two-digit-string (tminute dt)) (two-digit-string (tsecond dt))))

(defmethod make-datetime (yr mo dy hr mi sc) 
    (make-instance 'datetime 
                   :tdate (+ (* yr 10000) (* mo 100) dy)
                   :ttime (+ (* 3600 hr) (* 60 mi) sc)))

(defmethod now ()
  (multiple-value-bind (sc mn hr dy mo yr a b c)
    (get-decoded-time)
    (make-datetime yr mo dy hr mn sc)))

(defvar pfx-callid "\\|([0-9A-F]*)")
(defvar pfx-ivr "\\|([_0-9A-Z\.\-]*)")

(defvar chain-str "\\|reporting\.CDRUtil\\|chaining from >(.*)< to >(.*)<")

(defvar chain-pfx (concatenate 'string pfx-date "T" pfx-time pfx-callid pfx-ivr))
(defvar chaining  (concatenate 'string chain-pfx chain-str))

(defvar chainline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")

(defclass entry ()
  ((tdate :accessor tdate :initarg :tdate)
   (ttime :accessor ttime :initarg :ttime)
   (tmsec :accessor tmsec :initarg :tmsec)
   (id :accessor id :initarg :id)
   (ivr :accessor ivr :initarg :ivr)))

(defclass chain-entry (entry)
    ((tfrom :accessor tfrom :initarg :tfrom)
     (tto :accessor tto :initarg :tto)))
  
(defmethod parse-line (line)
  (ppcre:register-groups-bind (td tt tm tid tiv ttf ttt) (chaining line)
    (make-instance 'chain-entry 
        :tdate (date-of-string td)
        :ttime (time-of-string tt)
        :tmsec (parse-integer tm) 
        :id tid 
        :ivr tiv
        :tfrom ttf
        :tto ttt)))

(defmethod parse-file (fname) 
    (with-open-file (fin fname)
      (labels 
        ((looplines (lines) 
            (let ((line (read-line fin nil 'eof)))
              (cond
                ((eq line 'eof) (reverse lines))
                ((cl-ppcre:scan chaining line) (looplines (cons (parse-line line) lines)))
                (t (looplines lines))))))
        (looplines nil))))

(defmethod hash-entry ((e entry)) 
    (concatenate 'string (tdate (tdate e)) (ttime (ttime e)) (tmsec e))

(defmethod map-calls (calls) 
    (let ((tbl (make-hash-table :test 'equal)))
        (map 'list (lambda (e)
                     (format t "mapping id [~A] ~A ~A~%" (id e) (tfrom e) (tto e))
                     (setf (gethash (id e) tbl) (callmap e (gethash (id e) tbl))))
             calls)
        tbl))

(defun get-keys (tbl) 
    (let ((keys '())) 
      (maphash (lambda (key val)
                 (declare (ignore val))
                 (push key keys))
               tbl)
      (nreverse keys)))
