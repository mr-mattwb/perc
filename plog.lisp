
(defclass date () ((date 
                    :accessor date 
                    :initarg :date)))

(defvar pfx-date "([0-9][0-9][0-9][0-9]\-[0-9][0-9]\-[0-9][0-9])")

(defmethod date-of-ints (year month day) (make-instance 'date :date (+ (* 10000 year) (* 100 month) day)))
(defmethod date-of-strs (year month day) (date-of-ints (parse-integer year) (parse-integer month) (parse-integer day)))
(defmethod date-of-string (ymd) (date-of-strs (subseq ymd 0 4) (subseq ymd 5 7) (subseq ymd 8 10)))

(defmethod year ((d date)) (floor (/ (date d) 10000)))
(defmethod month ((d date)) (mod (floor (/ (date d) 100)) 100))
(defmethod day ((d date)) (mod (date d) 100))

(defmethod now-date ()
    (multiple-value-bind (a b c mday mon yr d e f) 
        (get-decoded-time) 
        (date-of-ints yr mon mday)))

(defclass mtime () ((mtime 
                    :accessor mtime 
                    :initarg :mtime)))

(defvar pfx-time "([0-9][0-9]:[0-9][0-9]:[0-9][0-9])")

(defmethod time-of-ints (hr mn sc) (make-instance 'mtime :mtime (+ (* 3600 hr) (* 60 mn) sc)))
(defmethod time-of-strs (hr mn sc) (time-of-ints (parse-integer hr) (parse-integer mn) (parse-integer sc)))
(defmethod time-of-string (hms) (time-of-strs (subseq hms 0 2) (subseq hms 3 5) (subseq hms 6 8)))

(defmethod mhour ((tm mtime)) (floor (/ (mtime tm) 3600)))
(defmethod mminute ((tm mtime)) (mod (floor (/ (mtime tm) 60)) 60))
(defmethod msecond ((tm mtime)) (mod (mtime tm) 60))

(defmethod now-time ()
    (multiple-value-bind (sc mn hr a b c d e f)
        (get-decoded-time)
        (time-of-ints hr mn sc)))

