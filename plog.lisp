

(defparameter pfx-date "\(\\d\\d\\d\\d-\\d\\d-\\d\\d\)")
(defparameter pfx-time "\(\\d\\d:\\d\\d:\\d\\d\)")
(defparameter pfx-msec "\(\\d+\)")
(defparameter pfx-callid "\([0-9A-F]*\)")
(defparameter pfx-level "\([_0-9A-Z\.-]+\)")
(defparameter pfx-method "\([A-Za-z0-9\.\_]+\)")
(defparameter line-data "\(.*\)$")

(defparameter prefix (concatenate 'string pfx-date "T" pfx-time "," pfx-msec "\\|" pfx-callid "\\|" pfx-level "\\|" pfx-method "\\|" line-data))

(defparameter chain-pat "chaining from >\(.*\)< to >\(.*\)<")

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")

(defparameter chainline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")

(defparameter level "MOD25.09.0.004-DEBUG")

(defparameter chain (concatenate 'string prefix chain-pat))

(defclass entry () 
  ((date   :initarg :date   :accessor entry-date)
   (time   :initarg :time   :accessor entry-time)
   (msec   :initarg :msec   :accessor entry-msec)
   (id     :initarg :id     :accessor entry-id)
   (level  :initarg :level  :accessor entry-level)
   (meth   :initarg :meth   :accessor entry-method)
   (data   :initarg :data   :accessor entry-data)))

(defmethod parse-date (dt)
    (ppcre:register-groups-bind (yr mn dy) ("\(\\d\\d\\d\\d\)-\(\\d\\d\)-\(\\d\\d\)" dt)
        (+ (* 10000 (parse-integer yr)) (* 100 (parse-integer mn)) (parse-integer dy))))
(defmethod parse-time (tm)
    (ppcre:register-groups-bind (hh mm ss) ("\(\\d\\d\):\(\\d\\d\):\(\\d\\d\)" tm)
        (+ (* 3600 (parse-integer hh)) (* 60 (parse-integer mm)) (parse-integer ss))))
(defmethod parse-msec (ms)
    (parse-integer ms))

(defmethod entry-from-strings (dt tm ms id wt fn d) 
  (make-instance 'entry
                 :date (parse-date dt)
                 :time (parse-time tm)
                 :msec (parse-msec ms)
                 :id id
                 :level wt
                 :meth fn
                 :data d))

(defmethod parse-entry (line)
  (ppcre:register-groups-bind (fst snd thr fth fif six sev) (prefix line)
    (entry-from-strings fst snd thr fth fif six sev)))

(defmethod hash-entry ((e entry))
    (+ (* 100000000 (entry-date e)) (* 1000 (entry-time e)) (entry-msec e)))

(defmethod parse-file (fname)
    (with-open-file (fin fname :direction :input) 
      (let ((line (read-line fin nil 'eof))
            (entries '()))
        (loop while (not (equal line 'eof)) do
              (let ((entry (parse-entry line)))
                (if (not (equal entry nil))
                    (setf entries (cons entry entries)))
              (setf line (read-line fin nil 'eof))))
        (reverse entries))))

(defmethod chain-entry-p ((e entry)) (not (equal nil (ppcre:scan chain-pat (entry-data e)))))
(defmethod filter-chains (ls) (remove-if-not #'chain-entry-p ls))
(defmethod chain-link ((e entry)) (ppcre:register-groups-bind (ef et) (chain-pat (entry-data e)) (list ef et)))
(defmethod chain-from ((e entry)) (car (chain-link e)))
(defmethod chain-to ((e entry)) (cadr (chain-link e)))
(defmethod call-ids (ls) (remove-duplicates (mapcar #'entry-id ls) :test #'equal))
(defmethod id-equal (id entry) (equal id (entry-id entry)))
(defmethod id-nodes (id links) (remove-if-not #'(lambda (e) (id-equal id e)) links))
(defmethod next-link (link links) (find-if #'(lambda (n) (equal (chain-to link) (chain-from n))) links))
(defmethod linked-p (link1 link2) (equal (chain-from link1) (chain-to link2)))


