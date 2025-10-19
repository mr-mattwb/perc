

(defparameter pfx-date "\(\\d\\d\\d\\d-\\d\\d-\\d\\d\)")
(defparameter pfx-time "\(\\d\\d:\\d\\d:\\d\\d\)")
(defparameter pfx-msec "\(\\d+\)")
(defparameter pfx-callid "\([0-9A-F]*\)")
(defparameter pfx-level "\([_0-9A-Z\.-]+\)")
(defparameter pfx-method "\([A-Za-z0-9\.\_]+\)")

(defparameter prefix (concatenate 'string pfx-date "T" pfx-time "," pfx-msec "\\|" pfx-callid "\\|" pfx-level "\\|" pfx-method "\\|"))

(defparameter chain-pat "chaining from >\(.*\)< to >\(.*\)<")

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")

(defparameter chainline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")

(defparameter level "MOD25.09.0.004-DEBUG")

(defparameter chain (concatenate 'string prefix chain-pat))

(defclass entry () 
  ((date   :initarg :date   :accessor :date)
   (time   :initarg :time   :accessor :time)
   (msec   :initarg :msec   :accessor :msec)
   (id     :initarg :id     :accessor :id)
   (weight :initarg :weight :accessor :weight)
   (func   :initarg :func   :accessor :func)))

(defmethod parse-date (dt)
    (ppcre:register-groups-bind (yr mn dy) ("\(\\d\\d\\d\\d\)-\(\\d\\d\)-\(\\d\\d\)" dt)
        (+ (* 10000 (parse-integer yr)) (* 100 (parse-integer mn)) (parse-integer dy))))
(defmethod parse-time (tm)
    (ppcre:register-groups-bind (hh mm ss) ("\(\\d\\d\):\(\\d\\d\):\(\\d\\d\)" tm)
        (+ (* 3600 (parse-integer hh)) (* 60 (parse-integer mm)) (parse-integer ss))))
(defmethod parse-msec (ms)
    (parse-integer ms))

(defmethod entry-from-strings (dt tm ms id wt fn) 
  (make-instance 'entry 
                 :date (date-of-string dt)
                 :time tm
                 :msec ms
                 :id id
                 :weight wt
                 :func fn))

(defmethod parse-entry (line)
  (ppcre:register-groups-bind (fst snd thr fth fif six) (prefix line)
    (entry-from-strings fst snd thr fth fif six)))
