
(defclass entry ()
  ((date            :initarg :entry-date        :accessor entry-date) 
   (time            :initarg :entry-time        :accessor entry-time)
   (msec            :initarg :entry-msec        :accessor entry-msec)
   (id              :initarg :entry-id          :accessor entry-id)
   (version         :initarg :entry-version     :accessor entry-version)
   (priority        :initarg :entry-priority    :accessor entry-priority)
   (funcname        :initarg :entry-funcname    :accessor entry-funcname)
   (data            :initarg :entry-data        :accessor entry-data)))

(defparameter year-pat "\([1-2][0-9][0-9][0-9]\)")
(defparameter month-pat "\(0[1-9]|1[0-2]\)")
(defparameter day-pat "\(0[1-9]|[1-2][0-9]|3[0-1]\)")
(defparameter date-pat (concatenate 'string year-pat "-" month-pat "-" day-pat))
(defparameter hour-pat "\(2[0-3]|[0-1][0-9]\)")
(defparameter minute-pat "\([0-5][0-9]\)")
(defparameter second-pat "\([0-5][0-9]\)")
(defparameter time-pat (concatenate 'string hour-pat ":" minute-pat ":" second-pat))
(defparameter msec-pat "\([0-9][0-9][0-9]\)")
(defparameter datetime-pat (concatenate 'string date-pat "T" time-pat "," msec-pat))

(defparameter call-id-pat "\([A-F0-9]*\)")
(defparameter version-pat "\([A-Z0-9\\.\\-\\_]+\)")
(defparameter priority-pat "\(DEBUG|WARN|INFO|ERROR\)")
(defparameter funcname-pat "\([^\\|]+\)")
(defparameter data-pat "\(.*\)")

(defparameter logline-pat 
    (concatenate 'string datetime-pat "\\|" call-id-pat "\\|" version-pat "-" priority-pat "[ ]*\\|" funcname-pat "\\|" data-pat))

(defmacro date-of-ints (yr mon day) `(+ (* ,yr 10000) (* ,mon 100) ,day))
(defmacro date-of-strs (yr mon day) `(date-of-ints (parse-integer ,yr) (parse-integer ,mon) (parse-integer ,day)))
(defmacro time-of-ints (hh mm ss) `(+ (* ,hh 10000) (* ,mm 100) ,ss))
(defmacro time-of-strs (hh mm ss) `(time-of-ints (parse-integer ,hh) (parse-integer ,mm) (parse-integer ,ss)))
(defmacro parse-date (line) `(ppcre:register-groups-bind (yr mn dy) (date-pat ,line) (date-of-strs yr mn dy)))
(defmacro parse-time (line) `(ppcre:register-groups-bind (hh mm ss) (time-pat ,line) (time-of-strs hh mm ss)))

(defmacro parse-entry (line) 
  `(ppcre:register-groups-bind (yr mn dy hh mm ss ms id ver pri fun data) (logline-pat ,line)
    (make-instance 'entry
                   :entry-date (date-of-strs yr mn dy)
                   :entry-time (time-of-strs hh mm ss)
                   :entry-msec (parse-integer ms)
                   :entry-id id
                   :entry-version ver
                   :entry-priority pri
                   :entry-funcname fun
                   :entry-data data)))

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")
(defparameter linkline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")
(defparameter labelline "2025-08-25T05:29:01,498|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: Always")
(defparameter stateline "2025-08-25T05:28:35,838|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state")


