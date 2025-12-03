
(defclass entry ()
  ((date            :initarg :entry-date        :accessor entry-date) 
   (time            :initarg :entry-time        :accessor entry-time)
   (msec            :initarg :entry-msec        :accessor entry-msec)
   (id              :initarg :entry-id          :accessor entry-id)
   (version         :initarg :entry-version     :accessor entry-version)
   (priority        :initarg :entry-priority    :accessor entry-priority)
   (funcname        :initarg :entry-funcname    :accessor entry-funcname)
   (data            :initarg :entry-data        :accessor entry-data)))

(defclass data () ())

(defclass link (data)
  ((link-from       :initarg :link-from         :accessor link-from)
   (link-to         :initarg :link-to           :accessor link-to)))

(defclass label (data)
  ((label           :initarg :label             :accessor label)))

(defclass state (data)
  ((state           :initarg :state             :accessor state)))

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

(defparameter link-pat "chaining from >\(.*\)< to >\(.*\)<")
(defparameter label-pat "Label: \(.*\)")
(defparameter state-pat "\(.*\): entering state")

(defparameter log-pat 
  (concatenate 'string datetime-pat "\\|" call-id-pat "\\|" version-pat "-" priority-pat "[ ]*\\|" funcname-pat "\\|"))
(defparameter entry-pat (concatenate 'string log-pat data-pat))

(defparameter entry-link-pat (concatenate 'string log-pat link-pat))
(defparameter entry-label-pat (concatenate 'string log-pat label-pat))
(defparameter entry-state-pat (concatenate 'string log-pat state-pat))

(defmacro date-of-ints (yr mon day) `(+ (* ,yr 10000) (* ,mon 100) ,day))
(defmacro date-of-strs (yr mon day) `(date-of-ints (parse-integer ,yr) (parse-integer ,mon) (parse-integer ,day)))
(defmacro time-of-ints (hh mm ss) `(+ (* ,hh 10000) (* ,mm 100) ,ss))
(defmacro time-of-strs (hh mm ss) `(time-of-ints (parse-integer ,hh) (parse-integer ,mm) (parse-integer ,ss)))
(defmacro parse-date (line) `(ppcre:register-groups-bind (yr mn dy) (date-pat ,line) (date-of-strs yr mn dy)))
(defmacro parse-time (line) `(ppcre:register-groups-bind (hh mm ss) (time-pat ,line) (time-of-strs hh mm ss)))

(defmacro parse-link (line)
  `(ppcre:register-groups-bind (nfrom nto) (link-pat ,line)
    (make-instance 'link
                   :link-from nfrom
                   :link-to nto)))

(defmacro parse-label (line)
  `(ppcre:register-groups-bind (nlabel) (label-pat ,line)
    (make-instance 'label :label nlabel)))

(defmacro parse-state (line)
 `(ppcre:register-groups-bind (nstate) (state-pat ,line)
    (make-instance 'state :state nstate)))

(defmacro parse-data (line)
  `(cond
     ((ppcre:scan link-pat ,line) (parse-link ,line))
     ((ppcre:scan label-pat ,line) (parse-label ,line))
     ((ppcre:scan state-pat ,line) (parse-state ,line))
     (t line)))

(defmacro parse-entry (line) 
  `(ppcre:register-groups-bind (yr mn dy hh mm ss ms id ver pri fun data) (entry-pat ,line)
    (make-instance 'entry
                   :entry-date (date-of-strs yr mn dy)
                   :entry-time (time-of-strs hh mm ss)
                   :entry-msec (parse-integer ms)
                   :entry-id id
                   :entry-version ver
                   :entry-priority pri
                   :entry-funcname fun
                   :entry-data (parse-data data))))

(defmethod input-entry (fin)
  (let ((line (read-line fin nil 'eof)))
    (cond 
      ((equal line 'eof) 'eof)
      ((ppcre:scan entry-pat line) (parse-entry line))
      (t (input-entry fin)))))

(defmethod aux-input-channel (fin entries)
  (let ((entry (input-entry fin)))
    (cond
      ((equal entry 'eof) entries)
      ((typep (entry-data entry) 'string) (aux-input-channel fin entries))
      (t (aux-input-channel fin (cons entry entries))))))

(defmacro input-channel (fin) `(aux-input-channel ,fin '()))
(defmacro input-file (fname) `(with-open-file (fin ,fname :direction :input) (input-channel fin)))

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")
(defparameter linkline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")
(defparameter labelline "2025-08-25T05:29:01,498|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: Always")
(defparameter stateline "2025-08-25T05:28:35,838|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state")



