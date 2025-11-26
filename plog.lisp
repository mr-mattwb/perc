
(defclass entry ()
  ((date        :initarg :date       :accessor entry-date)
   (time        :initarg :time       :accessor entry-time)
   (msec        :initarg :msec       :accessor entry-msec)
   (id          :initarg :id         :accessor entry-id)
   (ivr         :initarg :ivr        :accessor entry-ivr)
   (prio        :initarg :prio       :accessor entry-prio)
   (proc        :initarg :proc       :accessor entry-proc)
   (data        :initarg :data       :accessor entry-data)))

(defclass call ()
  ((id          :initarg :id        :accessor call-id)
   (nodes       :initarg :nodes     :accessor call-nodes)))

(defclass link ()
  ((from        :initarg :from      :accessor link-from)
   (to          :initarg :to        :accessor link-to)))

(defclass label ()
  ((label       :initarg :label     :accessor label)))

(defclass state ()
  ((state    :initarg :state        :accessor state)))

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")
(defparameter linkline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")
(defparameter labelline "2025-08-25T05:29:01,498|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: Always")
(defparameter stateline "2025-08-25T05:28:35,838|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state")

(defparameter level "MOD25.09.0.004-DEBUG")

(defparameter date-fmt "\(\\d\\d\\d\\d\)-\(\\d\\d\)-\(\\d\\d\)")
(defparameter time-fmt "\(\\d\\d\):\(\\d\\d\):\(\\d\\d\)")
(defparameter msec-fmt "\(\\d\\d\\d\)")
(defparameter id-fmt "\([A-F0-9]*\)")
(defparameter prio-fmt "\([A-Z0-9\.]+\)-\([A-Z]+\)")
(defparameter proc-fmt "\([a-zA-Z0-9\.\_]+\)")
(defparameter data-fmt "\(.*\)")
(defparameter link-fmt "chaining from >\(.*\)< to >\(.*\)<")
(defparameter label-fmt "Label: \(.*\)")
(defparameter state-fmt "\(.*\): entering state")

(defparameter line-pat (concatenate 'string date-fmt "T" time-fmt "," msec-fmt 
                                   "\\|" id-fmt "\\|" prio-fmt "[ ]*\\|" proc-fmt "\\|"))
(defparameter entry-pat (concatenate 'string line-pat data-fmt))
(defparameter link-pat (concatenate 'string  line-pat link-fmt))
(defparameter label-pat (concatenate 'string  line-pat label-fmt))
(defparameter state-pat (concatenate 'string  line-pat state-fmt))

(defmethod date-of-ints (yr mn dy) (+ (* yr 10000) (* mn 100) dy))
(defmethod time-of-ints (hh mm ss) (+ (* hh 10000) (* mm 100) ss))
(defmethod date-of-strs (yr mn dy) (date-of-ints (parse-integer yr) (parse-integer mn) (parse-integer dy)))
(defmethod time-of-strs (hh mm ss) (time-of-ints (parse-integer hh) (parse-integer mm) (parse-integer ss)))

(defmacro hd (nodes) `(car ,nodes))
(defmacro tl (nodes) `(cdr ,nodes))
(defmacro inspect-data (entry) `(inspect (entry-data ,entry)))
(defmacro inspect-hd (ls) `(inspect (car ,ls)))
(defmacro inspect-hd-data (ls) `(inspect-data (hd ,ls)))

(defmacro link-match (line) `(ppcre:scan link-fmt ,line))
(defmacro entry-match (line) `(ppcre:scan entry-pat ,line))
(defmacro label-match (line) `(ppcre:scan label-fmt ,line))
(defmacro state-match (line) `(ppcre:scan state-fmt ,line))

(defmacro copy-entry (e)
    `(make-instance 'entry
                   :date (entry-date ,e)
                   :time (entry-time ,e)
                   :msec (entry-msec ,e)
                   :id (entry-id ,e)
                   :ivr (entry-ivr ,e)
                   :prio (entry-prio ,e)
                   :proc (entry-proc ,e)
                   :data (entry-data ,e)))

(defmacro create-link (line)
  `(ppcre:register-groups-bind (nfrom nto) (link-fmt ,line)
    (make-instance 'link :from nfrom :to nto)))
(defmacro create-label (line)
  `(ppcre:register-groups-bind (label) (label-fmt ,line) 
    (make-instance 'label :label label)))
(defmethod create-state (line) 
  `(ppcre:register-groups-bind (state) (state-fmt ,line)
    (make-instance 'state :state state)))

(defmacro parse-data (line)
  `(cond 
    ((link-match line) (create-link ,line))
    ((label-match line) (create-label ,line))
    ((state-match line) (create-state ,line))
    (t ,line)))

(defmacro parse-entry (line) 
  `(ppcre:register-groups-bind (yr mn dy hh mm ss ms id ivr pri proc data) (entry-pat ,line)
    (make-instance 'entry 
                   :date (date-of-strs yr mn dy)
                   :time (time-of-strs hh mm ss)
                   :msec (parse-integer ms)
                   :id id
                   :ivr ivr
                   :prio pri
                   :proc proc
                   :data (parse-data data))))

(defmethod input-entry (fin)
  (let ((line (read-line fin nil 'eof)))
    (cond
      ((equal line 'eof) 'eof)
      (t (let ((entry (parse-entry line)))
           (cond 
             ((equal nil entry) nil)
             ((typep (entry-data entry) 'string) (input-entry fin))
             (t entry)))))))

(defmethod aux-input-channel (fin entries)
  (let ((entry (input-entry fin))) 
    (cond
      ((equal entry 'eof) entries)
      ((equal entry nil) (aux-input-channel fin entries))
      (t (aux-input-channel fin (cons entry entries))))))

(defmacro input-channel (fin) `(aux-input-channel ,fin '()))
(defmacro input-file (fname) `(with-open-file (fin ,fname :direction :input) (input-channel fin)))
(defmacro get-ids (entries) `(remove-duplicates (mapcar #'entry-id ,entries) :test #'equal))
(defmethod get-call (id entries) 
  (make-instance 'call
                 :id id
                 :nodes (remove-if-not #'(lambda (x) (equal (entry-id x) id)) entries)))
(defmacro calls-of-entries (entries) `(mapcar #'(lambda (id) (get-call id entries)) (get-ids ,entries)))

(defmacro input-calls (fname) `(calls-of-entries (input-file ,fname)))
 
