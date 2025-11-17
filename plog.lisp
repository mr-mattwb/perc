
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


(defparameter pfx-date "\(\\d\\d\\d\\d-\\d\\d-\\d\\d\)")
(defparameter pfx-time "\(\\d\\d:\\d\\d:\\d\\d\)")
(defparameter pfx-msec "\(\\d+\)")
(defparameter pfx-callid "\([0-9A-F]*\)")
(defparameter pfx-prio "\([_0-9A-Z\.-]+\)")
(defparameter pfx-proc "\([A-Za-z0-9\.\_]+\)")
(defparameter line-data "\(.*\)$")

(defparameter entryfmt (concatenate 'string pfx-date "T" pfx-time "," pfx-msec "\\|" pfx-callid "\\|" pfx-prio "\\|" pfx-proc "\\|" line-data))

(defparameter chain-pat "chaining from >\(.*\)< to >\(.*\)<")

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")

(defparameter chainline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")

(defparameter level "MOD25.09.0.004-DEBUG")

(defparameter date-fmt "\(\\d\\d\\d\\d\)-\(\\d\\d\)-\(\\d\\d\)")
(defparameter time-fmt "\(\\d\\d\):\(\\d\\d\):\(\\d\\d\)")
(defparameter msec-fmt "\(\\d\\d\\d\)")
(defparameter id-fmt "\([A-F0-9]+\)")
(defparameter prio-fmt "\([A-Z0-9\.]+\)-\([A-Z]+\)")
(defparameter proc-fmt "\([a-zA-Z0-9\.\_]+\)")
(defparameter data-fmt "\(.*\)")

(defparameter entry-pat (concatenate 'string date-fmt "T" time-fmt "," msec-fmt 
                                   "\\|" id-fmt "\\|" prio-fmt "\\|" proc-fmt "\\|" data-fmt))

(defmethod date-of-ints (yr mn dy) (+ (* yr 10000) (* mn 100) dy))
(defmethod time-of-ints (hh mm ss) (+ (* hh 10000) (* mm 100) ss))
(defmethod date-of-strs (yr mn dy) (date-of-ints (parse-integer yr) (parse-integer mn) (parse-integer dy)))
(defmethod time-of-strs (hh mm ss) (time-of-ints (parse-integer hh) (parse-integer mm) (parse-integer ss)))

(defmethod hd (ls) (car ls))
(defmethod tl (ls) (cdr ls))
(defmethod inspect-data (entry) (inspect (entry-data entry)))
(defmethod inspect-hd (ls) (inspect (car ls)))
(defmethod inspect-hd-data (ls) (inspect-data (hd ls)))

(defmethod link-match (e) (ppcre:scan chain-pat (entry-data e)))

(defmethod copy-entry (e)
    (make-instance 'entry
                   :date (entry-date e)
                   :time (entry-time e)
                   :msec (entry-msec e)
                   :id (entry-id e)
                   :ivr (entry-ivr e)
                   :prio (entry-prio e)
                   :proc (entry-proc e)
                   :data (entry-data e)))

(defmethod parse-entry (line) 
  (ppcre:register-groups-bind (yr mn dy hh mm ss ms id ivr pri proc data) (entry-pat line)
    (make-instance 'entry 
                   :date (date-of-strs yr mn dy)
                   :time (time-of-strs hh mm ss)
                   :msec (parse-integer ms)
                   :id id
                   :ivr ivr
                   :prio pri
                   :proc proc
                   :data data)))

(defmethod parse-link (line)
  (ppcre:register-groups-bind (nfrom nto) (chain-pat line) 
    (make-instance 'link :from nfrom :to nto)))

(defmethod input-line (fin)
  (let ((line (read-line fin nil 'eof)))
    (cond 
        ((equal line 'eof) nil)
        ((ppcre:scan entry-pat line) line)
        (t (input-line fin)))))

(defmethod input-entry (fin)
  (let ((line (input-line fin)))
    (cond 
      ((equal line nil) nil)
      (t (parse-entry line)))))

(defmethod input-channel (entries fin) 
  (let ((e (input-entry fin)))
    (cond
      ((equal e nil)  entries)
      (t (input-channel (cons e entries) fin)))))

(defmethod input-file (fname) 
  (with-open-file (fin fname :direction :input) (input-channel '() fin)))

(defmethod convert-2-link (e) 
  (let ((cpy (copy-entry e)))
    (setf (entry-data cpy) (parse-link (entry-data e)))
    cpy))

(defmethod link-match-hd (ls) (link-match (hd ls)))
(defmethod add-hd-link (links entries) (cons (convert-2-link (hd entries)) links))

(defmethod aux-links-of-entries (links entries)
  (cond
    ((equal nil entries) links)
    ((link-match (hd entries)) (aux-links-of-entries (add-hd-link links entries) (tl entries)))
    (t (aux-links-of-entries links (tl entries)))))

(defmethod links-of-entries (entries) (aux-links-of-entries '() entries))

(defmethod get-ids (links) (remove-duplicates (mapcar #'entry-id links) :test #'equal))
(defmethod get-call (id links) 
  (make-instance 'call :id id :nodes (remove-if-not #'(lambda (x) (string= id (entry-id x))) links)))
(defmethod get-calls (links) (mapcar #'(lambda (x) (get-call x links)) (get-ids links)))
(defmethod calls-of-links (links) (get-calls links))

(defmethod calls-of-entries (entries) (calls-of-links (links-of-entries entries)))



