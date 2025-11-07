
(defclass entry ()
  ((date        :initarg :date       :accessor entry-date)
   (time        :initarg :time       :accessor entry-time)
   (msec        :initarg :msec       :accessor entry-msec)
   (id          :initarg :id         :accessor entry-id)
   (ivr         :initarg :ivr        :accessor entry-ivr)
   (level       :initarg :level      :accessor entry-level)
   (meth        :initarg :meth       :accessor entry-meth)
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
(defparameter pfx-level "\([_0-9A-Z\.-]+\)")
(defparameter pfx-method "\([A-Za-z0-9\.\_]+\)")
(defparameter line-data "\(.*\)$")

(defparameter entryfmt (concatenate 'string pfx-date "T" pfx-time "," pfx-msec "\\|" pfx-callid "\\|" pfx-level "\\|" pfx-method "\\|" line-data))

(defparameter chain-pat "chaining from >\(.*\)< to >\(.*\)<")

(defparameter line "2025-08-25T05:24:35,588|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|ivr.GlobalAppUtil|File[/usr/local/tomcat/webapps/CharterModPrompts/en-US/prompts/application/default/generic_customer_loyalty_sales_intercept.ulaw] was found:true")

(defparameter chainline "2025-08-25T05:24:35,593|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >contr0105_CheckNodeCount_DS< to >contr0110_CheckGlobalHandling_DS<")

(defparameter level "MOD25.09.0.004-DEBUG")

(defparameter date-fmt "\(\\d\\d\\d\\d\)-\(\\d\\d\)-\(\\d\\d\)")
(defparameter time-fmt "\(\\d\\d\):\(\\d\\d\):\(\\d\\d\)")
(defparameter msec-fmt "\(\\d\\d\\d\)")
(defparameter id-fmt "\([A-F0-9]+\)")
(defparameter level-fmt "\([A-Z0-9\.]+\)-\([A-Z]+\)")
(defparameter meth-fmt "\([a-zA-Z0-9\.\_]+\)")

(defparameter link-pat (concatenate 'string date-fmt "T" time-fmt "," msec-fmt 
                                   "\\|" id-fmt "\\|" level-fmt "\\|" meth-fmt "\\|" chain-pat))

(defmethod date-of-ints (yr mn dy) (+ (* yr 10000) (* mn 100) dy))
(defmethod time-of-ints (hh mm ss) (+ (* hh 10000) (* mm 100) ss))
(defmethod date-of-strs (yr mn dy) (date-of-ints (parse-integer yr) (parse-integer mn) (parse-integer dy)))
(defmethod time-of-strs (hh mm ss) (time-of-ints (parse-integer hh) (parse-integer mm) (parse-integer ss)))

(defmethod parse-link (line) 
  (ppcre:register-groups-bind (yr mn dy hh mm ss ms id ivr lvl meth nfrom nto) (link-pat line)
    (make-instance 'entry 
                   :date (date-of-strs yr mn dy)
                   :time (time-of-strs hh mm ss)
                   :msec (parse-integer ms)
                   :id id
                   :ivr ivr
                   :level lvl
                   :meth meth
                   :data (make-instance 'link :from nfrom :to nto))))
(defmethod input-link (fin)
  (let ((line (read-line fin nil 'eof)))
    (cond 
      ((equal line 'eof) nil)
      ((ppcre:scan link-pat line) (parse-link line))
      (t (input-link fin)))))

(defmethod input-channel (links fin) 
  (let ((e (input-link fin)))
    (cond
      ((equal e nil)  links)
      (t (input-channel (cons e links) fin)))))

(defmethod input-file (fname) 
  (with-open-file (fin fname :direction :input) (input-channel '() fin)))


