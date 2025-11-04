
(defclass entry ()
  ((date        :initarg :date       :accessor entry-date)
   (time        :initarg :time       :accessor entry-time)
   (msec        :initarg :msec       :accessor entry-msec)
   (id          :initarg :id         :accessor entry-id)
   (ivr         :initarg :ivr        :accessor entry-ivr)
   (level       :initarg :level      :accessor entry-level)
   (meth        :initarg :meth       :accessor entry-meth)
   (data        :initarg :data       :accessor entry-data)))

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

(defmethod make-date (yr mn dy) (+ (* 10000 yr) (* 100 mn) dy))
(defmethod make-time (hr mn sc) (+ (* 3600 hr) (* 60 mn) sc))
(defmethod date-from-strs (yr mn dy)
  (make-date (parse-integer yr) (parse-integer mn) (parse-integer dy)))
(defmethod time-from-strs (hr mn sc)
  (make-time (parse-integer hr) (parse-integer mn) (parse-integer sc)))

(defmethod parse-date (d) 
  (ppcre:register-groups-bind (yr mn dy) (date-fmt d)
    (date-from-strs yr mn dy)))
(defmethod parse-time (tm)
  (ppcre:register-groups-bind (hr mn sc) (time-fmt tm)
    (time-from-strs hr mn sc)))
(defmethod parse-msec (ms) (parse-integer ms))
(defmethod parse-ivr (lv)
  (ppcre:register-groups-bind (ivr lv) (level-fmt lv)
    ivr))
(defmethod parse-level (lev)
  (ppcre:register-groups-bind (ivr lev) (level-fmt lev)
    lev))

(defmethod parse-entry (line)
    (ppcre:register-groups-bind (dt tm ms id lev me d) (entryfmt line)
        (make-instance 'entry :date (parse-date dt)
                              :time (parse-time tm)
                              :msec (parse-msec ms)
                              :id id
                              :ivr (parse-ivr lev)
                              :level (parse-level lev)
                              :meth me
                              :data d)))
(defmethod hash-entry ((e entry)) (+ (* 100000000 (entry-date e)) (* 1000 (entry-time e)) (entry-msec e)))

(defmethod match-link (e)
  (not (equal nil (ppcre:scan chain-pat (entry-data e))))) 

(defmethod input-entry (fin) 
  (let ((line (read-line fin nil 'eof)))
    (if (equal line 'eof)
      'eof
      (parse-entry line))))

(defmethod aux-input-file (fin acc)
  (let ((e (input-entry fin)))
    (cond
      ((equal e 'eof)   (reverse acc))
      ((equal nil e) (aux-input-file fin acc))              ;; Skips
      ((equal "" (entry-id e)) (aux-input-file fin acc))
      (t (aux-input-file fin (cons e acc))))))

(defmethod input-file (fname)
  (with-open-file (fin fname :direction :input)
    (aux-input-file fin '())))

(defmethod get-call (id entries) 
  (remove-if-not #'(lambda (e) (equal id (entry-id e))) entries))

(defmethod call-ids (entries) 
  (remove-duplicates (mapcar #'entry-id entries) :test #'equal))

(defmethod get-calls (entries) 
  (mapcar #'(lambda (id) (cons id (get-call id entries))) (call-ids entries)))

(defmethod copy-entry (e nfrom nto) 
  (make-instance 'entry :date (entry-date e)
                        :time (entry-time e)
                        :msec (entry-msec e)
                        :id   (entry-id e)
                        :ivr  (entry-ivr e)
                        :level (entry-level e)
                        :meth (entry-meth e)
                        :data (cons nfrom nto)))

(defmethod link-convert ((e entry)) 
    (ppcre:register-groups-bind (nfrom nto) (chain-pat (entry-data e))
        (copy-entry e nfrom nto)))
  
(defmethod aux-get-links (idcall)
    (let ((call (cdr idcall)))
        (mapcar #'link-convert (remove-if-not #'match-link call))))

(defmethod get-call-links (entries)
  (mapcar #'aux-get-links (get-calls entries)))
    

