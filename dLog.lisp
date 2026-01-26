
(defclass entry ()
  ((date            :accessor entry-date        :initarg entry-date)
   (time            :accessor entry-time        :initarg entry-time)
   (msec            :accessor entry-msec        :initarg entry-msec)
   (iden            :accessor entry-iden        :initarg entry-iden)
   (vers            :accessor entry-vers        :initarg entry-vers)
   (prio            :accessor entry-prio        :initarg entry-prio)
   (func            :accessor entry-func        :initarg entry-func)
   (data            :accessor entry-data        :initarg entry-data)))

(defclass data () ())

(defclass line (data)
   ((item            :accessor item              :initarg item)))

(deftype priority () '(DEBUG INFO WARN ERROR))

(defclass other (data) ())

(defparameter year-pat      "([1-2][0-9][0-9][0-9])")
(defparameter month-pat     "(0[1-9]|1[0-2])")
(defparameter day-pat       "(0[1-9]|[1-2][0-9]|3[0-1])")
(defparameter hour-pat      "([0-1][0-9]|2[0-3])")
(defparameter minute-pat    "([0-5][0-9])")
(defparameter second-pat    "([0-5][0-9])")
(defparameter msec-pat      "([0-9][0-9][0-9])")
(defparameter iden-pat      "([0-9A-F]*)")
(defparameter vers-pat      "([^-]+)")
(defparameter prio-pat      "(DEBUG|INFO|WARN|ERROR)")
(defparameter func-pat      "([^\\|]+)")
(defparameter data-pat      "(.*)")

(defparameter date-pat      (concatenate 'string year-pat "-" month-pat "-" day-pat))
(defparameter time-pat      (concatenate 'string hour-pat ":" minute-pat ":" second-pat))
(defparameter datetime-pat  (concatenate 'string date-pat "T" time-pat "," msec-pat))
(defparameter header-pat    (concatenate 'string datetime-pat "\\|" iden-pat "\\|" vers-pat "-" prio-pat "[ ]*\\|" func-pat "\\|" data-pat))

(defmacro date-of-ints (yr mo da) `(+ (* ,yr 10000) (* ,mo 100) ,da))
(defmacro date-of-strs (yr mo da) `(date-of-ints (parse-integer ,yr) (parse-integer ,mo) (parse-integer ,da)))

(defmacro time-of-ints (hr mi se) `(+ (* ,hr 10000) (* ,mi 100) ,se))
(defmacro time-of-strs (hr mi se) `(time-of-ints (parse-integer ,hr) (parse-integer ,mi) (parse-integer ,se)))
  
(defmacro parse-date (str) `(ppcre:register-groups-bind (yr mo da) (date-pat ,str) (date-of-strs yr mo da)))
(defmacro parse-time (str) `(ppcre:register-groups-bind (hr mi se) (time-pat ,str) (time-of-strs hr mi se)))

(defclass link (data)
  ((link-from           :accessor link-from         :initarg link-from)
   (link-to             :accessor link-to           :initarg link-to)))
(defclass label (data) ())
(defclass state (data) ())
(defclass returnValue (data) 
  ((funcName            :accessor funcName          :initarg funcName)
   (returnValue         :accessor returnValue       :initarg returnValue)))
(defclass nextModule (line) ())
(defclass returnNode (line) ())
(defclass lastVisitedModule (line) ())
(defclass portalName (line) ())
(defclass routingDs (data)
  ((returnValue         :accessor returnValue       :initarg returnValue)
   (identifiedFlag      :accessor identifiedFlag    :initarg identifiedFlag)
   (lastVisitedModule   :accessor lastVisitedModule :initarg lastVisitedModule)
   (appTag              :accessor appTag            :initarg appTag)
   (intentIntercept     :accessor intentIntercept   :initarg intentIntercept)
   (dialedPortalName    :accessor dialedPortalName  :initarg dialedPortalName)))
(defclass returnValueNode (line) ())
(defclass nodeHostname (line) ())
(defclass infoUser (data)
  ((dnis                    :accessor dnis                  :initarg dnis)
   (ani                     :accessor ani                   :initarg ani)
   (ucid                    :accessor ucid                  :initarg ucid)
   (firstHistoryInfoUser    :accessor firstHistoryInfoUser  :initarg firstHistoryInfoUser)
   (lastHistoryInfoUser     :accessor lastHistoryInfoUser   :initarg lastHistoryInfoUser)
   (receivedUcid            :accessor receivedUcid          :initarg receivedUcid)
   (receivedUui             :accessor receivedUui           :initarg receivedUui)))

(defmacro convert-null (item)
  `(cond 
     ((equal ,item "null") "")
     (t ,item)))

(defparameter link-pat "chaining from >(.*)< to >(.*)<")
(defparameter label-pat "Label: (.*)")
(defparameter state-pat "(.*): entering state")
(defparameter returnValue-pat "(.*) returnValue >(.*)<")
(defparameter nextModule-pat "nextModule: \\[([^\\]]+)\\]")
(defparameter returnNode-pat "Return\\[([^\\]]+)\\]")
(defparameter lastVisitedModule-pat "lastVisitedModule: \\[([^\\]]+)\\]")
(defparameter portalName-pat "portalName: \\[([^\\]]+)\\]")
(defparameter routingDs-pat "intent0110_Routing_DS: returnValue\\[([^\\]]+)\\] identifiedFlag\\[([^\\]]*)\\] lastVisitedModule\\[([^\\]]*)\\] appTag\\[([^\\]]*)\\] intentIntercept\\[([^\\]]*)\\]dialedPortalName\\[([^\\]]*)\\]")
(defparameter returnValueNode-pat "returnValue:\\[([^\\]]+)\\]")
(defparameter nodeHostname-pat "NODE_HOSTNAME \\[([^\\]]+)\\]")
(defparameter infoUser-pat "DNIS :([0-9]+) ANI :([0-9]*) UCID :([0-9A-F]*) FIRSTHISTORYINFOUSER :([0-9]*) LASTHISTORYINFOUSER :([0-9]*) RECEIVED_UCID :(null|[0-9]*) RECEIVED_UUI :(null|[0-9]*)")

(defmacro parse-link (verse) 
  `(ppcre:register-groups-bind (nfrom nto) (link-pat ,verse)
    (make-instance 'link
                   'link-from nfrom
                   'link-to nto)))
(defmacro parse-label (line)
  `(ppcre:register-groups-bind (label) (label-pat ,line)
    (make-instance 'label 'item label)))
(defmacro parse-state (line)
  `(ppcre:register-groups-bind (item) (state-pat ,line)
    (make-instance 'state 'item item)))
(defmacro parse-returnValue (line)
  `(ppcre:register-groups-bind (fn rv) (returnValue-pat ,line)
    (make-instance 'returnValue 
                   'funcName fn
                   'returnValue rv)))
(defmacro parse-nextModule (line)
  `(ppcre:register-groups-bind (item) (nextModule-pat ,line)
    (make-instance 'nextModule
                   'item item)))
(defmacro parse-returnNode (line)
  `(ppcre:register-groups-bind (node) (returnNode-pat ,line)
    (make-instance 'returnNode
                   'item node)))
(defmacro parse-lastVisitedModule (line)
  `(ppcre:register-groups-bind (node) (lastVisitedModule-pat ,line)
    (make-instance 'lastVisitedModule
                   'item node)))
(defmacro parse-portalName (line)
  `(ppcre:register-groups-bind (node) (portalName-pat ,line)
    (make-instance 'portalName
                   'item node)))
(defmacro parse-routingDs (line)
  `(ppcre:register-groups-bind (rv id lvm at ii dpn) (routingDs-pat ,line)
    (make-instance 'routingDs
                   'returnValue rv
                   'identifiedFlag id
                   'lastVisitedModule lvm
                   'appTag at 
                   'intentIntercept ii
                   'dialedPortalName dpn)))
(defmacro parse-returnValueNode (line)
  `(ppcre:register-groups-bind (node) (returnValueNode-pat ,line)
    (make-instance 'returnValueNode
                   'item node)))
(defmacro parse-nodeHostname (line)
  `(ppcre:register-groups-bind (node) (nodeHostname-pat ,line)
    (make-instance 'nodeHostname
                   'item node)))
(defmacro parse-infoUser (line)
  `(ppcre:register-groups-bind (dn an uc fhiu lhiu rucid ruui) (infoUser-pat ,line)
    (make-instance 'infoUser
                   'dnis dn
                   'ani an
                   'ucid uc
                   'firstHistoryInfoUser fhiu
                   'lastHistoryInfoUser lhiu
                   'receivedUcid (convert-null rucid)
                   'receivedUui (convert-null ruui))))
                                
(defmethod parse-data (line) 
  (cond
    ((ppcre:scan link-pat line)                 (parse-link line))
    ((ppcre:scan label-pat line)                (parse-label line))
    ((ppcre:scan state-pat line)                (parse-state line))
    ((ppcre:scan returnValue-pat line)          (parse-returnValue line))
    ((ppcre:scan nextModule-pat line)           (parse-nextModule line))
    ((ppcre:scan returnNode-pat line)           (parse-returnNode line))
    ((ppcre:scan lastVisitedModule-pat line)    (parse-lastVisitedModule line))
    ((ppcre:scan portalName-pat line)           (parse-portalName line))
    ((ppcre:scan routingDs-pat line)            (parse-routingDs line))
    ((ppcre:scan returnValueNode-pat line)      (parse-returnValueNode line))
    ((ppcre:scan nodeHostname-pat line)         (parse-nodeHostname line))
    ((ppcre:scan infoUser-pat line)             (parse-infoUser line))
    (t                                          line)))

(defmacro parse-entry (line)
    `(ppcre:register-groups-bind (yr mo da hr mi se ms id ve pr fu dt) (header-pat ,line)
        (make-instance 'entry 
                       'entry-date (date-of-strs yr mo da)
                       'entry-time (time-of-strs hr mi se)
                       'entry-msec (parse-integer ms)
                       'entry-iden id
                       'entry-vers ve
                       'entry-prio (read-from-string pr)
                       'entry-func fu
                       'entry-data (parse-data dt))))


