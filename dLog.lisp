
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
(defclass language (line) ())
(defclass dnis (line) ())
(defclass ani (line) ())
(defclass businessUnit (line) ())
(defclass callType (line) ())
(defclass customerUsingGet (data) 
  ((ucid                    :accessor ucid                  :initarg ucid)
   (ani                     :accessor ani                   :initarg ani)
   (ced                     :accessor ced                   :initarg ced)
   (acct                    :accessor acct                  :initarg acct)
   (dnis                    :accessor dnis                  :initarg dnis)
   (siteId                  :accessor siteId                :initarg siteId)))
(defclass accountNumber (line) ())
(defclass authentication (data)
  ((authenticationEligible  :accessor authenticationEligible        :initarg authenticationEligible)
   (voiceBioEnrolled        :accessor voiceBioEnrolled              :initarg voiceBioEnrolled)))
(defclass catCode (line) ())
(defclass catCodeSales (line) ())
(defclass createdToken (line) ())
(defclass aniLookup (line) ())
(defclass dtmfOnly (line) ())
(defclass cableProfileAcct (data)
  ((accountNumber           :accessor accountNumber                 :initarg accountNumber)
   (accountStatus           :accessor accountStatus                 :initarg accountStatus)))
(defclass identifiedFlag (line) ())
(defclass visitedNode (data)
  ((funcName                :accessor funcName                      :initarg funcName)
   (visited                 :accessor visited                       :initarg visited)))
(defclass businessUnitReturnCode (line) ())
(defclass businessUnitInfo (data)
  ((siteId                  :accessor siteId                        :initarg siteId)
   (businessUnit            :accessor businessUnit                  :initarg businessUnit)
   (returnCode              :accessor returnCode                    :initarg returnCode)))
(defclass playtransfer (data)
  ((routingCode             :accessor routingCode                   :initarg routingCode)
   (lastVisitedMOdule       :accessor lastVisitedModule             :initarg lastVisitedModule)
   (transferMsgPlayedFlag   :accessor transferMsgPlayedFlag         :initarg transferMsgPlayedFlag)
   (portalName              :accessor portalName                    :initarg portalName)
   (identifiedFlag          :accessor identifiedFlag                :initarg identifiedFlag)
   (delinquentLevel         :accessor delinquentLevel               :initarg delinquentLevel)
   (genericInterceptFOFlag  :accessor genericInterceptFOFlag        :initarg genericInterceptFOFlag)
   (retryZipCode            :accessor retryZipCode                  :initarg retryZipCode)
   (botEligible             :accessor botEligible                   :initarg botEligible)
   (oofFlag                 :accessor oofFlag                       :initarg oofFlag)
   (isPlayFreeSpecmoNotice  :accessor isPlayFreeSpecmoNotice        :initarg isPlayFreeSpecmoNotice)
   (callType                :accessor callType                      :initarg callType)
   (preAuthLastModule       :accessor preAuthLastModule             :initarg preAuthLastModule)))

(defmacro convert-null (item)
  `(cond 
     ((equal ,item "null") "")
     (t ,item)))

(defmacro bool-of-string (item)
  `(cond
     ((equal ,item "false")         nil)
     ((equal ,item "no")            nil)
     ((equal ,item "0")             nil)
     ((equal ,item "F")             nil)
     ((equal ,item "FALSE")         nil)
     ((equal ,item "n")             nil)
     ((equal ,item "NO")            nil)
     ((equal ,item nil)             nil)
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
(defparameter language-pat "language:\\[([^\\]]+)\\]")
(defparameter dnis-pat "dnis:\\[([^\\]]*)\\]")
(defparameter ani-pat "ani:\\[([^\\]]*)\\]")
(defparameter businessUnit-pat "businessUnit:\\[([^\\]]*)\\]")
(defparameter callType-pat "callType:\\[([^\\]]*)\\]")
(defparameter customerUsingGet-pat "Executing findCustomerUsingGET ucid\\[([^\\]]*)\\] ani\\[([^\\]]*)\\] ced\\[([^\\]]*)\\] accountNumber\\[([^\\]]*)\\] dnis\\[([^\\]]*)\\] siteId\\[([^\\]]*)\\]")
(defparameter accountNumber-pat "accountNumber: ([0-9]+)")
(defparameter authentication-pat "authenticationEligible:\\[(true|false)\\],voiceBioEnrolled:\\[(yes|no)\\]")
(defparameter catCode-pat "categoryCode: ([0-9]+)")
(defparameter catCodeSales-pat "catCodesForSalesTransfer: (.*)")
(defparameter createdToken-pat "Created _TOKEN \\[([^\\]]+)\\]")
(defparameter aniLookup-pat "\\[([^\\]]+)\\] Performing ANI Lookup")
(defparameter dtmfOnly-pat "dtmfOnly:\\[(true|false)\\]")
(defparameter cableProfileAcct-pat "intent0110_Routing_DS: cableProfile \\(accountNumber\\[([^\\]]+)\\] accountStatus\\[([A-Z])\\]\\)")
(defparameter identifiedFlag-pat "identifiedFlag: \\[(true|false)\\]")
(defparameter visitedNode-pat "isVisitedNode\\(([^\\)]+)\\): (false|true)")
(defparameter businessUnitReturnCode-pat "businessUnitReturnCode : (.*)")
(defparameter businessUnitInfo-pat "BusinessUnitInfo value: BusinessUnitInfo \\[siteID=([0-9]+), businessUnit=(.*), returnCode=(.*)\\]")
(defparameter playTransfer-pat "routingCode\\[(.*)\\] lastVisitedModule\\[(.*)\\] transferMsgPlayedFlag\\[(.*)\\] portalName\\[(.*)\\] identifiedFlag\\[(.*)\\] delinquientLevel\\[(.*)\\] genericInterceptFOFlag\\[(.*)\\] retryZipCode\\[(.*)\\] botEligible\\[(.*)\\] oofFlag\\[(.*)\\] isPlayFreeSpecmoNotice\\[(.*)\\] callType\\[(.*)\\] preAuthLastModule\\[(.*)\\]")

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
(defmacro parse-language (line)
  `(ppcre:register-groups-bind (node) (language-pat ,line)
    (make-instance 'language
                   'item node)))
(defmacro parse-dnis (line)
  `(ppcre:register-groups-bind (node) (dnis-pat ,line)
    (make-instance 'dnis
                   'item node)))
(defmacro parse-ani (line)
  `(ppcre:register-groups-bind (node) (ani-pat ,line)
    (make-instance 'ani
                   'item node)))
(defmacro parse-businessUnit (line)
  `(ppcre:register-groups-bind (node) (businessUnit-pat ,line)
    (make-instance 'businessUnit
                   'item node)))
(defmacro parse-callType (line)
  `(ppcre:register-groups-bind (node) (callType-pat ,line)
    (make-instance 'callType
                   'item node)))
(defmacro parse-customerUsingGet (line)
  `(ppcre:register-groups-bind (ucid ani ced acct dnis sid) (customerUsingGet-pat ,line)
    (make-instance 'customerUsingGet
                   'ucid ucid
                   'ani ani
                   'ced ced
                   'acct acct
                   'dnis dnis
                   'siteId sid)))
(defmacro parse-accountNumber (line)
  `(ppcre:register-groups-bind (node) (accountNumber-pat ,line)
    (make-instance 'accountNumber
                   'item node)))
(defmacro parse-authentication (line)
  `(ppcre:register-groups-bind (elig vbio) (authentication-pat ,line)
    (make-instance 'authentication
                   'authenticationEligible (bool-of-string elig)
                   'voiceBioEnrolled (bool-of-string vbio))))
(defmacro parse-catCode (line)
  `(ppcre:register-groups-bind (node) (catCode-pat ,line)
    (make-instance 'catCode
                   'item node)))
(defmacro parse-catCodeSales (line)
  `(ppcre:register-groups-bind (codes) (catCodeSales-pat ,line)
    (make-instance 'catCodeSales
                   'item (ppcre:split "," codes))))
(defmacro parse-createdToken (line)
  `(ppcre:register-groups-bind (node) (createdToken-pat ,line)
    (make-instance 'createdToken
                   'item node)))
(defmacro parse-aniLookup (line)
  `(ppcre:register-groups-bind (node) (aniLookup-pat ,line)
    (make-instance 'aniLookup
                   'item node)))
(defmacro parse-dtmfOnly (line)
  `(ppcre:register-groups-bind (node) (dtmfOnly-pat ,line)
    (make-instance 'dtmfOnly
                   'item (bool-of-string node))))
(defmacro parse-cableProfileAcct (line)
  `(ppcre:register-groups-bind (num status) (cableProfileAcct-pat ,line)
    (make-instance 'cableProfileAcct
                   'accountNumber num
                   'accountStatus status)))
(defmacro parse-identifiedFlag (line)
  `(ppcre:register-groups-bind (node) (identifiedFlag-pat ,line)
    (make-instance 'identifiedFlag
                   'item (bool-of-string node))))
(defmacro parse-visitedNode (line)
  `(ppcre:register-groups-bind (fn vn) (visitedNode-pat ,line)
    (make-instance 'visitedNode
                   'funcName fn
                   'visited (bool-of-string vn))))
(defmacro parse-businessUnitReturnCode (line)
  `(ppcre:register-groups-bind (node) (businessUnitReturnCode-pat ,line)
    (make-instance 'businessUnitReturnCode
                   'item node)))
(defmacro parse-businessUnitInfo (line)
  `(ppcre:register-groups-bind (sid bu rc) (businessUnitInfo-pat ,line)
    (make-instance 'businessUnitInfo
                   'siteId sid
                   'businessUnit bu
                   'returnCode rc)))
(defmacro parse-playTransfer (line)
  `(ppcre:register-groups-bind (rc lvm tmpf pn idf dl gifof rzc be oof ipfsn ct palm) (playTransfer-pat ,line)
    (make-instance 'playTransfer
                   'routingCode rc
                   'lastVisitedModule lvm
                   'transferMsgPlayedFlag (bool-of-string tmpf)
                   'portalName pn
                   'identifiedFlag (bool-of-string idf)
                   'delinquentLevel dl
                   'genericInterceptFOFlag (bool-of-string gifof)
                   'retryZipCode (bool-of-string rzc)
                   'botEligible (bool-of-string be)
                   'oofFlag (bool-of-string oof)
                   'isPlayFreeSpecmoNotice (bool-of-string ipfsn)
                   'callType ct
                   'preAuthLastModule palm)))
                                
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
    ((ppcre:scan language-pat line)             (parse-language line))
    ((ppcre:scan dnis-pat line)                 (parse-dnis line))
    ((ppcre:scan ani-pat line)                  (parse-ani line))
    ((ppcre:scan businessUnit-pat line)         (parse-businessUnit line))
    ((ppcre:scan callType-pat line)             (parse-callType line))
    ((ppcre:scan customerUsingGet-pat line)     (parse-customerUsingGet line))
    ((ppcre:scan accountNumber-pat line)        (parse-accountNumber line))
    ((ppcre:scan authentication-pat line)       (parse-authentication line))
    ((ppcre:scan catCode-pat line)              (parse-catCode line))
    ((ppcre:scan catCodeSales-pat line)         (parse-catCodeSales line))
    ((ppcre:scan createdToken-pat line)         (parse-createdToken line))
    ((ppcre:scan aniLookup-pat line)            (parse-aniLookup line))
    ((ppcre:scan dtmfOnly-pat line)             (parse-dtmfOnly line))
    ((ppcre:scan cableProfileAcct-pat line)     (parse-cableProfileAcct line))
    ((ppcre:scan identifiedFlag-pat line)       (parse-identifiedFlag line))
    ((ppcre:scan visitedNode-pat line)          (parse-visitedNode line))
    ((ppcre:scan businessUnitReturnCode-pat line) (parse-businessUnitReturnCode line))
    ((ppcre:scan playTransfer-pat line)         (parse-playTransfer line))
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


