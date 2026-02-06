import Data.Text
import Data.Char
import Text.Regex
import Text.Regex.Posix

yearpat :: String
yearpat = "[1-2][0-9][0-9][0-9]"

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

mylen :: [Int] -> Int
mylen xs = Prelude.length xs

head' :: [a] -> a
head' [] = error "Does anyone do a great impression of Macho man?"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty String"
capital all@(x:xs) = "First Letter of [" ++ all ++ "] is " ++ [x]

densityTell :: (RealFloat a) => a -> a -> String
densityTell mass volume
    | density < 1.2 = "Ride in sky"
    | density <= 1000.0 = "Swimming"
    | otherwise = "You will sink"
    where density = mass / volume

date_of_ints :: Int -> Int -> Int -> Int
date_of_ints yr mo da = (yr * 10000) + (mo * 100) + da

date_of_strs :: String -> String -> String -> Int
date_of_strs yr mo da = date_of_ints ((read yr)::Int) ((read mo)::Int) ((read da)::Int)

time_of_ints :: Int -> Int -> Int -> Int
time_of_ints hr mi se = (hr * 10000) + (mi * 100) + se

time_of_strs :: String -> String -> String -> Int
time_of_strs hr mi se = time_of_ints ((read hr)::Int) ((read mi)::Int) ((read se)::Int)
 
--(10000 * (read yr)::Int) + (100 * ((read mo)::Int)) + ((read da)::Int)
 
--date = "1999-10-31"

parse_date1 :: String -> Int
parse_date1 d = date_of_strs (Prelude.head ar) (Prelude.head (Prelude.tail ar)) (Prelude.head (Prelude.tail (Prelude.tail ar)))
    where ar = splitRegex (mkRegex "-") d

year_pat = "([1-2][0-9][0-9][0-9])"
month_pat = "(0[1-9]|1[0-2])"
day_pat = "(0[1-9]|3[0-1]|[1-2][0-9])"
date_pat = year_pat ++ "-" ++ month_pat ++ "-" ++ day_pat

hour_pat = "([0-1][0-9]|2[0-3])"
minute_pat = "([0-5][0-9])"
second_pat = "([0-5][0-9])"
time_pat = hour_pat ++ ":" ++ minute_pat ++ ":" ++ second_pat

msec_pat = "([0-9][0-9][0-9])"
iden_pat = "([0-9A-F]*)"
vers_pat = "([0-9A-z\\.]*)"
prio_pat = "(DEBUG|INFO|WARN|ERROR)"
func_pat = "([^\\|]*)"

parse_date :: String -> Int
parse_date str = date_of_strs year month day
    where (_, _, _, [year, month, day]) = str =~ date_pat :: (String,String,String,[String])
 
parse_time :: String -> Int
parse_time str = time_of_strs hour minute second
    where (_, _, _, [hour, minute, second]) = str =~ time_pat :: (String,String,String,[String])

data Priority = DEBUG | INFO | WARN | ERROR
    deriving (Show,Ord,Eq,Read)

bool_of_str :: String -> Bool
bool_of_str v
    | v!!0 == 'T' || v!!0 == 't' = True
    | otherwise = False

null_str :: String -> String
null_str v 
    | v=="null" = ""
    | otherwise = v

link_pat = "chaining from >(.*)< to >(.*)<"
label_pat = "Label: (.*)"
state_pat = "(.*): entering state"
nextModule_pat = "nextModule: \\[(.*)\\]"
returnValue_pat = "Return\\[(.*)\\]"
lastVisitedModule_pat = "lastVisitedModule: \\[(.*)\\]"
lastVisitedModule2_pat = "lastVisitedModule : (.*)"
portalName_pat = "portalName: \\[(.*)\\]"
routingDS_pat = "intent0110_Routing_DS: returnValue\\[(.*)\\] identifiedFlag\\[(.*)\\] lastVisitedModule\\[(.*)\\] appTag\\[(.*)\\] intentIntercept\\[(.*)\\]dialedPortalName\\[(.*)\\]"
returnVal_pat = "returnValue:\\[(.*)\\]"
nodeHostname_pat = "NODE_HOSTNAME \\[(.*)\\]"
infoUser_pat = "DNIS :(.*) ANI :(.*) UCID :(.*) FIRSTHISTORYINFOUSER :(.*) LASTHISTORYINFOUSER :(.*) RECEIVED_UCID :(.*) RECEIVED_UUI :(.*)"

data LogData = 
      Link { from :: String, to :: String } 
    | Label { label :: String }
    | State { state :: String }
    | NextModule { nextModule :: String }
    | ReturnValue { returnValue :: String }
    | LastVisitedModule { lastVisitedModule :: String }
    | PortalName { portalName :: String }
    | RoutingDS { 
            returnValue :: String,
            identifiedFlag :: Bool,
            lastVisitedModule :: String,
            appTag :: String,
            intentIntercept :: String,
            dialedPortalName :: String
        }
    | ReturnVal { returnValue :: String }
    | NodeHostname { host :: String }
    | InfoUser {
            dnis :: String,
            ani :: String,
            ucid :: String,
            firstHistoryInfoUser :: String,
            lastHistoryInfoUser :: String,
            receivedUcid :: String,
            receivedUui :: String
        }
    | Other { other :: String } deriving (Show,Eq)

parse_link :: String -> LogData
parse_link msg = Link { from = nfrom, to = nto }
    where (_,_,_,[nfrom,nto]) = msg =~ link_pat :: (String,String,String,[String])

parse_label :: String -> LogData
parse_label msg = Label { label = lbl }
    where (_,_,_,[lbl]) = msg =~ label_pat :: (String,String,String,[String])

parse_state :: String -> LogData
parse_state msg = State { state = st }
    where (_,_,_,[st]) = msg =~ state_pat :: (String,String,String,[String])

parse_nextModule :: String -> LogData
parse_nextModule msg = NextModule { nextModule = nm }
    where (_,_,_,[nm]) = msg =~ nextModule_pat :: (String,String,String,[String])

parse_returnValue :: String -> LogData
parse_returnValue msg = ReturnValue { returnValue = rv }
    where (_,_,_,[rv]) = msg =~ returnValue_pat :: (String,String,String,[String])

parse_lastVisitedModule :: String -> LogData
parse_lastVisitedModule msg = LastVisitedModule { lastVisitedModule = lvm }
    where (_,_,_,[lvm]) = msg =~ lastVisitedModule_pat :: (String,String,String,[String])

parse_lastVisitedModule2 :: String -> LogData
parse_lastVisitedModule2 msg = LastVisitedModule { lastVisitedModule = lvm }
    where (_,_,_,[lvm]) = msg =~ lastVisitedModule2_pat :: (String,String,String,[String])

parse_portalName :: String -> LogData
parse_portalName msg = PortalName { portalName = pn }
    where (_,_,_,[pn]) = msg =~ portalName_pat :: (String,String,String,[String])

parse_routingDS :: String -> LogData
parse_routingDS msg = RoutingDS { returnValue = rv, identifiedFlag = (bool_of_str idf)::Bool, lastVisitedModule = lvm, appTag = at,
                                  intentIntercept = ii, dialedPortalName = dpn }
    where (_,_,_,[rv,idf,lvm,at,ii,dpn]) = msg =~ routingDS_pat :: (String,String,String,[String])

parse_returnVal :: String -> LogData
parse_returnVal msg = ReturnVal { returnValue = rv }
    where (_,_,_,[rv]) = msg =~ returnVal_pat :: (String,String,String,[String])

parse_nodeHostname :: String -> LogData
parse_nodeHostname msg = NodeHostname { host = nh }
    where (_,_,_,[nh]) = msg =~ nodeHostname_pat :: (String,String,String,[String])

parse_infoUser :: String -> LogData
parse_infoUser msg = InfoUser { dnis = dn, ani = an, ucid = uc, firstHistoryInfoUser = fhiu, lastHistoryInfoUser = lhiu,
                                receivedUcid = (null_str rucid), receivedUui = (null_str ruui) }
    where (_,_,_,[dn,an,uc,fhiu,lhiu,rucid,ruui]) = msg =~ infoUser_pat :: (String,String,String,[String])

log_data :: String -> LogData
log_data msg 
    | msg =~ link_pat = parse_link msg
    | msg =~ label_pat = parse_label msg
    | msg =~ state_pat = parse_state msg
    | msg =~ nextModule_pat = parse_nextModule msg
    | msg =~ returnValue_pat = parse_returnValue msg
    | msg =~ lastVisitedModule_pat = parse_lastVisitedModule msg
    | msg =~ lastVisitedModule2_pat = parse_lastVisitedModule2 msg
    | msg =~ portalName_pat = parse_portalName msg
    | msg =~ routingDS_pat = parse_routingDS msg
    | msg =~ returnVal_pat = parse_returnVal msg
    | msg =~ nodeHostname_pat = parse_nodeHostname msg
    | msg =~ infoUser_pat = parse_infoUser msg
    | otherwise = Other { other = msg }

data Entry = Entry {
    date :: Int, 
    time :: Int,
    msec :: Int,
    identity :: String,
    version :: String,
    priority :: Priority,
    funcName :: String,
    logData :: LogData
} deriving (Show,Eq)

entry_pat = date_pat ++ "T" ++ time_pat ++ "," ++ msec_pat ++ "\\|" ++ iden_pat ++ "\\|" ++ 
            vers_pat ++ "\\-" ++ prio_pat ++ "[ ]*\\|" ++ func_pat ++ "\\|" ++ "(.*)"

parse_entry :: String -> Entry
parse_entry msg = Entry {date=date_of_strs year month day, time=time_of_strs hour minute second, msec=(read ms)::Int, identity=id, 
                            version=vers, priority=(read prio)::Priority, funcName=func, logData=(log_data rest) }
    where (_,_,_,[year,month,day,hour,minute,second,ms,id,vers,prio,func,rest]) = msg =~ entry_pat :: (String,String,String,[String])



linkline = "2025-08-25T10:59:33,343|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >end2025_BackendLogging_DB< to >end2030_ReportingLogging_DB<"
labelline = "2025-08-25T10:59:32,005|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: StartSession failure"
stateline = "2025-08-25T05:28:20,408|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state"
nextline = "2025-08-25T05:30:09,175|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|decision.controller_return_DS|nextModule: [actall]"
returnline = "2025-08-25T05:38:24,123|DF9941BC970FB64FF98CB929644CD997|MOD25.09.0.004-INFO |reporting.CDRDialogModuleState|Return[portal0225_GenericInterceptQuestion_DM_No_DS]"
lastvisitedline = "2025-08-25T05:49:19,893|B65B8A99CF70FD9AA3F29042C65B820B|MOD25.09.0.004-DEBUG|decision.portal0110_Routing_DS|lastVisitedModule: [start]"
lastvisitedline2 ="2025-08-25T05:30:14,748|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|decision.ts0105_Routing_DS|lastVisitedModule : outage"
portalline = "2025-08-25T06:01:11,409|5115444DD196B688BBA0139DBF7354D5|MOD25.09.0.004-DEBUG|decision.welcid0705_GetPhoneAccountNumber_DM_DS|portalName: [MainResidential]"
intent0110line = "2025-08-25T06:05:55,707|87506A078C3752E95DE004603FD6BAEB|MOD25.09.0.004-DEBUG|decision.intent0110_Routing_DS|intent0110_Routing_DS: returnValue[task_general_bcm] identifiedFlag[true] lastVisitedModule[bcm] appTag[] intentIntercept[]dialedPortalName[]"
returnValueline = "2025-08-25T10:58:47,558|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.Initialize|returnValue:[task_check_ani_match]"
nodehostnameline = "2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|NODE_HOSTNAME [comswmncwunxq07]"
infouserline = "2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|DNIS :11071905113 ANI :9542376966 UCID :119946ED68AC7A04 FIRSTHISTORYINFOUSER :8558955883 LASTHISTORYINFOUSER :8558955883 RECEIVED_UCID :null RECEIVED_UUI :null"
