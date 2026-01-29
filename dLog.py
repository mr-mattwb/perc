import datetime
import re
from datetime import datetime

class Data:
    def __init__(self, msg):
        self.parse(msg)

    def print_data(self):
        print(f"{self.__class__.__name__}")

    def bool_of_str(self, b):
        match (b.upper()):
            case "TRUE":
                return True
            case "YES":
                return True
            case "T":
                return True
            case "Y":
                return True
            case "1":
                return True
            case "FALSE":
                return False
            case "NO":
                return False
            case "F":
                return False
            case "N":
                return False
            case "0":
                return False
            case _:
                return False
    def nullstr(b):
        if (b.upper() == "NULL"):
            return ""
        else:
            return b

class Line:
    item = ""
    pat = r""

    def __init__(self, msg):
        self.parse(msg)

    def parse(self, msg):
        self.parse_line(self.pat, msg)

    def parse_line(self, pat, msg):
        a = re.match(pat, msg)
        self.item = a.group(1)

    def print_data(self):
        print(f"{self.__class__.__name__} item[{self.item}]")

class Label(Line):
    pat = r"Label: (.*)"

class State(Line):
    pat = r"(.*): entering state"

class NextModule(Line):
    pat = r"nextModule: \[(.*)\]"

class Link(Data):
    link_from = ""
    link_to = ""
    pat = r"chaining from >(.*)< to >(.*)<"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.link_from = a.group(1)
        self.link_to = a.group(2)
    def print_data(self):
        print(f"Link from[{self.link_from}] to[{self.link_to}]")

class LastVisitedModule(Line):
    pat = r"lastVisitedModule: \[(.*)\]"

class PortalName(Line):
    pat = r"portalName: \[(.*)\]"

class ReturnNode(Line):
    pat = r"Return\[(.*)\]"

class RoutingDS(Data):
    returnValue = ""
    identifiedFlag = False
    lastVisitedModule = ""
    appTag = ""
    intentIntercept = ""
    dialedPortalName = ""
    pat = r"intent0110_Routing_DS: returnValue\[(.*)\] identifiedFlag\[(.*)\] lastVisitedModule\[(.*)\] appTag\[(.*)\] intentIntercept\[(.*)\]dialedPortalName\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.returnValue = a.group(1)
        self.identifiedFlag = self.bool_of_str(a.group(2))
        self.lastVisitedModule = a.group(3)
        self.appTag = a.group(4)
        self.intentIntercept = a.group(5)
        self.dialedPortalName = a.group(6)
    def print_data(self):
        print(f"{self.__class__.__name__} returnValue[{self.returnValue}] identifiedFlag[{self.identifiedFlag}] lastVisitedModule[{self.lastVisitedModule}] appTag[{self.appTag}] intentIntercept[{self.intentIntercept}] dialedPortalName[{self.dialedPortalName}]")

class ReturnValue(Line):
    pat = r"returnValue:\[(.*)\]"

class NodeHostname(Line):
    pat = r"NODE_HOSTNAME \[(.*)\]"

class InfoUser(Data):
    dnis = ""
    ani = ""
    ucid = ""
    firstHistoryInfoUser = ""
    lastHistoryInfoUser = ""
    receivedUcid = ""
    receivedUui = ""
    pat = r"DNIS :(.*) ANI :(.*) UCID :(.*) FIRSTHISTORYINFOUSER :(.*) LASTHISTORYINFOUSER :(.*) RECEIVED_UCID :(.*) RECEIVED_UUI :(.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.dnis = a.group(1)
        self.ani = a.group(2)
        self.ucid = a.group(3)
        self.firstHistoryInfoUser = a.group(4)
        self.lastHistoryInfoUser = a.group(5)
        self.receivedUcid = Data.nullstr(a.group(6))
        self.receivedUui = Data.nullstr(a.group(7))
    def print_data(self):
        print(f"{self.__class__.__name__} dnis[{self.dnis}] ani[{self.ani}] ucid[{self.ucid}] firstHistoryInfoUser[{self.firstHistoryInfoUser}] lastHistoryInfoUser[{self.lastHistoryInfoUser}] receivedUcid[{self.receivedUcid}] receivedUui[{self.receivedUui}]")

class Language(Line):
    pat = r"language:\[(.*)\]"

class Dnis(Line):
    pat = r"dnis:\[(.*)\]"

class Ani(Line):
    pat = r"ani:\[(.*)\]"

class CallType(Line):
    pat = r"callType:\[(.*)\]"

class BusinessUnit(Line):
    pat = r"businessUnit:\[(.*)\]"

class CustomerUsingGet(Data):
    ucid = ""
    ani = ""
    ced = ""
    acct = ""
    dnis = ""
    siteId = ""
    pat = r"Executing findCustomerUsingGET ucid\[(.*)\] ani\[(.*)\] ced\[(.*)\] accountNumber\[(.*)\] dnis\[(.*)\] siteId\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.ucid = a.group(1)
        self.ani = a.group(2)   
        self.ced = a.group(3)
        self.acct = a.group(4)
        self.dnis = a.group(5)
        self.siteId = a.group(6)

    def print_data(self):
        print(f"{self.__class__.__name__} ucid[{self.ucid}] ani[{self.ani}] ced[{self.ced}] acct[{self.acct}] dnis[{self.dnis}] siteId[{self.siteId}]")
 
class AcctNumber(Line):
    pat = r"accountNumber: (.*)"

class Authentication(Data):
    authEligible = False
    voiceBioEnrolled = False
    pat = r"authenticationEligible:\[(.*)\],voiceBioEnrolled:\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.authEligible = self.bool_of_str(a.group(1))
        self.voiceBioEnrolled = self.bool_of_str(a.group(2))
    def print_data(self):
        print(f"{self.__class__.__name__} authEligible[{self.authEligible}] voiceBioEnrolled[{self.voiceBioEnrolled}]")

class CategoryCode(Line):
    pat = r"categoryCode: (.*)"

class InitConfig(Data):
    name = ""
    path = ""
    pat = r"initConfiguration: (.*) : (.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.name = a.group(1)
        self.path = a.group(2)
    def print_data(self):
        print(f"{self.__class__.__name__} name[{self.name}] path[{self.path}]")

class Entry:
    date = ""
    time = ""
    msec = 0
    iden = ""
    vers = ""
    prio = ""
    func = ""
    data = ""

    pattern = r"([1-2][0-9][0-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]),([0-9][0-9][0-9])\|([0-9A-F]*)\|([^\-]+)-(DEBUG|INFO|WARN|ERROR)[ ]*\|([^\|]+)\|(.*)"

    def __init__(self):
        items = str(datetime.today().strftime("%Y-%m-%d")).split("-")
        self.date = self.date_of_strs(items[0], items[1], items[2])
        items = str(datetime.today().strftime("%H:%M:%S")).split(":")
        self.time = self.time_of_strs(items[0], items[1], items[2])

    def date_of_ints(self, yr, mo, da):
        return (yr * 10000) + (mo * 100) + da

    def date_of_strs(self, yr, mo, da):
        return self.date_of_ints(int(yr), int(mo), int(da))

    def time_of_ints(self, hr, mi, se):
        return (hr * 10000) + (mi * 100) + se

    def time_of_strs(self, hr, mi, se):
        return self.time_of_ints(int(hr), int(mi), int(se))

    def parse(self, msg):
        m = re.match(self.pattern, msg)
        self.date = self.date_of_strs(m.group(1), m.group(2), m.group(3))
        self.time = self.time_of_strs(m.group(4), m.group(5), m.group(6))
        self.msec = int(m.group(7))
        self.iden = m.group(8)
        self.vers = m.group(9)
        self.prio = m.group(10)
        self.func = m.group(11)
        self.data = self.parse_data(m.group(12))


    def parse_data(self, msg):
        if (re.search(Label.pat, msg)):
            return Label(msg)
        elif (re.search(State.pat, msg)):
            return State(msg)
        elif (re.search(NextModule.pat, msg)):
            return NextModule(msg)
        elif (re.search(Link.pat, msg)):
            return Link(msg)
        elif (re.search(LastVisitedModule.pat, msg)):
            return LastVisitedModule(msg)
        elif (re.search(PortalName.pat, msg)):
            return PortalName(msg)
        elif (re.search(ReturnNode.pat, msg)):
            return ReturnNode(msg)
        elif (re.search(RoutingDS.pat, msg)):
            return RoutingDS(msg)
        elif (re.search(ReturnValue.pat, msg)):
            return ReturnValue(msg)
        elif (re.search(NodeHostname.pat, msg)):
            return NodeHostname(msg)
        elif (re.search(InfoUser.pat, msg)):
            return InfoUser(msg)
        elif (re.search(Language.pat, msg)):
            return Language(msg)
        elif (re.search(Dnis.pat, msg)):
            return Dnis(msg)
        elif (re.search(Ani.pat, msg)):
            return Ani(msg)
        elif (re.search(BusinessUnit.pat, msg)):
            return BusinessUnit(msg)
        elif (re.search(CallType.pat, msg)):
            return CallType(msg)
        elif (re.search(CustomerUsingGet.pat, msg)):
            return CustomerUsingGet(msg)
        elif (re.search(AcctNumber.pat, msg)):
            return AcctNumber(msg)
        elif (re.search(Authentication.pat, msg)):
            return Authentication(msg)
        elif (re.search(CategoryCode.pat, msg)):
            return CategoryCode(msg)
        elif (re.search(InitConfig.pat, msg)):
            return InitConfig(msg)
        else:
            return msg

entry = Entry()

entry.parse("2025-08-25T10:59:32,005|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|Label: StartSession failure")
entry.data.print_data()
entry.parse("2025-08-25T05:28:20,408|0B1AE898790FD83FFA8795DE1460D032|MOD25.09.0.004-DEBUG|reporting.CDRUtil|populateCDR: entering state")
entry.data.print_data()
entry.parse("2025-08-25T05:30:09,175|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|decision.controller_return_DS|nextModule: [actall]")
entry.data.print_data()
entry.parse("2025-08-25T10:59:33,343|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >end2025_BackendLogging_DB< to >end2030_ReportingLogging_DB<")
entry.data.print_data()
entry.parse("2025-08-25T05:49:19,893|B65B8A99CF70FD9AA3F29042C65B820B|MOD25.09.0.004-DEBUG|decision.portal0110_Routing_DS|lastVisitedModule: [start]")
entry.data.print_data()
entry.parse("2025-08-25T06:01:11,409|5115444DD196B688BBA0139DBF7354D5|MOD25.09.0.004-DEBUG|decision.welcid0705_GetPhoneAccountNumber_DM_DS|portalName: [MainResidential]")
entry.data.print_data()
entry.parse("2025-08-25T05:38:24,123|DF9941BC970FB64FF98CB929644CD997|MOD25.09.0.004-INFO |reporting.CDRDialogModuleState|Return[portal0225_GenericInterceptQuestion_DM_No_DS]")
entry.data.print_data()
entry.parse("2025-08-25T06:05:55,707|87506A078C3752E95DE004603FD6BAEB|MOD25.09.0.004-DEBUG|decision.intent0110_Routing_DS|intent0110_Routing_DS: returnValue[task_general_bcm] identifiedFlag[true] lastVisitedModule[bcm] appTag[] intentIntercept[]dialedPortalName[]")
entry.data.print_data()
entry.parse(r"2025-08-25T10:58:47,558|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.Initialize|returnValue:[task_check_ani_match]")
entry.data.print_data()
entry.parse("2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|NODE_HOSTNAME [comswmncwunxq07]")
entry.data.print_data()
entry.parse("2025-08-25T10:58:47,498|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|DNIS :11071905113 ANI :9542376966 UCID :119946ED68AC7A04 FIRSTHISTORYINFOUSER :8558955883 LASTHISTORYINFOUSER :8558955883 RECEIVED_UCID :null RECEIVED_UUI :null")
entry.data.print_data()
entry.parse("2025-08-25T10:58:47,501|502254645BAB9D0F2EA85C87D9AAFF36|MOD25.09.0.004-DEBUG|decision.languageSwitchOB|language:[en-US]")
entry.data.print_data()
entry.parse("2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|dnis:[11071900013]")
entry.data.print_data()
entry.parse("2025-08-25T05:34:47,494|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|dataaccess.ANILookup|ani:[9940830410]")
entry.data.print_data()
entry.parse("2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|businessUnit:[CSGEAST]")
entry.data.print_data()
entry.parse("2025-08-25T05:34:47,451|50BE7C124CA2AA4580B0E6F38044AB4D|MOD25.09.0.004-DEBUG|decision.welcid0100_Initialize_DS|callType:[Residential]")
entry.data.print_data()
entry.parse("2025-08-25T05:34:47,496||MOD25.09.0.004-DEBUG|client.AccountAndProfileSearchServiceClient|Executing findCustomerUsingGET ucid[1199290D68AC2E23] ani[9940830410] ced[] accountNumber[] dnis[11071900013] siteId[]")
entry.data.print_data()
entry.parse("2025-08-25T07:00:43,045|4745BB4ED85C393E89D328AAB47ACB76|MOD25.09.0.004-DEBUG|dataaccess.data0307_AccountDetailsAppointment_DB|accountNumber: 8337100240261185")
entry.data.print_data()
entry.parse("2025-08-25T07:00:56,737|4745BB4ED85C393E89D328AAB47ACB76|MOD25.09.0.004-DEBUG|decision.end0240_CheckAuthenticationStart_DS|authenticationEligible:[true],voiceBioEnrolled:[no]")
entry.data.print_data()
entry.parse("2025-08-25T09:58:47,902|1176F1F498AA1DE50F8030B588BA7739|MOD25.09.0.004-DEBUG|audio.end0305_PlayTransferMessage_PP|categoryCode: 010")
entry.data.print_data()
entry.parse("2025-08-25T05:17:55,485||MOD25.09.0.004-DEBUG|ivr.ConfigurationAccessor|initConfiguration: callerIntentConfigPath : /usr/local/shared/nuance-mod-v25-09-0_qa_ncw_app-1/nuance/external_config/nuancemoddockerconfig/caller_intent_config/qa//")
entry.data.print_data()
