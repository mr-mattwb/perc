import datetime
import re
from datetime import datetime

class Data:
    def __init__(self, msg):
        self.parse(msg)

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

    def to_str(self):
        return f"{self.__class__.__name__}"

    def print_data(self):
        print(self.to_str())

    def parse(self, msg):
        f"{self.__class__.__name__}"


class Line(Data):
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
        print(self.to_str())

    def to_str(self):
        return f"{self.__class__.__name__} item[{self.item}]"
        

class Label(Line):
    pat = r"^Label: (.*)"

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
    def to_str(self):
        return f"Link from[{self.link_from}] to[{self.link_to}]"

class LastVisitedModule(Line):
    pat = r"^lastVisitedModule: \[(.*)\]"

class PortalName(Line):
    pat = r"^portalName: \[(.*)\]"

class ReturnNode(Line):
    pat = r"^Return\[(.*)\]"

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
    def to_str(self):
        return(f"{self.__class__.__name__} returnValue[{self.returnValue}] identifiedFlag[{self.identifiedFlag}] lastVisitedModule[{self.lastVisitedModule}] appTag[{self.appTag}] intentIntercept[{self.intentIntercept}] dialedPortalName[{self.dialedPortalName}]")

class ReturnValue(Line):
    pat = r"^returnValue:\[(.*)\]"

class NodeHostname(Line):
    pat = r"^NODE_HOSTNAME \[(.*)\]"

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
    def to_str(self):
        return(f"{self.__class__.__name__} dnis[{self.dnis}] ani[{self.ani}] ucid[{self.ucid}] firstHistoryInfoUser[{self.firstHistoryInfoUser}] lastHistoryInfoUser[{self.lastHistoryInfoUser}] receivedUcid[{self.receivedUcid}] receivedUui[{self.receivedUui}]")

class Language(Line):
    pat = r"^language:\[(.*)\]"

class Dnis(Line):
    pat = r"^dnis:\[(.*)\]"

class Ani(Line):
    pat = r"^ani:\[(.*)\]"

class CallType(Line):
    pat = r"^callType:\[(.*)\]"

class BusinessUnit(Line):
    pat = r"^businessUnit:\[(.*)\]"

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

    def to_str(self):
        return(f"{self.__class__.__name__} ucid[{self.ucid}] ani[{self.ani}] ced[{self.ced}] acct[{self.acct}] dnis[{self.dnis}] siteId[{self.siteId}]")
 
class AcctNumber(Line):
    pat = r"^accountNumber: (.*)"

class Authentication(Data):
    authEligible = False
    voiceBioEnrolled = False
    pat = r"^authenticationEligible:\[(.*)\],voiceBioEnrolled:\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.authEligible = self.bool_of_str(a.group(1))
        self.voiceBioEnrolled = self.bool_of_str(a.group(2))
    
    def to_str(self):
        return(f"{self.__class__.__name__} authEligible[{self.authEligible}] voiceBioEnrolled[{self.voiceBioEnrolled}]")

class CategoryCode(Line):
    pat = r"^categoryCode: (.*)"

class InitConfig(Data):
    name = ""
    path = ""
    pat = r"initConfiguration: (.*) : (.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.name = a.group(1)
        self.path = a.group(2)
    def to_str(self):
        return(f"{self.__class__.__name__} name[{self.name}] path[{self.path}]")

class InvocationCounter(Line):
    pat = r"InvocationCounter.valueUnbound: call(start|end) was called \[0\]  times but it should have been called exactly once"

class CatCodesForSales(Line):
    pat = r"catCodesForSalesTransfer: (.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.item = a.group(1).split(",")

class PlayTransfer(Data):
    routingCode = ""
    lastVisitedModule = ""
    transerMsgPlayedFlag = False
    portalName = ""
    identifiedFlag = False
    delinquientLevel = ""
    genericInterceptFOFlag = False
    retryZipCode = False
    botEligible = False
    oofFlag = False
    isPlayFreeSpecmoNotice = False
    callType = "Residential"
    preAuthLastModule = ""
    pat = r"routingCode\[(.*)\] lastVisitedModule\[(.*)\] transferMsgPlayedFlag\[(.*)\] portalName\[(.*)\] identifiedFlag\[(.*)\] delinquientLevel\[(.*)\] genericInterceptFOFlag\[(.*)\] retryZipCode\[(.*)\] botEligible\[(.*)\] oofFlag\[(.*)\] isPlayFreeSpecmoNotice\[(.*)\] callType\[(.*)\] preAuthLastModule\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.routingCode = a.group(1)
        self.lastVisitedModule = a.group(2)
        self.transferMsgPlayedFlag = self.bool_of_str(a.group(3))
        self.portalName = a.group(4)
        self.identifiedFlag = self.bool_of_str(a.group(5))
        self.delinquientLevel = a.group(6)
        self.genericInterceptFOFlag = self.bool_of_str(a.group(7))
        self.retryZipCode = self.bool_of_str(a.group(8))
        self.botEligible = self.bool_of_str(a.group(9))
        self.oofFlag = self.bool_of_str(a.group(10))
        self.isPlayFreeSpecmoNotice = self.bool_of_str(a.group(11))
        self.callType = a.group(12)
        self.preAuthLastModule = a.group(13)
    def to_str(self):
        return(f"{self.__class__.__name__} routingCode[{self.routingCode}] lastVisitedModule[{self.lastVisitedModule}] transferMsgPlayedFlag[{self.transferMsgPlayedFlag}] portalName[{self.portalName}] identifiedFlag[{self.identifiedFlag}] delinquientLevel[{self.delinquientLevel}] genericInterceptFOFlag[{self.genericInterceptFOFlag}] retryZipCode[{self.retryZipCode}] botEligible[{self.botEligible}] oofFlag[{self.oofFlag}] isPlayFreeSpecmoNotice[{self.isPlayFreeSpecmoNotice}] callType[{self.callType}] preAuthLastModule[{self.preAuthLastModule}]")

class CatCodeEligTransfer(Line):
    pat = r"^catCodeEligigbleForSalesTransfer: (.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.item = self.bool_of_str(a.group(1))

class CreatedToken(Line):
    pat = r"^Created _TOKEN \[(.*)\]"

class AniLookup(Line):
    pat = r"^\[(.*)\] Performing ANI Lookup"

class DtmfOnly(Line):
    pat = r"^dtmfOnly:\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.item = self.bool_of_str(a.group(1))

class CableProfileAccount(Data):
    acctNumber = ""
    acctStatus = ""
    pat = r"intent0110_Routing_DS: cableProfile \(accountNumber\[(.*)\] accountStatus\[(.*)\]\)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.acctNumber = a.group(1)
        self.acctStatus = a.group(2)
    def to_str(self):
        return(f"{self.__class__.__name__} Number[{self.acctNumber}] Status[{self.acctStatus}]")

class IdentifiedFlag(Line):
    pat = r"^identifiedFlag: \[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.item = self.bool_of_str(a.group(1))

class VisitedNode(Data):
    node = ""
    visited = False
    pat = r"isVisitedNode\((.*)\): (.*)"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.node = a.group(1)
        self.visited = self.bool_of_str(a.group(2))
    def to_str(self):
        return(f"{self.__class__.__name__} Node[{self.node}] Visited[{self.visited}]")

class BusinessUnitReturnCode(Line):
    pat = r"businessUnitReturnCode : (.*)"

class BusinessUnitInfo(Data):
    siteId = ""
    businessUnit = ""
    returnCode = ""
    pat = r"BusinessUnitInfo value: BusinessUnitInfo \[siteID=(.*), businessUnit=(.*), returnCode=(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.siteId = a.group(1)
        self.businessUnit = a.group(2)
        self.returnCode = a.group(3)
    def to_str(self):
        return(f"{self.__class__.__name__} siteId[{self.siteId}] businessUnit[{self.businessUnit}] returnCode[{self.returnCode}]")

class ExecSql(Line):
    pat = r"executing >(.*)<"

class Table(Data):
    table = {}
    pat = r""
    def parse(self, msg):
        a = re.match(self.pat, msg)
        b = a.group(1).split(", ")
        for item in b:
            kv = item.split("=")
            self.table[kv[0]] = kv[1]
    def to_str(self):
        return f"{self.__class__.__name__} table [{self.table}]"

class BusinessUnitTable(Table):
    pat = r"businessUnitTable={(.*)}"

class PortalMap(Table):
    pat = r"PortalMap\[{(.*)}\]"

class CatCodeTable(Table):
    pat = r"categoryCodeTable={(.*)}"

class ProfileInfoUsingGet(Data):
    ucid = ""
    acctNumber = ""
    businessUnit = ""
    siteId = ""
    mobileAcctNumber = ""
    dnis = ""
    pat = r"Executing getProfileInfoUsingGET ucid\[(.*)\] accountNumber\[(.*)\] businessUnit\[(.*)\] siteId\[(.*)\] mobileAccountNumber\[(.*)\] dnis\[(.*)\]"
    def parse(self, msg):
        a = re.match(self.pat, msg)
        self.ucid = a.group(1)
        self.acctNummber = a.group(2)
        self.businessUnit = a.group(3)
        self.siteId = a.group(4)
        self.mobileAcctNumber = a.group(5)
        self.dnis = a.group(6)
    def to_str(self):
        return f"{self.__class__.__name__} ucid[{self.ucid}] acctNumber[{self.acctNumber}] businessUnit[{self.businessUnit}] siteId[{self.siteId}] mobileAcctNumber[{self.mobileAcctNumber}] dnis[{self.dnis}]"

class Entry:
    date = ""
    time = ""
    msec = 0
    identifier = ""
    version = ""
    priority = ""
    funcName = ""
    data = ""
    pattern = r"([1-2][0-9][0-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]),([0-9][0-9][0-9])\|([0-9A-F]*)\|([^\-]+)-(DEBUG|INFO|WARN|ERROR)[ ]*\|([^\|]+)\|(.*)"

    def __init__(self):
        items = str(datetime.today().strftime("%Y-%m-%d")).split("-")
        self.date = self.date_of_strs(items[0], items[1], items[2])
        items = str(datetime.today().strftime("%H:%M:%S")).split(":")
        self.time = self.time_of_strs(items[0], items[1], items[2])

    def __init__(self, line):
        self.parse(line)

    def date_of_ints(self, yr, mo, da):
        return (yr * 10000) + (mo * 100) + da

    def date_of_strs(self, yr, mo, da):
        return self.date_of_ints(int(yr), int(mo), int(da))

    def time_of_ints(self, hr, mi, se):
        return (hr * 10000) + (mi * 100) + se

    def time_of_strs(self, hr, mi, se):
        return self.time_of_ints(int(hr), int(mi), int(se))

    def parse(self, msg):
        if (re.search(self.pattern, msg)):
            m = re.match(self.pattern, msg)
            self.date = self.date_of_strs(m.group(1), m.group(2), m.group(3))
            self.time = self.time_of_strs(m.group(4), m.group(5), m.group(6))
            self.msec = int(m.group(7))
            self.identifier = m.group(8)
            self.version = m.group(9)
            self.priority = m.group(10)
            self.funcName = m.group(11)
            self.data = self.parse_data(m.group(12))

    def to_str(self):
        return(f"{self.__class__.__name__} date[{self.date}] time[{self.time}] msec[{self.msec}] identifier[{self.identifier}] version[{self.version}] priority[{self.priority}] function[{self.funcName}] data[{self.data.to_str()}]")

    def print_data(self):
        print(self.to_str())

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
        elif (re.search(InvocationCounter.pat, msg)):
            return InvocationCounter(msg)
        elif (re.search(CatCodesForSales.pat, msg)):
            return CatCodesForSales(msg)
        elif (re.search(PlayTransfer.pat, msg)):
            return PlayTransfer(msg)
        elif (re.search(CatCodeEligTransfer.pat, msg)):
            return CatCodeEligTransfer(msg)
        elif (re.search(CreatedToken.pat, msg)):
            return CreatedToken(msg)
        elif (re.search(AniLookup.pat, msg)):
            return AniLookup(msg)
        elif (re.search(DtmfOnly.pat, msg)):
            return DtmfOnly(msg)
        elif (re.search(CableProfileAccount.pat, msg)):
            return CableProfileAccount(msg)
        elif (re.search(IdentifiedFlag.pat, msg)):
            return IdentifiedFlag(msg)
        elif (re.search(VisitedNode.pat, msg)):
            return VisitedNode(msg)
        elif (re.search(BusinessUnitReturnCode.pat, msg)):
            return BusinessUnitReturnCode(msg)
        elif (re.search(BusinessUnitInfo.pat, msg)):
            return BusinessUnitInfo(msg)
        elif (re.search(ExecSql.pat, msg)):
            return ExecSql(msg)
        elif (re.search(BusinessUnitTable.pat, msg)):
            return BusinessUnitTable(msg)
        elif (re.search(PortalMap.pat, msg)):
            return PortalMap(msg)
        elif (re.search(CatCodeTable.pat, msg)):
            return CatCodeTable(msg)
        elif (re.search(ProfileInfoUsingGet.pat, msg)):
            return ProfileInfoUsingGet(msg)
        else:
            return Data(msg)

class Entries:
    infile = "entries.log" 
    entries = {}

    def __init__(self, fname):
        self.infile = fname
        self.load()

    def load(self):
        i = 1
        with open(self.infile, 'r') as fin:
            for line in fin:
                if (len(line) <= 2000):
                    self.entries[i] = Entry(line)
                    i = i + 1

#entries = Entries("logs/ndf.log")
