import datetime
import re
from enum import Enum

class Data:
    data = ""
    def __init__(self, msg):
        self.parse(msg)
    def __str__(self):
        return f"{self.__class__.__name__} [{self.data}]"
    def parse(self,line):
        self.data = line
        return self
    def nullstr(self,item):
        if item == "null":
            return ""

class Line(Data):
    pat = r"(.*)"
    def __str__(self):
        return f"{self.__class__.__name__} [{self.data}]"
    def parse(self, line):
        m = re.match(self.pat, line)
        self.data = m.group(1)
        return self

class Table(Data):
    table = {}
    pat = r"{(.*)}"
    def parse(self, line):
        m = re.match(self.pat,line)
        keyval = re.split(", ", m.group(1));
        for item in keyval:
            items = re.split("=", item);
            self.table[items[0]] = items[1]
        return self
    def __str__(self):
        return f"{self.__class__.__name__} Table[{self.table}]"

class Label(Line):
    pat = r"^Label: (.*)"

class Link(Data):
    pat = r"chaining from >(.*)< to >(.*)<"
    link_from = ""
    link_to = ""
    def __str__(self):
        return f"{self.__class__.__name__} from[{self.link_from}] to[{self.link_to}]"
    def parse(self, line):
        m = re.match(self.pat, line)
        self.link_from = m.group(1)
        self.link_to = m.group(2)
        return self

class ResponseCode(Data):
    pat = r"^responseCode: (.*)"
    code = 0
    def parse(self, line):
        m = re.match(self.pat, line)
        self.code = int(m.group(1))
        return self
    def __str__(self):
        return f"{self.__class__.__name__} Code[{self.code}]"

class AccountStatus(Line):
    pat = r"^accountStatus:\[(.*)\]"

class AccntNum(Line):
    pat = r"^accntNum:\[(.*)\]"

class CustomerType(Line):
    pat = r"^customerType:\[(.*)\]"

class AniType(Line):
    pat = r"^aniType:\[(.*)\]"

class AccntMatchSource(Line):
    pat = r"^accntMatchSource:\[(.*)\]"

class PortalName(Line):
    pat = r"^portalName :(.*)"

class Direction(Enum):
    Start = 0
    End = 1

class InvocationCounter(Data):
    pat = r"InvocationCounter.valueUnbound: call(start|end) was called \[0\]  times but it should have been called exactly once"
    direction = Direction.Start
    def parse(self, line):
        m = re.match(self.pat, line)
        if m.group(1) == "start":
            self.direction = Direction.Start
        else:
            self.direction = Direction.End
        return self
    def __str__(self):
        return f"{self.__class__.__name__} Direction[{self.direction}]"

class CallInfo(Data):
    dnis = ""
    ani = ""
    ucid = ""
    firstHistoryInfoUser = ""
    lastHistoryInfoUser = ""
    receivedUcid = ""
    receivedUui = ""
    pat = r"DNIS :(.*) ANI :(.*) UCID :(.*) FIRSTHISTORYINFOUSER :(.*) LASTHISTORYINFOUSER :(.*) RECEIVED_UCID :(.*) RECEIVED_UUI :(.*)" 
    def parse(self, line):
        m = re.match(self.pat, line)
        self.dnis = m.group(1)
        self.ani = m.group(2)
        self.ucid = m.group(3)
        self.firstHistoryInfoUser = m.group(4)
        self.lastHistoryInfoUser = m.group(5)
        self.receivedUcid = self.nullstr(m.group(6))
        self.receivedUui = self.nullstr(m.group(7))
        return self
    def __str__(self):
        return f"{self.__class__.__name__} dnis[{self.dnis}] ani[{self.ani}] ucid[{self.ucid}] firstHistoryInfoUser[{self.firstHistoryInfoUser}] lastHistoryInfoUser[{self.lastHistoryInfoUser}] receivedUcid[{self.receivedUcid}] receivedUuip[{self.receivedUui}]"

class InitClient(Line):
    pat = r"Initializing (.*) Restful [sS]ervice [cC]lient"

class ExecutingSQL(Line):
    pat = r"executing >(.*)<"

class WebServiceURL(Data):
    name = ""
    url = ""
    pat = r"([^\ ]*URL)[ ]*: (.*)"
    def parse(self,line):
        m = re.match(self.pat, line)
        if m:
            self.name = m.group(1)
            self.url = m.group(2)
        else:
            print(f"Parsing failed [{line}]")
        return self
    def __str__(self):
        return f"{self.__class__.__name__} name[{self.name}] url[{self.url}]"

class Priority(Enum):
    DEBUG = 0
    INFO = 1
    WARN = 2
    ERROR = 3

class Entry:
    date = 0
    time = 0
    msec = 0
    identity = ""
    version = ""
    priority = Priority.INFO
    funcName = ""
    data = Data("")

    pattern = r"([1-2][0-9][0-9][0-9])-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1])T([0-1][0-9]|2[0-3]):([0-5][0-9]):([0-5][0-9]),([0-9][0-9][0-9])\|(.*)\|(.*)-(DEBUG|INFO|WARN|ERROR)[ ]*\|(.*)\|(.*)"

    def __str__(self):
        return f"{__class__.__name__} Date[{self.date}] Time[{self.time}] Msec[{self.msec}] Identity[{self.identity}] Version[{self.version}] Priority[{self.priority}] FuncName[{self.funcName}] data[{self.data}]"
    def date_of_ints(self, year, month, day):
        self.date = (year * 10000) + (month * 100) + day
        return self.date
    def date_of_strs(self, year, month, day):
        return self.date_of_ints(int(year), int(month), int(day))
    def time_of_ints(self, hour, minute, second):
        self.time = (hour * 10000) + (minute * 100) + second
        return self.time
    def time_of_strs(self, hour, minute, second):
        return self.time_of_ints(int(hour), int(minute), int(second))
    def parse(self, line):
        if re.search(self.pattern, line):
            ms = re.match(self.pattern, line) 
            self.date_of_strs(ms.group(1), ms.group(2), ms.group(3))
            self.time_of_strs(ms.group(4), ms.group(5), ms.group(6))
            self.msec = int(ms.group(7))
            self.identity = ms.group(8)
            self.version = ms.group(9)
            self.priority = ms.group(10)
            self.funcName = ms.group(11)
            self.data = self.parseData(ms.group(12))
    def parseData(self, line):
        if re.search(Label.pat, line):
            return Label(line)
        elif re.search(Link.pat, line):
            return Link(line)
        elif re.search(ResponseCode.pat, line):
            return ResponseCode(line)
        elif re.search(AccountStatus.pat, line):
            return AccountStatus(line)
        elif re.search(AccntNum.pat, line):
            return AccntNum(line)
        elif re.search(CustomerType.pat, line):
            return CustomerType(line)
        elif re.search(AniType.pat, line):
            return AniType(line)
        elif re.search(AccntMatchSource.pat, line):
            return AccntMatchSource(line)
        elif re.search(PortalName.pat, line):
            return PortalName(line)
        elif re.search(InvocationCounter.pat, line):
            return InvocationCounter(line)
        elif re.search(CallInfo.pat, line):
            return CallInfo(line)
        elif re.search(InitClient.pat, line):
            return InitClient(line)
        elif re.search(ExecutingSQL.pat, line):
            return ExecutingSQL(line)
        elif re.search(WebServiceURL.pat, line):
            return WebServiceURL(line)
        else:
            return Data(line)

class Entries:
    entries = []
    def __init__(self):
        entries = []
    def load(self, fname):
        lineno = 0
        with open(fname, 'r') as fin:
            for line in fin:
                lineno = lineno + 1
                if (len(line) <= 16000):
                    entry = Entry()
                    entry.parse(line)
                    self.entries.append(entry)
                else:
                    print(f"[{lineno}]:  Line too long.")


entry = Entry()
entry.parse("2025-08-25T05:28:57,084|659E437BD782705B6FFEDA02E7FC6758|MOD25.09.0.004-DEBUG|reporting.CDRUtil|chaining from >welcid0810_CheckAccountMatchLogic_DS< to >welcid1005_CheckNumAccounts_DS<")
print(entry)
entry.parse("2025-08-25T06:12:03,779|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|client.AccountAndProfileSearchServiceClient|responseCode: 200")
print(entry)
entry.parse("2025-08-25T06:12:03,781|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|welcomeid.WelcomeIDAppUtil|accountStatus:[A]")
print(entry)
entry.parse("2025-08-25T06:12:03,781|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|welcomeid.WelcomeIDAppUtil|accntNum:[8313200011869345]")
print(entry)
entry.parse("2025-08-25T06:12:03,781|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|welcomeid.WelcomeIDAppUtil|customerType:[R]")
print(entry)
entry.parse("2025-08-25T06:12:03,781|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|welcomeid.WelcomeIDAppUtil|aniType:[null]")
print(entry)
entry.parse("2025-08-25T06:12:03,781|D48854B07B315F9C657793B05D17D540|MOD25.09.0.004-DEBUG|welcomeid.WelcomeIDAppUtil|accntMatchSource:[MASTERDB]")
print(entry)
entry.parse("2025-08-25T13:05:26,965||MOD25.09.0.004-WARN |calllog.InvocationCounter|InvocationCounter.valueUnbound: callstart was called [0]  times but it should have been called exactly once")
print(entry)
entry.parse("2025-08-25T13:05:26,966||MOD25.09.0.004-WARN |calllog.InvocationCounter|InvocationCounter.valueUnbound: callend was called [0]  times but it should have been called exactly once")
print(entry)
entry.parse("2025-08-25T05:24:35,138|6CF7AC02C98898345960E7A47D41C6E1|MOD25.09.0.004-DEBUG|decision.start0110_GetCallInfoFromWrapper_DS|DNIS :11071905113 ANI :9542376966 UCID :1299376E68AC2BC1 FIRSTHISTORYINFOUSER :8558955883 LASTHISTORYINFOUSER :8558955883 RECEIVED_UCID :null RECEIVED_UUI :null")
print(entry)
entry.parse("2025-08-25T06:02:37,551|4FE522579C14D098CDC7D6D6620B8CAF|MOD25.09.0.004-DEBUG|client.OutageServiceClientV2|Initializing Outage Services Restful service client")
print(entry)
entry.parse("2025-08-25T06:07:05,813|4FE522579C14D098CDC7D6D6620B8CAF|MOD25.09.0.004-DEBUG|dataaccess.DatabaseWrapper|executing >SELECT PORTAL_NAME, LASTHISTORYINFOUSER,WELCOME_PROMPT,INTERCEPT_PROMPT,TRANSFER_VDN,LANGUAGE,CUSTOMER_TYPE,CATEGORY_CODE,POC,LOB,TRANSFER_VDN_SECONDLANGUAGE,LANGUAGE_SWITCH,INTERCEPT_PROMPT_SECONDLANG,INTERCEPT_DTMF_ONLY,INTERCEPT_SKIP_ID,INTERCEPT_PORTAL_AFTER_NO FROM IVR_CALLFLOW.IVR_GENERIC_PORTAL_INFO WHERE PORTAL_NAME NOT LIKE 'bulk_portal%'<")
print(entry)
entry.parse("2025-08-25T11:58:22,023||MOD25.09.0.004-DEBUG|ivr.ConfigurationAccessor|FreeSpeechServiceWSDLURL: nss-vas.est.qa.desk.spctrm.netSecuritySuite/FreeSpeechServer.asmx?wsdl")
print(entry)

entries = Entries()
#entries.load("logs/ndf.log")
#nodata = { x for x in entries.entries if type(x.data) is Data }
#for entry in nodata:
#    print(entry)



