import datetime
import re
from datetime import datetime

class Data:
    def __init__(self, msg):
        self.parse(msg)

    def print_data(self):
        print(f"Empty")

class Line:
    item = ""

    def __init__(self, msg):
        self.parse(msg)

    def parse(self, msg):
        print(f"None")

    def parse_line(self, pat, msg):
        a = re.match(pat, msg)
        self.item = a.group(1)

    def print_data(self):
        print(f"{self.__class__.__name__} item[{self.item}]")

class Label(Line):
    pat = r"Label: (.*)"
    def parse(self, msg):
        self.parse_line(self.pat, msg)

class State(Line):
    pat = r"(.*): entering state"
    def parse(self, msg):
        self.parse_line(self.pat, msg)

class NextModule(Line):
    pat = r"nextModule: \[(.*)\]"
    def parse(self, msg):
        self.parse_line(self.pat, msg)

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
    def parse(self, msg):
        self.parse_line(self.pat, msg)

class PortalName(Line):
    pat = r"portalName: \[(.*)\]"
    def parse(self, msg):
        self.parse_line(self.pat, msg)

class ReturnNode(Line):
    pat = r"Return\[(.*)\]"
    def parse(self, msg):
        self.parse_line(self.pat, msg)

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
        self.identifiedFlag = bool(a.group(2))
        self.lastVisitedModule = a.group(3)
        self.appTag = a.group(4)
        self.intentIntercept = a.group(5)
        self.dialedPortalName = a.group(6)
    def print_data(self):
        print(f"{self.__class__.__name__} returnValue[{self.returnValue}] identifiedFlag[{self.identifiedFlag}] lastVisitedModule[{self.lastVisitedModule}] appTag[{self.appTag}] intentIntercept[{self.intentIntercept}] dialedPortalName[{self.dialedPortalName}]")

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

