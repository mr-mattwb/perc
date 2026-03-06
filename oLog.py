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

class Line(Data):
    pat = r"(.*)"
    def __str__(self):
        return f"{self.__class__.__name__} [{self.data}]"
    def parse(self, line):
        m = re.match(self.pat, line)
        self.data = m.group(1)
        return self

class Label(Line):
    pat = r"^Label: (.*)"

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

entries = Entries()
entries.load("logs/ndf.log")
labels = { x for x in entries.entries if not type(x.data) is Data  }
for entry in labels:
    print(entry)




