import datetime
import re
from enum import Enum

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
    def __str__(self):
        return f"{__class__.__name__} Date[{self.date}] Time[{self.time}] Msec[{self.msec}] Identity[{self.identity}] Version[{self.version}] Priority[{self.priority}] FuncName[{self.funcName}]"
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


entry = Entry()
entry.date_of_strs("2025","12","11")
entry.time_of_strs("23","22","51"r
print(f"[{entry}]")


