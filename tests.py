import datetime
import re
import dLog as DLog

dlog = DLog.Entries("logs/ndf.log")
for e in [x for x in dlog.entries if not (type(x.data) == DLog.Data or type(x.data) == DLog.InvocationCounter)]:
    print(e)




