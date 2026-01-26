%{
open DLog
%}
%token <Date.t> DATE
%token <Time.t> TIME
%token <int> MSEC
%token <string * string> IDEN
%token <DLog.priority * string> PRIO
%token <DLog.link> LINK
%token <string> LABEL
%token <DLog.dir> INVOCATION
%token <string> STATE 
%token <string> NEXTMODULE
%token <string> RETURNFUNCTION
%token <string> LASTVISITEDMODULE
%token <string> PORTALNAME
%token <string> RETURNVALUE
%token <string> NODEHOSTNAME
%token <string> LANGUAGE
%token <int list> CATCODESFORSALESTRANSFER
%token <DLog.call_info> CALLINFO
%token <DLog.find_customer_using_get> FINDCUSTOMERUSINGGET
%token <string> CREATEDTOKEN
%token <DLog.portalmap> PORTALMAP
%token <string * bool> VISITEDNODE
%token <DLog.catcode_table> CATCODETABLE
%token <string> SQLEXECUTING
%token <string * string> NULLITEMPATH
%token <string * string> EMPTYCALLFLOWMAP
%token <string> LOADLOCALCONFIG
%token <string> APPTAG
%token <string> IPADDRESS
%token <DLog.business_unit_table> BUSINESSUNITTABLE
%token <string> OTHER
%start header
%start data
%type <DLog.header> header
%type <DLog.data> data
%%

data:
    | LINK                                  { Link $1 }
    | LABEL                                 { Label $1 } 
    | INVOCATION                            { Invocation $1 }
    | STATE                                 { State $1 }
    | NEXTMODULE                            { NextModule $1 }
    | RETURNFUNCTION                        { ReturnFunction $1 }
    | LASTVISITEDMODULE                     { LastVisitedModule $1 }
    | PORTALNAME                            { PortalName $1 }
    | RETURNVALUE                           { ReturnValue $1 }
    | NODEHOSTNAME                          { NodeHostname $1 }
    | CALLINFO                              { CallInfo $1 }
    | LANGUAGE                              { Language $1 }
    | FINDCUSTOMERUSINGGET                  { FindCustomerUsingGET $1 }
    | CATCODESFORSALESTRANSFER              { CatCodesForSalesTransfer $1 }
    | CREATEDTOKEN                          { CreatedToken $1 }
    | PORTALMAP                             { PortalMap $1 }
    | VISITEDNODE                           { let (v, n) = $1 in VisitedNode(v, n) }
    | CATCODETABLE                          { CatCodeTable $1 }
    | SQLEXECUTING                          { SqlExecuting $1 }
    | NULLITEMPATH                          { let (v, p) = $1 in NullItemPath (v, p) }
    | EMPTYCALLFLOWMAP                      { let (v, p) = $1 in EmptyCallflowMap (v, p) }
    | LOADLOCALCONFIG                       { LoadLocalConfig $1 }
    | APPTAG                                { AppTag $1 }
    | IPADDRESS                             { IpAddress $1 }
    | BUSINESSUNITTABLE                     { BusinessUnitTable $1 }
    | OTHER                                 { Other $1 }
header:
    | DATE TIME MSEC IDEN PRIO { 
            let (iden, vers) = $4 in
            let (prio, func) = $5 in
            { 
                date = $1;
                time = $2;
                msec = $3;
                iden = iden;
                vers = vers;
                prio = prio;
                func = func
            }
        }

%%

