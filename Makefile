
OLEX=ocamllex
OPRS=ocamlyacc
UCC=ocamlc -I +unix -I +str
OCC=ocamlc -I +unix -I +str unix.cma str.cma 
OCA=ocamlc -I +unix -I +str
OCO=ocamlopt -I +unix -I +str unix.cmxa str.cmxa
UCO=ocamlopt -I +unix -I +str
TOP=ocamlmktop -I +unix -I +str
MODS=tools lex propBase iniLex propLex ser fileOps envParam env log dayTime date dateTime soxi pLog
ML=tools.ml lex.ml propBase.ml iniParse.ml iniLex.ml propParse.ml propLex.ml ser.ml fileOps.ml envParam.ml env.ml log.ml dayTime.ml date.ml dateTime.ml soxi.ml pLog.ml
PERCML=perc5sWav.ml percCfg.ml
PERCCMO=$(subst ml,cmo,$(PERCML))
PERCCMX=$(subst ml,cmx,$(PERCML))
CMO=$(subst ml,cmo,$(ML))
CMI=$(subst ml,cmi,$(ML))
CMX=$(subst ml,cmx,$(ML))
MLI=tools.mli ser.mli fileOps.mli envParam.mli env.mli log.mli dayTime.mli date.mli dateTime.mli soxi.mli pLog.mli
CFGCMA=cfg.cma

all:  cfg.cma cfg.cmxa perc cfg perc-x cfg-x syslog.cma mlog 

install:
	

perc:  cfg.cma $(PERCCMO) perc.ml
	$(UCC) $+ -o $@

perc-x:  cfg.cmxa $(PERCCMX) perc.ml
	$(OCO) $+ -o $@

cfg:  cfg.cma cfg.ml
	$(UCC) $+ -o $@

cfg-x:  cfg.cmxa cfg.ml
	$(OCO) $+ -o $@

mlog:  cfg.cma syslog.cma mlog.ml
	$(UCC) $+ -o $@

logp:  cfg.cma logp.ml
	$(UCC) $+ -o $@

craml:  syslog.cma 
	$(TOP) -custom -o $@ unix.cma str.cma $+

syslog.cma:  libcsyslog.a syslog.cmo syslog.cmi
	$(OCA) -custom -ccopt -L. -cclib -lcsyslog -a -o $@ syslog.cmo

libcsyslog.a:  csyslog.o 
	ar rcs $@ $+

csyslog.o:  csyslog.c syslog.cmo syslog.cmi
	$(UCC) -o $@ csyslog.c

lex.ml:  lex.mll
	$(OLEX) lex.mll

lex.cmo:  lex.ml
	$(OCC) -c lex.ml

lex.cmx:  lex.ml
	$(OCO) -c lex.ml

iniLex.ml:  iniParse.ml iniParse.mli  iniLex.mll
	$(OLEX) iniLex.mll

iniLex.cmo:  iniParse.cmo iniLex.ml
	$(OCC) -c iniLex.ml

iniLex.cmx:  iniParse.cmx iniLex.ml
	$(OCO) -c iniLex.ml

iniParse.ml:  iniParse.mly
	$(OPRS)  $<

propLex.ml:  propParse.ml propParse.mli  propLex.mll
	$(OLEX) propLex.mll

propLex.cmo:  propParse.cmo propLex.ml
	$(OCC) -c propLex.ml

propLex.cmx:  propParse.cmx propLex.ml
	$(OCO) -c propLex.ml

propParse.ml:  propParse.mly
	$(OPRS)  $<

%.cmi:  %.mli
	$(OCC) -c $<

%.cmo:  %.ml
	$(OCC) -c $<i
	$(OCC) -c $<
	$(OCO) -c $<

%.cmx:  %.ml
	$(OCC) -c $<i
	$(OCC) -c $<
	$(OCO) -c $<

cfg.cma:  $(CMO) $(CMI)
	$(OCC) -o $@ -a $(CMO)

cfg.cmxa:  $(CMX)
	$(UCO) -o $@ -a $(CMX)

#%.cmi:  %.mli
#	$(OCC) -c $<

clean:
	rm -f *.cm[ioax] *.cmxa *.o lex.ml iniLex.ml iniParse.ml iniParse.mli propLex.ml propParse.ml propParse.mli *.a perc perc-x cfg cfg-x
	rm -f csyslog.o libcsyslog.a syslog.cm? mlog

depend: dep
dep:  *.ml *.mli
	ocamldep *.ml > .depend.ml
	ocamldep *.mli > .depend.mli

include .depend.ml
include .depend.mli

# Dependencies
