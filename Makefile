
OLEX=ocamllex
OPRS=ocamlyacc
UCC=ocamlc -I +unix -I +str
OCC=ocamlc -I +unix -I +str unix.cma str.cma 
OCO=ocamlopt -I +unix -I +str unix.cmxa str.cmxa
UCO=ocamlopt -I +unix -I +str
MODS=tools lex propBase iniLex propLex ser env log soxi
ML=tools.ml lex.ml propBase.ml iniParse.ml iniLex.ml propParse.ml propLex.ml ser.ml env.ml log.ml soxi.ml
PERCML=perc5sWav.ml percCfg.ml
PERCCMO=$(subst ml,cmo,$(PERCML))
PERCCMX=$(subst ml,cmx,$(PERCML))
CMO=$(subst ml,cmo,$(ML))
CMI=$(subst ml,cmi,$(ML))
CMX=$(subst ml,cmx,$(ML))
MLI=tools.mli ser.mli env.mli log.mli soxi.mli
CFGCMA=cfg.cma

all:  cfg.cma cfg.cmxa perc cfg perc-x cfg-x

perc:  cfg.cma $(PERCCMO) perc.ml
	$(UCC) $+ -o $@

perc-x:  cfg.cmxa $(PERCCMX) perc.ml
	$(OCO) $+ -o $@

cfg:  cfg.cma cfg.ml
	$(UCC) $+ -o $@

cfg-x:  cfg.cmxa cfg.ml
	$(OCO) $+ -o $@

redis.cma:  libcredis.a
	$(OCC) -custom -cclib -L. -ccopt -lcredis -ccopt -lhiredis -a -o $@

libcredis.a:  credis.o
	ar rcs $@ $+

credis.o:  credis.c redis.cmo redis.cmi
	$(OCC) -o $@ credis.c

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
	rm *.cm[ioax] *.cmxa *.o lex.ml iniLex.ml iniParse.ml iniParse.mli propLex.ml propParse.ml propParse.mli *.a perc perc-x cfg cfg-x
	rm .depend.ml
	rm .depend.mli

depend: dep
dep:  *.ml *.mli
	ocamldep *.ml > .depend.ml
	ocamldep *.mli > .depend.mli

# Dependencies
