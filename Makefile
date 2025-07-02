
OLEX=ocamllex
UCC=ocamlc -I +unix
OCC=ocamlc -I +unix unix.cma 
OCO=ocamlopt -I +unix unix.cmxa
UCO=ocamlopt -I +unix
MODS=tools lex ser  env log
ML=tools.ml lex.ml ser.ml env.ml log.ml
PERCML=perc_5s_wav.ml percCfg.ml
PERCCMO=$(subst ml,cmo,$(PERCML))
PERCCMX=$(subst ml,cmx,$(PERCML))
CMO=$(subst ml,cmo,$(ML))
CMX=$(subst ml,cmx,$(ML))
MLI=tools.mli ser.mli env.mli log.mli
CFGCMA=cfg.cma

all:  cfg.cma cfg.cmxa perc cfg

depend: dep
dep:  *.ml *.mli
	ocamldep *.ml > .depend.ml
	ocamldep *.mli > .depend.mli

perc:  cfg.cma $(PERCCMO) perc.ml
	$(UCC) $+ -o $@

perc-x:  cfg.cmxa $(PERCCMX) perc.ml
	$(OCO) $+ -o $@

cfg:  cfg.cma cfg.ml
	$(UCC) $+ -o $@

cfg-x:  cfg.cmxa cfg.ml
	$(OCO) $+ -o $@

lex.ml:  lex.mll
	$(OLEX) lex.mll

lex.cmo:  lex.ml
	$(OCC) -c lex.ml

lex.cmx:  lex.ml
	$(OCO) -c lex.ml

%.cmo:  %.ml
	$(OCC) -c $<i
	$(OCC) -c $<
	$(OCO) -c $<

%.cmx:  %.ml
	$(OCC) -c $<i
	$(OCC) -c $<
	$(OCO) -c $<

cfg.cma:  $(CMO)
	$(OCC) -o $@ -a $(CMO)

cfg.cmxa:  $(CMX)
	$(UCO) -o $@ -a $(CMX)

#%.cmi:  %.mli
#	$(OCC) -c $<

clean:
	rm *.cm[ioax] *.cmxa *.o lex.ml
	rm .depend.ml
	rm .depend.mli


