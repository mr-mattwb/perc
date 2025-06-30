
OLEX=ocamllex
UCC=ocamlc -I +unix
OCC=ocamlc -I +unix unix.cma 
MODS=tools lex ser  env log
ML=tools.ml lex.ml ser.ml env.ml log.ml
PERCML=perc_5s_wav.ml percCfg.ml
PERCCMO=$(subst ml,cmo,$(PERCML))
CMO=$(subst ml,cmo,$(ML))
MLI=tools.mli ser.mli env.mli log.mli
CFGCMA=cfg.cma

all:  cfg.cma perc cfg

depend: dep
dep:  *.ml *.mli
	ocamldep *.ml > .depend.ml
	ocamldep *.mli > .depend.mli

perc:  cfg.cma $(PERCCMO) perc.ml
	$(UCC) $+ -o $@

cfg:  cfg.cma cfg.ml
	$(UCC) $+ -o $@

%.cmo:  %.ml
	$(UCC) -c $<i
	$(UCC) -c $<

lex.ml:  lex.mll
	$(OLEX) lex.mll

lex.cmo:  lex.ml
	$(OCC) -c lex.ml

env.cmo:  lex.cmo 
ser.cmo:  lex.cmo

%.cmo:  %.ml
	$(OCC) -c $<i
	$(OCC) -c $<

cfg.cma:  $(CMO)
	$(OCC) -o $@ -a $(CMO)

#%.cmi:  %.mli
#	$(OCC) -c $<

clean:
	rm *.cm[ioa]
	rm lex.ml
	rm .depend.ml
	rm .depend.mli


