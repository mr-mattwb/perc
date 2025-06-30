
OLEX=ocamllex
UCC=ocamlc -I +unix
OCC=ocamlc -I +unix unix.cma 
MODS=tools lex ser  env log
ML=tools.ml lex.ml ser.ml env.ml log.ml
CMO=$(subst ml,cmo,$(ML))
MLI=tools.mli ser.mli env.mli log.mli
CFGCMA=cfg.cma

all:  cfg.cma perc cfg

perc:  cfg.cma percCfg.cmo perc.ml
	$(UCC) $+ -o $@

cfg:  cfg.cma cfg.ml
	$(UCC) $+ -o $@


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

%.cmi:  %.mli
	$(OCC) -c $<

clean:
	rm *.cm[ioa]
	rm lex.ml

