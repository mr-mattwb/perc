
alias olex='ocamllex lex.mll'
alias ocmi='occ -c tools.mli ser.mli env.mli log.mli percCfg.mli'
alias ocmo='occ -c tools.ml lex.ml ser.ml env.ml log.ml'
alias ocma='occ -a -o cfg.cma tools.ml lex.ml ser.ml env.ml log.ml'
alias pcmi='occ -c percCfg.mli'
alias perco='ocamlc -I +unix cfg.cma percCfg.ml perc.ml -o perc'
alias occlean='rm *.cm? lex.ml a.out perc'

export LOGMODNAME=perc
export LOGLEVEL=Debug
export LOGTARGET=STDERR
