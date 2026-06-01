
export LOGMODNAME=perc
export LOGMODSUBNAME=main
export logging_level=Debug
export logging_targets=STDERR
export ALLOW_OVERRIDE=true

export PATH=$HOME/mine/perc:$PATH
export PATH=$HOME/mine/perc/_build/default:$PATH

#load "cfg.cma"
#load "perc5sWav.cmo"

echo initialized

alias ctop='ledit ./ccaml -I +unix -I +str -I +../pcre -I +../sqlite3 -I .'

ocamlincdirs='-I +unix -I +str -I +../dbm -I +../pcre -I +../re -I +../sqlite3 -I _build/default -I _build/default/.cfg.objs/byte str.cma'
