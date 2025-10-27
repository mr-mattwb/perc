
export LOGMODNAME=perc
export LOGMODSUBNAME=main
export logging_level=Debug
export logging_targets=STDERR
export ALLOW_OVERRIDE=true

export PATH=$HOME/mine/perc:$PATH

#load "cfg.cma"
#load "perc5sWav.cmo"

echo initialized

alias ctop='ledit ./ccaml -I +unix -I +str -I +../pcre -I .'
