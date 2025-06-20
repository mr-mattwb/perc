#!/bin/bash

export CMD=/usr/bin/sox
export PLAY=/usr/bin/play
export PERC5S=perc-5s.wav
export OUTPFX=out
export SECONDS=5
export ITERATOR=5

runperc () {
    local verbose=0
    local help=0
    local cmd=""
    local play=""
    local sndf=""
    local outf=""
    local secs=5
    set -- $(getopt -o vhc:s:S:p: --longoptions verbose,help,cmd:,snd:,secs:,play: -- $@);
    while [ "$1" != "--" ]; do
        case "$1" in
            -v|--verbose)
                verbose=1;
                shift;;
            -h|--help)
                help=1;
                shift;;
            -c|--cmd)
                shift;
                cmd=$(echo $1 | tr -d "'");
                shift;;
            -s|--snd)
                shift;
                sndf=$(echo $1 | tr -d "'");
                shift;;
            -S|--secs)
                shift;
                secs=$(echo $1 | tr -d "'");
                shift;;
            -p|--play)
                shift;
                play=$(echo $! | tr -d "'");
                shift;;
            *)
                echo "${FUNCNAME[0]}:   Invalid argument [$1]";
                help=1;
                shift;;
        esac
    done;
    shift # --
    if [ $help -gt 0 ]; then
        echo "${FUNCNAME[0]} [-v|--verbose] [-h|--help] [-c|--cmd <command>] [-s|--snd <sound-file>] [-S|--secs <seconds>] [-p|--play <play-command>] [<output-file>]"
        return 1
    fi
    [ $verbose -gt 0 ] && echo "cmd[$cmd] snd[$sndf] secs[$secs] play[$play]"
    [ -z "$cmd" ] && cmd=$CMD
    [ -z "$sndf" ] && sndf=$PERC5S
    [ -z "$secs" ] && secs=$SECONDS
    [ -z "$play" ] && play=$PLAY
    iter=$(( $secs / $ITERATOR ))
    infiles=""
    for i in $(seq $iter); do
        infiles=$(echo "$infiles $sndf")
    done;
    if [ $# -gt 0 ]; then       # output file
        exe=$(echo "$cmd $infiles $1")
        shift;
    else
        exe=$(echo "$play $infiles")
    fi
    [ $verbose -gt 0 ] && echo "cmd[$cmd] snd[$sndf] secs[$secs] play[$play] iter[$iter] infiles[$infiles] exe[$exe]"
    eval $exe
    return 0
}


runperc $@

