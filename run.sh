#!/usr/local/bin/bash

#================================================================================ 
# Last Upate: Fri Oct  7 12:39:43 PDT 2016 
# Tue Nov 15 00:08:03 PST 2016  - add rsync, home dotfile to GoogleDrive/homedotfile
# 
# Script to manage all the small tasks such as editing and backup
#================================================================================ 
# all the colors in color.sh file
# [gf] open it 
# how to use in /Users/cat/myfile/script/jav.sh 
# e.g. printf "${FG_BR_RED}Hello World${RESET_ALL}\n"
# -------------------------------------------------------------------------------- 
# Tue May  7 16:46:25 2019 
# Add full path to ghc, Emacs can't find ghc from M-:
#================================================================================ 
# Sun Jul 28 18:21:09 2019 
# update to more generic file path
#================================================================================ 

# $(basename file.cpp) => file
#if [ "$#" -eq 0 ]; then
#else
#fi

#if [ "$?" -eq 0 ]; then
#else
#fi

#for var in $(ls) 
#do
#    $echo $var
#done 

function help(){
    printc 196 "help message"
}

source $HOME/myfile/bitbucket/script/AronLib.sh  
getpwd

MySymbin="$HOME/myfile/symbin"
MyBin="$HOME/myfile/mybin"
HaskellLib="$HOME/myfile/bitbucket/haskelllib"
hweb="$HOME/myfile/bitbucket/haskell_webapp"

hcmd="/usr/local/bin/ghc -i$HOME/myfile/bitbucket/haskelllib $1 -o "$(basename $1)
ghcProfile="/usr/local/bin/ghc -i$HOME/myfile/bitbucket/haskelllib -prof -fprof-auto -rtsopts $1" 

# stack build
# http://docs.haskellstack.org/en/stable/GUIDE/#flags-and-ghc-options 
# stack build --ghc-options=-O2 haskellwebapp2 
# stack build GeneXfidoHtml 
# stack exec GeneXfidoHtml 

exeName="GeneXfidoHtml"

if [[ "$#" -eq 1 ]]; then
    
    # KEY: build only
    if [[ "$1" == 'c' ]]; then 
	    stack build "$exeName" 
        printcText 'Build only'
    elif [[ "$1" == 'local' ]]; then
        # stack build GeneXfidoHtml && sudo stack --allow-different-user exec GeneXfidoHtml -- l
        stack build "$exeName" && stack exec "$exeName" -- l 
        echo "Gene Html for localhost"
    elif [[ "$1" == 'remote' ]]; then
        stack build "$exeName" && sudo stack --allow-different-user exec "$exeName" -- remote 
        echo "Gene Html for xfido.com"
    else 
        printError "Invalid argument option [$1]"
    fi
else
    # Wed 12 May 20:12:21 2021 
    # permission issue, stack flag => --allow-different-user
    repeat 40 '-' 
    echo "run.sh c     => build"
    echo "run.sh local => Generate Html for local host"
    echo "run.sh remote => Generate Html for remote host xfido.com"
    repeat 40 '-' 
    # stack build GeneXfidoHtml && stack exec GeneXfidoHtml 
fi
