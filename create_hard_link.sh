#!/bin/bash

files=(
    ".zshrc"
    ".vimrc"
    ".emacs.d/init.el"
)

function main(){
    for file in ${files[@]}; do
        ln -f $file ~/$file
    done
}

main "$@"
