" I'm not sure why, but I wasn't getting syntax highlighting on custom file
" types until I moved this line before everything else.
source ~/work-configs/vimrc

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
