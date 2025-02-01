" I'm not sure why, but I wasn't getting syntax highlighting on custom file
" types until I moved this before everything else.
if !empty(glob('~/.work-configs/vimrc'))
    source ~/.work-configs/vimrc
endif

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
