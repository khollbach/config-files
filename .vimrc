" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Init Pathogen (if it exists).
if !empty(glob('~/.vim/autoload/pathogen.vim'))
    execute pathogen#infect()
endif

" Set leader to space.
let mapleader = "\<Space>"
noremap <Space> <Nop>

" Syntax highlighting.
syntax on

" Load plugin and indent files for known filetypes.
filetype plugin indent on

" -----------------------------------------------------------------------------
" Plugin Settings
" -----------------------------------------------------------------------------

" Dark background color.
set background=dark

" Solarized color scheme
if !empty(glob('~/.vim/bundle/vim-colors-solarized'))
    colorscheme solarized
endif

" Deoplete (autocompletion)
if has('nvim') && !empty(glob('~/.vim/bundle/deoplete.nvim'))
    let g:deoplete#enable_at_startup = 1

    " Options.
    call deoplete#custom#option({
    \ 'ignore_case': v:true,
    \ 'on_insert_enter': v:false,
    \ })

    " Initially disable auto_complete.
    call deoplete#custom#option('auto_complete', v:false)
    let s:my_deoplete_enabled = 0

    " Toggle auto_complete: <F9>
    noremap <expr> <F9> <sid>toggle_deoplete()
    function! s:toggle_deoplete() abort
        if s:my_deoplete_enabled
            " Disable
            echo 'nodeoplete'
            call deoplete#custom#option('auto_complete', v:false)
            let s:my_deoplete_enabled = 0
        else
            " Enable
            echo '  deoplete'
            call deoplete#custom#option('auto_complete', v:true)
            let s:my_deoplete_enabled = 1
        endif
        return ""
    endfunction

    " Fix the way enter interacts with deoplete.
    " If the `pop-up-menu' is visible (ie, if autocomplete suggestions are
    " showing), then close the menu (C-y is `yes/confirm') and then insert a
    " newline.
    inoremap <expr> <CR> pumvisible() ? "\<C-y>\<CR>" : "\<CR>"

    " Use tab/shift-tab for completion (if the PUM is active or if there's
    " something other than whitespace behind the cursor).
    inoremap <expr> <Tab> pumvisible() \|\| !<sid>check_space_behind() ?
        \ "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() \|\| !<sid>check_space_behind() ?
        \ "\<C-p>" : "\<S-Tab>"
    function! s:check_space_behind() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1] =~ '\s'
    endfunction

    " Disable preview window. Deoplete would sometime uses this to show
    " documentation of, e.g., python functions.
    set completeopt-=preview
endif

if !empty(glob('~/.vim/bundle/vim-sneak'))
    " Accept `s` and `S` in operator-pending mode.
    " Note that this conflicts with surround.vim's default binds,
    " which I've changed to z/Z.
    omap s <Plug>Sneak_s
    omap S <Plug>Sneak_S
    xmap S <Plug>Sneak_S

    " Disable highlighting of matches.
    autocmd User SneakLeave highlight clear Sneak

    " todo: disable jumping across lines
endif

" Additional RSI binds.
if !empty(glob('~/.vim/bundle/vim-rsi'))
    " The following two binds have corner cases near the end of lines.
    inoremap <expr> <C-u> <sid>beyond_eol() ? "<C-o>d0<C-o>x" : "<C-o>d0"
    if has('nvim')
        " Nvim has a different cursor position for C-o at the end of a line
        " compared to Vim. I consider this a bug.
        inoremap <expr> <C-k> <sid>beyond_eol() ? "" : "<C-o>d$"
    else
        inoremap <expr> <C-k> <sid>at_eol() ? "" : "<C-o>d$"
    endif

    function! s:at_eol() abort
        return col('.') ==# len(getline('.'))
    endfunction
    function! s:beyond_eol() abort
        return col('.') > len(getline('.'))
    endfunction


    " These shadow Vim's completion binds, but I use Tab/S-Tab for that
    " anyways.
    " If the PUM is active, close it first.
    inoremap <expr> <C-n> pumvisible() ? "\<C-y>\<Down>" : "\<Down>"
    inoremap <expr> <C-p> pumvisible() ? "\<C-y>\<Up>" : "\<Up>"
endif

" zurround.vim
if !empty(glob('~/.vim/bundle/vim-surround'))
    let g:surround_no_mappings = 1

    " Use z/Z instead of s/S, since I use s for sneak.vim motions.
    nmap dz  <Plug>Dsurround
    nmap cz  <Plug>Csurround
    nmap cZ  <Plug>CSurround
    nmap yz  <Plug>Ysurround
    nmap yZ  <Plug>YSurround
    nmap yzz <Plug>Yssurround
    nmap yZz <Plug>YSsurround
    nmap yZZ <Plug>YSsurround
    xmap z   <Plug>VSurround
    xmap gz  <Plug>VgSurround
endif

" Fugitive binds.
if !empty(glob('~/.vim/bundle/vim-fugitive'))
    " Toggle NERDTree.
    noremap <Leader>i :NERDTreeToggle<CR>
    noremap <expr> <Leader>gg ":Git! "
    noremap <Leader>gs :Gstatus<CR>
    noremap <Leader>gc :Gcommit<CR>
    noremap <Leader>gd :Gdiff<CR>
    noremap <Leader>gb :Gblame<CR>
endif

" incsearch.vim
if !empty(glob('~/.vim/bundle/incsearch.vim'))
    " Case insensitive by default.
    map / <Plug>(incsearch-forward)\c
    map ? <Plug>(incsearch-backward)\c
    map g/ <Plug>(incsearch-stay)\c

    " Case-sensitive search
    map <Leader>/ <Plug>(incsearch-forward)
    map <Leader>? <Plug>(incsearch-backward)
    map <Leader>g/ <Plug>(incsearch-stay)
endif

" ack.vim
if !empty(glob('~/.vim/bundle/ack.vim'))
    " ripgreg
    let g:ackprg = "rg --vimgrep"
    noremap <expr> <Leader>a ":Ack "
endif

" ctrlp
if !empty(glob('~/.vim/bundle/ctrlp.vim'))
    " Open ctrlp.
    let g:ctrlp_map = "<Leader>o"

    " Use mixed mode: search for files, buffers, and 'most-recently-used' files.
    let g:ctrlp_cmd = "CtrlPMixed"

    " Don't include MRU files in the search results at all.
    let g:ctrlp_mruf_max = 0

    " Don't try to guess a good choice of working directory.
    let g:ctrlp_working_path_mode = 0

    " Show me hidden files. This doesn't work well when opening ctrlp in $HOME,
    " it takes forever to index.
    " See https://github.com/kien/ctrlp.vim/issues/279
    let g:ctrlp_show_hidden = 1
endif

" NERD Commenter.
if !empty(glob('~/.vim/bundle/nerdcommenter'))
    " Don't give me the default mappings.
    let g:NERDCreateDefaultMappings = 0

    " No spaces after the comment character.
    let g:NERDSpaceDelims = 0

    " Comment.
    noremap <Leader>c :call NERDComment(0, "comment")<CR>

    " Uncomment.
    noremap <Leader>u :call NERDComment(0, "uncomment")<CR>
endif

" NERDTree
if !empty(glob('~/.vim/bundle/nerdtree'))
    " Toggle NERDTree.
    noremap <Leader>i :NERDTreeToggle<CR>

    " Go to current file in NERDTree.
    noremap <Leader>I :NERDTreeFind<CR>

    " Hide NERDTree after opening a file.
    let NERDTreeQuitOnOpen = 1

    " The default help bind '?' conflicts with vim's search-backwards.
    let NERDTreeMapHelp = '<F1>'
endif

" undotree
if !empty(glob('~/.vim/bundle/undotree'))
    " Toggle undotree
    noremap <Leader>U :UndotreeToggle<CR>
endif

" -----------------------------------------------------------------------------
" Settings
" -----------------------------------------------------------------------------

" This wasn't the default on CDF for some reason. Caused issues with NERDTree.
set encoding=utf-8

" Make backspace behave as expected in insert mode.
set backspace=indent,eol,start

" Allow hiding of buffers with unwritten changes.
set hidden

" Write swap files to a particular directory, if it exists.
set directory=~/.vim/swap//,.

" Persistent undo. Only happens if the specified directory exists.
set undofile
set undodir=~/.vim/undo//

" Don't flash the screen (or beep) in Windows
set t_vb=



" No startup message
set shortmess+=I

" Line numbers.
set nonumber

" Don't wrap long lines by default.
set nowrap

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Vertical lines after 80, 100, and 120 chars.
set colorcolumn=81,101,121

" Maximum line length for various formatting-related things.
set textwidth=79

" Don't add an extra space after a '.' character when joining lines.
" Also applies to '?' and '!' characters.
set nojoinspaces

" Text-formatting options. Some done on load to override plugin-file settings.
set formatoptions-=tc
autocmd BufNewFile,BufRead * set formatoptions+=rqj formatoptions-=o



" Start scrolling as soon as the cursor gets close to the edge of the screen.
set scrolloff=5

" Don't automatically perform ^ after each gg, H, M, L, etc.
set nostartofline

" Go to the first match as you are typing your search.
set incsearch

" Don't highlight search matches by default.
set nohlsearch

" Show commandline completion matches above the commandline on <Tab> keypress.
set wildmenu



" Don't show status line (filename, etc) when there's only one window.
set laststatus=1

" Show certain info at the bottom-right of the screen.
" Intead of the default information (cursor's current line/column numbers),
" show the filename, and whether the current buffer has been modified.
set ruler
set rulerformat=%39(%=%{My_bufname()}%4(%m%)%)
" We use this function instead of the %t rulerformat/statusline builtin,
" since that one shows "[No Name]" when there's no buffer name.
" Also, this allows us to truncate from the right instead of from the left.
function! My_bufname() abort
    let filename = expand('%:t')
    if strlen(filename) > 35
        let filename = filename[0:33] . ">"
    endif
    return filename
endfunction

" Don't give visual feedback for normal mode commands requiring multiple
" keypresses.
set noshowcmd

" Don't show an indicator in the echo area when you're in insert mode.
" (The cursor shape already indicates that in Neovim.)
set noshowmode

" Don't show a how-to-quit message when you press <C-c> in normal mode.
nnoremap <C-c> <Nop>



" Enable autoindent.
set autoindent

" Use 4 spaces for indent commands.
set shiftwidth=4

" Expand tab into spaces when pressed in insert mode.
set expandtab

" Allow backspacing through spaces as if they were tabs.
set softtabstop=4

" Hard tab width = 8 spaces. (Linux kernel style... completely insane.)
set tabstop=8

" Show hard tabs and trailing spaces.
" Also show indicators for text that extends past the edge of the screen.
set list
set listchars=tab:»\ ,extends:▶,precedes:◀,trail:·

" Don't show trailing spaces when typing.
autocmd InsertEnter * set listchars-=trail:·
autocmd InsertLeave * set listchars+=trail:·



" Two spaces indent by default for html files.
autocmd BufNewFile,BufRead *.html
    \ setlocal shiftwidth=2 |
    \ setlocal softtabstop=2

" Treat more things as Makefile.
autocmd BufNewFile,BufRead make*.inc
    \ setlocal syntax=make

" Treat TypeScript as JavaScript.
autocmd BufNewFile,BufRead *.ts
    \ setlocal syntax=javascript

" Treat .sc as Scala.
autocmd BufNewFile,BufRead *.sc
    \ setlocal syntax=scala

" Jump to the last position when reopening a file.
" Don't do this for git commit messages though.
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \ exe "normal! g`\"" |
    \ endif
autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"

" Highlight lowercase "todo" in comments as well as "TODO".
" https://stackoverflow.com/a/30552423
augroup lowercase_todo
    autocmd!
    autocmd Syntax * syn keyword LowercaseTodo contained todo
        \ containedin=.*Comment,vimCommentTitle,cCommentL
augroup END
hi def link LowercaseTodo Todo



" Terminal settings
if has('nvim') || has('terminal')
    " Make escape key behave as expected.
    tnoremap <Esc> <C-\><C-n>

    " jk = Esc
    tnoremap jk <C-\><C-n>
    tnoremap jK <C-\><C-n>
    tnoremap Jk <C-\><C-n>
    tnoremap JK <C-\><C-n>
endif

" Neovim terminal settings
if has('nvim')
    " Line numbers off by default.
    autocmd TermOpen * setlocal nonumber

    " Start terminal in insert mode (called "Terminal-mode" in the docs).
    autocmd TermOpen * startinsert
endif

" -----------------------------------------------------------------------------
" Mappings
" -----------------------------------------------------------------------------

" jk = Exit insert mode or command-line mode.
inoremap jk <Esc>
inoremap jK <Esc>
inoremap Jk <Esc>
inoremap JK <Esc>
cnoremap jk <C-c>
cnoremap jK <C-c>
cnoremap Jk <C-c>
cnoremap JK <C-c>

" <CR> = gg
noremap <CR> gg

" Scroll 5x faster.
noremap <C-y> 5<C-y>
noremap <C-e> 5<C-e>

" Scroll one line at a time.
if has('nvim')
    noremap <M-C-y> <C-y>
    noremap <M-C-e> <C-e>
else
    noremap <Esc><C-y> <C-y>
    noremap <Esc><C-e> <C-e>
endif

" <Home> = ^
" (By default it behaves like the 0 key instead.)
noremap <Home> ^

" Select recently pasted text.
" (Built-in gv selects recently selected text.)
nnoremap gp `[v`]



" Toggle line wrapping.
noremap <F4> :set wrap! wrap?<CR>

" Toggle line numbers.
noremap <F5> :set number! number?<CR>

" Toggle colorcolumn
noremap <expr> <F6> <sid>toggle_colorcolumn()
function! s:toggle_colorcolumn() abort
    if &colorcolumn !=# ""
        let s:colorcolumn_previous_value = &colorcolumn
        set colorcolumn=
        set colorcolumn?
    else
        let &colorcolumn=s:colorcolumn_previous_value
        set colorcolumn?
    endif

    " Need to redraw here to get the effect right away.
    redraw

    return ""
endfunction

" Toggle autoindent/mappings/etc, for pasting text.
noremap <F7> :set paste! paste?<CR>
set pastetoggle=<F7>



" <M-q> = Quit Vim.
" <M-w> = Save.
" <M-p> = Execute last command.
if has('nvim')
    noremap <M-q> :qa<CR>
    inoremap <M-q> <C-o>:qa<CR>
    noremap <M-w> :w<CR>
    inoremap <M-w> <C-o>:w<CR>
    noremap <M-p> :<Up><CR>
else
    noremap <Esc>q :qa<CR>
    inoremap <Esc>q <C-o>:qa<CR>
    noremap <Esc>w :w<CR>
    inoremap <Esc>w <C-o>:w<CR>
    noremap <Esc>p :<Up><CR>
endif



" Navigate windows: C-hjkl
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Resize current window: C-M-hjkl
if has('nvim')
    noremap <M-C-h> 5<C-w><
    noremap <M-C-j> 5<C-w>+
    noremap <M-C-k> 5<C-w>-
    noremap <M-C-l> 5<C-w>>
else
    noremap <Esc><C-h> 5<C-w><
    noremap <Esc><C-j> 5<C-w>+
    noremap <Esc><C-k> 5<C-w>-
    noremap <Esc><C-l> 5<C-w>>
endif



" Copy/cut/paste to/from system clipboard.
noremap <Leader>y "+y
noremap <Leader>Y "+Y
noremap <Leader>d "+d
noremap <Leader>D "+D
noremap <Leader>p "+p
noremap <Leader>P "+P

" Paste from the 0-register (ie yanked text only).
noremap <Leader>h "0p
noremap <Leader>H "0P



" Quit current window.
noremap <Leader>q :q<CR>

" Strip trailing whitespace
noremap <Leader>w :%s/\s\+$//<CR>

" Update configs
noremap <Leader>e :!source ~/config-files/update_configs<CR>

" Reload .vimrc
noremap <Leader>r :source $MYVIMRC<CR>



" List buffers.
noremap <Leader><Tab> :ls<CR>

" Next/previous buffer.
noremap <Leader>; :bn<CR>
noremap <Leader>, :bp<CR>

" Toggle search highlight.
noremap <Leader>' :set hlsearch! hlsearch?<CR>

" Search and replace.
nnoremap <Leader>s :%s///gc<left><left><left><left>
vnoremap <Leader>s :s///gc<left><left><left><left>

" Count occurences (ignoring case by default)
nnoremap <Leader>n :%s/\c//gn<left><left><left><left>
vnoremap <Leader>n :s/\c//gn<left><left><left><left>

" Change file permissions to be executable or not.
noremap <Leader>x :!chmod +x %<CR>
noremap <Leader>X :!chmod -x %<CR>

" -----------------------------------------------------------------------------
" Misc
" -----------------------------------------------------------------------------

" Work-related settings / defs / etc
if filereadable(glob("~/notes/config/nvimrc-snippet"))
    source ~/notes/config/nvimrc-snippet
endif
