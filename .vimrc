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
if !empty(glob('~/.vim/bundle/deoplete.nvim'))
    " Automatic suggestions
    let g:deoplete#enable_at_startup = 1

    " Deoplete options.
    call deoplete#custom#option({
    \ 'ignore_case': v:true,
    \ 'on_insert_enter': v:false,
    \ })

    " Initially disable auto_complete.
    " Toggle auto_complete: <F6>
    call deoplete#custom#option('auto_complete', v:false)
    let s:my_deoplete_enabled = 0
    function! s:toggle_deoplete() abort
        if s:my_deoplete_enabled
            call deoplete#custom#option('auto_complete', v:false)
            let s:my_deoplete_enabled = 0
        else
            call deoplete#custom#option('auto_complete', v:true)
            let s:my_deoplete_enabled = 1
        endif
        return ""
    endfunction
    noremap <expr> <F6> <sid>toggle_deoplete()

    " Fix the way enter interacts with deoplete.
    " If the `pop-up-menu' is visible (ie, if autocomplete suggestions are
    " showing), then close the menu (C-y is `yes/confirm') and then insert a
    " newline.
    inoremap <expr> <CR> pumvisible() ? "\<C-y>\<CR>" : "\<CR>"

    " Use tab/shift-tab for completion (if the PUM is active or if there's
    " something other than whitespace behind the cursor).
    function! s:check_space_behind() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1] =~ '\s'
    endfunction
    inoremap <expr> <Tab> pumvisible() \|\| !<sid>check_space_behind() ?
        \ "\<C-n>" : "\<Tab>"
    inoremap <expr> <S-Tab> pumvisible() \|\| !<sid>check_space_behind() ?
        \ "\<C-p>" : "\<S-Tab>"

    " Disable preview window. Deoplete would sometime uses this to show
    " documentation of, e.g., python functions.
    set completeopt-=preview
endif

" Additional RSI binds.
if !empty(glob('~/.vim/bundle/vim-rsi'))
    " The following two binds have corner cases near the end of lines,
    " so we use functions to check for edge cases.
    function! s:at_eol() abort
        return col('.') ==# len(getline('.'))
    endfunction
    function! s:beyond_eol() abort
        return col('.') > len(getline('.'))
    endfunction

    " C-u
    inoremap <expr> <C-u> <sid>beyond_eol() ? "<C-o>d0<C-o>x" : "<C-o>d0"

    " C-k
    if has('nvim')
        " Nvim has a different cursor position for C-o at the end of a line
        " compared to Vim. I consider this a bug.
        inoremap <expr> <C-k> <sid>beyond_eol() ? "" : "<C-o>d$"
    else
        inoremap <expr> <C-k> <sid>at_eol() ? "" : "<C-o>d$"
    endif

    " These shadow Vim's completion binds, but I use Tab/S-Tab for that
    " anyways.
    " If the PUM is active, close it first.
    inoremap <expr> <C-n> pumvisible() ? "\<C-y>\<Down>" : "\<Down>"
    inoremap <expr> <C-p> pumvisible() ? "\<C-y>\<Up>" : "\<Up>"
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

" ack.vim
if !empty(glob('~/.vim/bundle/ack.vim'))
    " ripgreg
    let g:ackprg = "rg --vimgrep"
    noremap <expr> <Leader>a ":Ack "
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

" EasyMotion
if !empty(glob('~/.vim/bundle/vim-easymotion'))
    " Better long line-wise motions, without having to count manually or use
    " relative line numbers.
    map <Leader>j <Plug>(easymotion-j)
    map <Leader>k <Plug>(easymotion-k)
endif

" incsearch.vim
if !empty(glob('~/.vim/bundle/nerdtree'))
    " Case insensitive by default.
    map / <Plug>(incsearch-forward)\c
    map ? <Plug>(incsearch-backward)\c
    map g/ <Plug>(incsearch-stay)\c

    " Case-sensitive search
    map <Leader>/ <Plug>(incsearch-forward)
    map <Leader>? <Plug>(incsearch-backward)
    map <Leader>g/ <Plug>(incsearch-stay)
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



" No startup message
set shortmess+=I

" Line numbers.
set number

" Don't wrap long lines by default.
set nowrap

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Vertical lines at 80, 100, and 120 chars.
set colorcolumn=80,100,120

" Maximum line length for various formatting-related things.
set textwidth=79

" Don't add an extra space after a '.' character when joining lines.
" Also applies to '?' and '!' characters.
set nojoinspaces

" Text-formatting options. Done on load to override plugin-file settings.
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
" show whether the current buffer has been modified.
set ruler
set rulerformat=%=%m

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

" Jump to the last position when reopening a file.
" Don't do this for git commit messages though.
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \ exe "normal! g`\"" |
    \ endif
autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"



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

" Unmap s and S for now. I'm considering using these for 'sneak.vim'.
noremap s <Nop>
noremap S <Nop>



" Toggle line numbers.
noremap <F4> :set number! number?<CR>

" Toggle line wrapping.
noremap <F5> :set wrap! wrap?<CR>

" Toggle autoindent/mappings/etc, for pasting text.
noremap <F9> :set paste! paste?<CR>
set pastetoggle=<F9>

" <M-q> = Quit Vim.
" <M-w> = Save.
if has('nvim')
    noremap <M-q> :qa<CR>
    inoremap <M-q> <C-o>:qa<CR>
    noremap <M-w> :w<CR>
    inoremap <M-w> <C-o>:w<CR>
else
    noremap <Esc>q :qa<CR>
    inoremap <Esc>q <C-o>:qa<CR>
    noremap <Esc>w :w<CR>
    inoremap <Esc>w <C-o>:w<CR>
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

" Clear and redraw the screen; usually bound to <C-l>
" This is currently a useful bind since Neovim mangles the top line of the
" screen sometimes when resized.
" See https://github.com/neovim/neovim/issues/8322
" Fixed in Neovim 3.2, yay!
noremap <Leader>f <C-l>



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
