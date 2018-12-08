" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Init Pathogen.
execute pathogen#infect()

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

" Always assume 256 color support
set t_Co=256

" Dark background color.
set background=dark

" Solarized color scheme
colorscheme solarized

" See through background
"highlight Normal guibg=NONE ctermbg=NONE

" No startup message
set shortmess+=I



" Enable deoplete (autocompletion).
let g:deoplete#enable_at_startup = 1

" Deoplete options.
call deoplete#custom#option({
\ 'ignore_case': v:true,
\ })

" Fix the way enter interacts with deoplete.
inoremap <expr> <CR> pumvisible() ? "\<C-y>\<CR>" : "\<CR>"

" Use tab for completion.
function! s:check_space_behind() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~ '\s'
endfunction
inoremap <expr> <Tab> pumvisible() \|\| !<sid>check_space_behind() ?
    \ "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() \|\| !<sid>check_space_behind() ?
    \ "\<C-p>" : "\<S-Tab>"

" Disable preview window.
set completeopt-=preview



" Additional RSI binds.
" These two have corner cases near the end of lines.
function! s:at_eol() abort
    return col('.') ==# len(getline('.'))
endfunction
function! s:beyond_eol() abort
    return col('.') > len(getline('.'))
endfunction
inoremap <expr> <C-u> <sid>beyond_eol() ? "<C-o>d0<C-o>x" : "<C-o>d0"
inoremap <expr> <C-k> <sid>at_eol() ? "" : "<C-o>d$"

" These shadow Vim's completion binds, but I use Tab/S-Tab for that anyways.
inoremap <C-n> <C-o><down>
inoremap <C-p> <C-o><up>



" Toggle NERDTree.
noremap <Leader>i :NERDTreeToggle<CR>

" The default help bind '?' conflicts with vim's search-backwards.
let NERDTreeMapHelp = '<F1>'



" Open ctrlp.
let g:ctrlp_map = "<Leader>o"

" Use mixed mode: search for files, buffers, and 'most-recently-used' files.
let g:ctrlp_cmd = "CtrlPMixed"

" Don't include MRU files in the search results at all.
let g:ctrlp_mruf_max = 0

" Don't try to guess a good choice of working directory.
let g:ctrlp_working_path_mode = 0



" ack/rg
let g:ackprg = "rg --vimgrep"
noremap <expr> <Leader>a ":Ack "



" Fugitive binds.
noremap <expr> <Leader>gg ":Git! "
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gd :Gdiff<CR>
noremap <Leader>gb :Gblame<CR>



" NERD Commenter.
" Don't give me the default mappings.
let g:NERDCreateDefaultMappings = 0

" No spaces after the comment character.
let g:NERDSpaceDelims = 0

" Comment.
noremap <Leader>c :call NERDComment(0, "comment")<CR>

" Uncomment.
noremap <Leader>u :call NERDComment(0, "uncomment")<CR>



" EasyMotion
let g:EasyMotion_smartcase = 1
map <Leader>f <plug>(easymotion-s)



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



" Line numbers.
set number

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Don't wrap long lines.
set nowrap

" Don't go to the first non-blank on a line automatically when moving lines.
set nostartofline

" Show status bar (filename, etc) even when only one window is open.
set laststatus=2

" Show line and column number of the cursor in the status bar.
set ruler

" Show visual feedback for normal mode commands requiring multiple keypresses.
set showcmd

" Show commandline completion matches in a status line.
set wildmenu

" Start scrolling as soon as the cursor gets close to the edge of the screen.
set scrolloff=5

" Go to the first match as you are typing your search.
set incsearch

" Don't highlight search matches by default.
set nohlsearch

" Case insensitive by default
noremap / /\c
noremap ? ?\c



" Autoindent.
set autoindent

" Use 4 spaces for indent commands.
set shiftwidth=4

" Expand tab into spaces when pressed in insert mode.
set expandtab

" Allow backspacing through spaces as if they were tabs.
set softtabstop=4

" Show hard tabs.
set list
set listchars=tab:»\ ,extends:▶,precedes:◀,trail:·
autocmd InsertEnter * set listchars-=trail:·
autocmd InsertLeave * set listchars+=trail:·

" Hard tab width = 8 spaces. (Linux kernel style...)
set tabstop=8



" Easily toggle autoindent/mappings/etc for pasting text.
set pastetoggle=<F9>

" Toggle line wrapping.
noremap <F4> :set wrap! wrap?<CR>



" Vertical lines at 80, 100, and 120 chars.
set colorcolumn=80,100,120

" Maximum line length for various formatting-related things.
set textwidth=79

" Don't add an extra space after a '.' character when joining lines.
" Also applies to '?' and '!' characters.
set nojoinspaces



" Custom format options: don't autowrap; only insert comment characters
" when pressing <enter> on a commented line (and don't for the 'o' command).
" Done on load to override plugin-file settings.
autocmd BufNewFile,BufRead * set formatoptions=rqj



" Simple and unobtrusive folding in text files.
set foldtext=''
autocmd BufNewFile,BufRead *.txt set foldmethod=indent
autocmd BufNewFile,BufRead *.txt normal zR



" Two spaces indent by default for html files.
autocmd BufNewFile,BufRead *.html
    \ setlocal shiftwidth=2 |
    \ setlocal tabstop=2 |
    \ setlocal softtabstop=2

" Treat more things as Makefile.
autocmd BufNewFile,BufRead make*.inc
    \ setlocal syntax=make



" Jump to the last position when reopening a file.
" Don't do this for git commit messages though.
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \ exe "normal! g`\"" |
    \ endif
autocmd BufReadPost COMMIT_EDITMSG exe "normal! gg"



" -----------------------------------------------------------------------------
" Mappings
" -----------------------------------------------------------------------------

" jk = Exit insert mode or command-line mode.
inoremap jk <Esc>
inoremap Jk <Esc>
inoremap JK <Esc>
inoremap jK <Esc>

cnoremap jk <C-c>
cnoremap Jk <C-c>
cnoremap JK <C-c>
cnoremap jK <C-c>



" <CR> = gg
noremap <CR> gg

" <Home> = ^
" (By default it behaves like the 0 key instead.)
noremap <Home> ^

" <BS> = ^
noremap <BS> ^

" \ = $
noremap \ $

" Scroll faster.
noremap <C-y> 5<C-y>
noremap <C-e> 5<C-e>

" Scroll one line at a time.
noremap <M-C-y> <C-y>
noremap <Esc><C-y> <C-y>
noremap <M-C-e> <C-e>
noremap <Esc><C-e> <C-e>



" <M-q> = Quit all windows.
noremap <M-q> :qa<CR>
noremap <Esc>q :qa<CR>
inoremap <M-q> <C-o>:qa<CR>
inoremap <Esc>q <C-o>:qa<CR>

" <M-w> = Save.
noremap <M-w> :w<CR>
noremap <Esc>w :w<CR>
inoremap <M-w> <C-o>:w<CR>
inoremap <Esc>w <C-o>:w<CR>



" Navigate windows.
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l

" Resize window.
noremap <M-C-j> 5<C-w>+
noremap <M-C-k> 5<C-w>-
noremap <M-C-h> 5<C-w><
noremap <M-C-l> 5<C-w>>



" Copy/paste to/from system clipboard.
noremap <Leader>y "+y
noremap <Leader>Y "+Y
noremap <Leader>d "+d
noremap <Leader>D "+D
noremap <Leader>p "+p
noremap <Leader>P "+P

" Paste from the 0-register (ie yanked text only).
noremap <Leader>h "0p
noremap <Leader>H "0P

" Select recently pasted text (the default keybinding for this is gv).
nnoremap <Leader>v `[v`]



" Quit current window.
noremap <Leader>q :q<CR>

" Strip trailing whitespace
noremap <Leader>w :%s/\s\+$//<CR>

" Reload .vimrc
noremap <Leader>r :source $MYVIMRC<CR>

" Disable search highlighting until the next search.
"noremap <Leader>j :nohlsearch<CR>

" Toggle search highlighting.
"noremap <Leader>k :set hlsearch! hlsearch?<CR>

" List buffers.
noremap <Leader><Tab> :ls<CR>

" Next/previous buffer.
noremap <Leader>; :bn<CR>
noremap <Leader>, :bp<CR>

" Case-sensitive search.
noremap <Leader>/ /
noremap <Leader>? ?

" Search and replace.
nnoremap <Leader>s :%s///gc<left><left><left><left>
vnoremap <Leader>s :s///gc<left><left><left><left>

" Count occurences (ignoring case by default)
nnoremap <Leader>n :%s/\c//gn<left><left><left><left>
vnoremap <Leader>n :s/\c//gn<left><left><left><left>

" Change file permissions to be executable or not.
noremap <Leader>x :!chmod +x %<CR>
noremap <Leader>X :!chmod -x %<CR>



" Unmap s and S for now. I'll probably map them to 'sneak.vim' at some point.
noremap s <Nop>
noremap S <Nop>
