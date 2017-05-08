" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Init Pathogen (plugin manager).
execute pathogen#infect()

" Set leader to space.
let mapleader = "\<space>"
noremap <space> <nop>



" -----------------------------------------------------------------------------
" Plugin Settings
" -----------------------------------------------------------------------------

" If your terminal emulator has a 'Solarized' color scheme, use it and
" leave the following line commented out. Otherwise, uncomment this line.
"let g:solarized_termcolors=256

" Force vim to assume the terminal supports 256 colors.
set t_Co=256

" Dark background color.
set background=dark

" Solarized color scheme.
colorscheme solarized



" Enable neocomplete.
let g:neocomplete#enable_at_startup = 1

" Ignore case when looking for matches.
let g:neocomplete#enable_ignore_case = 1

" Fix the way enter interacts with neocomplete.
inoremap <expr> <cr> pumvisible() ? "\<C-y>\<cr>" : "\<cr>"

" Use <tab> for completion.
function! s:check_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1] =~ '\s'
endfunction
inoremap <expr> <tab> pumvisible() \|\| !<sid>check_space() ?
    \ "\<C-n>" : "\<tab>"
inoremap <expr> <S-tab> pumvisible() \|\| !<sid>check_space() ?
    \ "\<C-p>" : "\<S-tab>"

" Disable preview window.
set completeopt-=preview



" Additional RSI binds.
inoremap <C-u> <C-o>d^
function! s:check_end() abort
    return col('.') ==# len(getline('.'))
endfunction
inoremap <expr> <C-k> <sid>check_end() ? "<C-o>d$" : ""

" These shadow Vim's completion binds, but I use tab/S-tab for that anyways.
inoremap <C-n> <down>
inoremap <C-p> <up>



" <leader>i = NERDTree.
noremap <leader>i :NERDTreeToggle<cr>



" <leader>o = ctrlp.
let g:ctrlp_map = "<leader>o"

" Use 'mixed mode'; search for files, buffers, and 'most-recently-used' files.
let g:ctrlp_cmd = "CtrlPMixed"

" Don't include MRU files in the search results at all.
let g:ctrlp_mruf_max = 0

" Don't try to guess a good choice of working directory.
let g:ctrlp_working_path_mode = 0



" <leader>a = ack.
noremap <expr> <leader>a ":Ack "



" Fugitive binds.
noremap <expr> <leader>gg ":Git! "
noremap <leader>gs :Gstatus<cr>
noremap <leader>gc :Gcommit<cr>
noremap <leader>gd :Gdiff<cr>
noremap <leader>gb :Gblame<cr>



" Toggle GitGutter.
noremap <leader>h :GitGutterToggle<cr>



" NERD Commenter
" No spaces.
let g:NERDSpaceDelims = 0

" <leader>f = comment
noremap <leader>f :call NERDComment(0, "comment")<CR>

" <leader>k = uncomment
noremap <leader>k :call NERDComment(0, "uncomment")<CR>


" -----------------------------------------------------------------------------
" Settings
" -----------------------------------------------------------------------------

" Syntax highlighting.
syntax on

" Load plugin and indent files for known filetypes.
filetype plugin indent on



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

" Show status bar (filename, etc) even when only one window is open.
set laststatus=2

" Show line and column number of the cursor in the status bar.
set ruler

" Show visual feedback for normal mode commands requiring multiple keypresses.
set showcmd

" Show commandline completion matches in a status line.
set wildmenu

" Start scrolling as soon as the cursor gets close to the edge of the screen.
set scrolloff=3

" Highlight the first match as you are typing your search.
set incsearch

" Don't highlight search matches by default.
set nohlsearch



" Autoindent.
set autoindent

" Use 4 spaces for indent commands.
set shiftwidth=4

" Expand tab into spaces when pressed in insert mode.
set expandtab

" Tab width = 4 spaces.
set tabstop=4

" Allow backspacing through spaces as if they were tabs.
set softtabstop=4

" Easily toggle autoindent/mappings/etc for pasting text.
set pastetoggle=<F9>



" Vertical line at 80 chars.
set colorcolumn=80

" Maximum line length for various formatting-related things.
set textwidth=79

" Autowrap comments to textwidth.
" Done on load to override plugin-file settings.
autocmd BufNewFile,BufRead * set formatoptions=jcrq

" In .txt and .tex files, autowrap non-comment lines as well.
autocmd BufNewFile,BufRead *.txt set formatoptions+=t
autocmd BufNewFile,BufRead *.tex set formatoptions+=t



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
inoremap jk <esc>
cnoremap jk <C-c>



" <CR> = gg
noremap <CR> gg

" Scroll faster.
noremap <C-y> 3<C-y>
noremap <C-e> 3<C-e>

" Format paragraph.
nnoremap Q gqip

" Select recently pasted text.
nnoremap gp `[v`]



" <M-q> = Quit all windows.
noremap <esc>q :qa<cr>
inoremap <esc>q <C-o>:qa<cr>

" <M-w> = Save.
noremap <esc>w :w<cr>
inoremap <esc>w <C-o>:w<cr>



" Navigate windows.
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l

" Split window.
noremap <C-w>j <C-w>s<C-w>j
noremap <C-w>k <C-w>s
noremap <C-w>h <C-w>v
noremap <C-w>l <C-w>v<C-w>l

" Resize window.
noremap <esc><C-j> 5<C-w>+
noremap <esc><C-k> 5<C-w>-
noremap <esc><C-h> 5<C-w><
noremap <esc><C-l> 5<C-w>>



" <leader>j = Toggle search highlighting.
noremap <leader>j :set hlsearch! hlsearch?<cr>

" <leader>q = Quit current window.
noremap <leader>q :q<cr>

" <leader>w = Strip trailing whitespace.
noremap <leader>w :%s/\s\+$//<cr>

" <leader>e = Edit vimrc.
noremap <leader>e :e ~/config-files/.vimrc<cr>

" <leader>r = Reload vimrc.
noremap <leader>r :source $MYVIMRC<cr>

" <leader>t = Replace tabs with spaces.
noremap <leader>t :%retab<cr>

" <leader>u = Run update_configs script.
noremap <leader>u :!~/config-files/update_configs<cr>



" Next/previous buffer.
noremap <leader>; :bn<cr>
noremap <leader>, :bp<cr>

" Copy/paste to/from system clipboard.
noremap <leader>y "+y
noremap <leader>Y "+Y
noremap <leader>d "+d
noremap <leader>D "+D
noremap <leader>p "+p
noremap <leader>P "+P

" Paste from the 0-register (ie yanked text only).
noremap <leader>l "0p
noremap <leader>L "0P

" Case insensitive search.
noremap <leader>/ /\c
noremap <leader>? ?\c

" Next/previous match, and center the screen on the cursor.
noremap <leader>n nzz
noremap <leader>N Nzz

" Search and replace.
nnoremap <leader>s :%s///gc<left><left><left><left>
vnoremap <leader>s :s///gc<left><left><left><left>

" Paste, fixing indentation automatically.
noremap <leader>] ]p
noremap <leader>} ]P
