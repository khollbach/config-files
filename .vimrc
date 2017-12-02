" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Init Pathogen.
execute pathogen#infect()

" Set leader to space.
let mapleader = "\<space>"
noremap <space> <nop>



" -----------------------------------------------------------------------------
" Plugin Settings
" -----------------------------------------------------------------------------

" Always assume 256 color support
set t_Co=256

" Dark background color.
set background=dark

" Solarized color scheme
colorscheme solarized



" Enable neocomplete.
let g:neocomplete#enable_at_startup = 1

" Ignore case when looking for matches.
let g:neocomplete#enable_ignore_case = 1

" Fix the way enter interacts with neocomplete.
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
inoremap <C-n> <down>
inoremap <C-p> <up>



" Toggle NERDTree.
noremap <leader>i :NERDTreeToggle<CR>



" Open ctrlp.
let g:ctrlp_map = "<leader>o"

" Use mixed mode: search for files, buffers, and 'most-recently-used' files.
let g:ctrlp_cmd = "CtrlPMixed"

" Don't include MRU files in the search results at all.
let g:ctrlp_mruf_max = 0

" Don't try to guess a good choice of working directory.
let g:ctrlp_working_path_mode = 0



" ack bind.
noremap <expr> <leader>a ":Ack "



" Fugitive binds.
noremap <expr> <leader>gg ":Git! "
noremap <leader>gs :Gstatus<CR>
noremap <leader>gc :Gcommit<CR>
noremap <leader>gd :Gdiff<CR>
noremap <leader>gb :Gblame<CR>



" NERD Commenter.
" Don't give me the default mappings.
let g:NERDCreateDefaultMappings = 0

" No spaces after the comment character.
let g:NERDSpaceDelims = 0

" Comment.
noremap <leader>c :call NERDComment(0, "comment")<CR>

" Uncomment.
noremap <leader>u :call NERDComment(0, "uncomment")<CR>



" EasyMotion
map <leader><leader> <plug>(easymotion-prefix)
map <leader>f <plug>(easymotion-s)



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

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Wrap long lines.
set wrap

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

" Highlight the first match as you are typing your search.
set incsearch

" Highlight search matches by default.
set hlsearch

" Case insensitive by default
noremap / /\c
noremap ? ?\c



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



" Vertical lines at 80, 100, and 120 chars.
set colorcolumn=80,100,120

" Maximum line length for various formatting-related things.
set textwidth=79

" Autowrap comments to textwidth.
" Done on load to override plugin-file settings.
autocmd BufNewFile,BufRead * set formatoptions=jcrq

" In .txt and .tex files, autowrap non-comment lines as well.
autocmd BufNewFile,BufRead *.txt set formatoptions+=t
"autocmd BufNewFile,BufRead *.tex set formatoptions+=t

" Simple and unobtrusive folding in text files.
set foldtext='\ '
autocmd BufNewFile,BufRead *.txt set foldmethod=indent
autocmd BufNewFile,BufRead *.txt normal zR



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

" <Tab> = ^
noremap <Tab> ^

" \ = $
noremap \ $

" Scroll faster.
noremap <C-y> 3<C-y>
noremap <C-e> 3<C-e>



" <M-q> = Quit all windows.
noremap <Esc>q :qa<CR>
inoremap <Esc>q <C-o>:qa<CR>

" <M-w> = Save.
noremap <Esc>w :w<CR>
inoremap <Esc>w <C-o>:w<CR>



" Navigate windows.
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-h> <C-w>h
noremap <C-l> <C-w>l

" Resize window.
noremap <Esc><C-j> 5<C-w>+
noremap <Esc><C-k> 5<C-w>-
noremap <Esc><C-h> 5<C-w><
noremap <Esc><C-l> 5<C-w>>



" Copy/paste to/from system clipboard.
noremap <leader>y "+y
noremap <leader>Y "+Y
noremap <leader>d "+d
noremap <leader>D "+D
noremap <leader>p "+p
noremap <leader>P "+P

" Paste from the 0-register (ie yanked text only).
noremap <leader>h "0p
noremap <leader>H "0P



" Quit current window.
noremap <leader>q :q<CR>

" Strip trailing whitespace
noremap <leader>w :%s/\s\+$//<CR>

" Disable search highlighting until the next search.
noremap <leader>j :nohlsearch<CR>

" Toggle search highlighting.
noremap <leader>k :set hlsearch! hlsearch?<CR>

" List buffers.
noremap <leader><TAB> :ls<CR>

" Next/previous buffer.
noremap <leader>; :bn<CR>
noremap <leader>, :bp<CR>

" Case-sensitive search.
noremap <leader>/ /
noremap <leader>? ?

" Search and replace.
nnoremap <leader>s :%s///gc<left><left><left><left>
vnoremap <leader>s :s///gc<left><left><left><left>

" Change file permissions to be executable or not.
noremap <leader>x :!chmod +x %<CR>
noremap <leader>X :!chmod -x %<CR>

" Select recently pasted text.
nnoremap <leader>v `[v`]
