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

" If .vim/bundle directory exists, run this section.
if !empty(glob('~/.vim/bundle'))



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



" Additional RSI binds.

" The following two have corner cases near the end of lines.
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

" These shadow Vim's completion binds, but I use Tab/S-Tab for that anyways.
" (If the PUM is active, close it first.)
inoremap <expr> <C-n> pumvisible() ? "\<C-y>\<Down>" : "\<Down>"
inoremap <expr> <C-p> pumvisible() ? "\<C-y>\<Up>" : "\<Up>"



" Toggle NERDTree.
noremap <Leader>i :NERDTreeToggle<CR>

" Go to current file in NERDTree.
noremap <Leader>I :NERDTreeFind<CR>

" Hide NERDTree after opening a file.
let NERDTreeQuitOnOpen = 1

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

map <Leader>f <Plug>(easymotion-s)

map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)



" quick-scope
" Highlight only on keypress.
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']



" sneak.vim

" Disable highlighting of sneak matches.
" This isn't quite the right fix, but it's pretty close.
"hi! link Sneak Normal

" Highlight matches in blue instead. Magenta was a little too much.
hi! Sneak ctermbg=darkblue ctermfg=black guibg=darkblue guibg=black

" Fix an interaction with quick-scope. The issue was that `;' would repeat the
" last `s', even if `f' was more recently used.
" Unfortunately this now means we can't use the same binds (`;' and `,') for
" repeating `f' and `s'.
noremap ; ;
noremap , ,

" Press `s' or `S' while sneaking to repeat.
let g:sneak#s_next = 1



" incsearch.vim
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



" Line numbers.
set number

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Don't wrap long lines.
set nowrap

" Toggle line numbers.
noremap <F7> :set number! number?<CR>

" Toggle line wrapping.
noremap <F8> :set wrap! wrap?<CR>

" Easily toggle autoindent/mappings/etc for pasting text.
set pastetoggle=<F9>



" Start scrolling as soon as the cursor gets close to the edge of the screen.
set scrolloff=5

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

" Go to the first match as you are typing your search.
set incsearch

" Don't highlight search matches by default.
set nohlsearch



" Autoindent.
set autoindent

" Use 4 spaces for indent commands.
set shiftwidth=4

" Expand tab into spaces when pressed in insert mode.
set expandtab

" Allow backspacing through spaces as if they were tabs.
set softtabstop=4

" Show hard tabs and trailing spaces.
" Also show indicators for text that extends past the edge of the screen.
set list
set listchars=tab:»\ ,extends:▶,precedes:◀,trail:·
" Don't show trailing spaces when typing.
autocmd InsertEnter * set listchars-=trail:·
autocmd InsertLeave * set listchars+=trail:·

" Hard tab width = 8 spaces. (Linux kernel style... completely insane.)
set tabstop=8



" Vertical lines at 80, 100, and 120 chars.
set colorcolumn=80,100,120

" Maximum line length for various formatting-related things.
set textwidth=79

" Don't add an extra space after a '.' character when joining lines.
" Also applies to '?' and '!' characters.
set nojoinspaces



" Format options: don't autowrap; only insert comment characters
" when pressing <enter> on a commented line (and don't for the 'o' command).
" Done on load to override plugin-file settings.
autocmd BufNewFile,BufRead * set formatoptions=rqj



" Simple and unobtrusive folding in text files.
" At some point I'll probably just switch to org mode for notes and todos.
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

cnoremap jk <C-c>



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

" Select recently pasted text. (Built-in gv selects recently selected text.)
nnoremap gp `[v`]



" Quit current window.
noremap <Leader>q :q<CR>

" Strip trailing whitespace
noremap <Leader>w :%s/\s\+$//<CR>

" Update configs
noremap <Leader>e :!source ~/config-files/update_configs<CR>

" Reload .vimrc
noremap <Leader>r :source $MYVIMRC<CR>

" Clear and redraw the screen; usually bound to <C-l>
" This is currently a necessary bind since Nvim mangles the screen sometimes
" when resized; e.g. when the containing terminal goes from full screen width
" (or height) to half. The symptom is that text from other lines will appear
" in places it shouldn't; often on the top line of the screen.
noremap <Leader>t <C-l>

" List buffers.
noremap <Leader><Tab> :ls<CR>

" Next/previous buffer.
noremap <Leader>; :bn<CR>
noremap <Leader>, :bp<CR>

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
"noremap s <Nop>
"noremap S <Nop>
