" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Enable syntax highlighting.
syntax on

" Automatically detect known filetypes.
filetype plugin indent on

" Set leader to space.
let mapleader = "\<Space>"
noremap <Space> <Nop>
noremap <Space><Space> <Nop>

" -----------------------------------------------------------------------------
" Plugins
" -----------------------------------------------------------------------------

function! PluginExists(name) abort
    return !empty(glob('~/.vim/pack/plugins/start/' . a:name . '/'))
endfunction

if PluginExists('vim-colors-solarized')
    set background=dark
    colorscheme solarized
endif

if PluginExists('vim-sneak')
    " Respect Vim's ignorecase and smartcase settings.
    let g:sneak#use_ic_scs = 1

    " Don't highlight matches.
    autocmd User SneakLeave highlight clear Sneak

    " Accept `s` and `S` in operator-pending mode. Note that this conflicts
    " with surround.vim's default binds, which I've changed to z/Z.
    omap s <Plug>Sneak_s
    omap S <Plug>Sneak_S
    xmap S <Plug>Sneak_S
endif

if PluginExists('vim-surround')
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

if PluginExists('vim-rsi')
    " <C-y> = put recent
    " <M-y> = put yanked
    inoremap <C-y> <C-r>"
    if has('nvim')
        inoremap <M-y> <C-r>0
    else
        inoremap <Esc>y <C-r>0
    endif

    " <C-n> and <C-p> shadow Vim's completion binds, but I use Tab/S-Tab for
    " that anyways. If the PUM is active, we close it first.
    inoremap <expr> <C-n> pumvisible() ? "\<C-y>\<Down>" : "\<Down>"
    inoremap <expr> <C-p> pumvisible() ? "\<C-y>\<Up>" : "\<Up>"

    " <C-u> and <C-k> have corner cases near the end of lines.
    inoremap <expr> <C-u> <SID>beyond_eol() ? "<C-o>d0<C-o>x" : "<C-o>d0"
    if has('nvim')
        " Nvim seems to have a different cursor position for C-o at end of line
        " compared to Vim. It's probably a bug.
        inoremap <expr> <C-k> <SID>beyond_eol() ? "" : "<C-o>d$"
    else
        inoremap <expr> <C-k> <SID>at_eol() ? "" : "<C-o>d$"
    endif

    function! s:at_eol() abort
        return col('.') ==# len(getline('.'))
    endfunction

    function! s:beyond_eol() abort
        return col('.') > len(getline('.'))
    endfunction
endif

if PluginExists('vim-fugitive')
    noremap <Leader>gs :Gstatus<CR>
    noremap <Leader>gd :Gdiffsplit<CR>
    noremap <Leader>gc :Gcommit<CR>
    noremap <Leader>gm :Gmerge<CR>
    noremap <Leader>gr :Grebase<CR>
    noremap <Leader>gu :Gpush<CR>
    noremap <Leader>gf :Gfetch<CR>
    noremap <Leader>gp :Gpull<CR>
    noremap <Leader>gb :Gblame<CR>
    noremap <Leader>gl :Gclog<CR>
    noremap <expr> <Leader>gg ":Ggrep "
endif

if PluginExists('vim-gitgutter')
    " Update the gutter every 100ms instead of every 4 seconds.
    set updatetime=100

    " Disable the default maps, since it would map things like `<leader>hp`,
    " which would cause my own `<leader>h` mapping to be sticky while waiting
    " to see if I was going to press `p` or not.
    let g:gitgutter_map_keys = 0

    nmap ]c <Plug>(GitGutterNextHunk)
    nmap [c <Plug>(GitGutterPrevHunk)

    nmap ghs <Plug>(GitGutterStageHunk)
    nmap ghu <Plug>(GitGutterUndoHunk)
    nmap ghp <Plug>(GitGutterPreviewHunk)
endif

if PluginExists('incsearch.vim')
    " Afaict, the only feature these mappings have over vanilla `/` is
    " highlighting search matches as you are typing *even when* 'hlsearch' is
    " off. It's pretty nice though, not gonna lie.
    map / <Plug>(incsearch-forward)
    map ? <Plug>(incsearch-backward)

    " Case-sensitive search.
    map <Leader>/ <Plug>(incsearch-forward)\C
    map <Leader>? <Plug>(incsearch-backward)\C
endif

if PluginExists('ack.vim')
    let g:ackprg = "rg --vimgrep"

    noremap <expr> <Leader>a ":Ack "
endif

if PluginExists('nerdcommenter')
    let g:NERDCreateDefaultMappings = 0

    map <Leader>c <plug>NERDCommenterComment
    map <Leader>u <plug>NERDCommenterUncomment

    " No spaces after the comment character(s). Doesn't work for python files,
    " even if I use `set commentstring=#%s`. I'm not sure why.
    let g:NERDSpaceDelims = 0
endif

if PluginExists('nerdtree')
    noremap <Leader>i :NERDTreeToggle<CR>
    noremap <Leader>I :NERDTreeFind<CR>

    let NERDTreeQuitOnOpen = 1

    " Otherwise it's bound to `?`, which shadows search-backwards.
    let NERDTreeMapHelp = '<F1>'
endif

if PluginExists('undotree')
    noremap <Leader>U :UndotreeToggle<CR>
endif

if PluginExists('context.vim')
    " Disabled for now; it's not exactly pretty. Maybe it's useful though.
    let g:context_enabled = 0

    " Without the following, <C-y> and <C-e> are line-by-line, instead of 5 at
    " a time as per my custom mappings. This fixes it, but I'm not sure why. I
    " just disabled context.vim's default mappings, and then re-added them.
    let g:context_add_mappings = 0
    nnoremap <silent>        <C-Y> <C-Y>:call context#update('C-Y')<CR>
    nnoremap <silent>        zz     zzzz:call context#update('zz')<CR>
    nnoremap <silent>        zb     zbzb:call context#update('zb')<CR>
    nnoremap <silent> <expr> <C-E>            context#mapping#ce()
    nnoremap <silent> <expr> zt               context#mapping#zt()
    nnoremap <silent> <expr> k                context#mapping#k()
    nnoremap <silent> <expr> H                context#mapping#h()
endif

if PluginExists('goyo.vim')
    noremap <silent> <Leader>f :Goyo<CR>:echo ''<CR>
endif

if PluginExists('fzf.vim') && PluginExists('fzf.vim')
    " Display fzf in a popup window instead.
    let g:fzf_layout = { 'window': { 'width': 0.9, 'height': 0.6, 'highlight': 'Todo', 'rounded': v:true } }

    noremap <leader>o :Files<CR>
    noremap <leader>b :Buffers<CR>
    noremap <leader>t :Tags<CR>

    " todo
    "nnoremap <leader>m :MRU<CR>
endif

"if PluginExists('ctrlp.vim')
if 0
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

"if has('nvim') && !empty(glob('~/.vim/bundle/deoplete.nvim'))
if 0
    let g:deoplete#enable_at_startup = 1

    call deoplete#custom#option({
    \ 'on_insert_enter': v:false,
    \ })

    " Initially disabled.
    call deoplete#custom#option('auto_complete', v:false)
    let s:my_deoplete_enabled = 0

    " Toggle deoplete.
    noremap <expr> <F9> <SID>toggle_deoplete()
    inoremap <expr> <F9> <SID>toggle_deoplete()
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
    " If the 'pop-up-menu' is visible (i.e., if autocomplete suggestions are
    " showing), close it then insert a newline.
    inoremap <expr> <CR> <SID>enter_fn()
    function! s:enter_fn() abort
        if pumvisible()
            " <C-y> confirms the current selection.
            return "\<C-y>\<CR>"
        else
            return "\<CR>"
        endif
    endfunction

    " Disable preview window. Deoplete would sometime uses this to show
    " documentation of, e.g., python functions.
    set completeopt-=preview
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

" Show commandline completion matches above the commandline on <Tab> keypress.
set wildmenu



" No startup message
set shortmess+=I

" Don't show the tildes after the last line in the file.
" Doesn't work on Vim 8.0 for me; I haven't looked into why.
if has('nvim')
    " Note the trailing space.
    set fillchars+=eob:\ 
endif

" Don't give visual feedback for normal mode commands requiring multiple
" keypresses.
set noshowcmd

" Don't show an indicator in the echo area when you're in insert mode.
" (The cursor shape already indicates that in Neovim.)
set noshowmode

" Don't show a how-to-quit message when you press <C-c> in normal mode.
nnoremap <C-c> <Nop>



" Line numbers.
set number

" Toggle line numbers.
if PluginExists('vim-gitgutter')
    noremap <F4> :GitGutterSignsToggle<CR>:set number! number?<CR>
    inoremap <F4> <C-o>:GitGutterSignsToggle<CR><C-o>:set number! number?<CR>
else
    noremap <F4> :set number! number?<CR>
    inoremap <F4> <C-o>:set number! number?<CR>
endif

" Wrap long lines by default.
set wrap

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

" Toggle line wrapping.
noremap <F5> :set wrap! wrap?<CR>
inoremap <F5> <C-o>:set wrap! wrap?<CR>

" Vertical lines after 80, 100, and 120 chars.
"set colorcolumn=81,101,121
set colorcolumn=

" Toggle colorcolumn
noremap <expr> <F6> <SID>toggle_colorcolumn()
inoremap <expr> <F6> <SID>toggle_colorcolumn()
function! s:toggle_colorcolumn() abort
    if &colorcolumn !=# ""
        set colorcolumn=
    else
        set colorcolumn=81,101,121
    endif
    set colorcolumn?

    " Need to redraw here to get the effect right away.
    redraw
    return ""
endfunction

" Toggle autoindent/mappings/etc, for pasting text.
noremap <F7> :set paste! paste?<CR>
set pastetoggle=<F7>



" Maximum line length for various formatting-related things.
set textwidth=79

" Don't add an extra space after a '.' character when joining lines.
" Also applies to '?' and '!' characters.
set nojoinspaces

" Text-formatting options. Some done on load to override plugin-file settings.
set formatoptions-=tc
autocmd BufNewFile,BufRead,BufAdd,BufFilePost *
    \ set formatoptions+=rqj formatoptions-=o



" Start scrolling as soon as the cursor gets close to the edge of the screen.
set scrolloff=5

" Don't automatically perform ^ after each C-u, C-d, C-b, C-f, H, M, L, etc.
" Basically, leave my cursor in the current column when scrolling around.
set nostartofline

" Case insensitive search.
set ignorecase

" (Unless you typed a capital letter in your search.)
set smartcase

" Go to the first match as you are typing your search.
set incsearch

" Highlight search matches by default.
set hlsearch

" Show number of matches (and current position therin) when searching.
" Available starting in Vim 8.1.1270 and Nvim 0.4.0
set shortmess-=S



" Set the "terminal title", which may be read by terminal emulators to decide
" on a window name. In my case, tmux is configured to show the terminal title
" for each open window.
set title

" The terminal title will be the current file name (but not the path).
" We use %{expand("%:t")} instead of just %t here, since expand won't output
" "[No Name]" for new buffers. Unfortunately, in %{ } blocks, Vim will try
" format numbers cleverly, so e.g. the filename "001" becomes "1". I don't see
" an easy way to fix that here.
set titlestring=%{expand(\"%:t\")}

" Show status line (filename, etc) even when there's only one window.
set laststatus=2

" Hide the vertical bar between splits. TODO: get a better fix for this.
highlight! VertSplit ctermfg=8 ctermbg=8

" Toggle the status line.
noremap <expr> <Leader>' <SID>toggle_laststatus()
function! s:toggle_laststatus() abort
    if &laststatus ==# 2
        set laststatus=1
    else
        set laststatus=2
    endif

    " Need to redraw here to get the effect right away.
    mode
    return ""
endfunction

" Change status line color.
highlight! StatusLine ctermbg=8 ctermfg=11 cterm=reverse
highlight! StatusLineNC ctermbg=0 ctermfg=12 cterm=none

" This last highlight group is made-up by me.
highlight! StatusLineTrailing ctermbg=8 ctermfg=12

function! StatusLine()
    if expand("%:t") !=# ""
        return ' %f %#StatusLineTrailing#%( %h%w%r%m%)' . GitStatus()
    else
        return '%#StatusLineTrailing#%=%(%h%w%r%m %)'
    endif
endfunction
set statusline=%!StatusLine()



" Get the number of lines added/edited/deleted to show up in the status bar.
if PluginExists('vim-gitgutter')
    highlight! GSAdded ctermfg=2
    highlight! GSModified ctermfg=3
    highlight! GSRemoved ctermfg=1
    function! GitStatus()
        let result = ''
        let result = result . '%#GSAdded#' . '%( %{GSAdd()}%)'
        let result = result . '%#GSModified#' . '%( %{GSMod()}%)'
        let result = result . '%#GSRemoved#' . '%( %{GSRem()}%)'
        let result = result . '%#StatuslineTrailing#'
        return result
    endfunction

    " These each need to be expanded in '%{FuncName()}' blocks, otherwise they
    " will have the current window's info even if they're being rendered in
    " another window. And since '%#HiGroup#' elements need to appear in the
    " statusline, and *not* in %{} blocks, we're forced to awkardly break the
    " following up into their own separate functions just to get them to
    " highlight as different colors.
    function! GSAdd()
        let [a,m,r] = GitGutterGetHunkSummary()
        if a !=# 0
            return '+' . a
        else
            return ''
        endif
    endfunction
    function! GSMod()
        let [a,m,r] = GitGutterGetHunkSummary()
        if m !=# 0
            return '~' . m
        else
            return ''
        endif
    endfunction
    function! GSRem()
        let [a,m,r] = GitGutterGetHunkSummary()
        if r !=# 0
            return '-' . r
        else
            return ''
        endif
    endfunction
else
    function! GitStatus()
        return ''
    endfunction
endif



" Show certain info at the bottom-right of the screen. This only appears when
" the statusline is hidden.
set ruler

" Show the current filename, and whether the current buffer has been modified.
" 36 (total width) = 32 (filename) + 1 (space) + 3 (modified)
set rulerformat=%36(%=%{PercentT()}%3(%m%)%)

"" Instead of the above, we update the rulerformat width dynamically when the
"" current buffer name changes. This way we're not wasting space in the "echo
"" area". It's too bad Vim doesn't do this for us automatically. :(
"" This doesn't play that nicely with multiple windows though, since rulerformat
"" is a global setting with no window-local (or buffer-local) value. :(
"autocmd BufEnter,BufAdd,BufFilePost *
    "\ let &rulerformat =
    "\ "%" . (strlen(Percent_t()) + 3) . "(%=%{Percent_t()}%3(%m%)%)"

" We use this function instead of %t in rulerformat, since this won't show
" "[No Name]" when there's no open file. Also, this allows us to truncate from
" the right instead of from the left.
"
" This function returns one additional space at the end of the filename, if it
" exists; this is a hack to get around automatic number formatting.
function! PercentT() abort
    let filename = expand("%:t")

    " If the filename is too long, display the first 31 chars of it, and ">".
    if strlen(filename) > 32
        let filename = filename[0:30] . ">"
    endif

    " We want one space between the filename and the 'modified' indicator.
    " We would have put this space in the 'rulerformat' string, except that
    " this way if the file is named something numeric like "0123", Vim won't
    " try to be clever and format it as "123" instead, since we return "0123 "
    " from the %{ } block. Terrible hack, I know.
    if strlen(filename) > 0
        let filename = filename . " "
    endif

    return filename
endfunction



" Enable autoindent.
set autoindent

" Use 4 spaces for indent commands.
set shiftwidth=4

" Expand tab into spaces when pressed in insert mode.
set expandtab

" Allow backspacing through spaces as if they were tabs.
set softtabstop=4

" Hard tab width.
set tabstop=4

" Show hard tabs and trailing spaces.
" Also show indicators for text that extends past the edge of the screen.
set list
" TODO: Get tabs to show visually, but only in languages other than go.
set listchars=tab:\ \ ,extends:▶,precedes:◀,trail:·

" Don't show trailing spaces when typing.
autocmd InsertEnter * set listchars-=trail:·
autocmd InsertLeave * set listchars+=trail:·



" Syntax highlighting, etc, for different file extensions.
augroup filetype_settings
    autocmd!

    " Cheat-sheet:
    "   BufNewFile
    "       open new file
    "   BufRead
    "       open existing file
    "   BufAdd (don't use BufNew; see the docs for gotchas)
    "       write file from a new buffer
    "   BufFilePost
    "       rename file
    " I'm pretty sure these cover all the ways for a filename to change.
    " For completeness, there's also:
    "   BufEnter (which strictly subsumes BufNewFile and BufRead)
    "       switching between existing buffers, or creating a new one

    " Two spaces indent for html files.
    autocmd BufNewFile,BufRead,BufAdd,BufFilePost *.html
        \ setlocal shiftwidth=2 |
        \ setlocal softtabstop=2

    " Treat .sc as Scala.
    autocmd BufNewFile,BufRead,BufAdd,BufFilePost *.sc
        \ setlocal syntax=scala

    " Treat TypeScript as JavaScript.
    autocmd BufNewFile,BufRead,BufAdd,BufFilePost *.ts
        \ setlocal syntax=javascript

    " Avro schemas are JSON.
    autocmd BufNewFile,BufRead,BufAdd,BufFilePost *.avsc
        \ setlocal syntax=json |
        \ setlocal shiftwidth=2 |
        \ setlocal softtabstop=2 |
        \ setlocal foldmethod=indent |
        \ normal zR

    " Treat more things as Makefile.
    autocmd BufNewFile,BufRead,BufAdd,BufFilePost make*.inc
        \ setlocal syntax=make
augroup END



" Jump to the last position when reopening a file.
augroup reopen_file
    autocmd!
    autocmd BufRead *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
            \ exe "normal! g`\"" |
        \ endif

    " Don't do this for git commit messages though (since you're not really
    " re-opening anything, but the filename/path just happens to be the same).
    autocmd BufRead COMMIT_EDITMSG exe "normal! gg"
augroup END



" Highlight lowercase "todo" in comments as well as uppercase "TODO".
" https://stackoverflow.com/a/30552423
augroup lowercase_todo
    autocmd!

    " I don't just match against `.*Comment.*` here because that would match
    " things like vimCommentString, rustCommentLineDocError, etc, etc.
    autocmd Syntax * syntax keyword LowercaseTodo contained todo
        \ containedin=.*Comment
        \,vimCommentTitle,cCommentL
        \,rustCommentLine,rustCommentBlock
augroup END
highlight def link LowercaseTodo Todo



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

" Exit insert mode or command-line mode.
inoremap jk <Esc>
inoremap jK <Esc>
inoremap Jk <Esc>
inoremap JK <Esc>
cnoremap jk <C-c>
cnoremap jK <C-c>
cnoremap Jk <C-c>
cnoremap JK <C-c>

" 123<CR> takes you to line 123.
noremap <CR> gg

" Scroll 5x faster.
noremap <C-y> 5<C-y>
noremap <C-e> 5<C-e>

" Go to the first non-blank character, instead of the actual start of line.
noremap <Home> ^
inoremap <Home> <C-o>^

" Select recently pasted text. (Built-in) gv selects recently selected text.
nnoremap gp `[v`]

if has('nvim')
    " Save.
    noremap <M-w> :w<CR>
    inoremap <M-w> <C-o>:w<CR>

    " Quit.
    noremap <M-q> :qa<CR>
    inoremap <M-q> <C-o>:qa<CR>

    " Execute last command.
    noremap <M-p> :<Up><CR>
else
    noremap <Esc>w :w<CR>
    inoremap <Esc>w <C-o>:w<CR>

    noremap <Esc>q :qa<CR>
    inoremap <Esc>q <C-o>:qa<CR>

    noremap <Esc>p :<Up><CR>
endif

" Use tab/shift-tab for completion. Happens only if the PUM is active or if
" there's a word-character behind the cursor; otherwise tab is just
" indentation.
inoremap <silent><expr> <Tab> <SID>tab_fn()
inoremap <silent><expr> <S-Tab> <SID>s_tab_fn()

function! s:tab_fn() abort
    if pumvisible() || s:check_word_behind()
        " Next suggestion.
        return "\<C-n>"
    else
        " Indent.
        return "\<Tab>"
    endif
endfunction

function! s:s_tab_fn() abort
    if pumvisible() || s:check_word_behind()
        " Previous suggestion.
        return "\<C-p>"
    else
        " Indent.
        return "\<S-Tab>"
    endif
endfunction

" Check if the character immediately to the left of the cursor is a
" "word-character", ie [0-9A-Za-z_]. False if at the beginning of the line.
function! s:check_word_behind() abort
    let col = col('.') - 1
    return col > 0 && getline('.')[col - 1] =~ '\w'
endfunction



" Navigate windows with C-hjkl.
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Resize current window with C-M-hjkl.
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

" Paste from 0-register (yanked text only).
noremap <Leader>h "0p
noremap <Leader>H "0P

" :noh
noremap <Leader>j :nohlsearch<CR>:echo<CR>
noremap <Leader>k :set hlsearch! hlsearch?<CR>

" Close current window.
noremap <Leader>q :q<CR>

" Search and replace (case sensitive, please!)
nnoremap <Leader>s :%s/\C//gc<left><left><left><left>
vnoremap <Leader>s :s/\C//gc<left><left><left><left>

" Edit `.vimrc`.
noremap <Leader>v :e ~/config-files/.vimrc<CR>

" Update configs.
noremap <Leader>e :!source ~/config-files/update_configs<CR>:source $MYVIMRC<CR>

" Strip trailing whitespace.
noremap <silent> <Leader>w :%s/\s\+$//<CR>:noh<CR>

" Change file permissions to be executable or not.
noremap <Leader>x :!chmod +x %<CR>
noremap <Leader>X :!chmod -x %<CR>

" List buffers.
noremap <Leader><Tab> :ls<CR>

" Next/previous buffer.
noremap <Leader>; :bn<CR>
noremap <Leader>, :bp<CR>
