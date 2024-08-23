" -----------------------------------------------------------------------------
" Init
" -----------------------------------------------------------------------------

" Enable syntax highlighting.
syntax on

" Automatically detect known filetypes.
filetype plugin indent on

" -----------------------------------------------------------------------------
" Settings
" -----------------------------------------------------------------------------

" Make backspace behave as expected in insert mode.
set backspace=indent,eol,start

" Allow hiding of buffers with unwritten changes.
set hidden

" Show commandline completion matches above the commandline on <Tab> keypress.
set wildmenu

" Write swap files to a particular directory, if it exists.
set directory=~/.vim/swap//,.

" Persistent undo. Only happens if the specified directory exists.
set undofile
set undodir=~/.vim/undo//

" Don't flash the screen (or beep) in Windows
set t_vb=

" Enable mouse support.
"
" I don't actually want mouse support (besides the scroll wheel), but this
" option seems like the easiest way to make the scroll wheel work both in tmux
" and vim.
set mouse=a

" Reload file from disk if it changes (as long as there's no edit conflict).
" https://unix.stackexchange.com/a/383044
set autoread
autocmd FocusGained,BufEnter,CursorHold *
    \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' |
    \ checktime |
    \ endif
autocmd FileChangedShellPost *
    \ echohl WarningMsg |
    \ echo "File changed on disk. Buffer reloaded." |
    \ echohl None



" No startup message.
set shortmess+=I

" Don't show an indicator in the echo area when you're in insert mode.
" (The cursor shape already indicates that in Neovim.)
set noshowmode

" Don't give visual feedback for normal mode commands requiring multiple
" keypresses.
set noshowcmd

" Don't show a how-to-quit message when you press <C-c> in normal mode. This
" would be mapped to <Nop> except that I want to be able to discard numeric
" arguments with <C-c>. E.g. if I type 93<C-c>83gg I'll be taken to line 83 and
" not line 9383.
nnoremap <C-c> <Esc>

" Don't show the tildes after the last line in the file.
" Doesn't work on Vim 8.0 for me; I haven't looked into why.
if has('nvim')
    " Note the trailing space.
    set fillchars+=eob:\ 
endif



" No line numbers.
set nonumber

" No signs in the gutter.
set signcolumn=no

" Wrap long lines by default.
set wrap

" When 'wrap' is enabled, break lines at word boundaries.
set linebreak

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

" Show number of matches (and current position therein) when searching.
" Available starting in Vim 8.1.1270 and Nvim 0.4.0
set shortmess-=S



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

if &background ==# 'dark'
    " Hide the vertical bar between splits.
    hi! VertSplit ctermfg=8 ctermbg=8

    " Change status line color.
    hi! StatusLine ctermbg=8 ctermfg=11 cterm=reverse
    hi! StatusLineNC ctermbg=8 ctermfg=11 cterm=reverse

    " This last highlight group is made-up by me.
    hi! StatusLineTrailing ctermbg=8 ctermfg=12
else
    " Same, but for light background.
    hi! VertSplit ctermfg=15 ctermbg=15
    hi! StatusLine ctermbg=15 ctermfg=12 cterm=reverse
    hi! StatusLineNC ctermbg=15 ctermfg=12 cterm=reverse
    hi! StatusLineTrailing ctermbg=15 ctermfg=12
endif

" Don't show status line (filename, etc) when there's only one window. There's
" a strange nvim bug where unless I do this on load it resets to the original
" value of 2 some time after loading my vimrc.
autocmd VimEnter * set laststatus=1

" Status line shows current file path and whether the file was modified.
function! StatusLine()
    if expand("%:t") !=# ""
        return ' %f %#StatusLineTrailing#%( %h%w%r%m%)' . GitStatus()
    else
        return '%#StatusLineTrailing#%=%(%h%w%r%m %)'
    endif
endfunction
set statusline=%!StatusLine()




" Show certain info at the bottom-right of the screen. This only appears when
" the statusline is hidden.
set ruler

" Show the current filename, and whether the current buffer has been modified.
" 36 (total width) = 32 (filename) + 1 (space) + 3 (modified)
set rulerformat=%36(%=%{PercentT()}%3(%m%)%)

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
    " from the %{ } block.
    if strlen(filename) > 0
        let filename = filename . " "
    endif

    return filename
endfunction



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

    " No line numbers in terminals.
    autocmd TermOpen * setlocal nonumber

    " Start terminals in insert mode.
    autocmd TermOpen * startinsert
endif



" Auto-close brackets/braces/parens spanning multiple lines. For some (unknown)
" reason this mapping doesn't seem to take effect unless I do it on load.
autocmd VimEnter * inoremap <silent><expr> <CR> <SID>enter_fn()
function! s:enter_fn() abort
    let line = getline('.')
    let cursor = col('.') - 1

    " If the cursor is between two matching brackets, pressing enter causes the
    " closing bracket to go to a new line of its own.
    if 1 <= cursor && cursor < len(line)
        let open = line[cursor - 1]
        let close = line[cursor]

        if open ==# '(' && close ==# ')' ||
        \ open ==# '[' && close ==# ']' ||
        \ open ==# '{' && close ==# '}'
            return "\<CR>\<Esc>O"
        endif
    endif

    " If the cursor is immediately after an opening bracket, automatically
    " close that bracket on its own line.
    if cursor >= 1
        let open = line[cursor - 1]

        let close = ''
        if open ==# '('
            let close = ')'
        elseif open ==# '['
            let close = ']'
        elseif open ==# '{'
            let close = '}'
        endif

        if close !=# ''
            if cursor ==# len(line)
                return "\<CR>" . close . "\<Esc>O"
            else
                " If there's text after the cursor, make sure it ends up on the
                " line between the two matching brackets.
                return "\<CR>\<Esc>o" . close . "\<Esc>kI"
            endif
        endif
    endif

    return "\<CR>"
endfunction

" -----------------------------------------------------------------------------
" Mappings
" -----------------------------------------------------------------------------

if has('nvim')
    " Save.
    noremap <M-w> :w<CR>
    inoremap <M-w> <C-o>:w<CR>

    " Quit.
    noremap <M-q> :qa<CR>
    inoremap <M-q> <C-o>:qa<CR>

    " Execute last command.
    noremap <M-;> :<Up><CR>
else
    noremap <Esc>w :w<CR>
    inoremap <Esc>w <C-o>:w<CR>
    noremap <Esc>q :qa<CR>
    inoremap <Esc>q <C-o>:qa<CR>
    noremap <Esc>; :<Up><CR>
endif

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



" Use tab/shift-tab for completion. Happens only if the cursor is at the end of
" a word, or if the popup menu is already active; otherwise tab is just
" indentation.
inoremap <silent><expr> <Tab> <SID>tab_fn("\<C-n>")
inoremap <silent><expr> <S-Tab> <SID>tab_fn("\<C-p>")

function! s:tab_fn(next_prev) abort
    if s:check_word_behind() || pumvisible()
        if !pumvisible() && &omnifunc != ''
            " Trigger omnicompletion.
            return "\<C-x>\<C-o>"
        else
            " Next/prev suggestion.
            return a:next_prev
        endif
    else
        " Indent.
        return "\<Tab>"
    endif
endfunction

" Check if the character immediately to the left of the cursor is a
" "word-character", ie [0-9A-Za-z_]. False if at the beginning of the line.
"
" We'll also allow `.` and `::` to count as part of a word, so that we can
" auto-complete methods like "a.b()", and paths of the form "a::b::c()".
function! s:check_word_behind() abort
    let col = col('.') - 1
    let line = getline('.')
    return col >= 1 && (line[col-1] =~# '\w' || line[col-1] ==# '.') ||
        \ col >= 2 && line[col-1] ==# ':' && line[col-2] ==# ':'
endfunction



" Hide status line, line numbers, etc.
noremap <expr> <Leader>' ToggleDecorations()

" Non script-local function, so we can do:
" (bash)$ vim -c "call ToggleDecorations()"
function! ToggleDecorations() abort
    if &number
        " Hide decorations.
        set laststatus=1
        set nonumber
        set signcolumn=no
    else
        " Show decorations.
        set laststatus=2
        set number
        set signcolumn=auto
    endif

    " Need to redraw here to get the effect right away.
    mode

    return ""
endfunction

" Toggle line numbers.
noremap <expr> <F4> <SID>toggle_number()
inoremap <expr> <F4> <SID>toggle_number()

function! s:toggle_number() abort
    if &number
        set nonumber
        set signcolumn=no
    else
        set number
        set signcolumn=auto
    endif

    " Need to redraw here to get the effect right away.
    mode

    return ""
endfunction

" Toggle line wrapping.
noremap <F5> :set wrap! wrap?<CR>
inoremap <F5> <C-o>:set wrap! wrap?<CR>

" Toggle colorcolumn.
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

" Re-select recently-selected text.
nnoremap <Leader>v gv

" Copy/cut/paste to/from system clipboard.
noremap <Leader>y "+y
noremap <Leader>Y "+Y
noremap <Leader>d "+d
noremap <Leader>D "+D
noremap <Leader>p "+p
noremap <Leader>P "+P

" Paste from 0-register (yanked text only).
noremap <Leader>l "0p
noremap <Leader>L "0P

" Select recently changed text; i.e. recently pasted / inserted / deleted, etc.
" This gets messed-up by ":w" though, since `[ and `] reset to the whole file.
nnoremap <Leader>v `[v`]
nnoremap gp <Nop>

" :noh
noremap <Leader>j :nohlsearch<CR>:echo<CR>
noremap <Leader>k :set hlsearch! hlsearch?<CR>

" Close current window.
noremap <Leader>q :q<CR>

" Search and replace (case sensitive, please!)
nnoremap <Leader>s :%s/\C//gc<left><left><left><left>
vnoremap <Leader>s :s/\C//gc<left><left><left><left>

" Update configs.
noremap <Leader>e :!source ~/.config-files/update_configs<CR>:source $MYVIMRC<CR>

" Edit `.vimrc`.
noremap <Leader>E :e ~/.config-files/.vimrc<CR>

" Invert colors.
noremap <silent> <Leader>z :!toggle-colors<CR>:source $MYVIMRC<CR>:<CR>

" List buffers.
noremap <Leader><Tab> :ls<CR>



" Include work-specific configs.
if !empty(glob('~/notes/work/vimrc'))
    source $HOME/notes/work/vimrc
endif

" Alternate layout for the main editing keys.
source $HOME/.config/nvim/mappings.vim

" Make ctrl+arrows do what you expect: move by words or paragraphs.
noremap <C-Left> b
noremap <C-Right> w
noremap <C-Up> {
noremap <C-Down> }
inoremap <C-Left> <C-o>b
inoremap <C-Right> <C-o>w
inoremap <C-Up> <C-o>{
inoremap <C-Down> <C-o>}

" Make ctrl+backspace and ctrl+delete work as expected.
inoremap <C-h> <C-\><C-o>db
inoremap <C-del> <C-\><C-o>de

" Strip trailing whitespace.
noremap <silent> <leader>s :%s/\s\+$//<CR>:noh<CR>

" Change file permissions to be executable or not.
noremap <leader>x :!chmod +x %<CR>
noremap <leader>X :!chmod -x %<CR>

" Comment / uncomment -- these cause issues, e.g. "." gets remapped after. :/
"map <leader>/ <plug>NERDCommenterComment
"map <leader>? <plug>NERDCommenterUncomment

" Update configs.
noremap <leader>u :!source ~/.config-files/update_configs<CR>:source $MYVIMRC<CR>

" Edit `.vimrc`.
noremap <leader>U :e ~/.config-files/.vimrc<CR>

" :noh
noremap <leader>n :nohlsearch<CR>:echo<CR>
noremap <leader>e :set hlsearch! hlsearch?<CR>

" Copy/cut/paste to/from system clipboard.
noremap <leader>d "+y
noremap <leader>r "+d
noremap <leader>w "+c
noremap <leader>c "+p
noremap <leader>C "+P

" Next/previous buffer.
noremap - :bn<CR>
noremap = :bp<CR>

" Open file.
nnoremap <leader>o :Files<CR>

" Make this keybind work in rust files.
augroup Racer
    autocmd!
    autocmd FileType rust nmap <buffer> <C-f> <Plug>(rust-def)
augroup END
