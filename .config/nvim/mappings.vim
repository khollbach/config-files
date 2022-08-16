" Save and quit
noremap <M-s> :w<CR>
noremap <M-a> :qall<CR>
inoremap <M-s> <C-o>:w<CR>
inoremap <M-a> <C-o>:qall<CR>

" 123<CR> takes you to line 123.
noremap <CR> gg

" Start of line is first non-blank character.
noremap <Home> ^
inoremap <Home> <C-o>^

" Jump to definition / jump back.
noremap <C-d> <C-t>
noremap <C-f> <C-]>

" Select recently changed / pasted text.
nnoremap gp `[v`]



" hjkl
noremap l h
noremap n j
noremap e k
noremap u l

" Insert
noremap s i
noremap S I
noremap o a
noremap O A
noremap a o
noremap A O

" Words
noremap f b
noremap F B
noremap p w
noremap P W
noremap i e
noremap I E

" Paragraphs, home/end
noremap , {
noremap . }
noremap q ^
noremap Q 0
noremap ; $
noremap : :

" Copy/paste
noremap d y
noremap r d
noremap w c
noremap c p
noremap C P

" Scrolling
noremap h <C-u>
noremap t <C-d>
noremap H <C-u><C-u>
noremap T <C-d><C-d>
noremap N 5<C-e>
noremap E 5<C-y>

" Undo, replace, join, repeat
noremap k u
noremap K <C-r>
noremap b r
noremap B R
noremap L J
noremap <tab> .

" Visual
noremap m V
noremap M v
noremap <C-c> <C-v>

" Macros, marks
noremap v q
noremap V @
noremap <Backspace> @@
noremap y m

" Cursor top/mid/bot
noremap D H
noremap R M
noremap W L

" Find
noremap <space> f
noremap <s-space> F
noremap <del> t
noremap <s-del> T
noremap <bslash> ;
noremap <bar> ,
" Workaround for a bug in IdeaVim.
inoremap <s-space> <space>

" Next / prev search.
noremap j n
noremap J N

" Cursor to screen bottom / top.
noremap zn zb
noremap ze zt

" Switch between splits.
"
" Note that my tmux keybindings shadow these ones, so typically these
" keypresses will result in changing tmux panes instead.
"
" Note: it turns out that this doesn't work in VSCodeVim, since they don't
" support mappings using Alt. I fixed this by putting these keybindings
" directly in the vscode keybindings.json settings file. See this link for more
" details:
" https://github.com/VSCodeVim/Vim#remapping-more-complex-key-combinations
noremap <A-J> <C-w>h
noremap <A-K> <C-w>j
noremap <A-I> <C-w>k
noremap <A-L> <C-w>l
inoremap <A-J> <C-o><C-w>h
inoremap <A-K> <C-o><C-w>j
inoremap <A-I> <C-o><C-w>k
inoremap <A-L> <C-o><C-w>l
