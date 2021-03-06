" Save and quit
noremap <M-s> :w<CR>
noremap <M-a> :qall<CR>
inoremap <M-s> <C-o>:w<CR>
inoremap <M-a> <C-o>:qall<CR>

" Start of line is first non-blank character.
noremap <Home> ^
inoremap <Home> <C-o>^

" Jump to definition / jump back.
noremap <C-d> <C-t>
noremap <C-f> <C-]>

" Select recently changed / pasted text.
nnoremap gp `[v`]

" Leader.
"noremap <CR> ???



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
