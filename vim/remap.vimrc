" Save and quit
noremap <M-s> :w<CR>
noremap <M-a> :qall<CR>
inoremap <M-s> <C-o>:w<CR>
inoremap <M-a> <C-o>:qall<CR>

" Jump to definition / jump back.
noremap <C-d> <C-t>
noremap <C-f> <C-]>

" Select recently changed / pasted text.
nnoremap gp `[v`]

" Toggle comments.
"map <space>/ <plug>NERDCommenterComment
"map <space>? <plug>NERDCommenterUncomment


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

" Macros
noremap v q
noremap V @
noremap <Backspace> @@

" Cursor top/mid/bot
noremap D H
noremap R M
noremap W L

" These are just the same as before.
noremap <return> gg
noremap g g
noremap G G
noremap z z
noremap zn zb
noremap ze zt
noremap < <
noremap > >
noremap x x
noremap X X
noremap / /
noremap ? ?
noremap j n
noremap J N

" Start of line is first non-blank character.
noremap <Home> ^
inoremap <Home> <C-o>^

" Up/down by 1 line.
noremap <C-j> <C-e>
noremap <C-k> <C-y>
