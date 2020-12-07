noremap <M-d> <nop>
noremap <M-q> <nop>
inoremap <M-d> <nop>
inoremap <M-q> <nop>
noremap <M-s> :w<CR>
noremap <M-a> :qall<CR>
inoremap <M-s> :w<CR>
inoremap <M-a> :qall<CR>

" For now...
noremap D H
noremap R M
noremap W L
noremap m v
noremap M V
noremap L J
noremap k u
noremap K <C-r>
noremap b r
noremap B R
noremap <tab> .
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

" Words, paragraphs, home/end
noremap f b
noremap F B
noremap p w
noremap P W
noremap i e
noremap I E
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
" todo idea: Escape enters "scrolling mode" where cursor disappears,
"               n/e are screen mov't, s/o are full-page, a/i are first/last line,
"               and all other horiz mov't keys put you back in normal mode.
noremap h <C-u>
noremap t <C-d>
noremap H <C-u><C-u>
noremap T <C-d><C-d>
noremap N 5<C-e>
noremap E 5<C-y>

" Find
" todo: other details
noremap <space> f
