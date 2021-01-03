#SingleInstance force
#NoEnv ; Recommended for performance and compatibility with future AutoHotkey releases.
SendMode Input ; Recommended for new scripts due to its superior speed and reliability.
SetTitleMatchMode 3  ; Exact matching to avoid confusing T/B with Tab/Backspace.

; Right alt => Escape
RAlt::Escape

; Workman keyboard layout, with j and k flipped.

q::q
w::d
e::r
r::w
t::b
y::k
u::f
i::u
o::p
p::`;

a::a
s::s
d::h
f::t
g::g
h::y
j::n
k::e
l::o
`;::i
'::'

z::z
x::x
c::m
v::c
b::v
n::j
m::l
,::,
.::.
/::/

; Control keys stay qwerty (ctrl-z/x/c/v, etc)

^q::^q
^w::^w
^e::^e
^r::^r
^t::^t
^y::^y
^u::^u
^i::^i
^o::^o
^p::^p

^a::^a
^s::^s
^d::^d
^f::^f
^g::^g
^h::^h
^j::^j
^k::^k
^l::^l
^`;::^`;
^'::^'

^z::^z
^x::^x
^c::^c
^v::^v
^b::^b
^n::^n
^m::^m
^,::^,
^.::^.
^/::^/

; Same for alt:

!q::!q
!w::!w
!e::!e
!r::!r
!t::!t
!y::!y
!u::!u
!i::!i
!o::!o
!p::!p

!a::!a
!s::!s
!d::!d
!f::!f
!g::!g
!h::!h
!j::!j
!k::!k
!l::!l
!`;::!`;
!'::!'

!z::!z
!x::!x
!c::!c
!v::!v
!b::!b
!n::!n
!m::!m
!,::!,
!.::!.
!/::!/

; Same for win:

#q::#q
#w::#w
#e::#e
#r::#r
#t::#t
#y::#y
#u::#u
#i::#i
#o::#o
#p::#p

#a::#a
#s::#s
#d::#d
#f::#f
#g::#g
#h::#h
#j::#j
#k::#k
#l::#l
#`;::#`;
#'::#'

#z::#z
#x::#x
#c::#c
#v::#v
#b::#b
#n::#n
#m::#m
#,::#,
#.::#.
#/::#/
