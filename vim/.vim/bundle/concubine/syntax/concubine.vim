" Vim syntax file
" Language: Concubine
" Maintainer: Niklas Koep
" Latest Revision 28 October 2013

if exists("b:current_syntax")
    finish
endif

command! -nargs=+ CnHL hi def link <args>

" Keywords
syn keyword cnKeywords pull fx as on if for and or
CnHL cnKeywords Conditional

" Operators
syn match cnOperator "+=\=\|-=\=\|*=\=\|/=\=\|%=\=\|\~/=\=\|<<=\="
syn match cnOperator ">>=\=\|[<>]=\=\|===\=\|\!==\=\|&=\=\|\^=\="
syn match cnOperator "|=\=\|||\|&&\|\[\]=\=\|=>\|!\|\~\|?\|::\|:=\|'"
CnHL cnOperator Operator

" Comments
syn keyword cnTodo contained TODO FIXME
syn match cnCommentLine "#.*$" contains=cnTodo
syn region cnCommentBlock start="#{" end="}#" contains=cnTodo
CnHL cnTodo Todo
CnHL cnCommentString cnCommentLine
CnHL cnCommentString cnCommentBlock
CnHL cnCommentLine Comment
CnHL cnCommentBlock Comment

" Integer literals
syn match cnInteger "\<[0-9]\+\>\|\<0[xX][0-9a-fA-F]\+\>\|\<0[oO][0-7]\+\>"
CnHL cnInteger Number

" Floating point literals
syn match cnFloat "\<[0-9]\+\.[0-9]\+\([eE][-+]\=[0-9]\+\)\=\>"
CnHL cnFloat Number

" String literals
syn region cnString start=+\z("\)+ end=+\z1+ contains=cnStringInterpolation
CnHL cnString String

" String interpolation
syn match cnStringInterpolation contained "\$\(\w\+\|{[^}]\+}\)"
CnHL cnStringInterpolation PreProc

" Storage class
syn keyword cnStorageClass imut
CnHL cnStorageClass StorageClass

" Structures
syn keyword cnType type
CnHL cnType Structure

" Primitive types
syn keyword cnType int float string bool list map tuple
CnHL cnType Type

let b:current_syntax = "concubine"

