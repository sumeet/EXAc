" Vim syntax file
" Language: exC

" Usage Instructions
" Put this file in .vim/syntax/exc.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.exc set filetype=exc

if exists("b:current_syntax")
  finish
endif

syntax keyword excTodos TODO XXX FIXME NOTE

" Language keywords
syntax keyword excKeywords feof fread fdrop fseek fcreate fcreate_then_wipe fwrite fvoid kill HALT link const spawn chready wait chtoggle fopen loop chignore hostid continue repeat
syntax keyword excLoopKeywords while if else

" Comments
syntax region excCommentLine start="//" end="$"   contains=excTodos
syntax region excDirective start="%" end=" "

syntax match excCommentIdent		"[#$][a-z_][a-z0-9_]*\>"
syntax match excIdent		"\<[a-z_][a-z0-9_]*\>"

" Numbers
syntax match excDecInt display "\<[0-9][0-9_]*"
"syntax match excHexInt display "\<0[xX][0-9a-fA-F][0-9_a-fA-F]*"
"syntax match excFloat  display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)"

" Functions
syntax match excFunction display "\<[a-z_][a-z0-9_]*\>("he=e-1

" TODO: we don't have string literalls... yet? or at all?
" Strings
" syntax region excString start=/\v"/ skip=/\v\\./ end=/\v"/
" syntax region excString start=/\v'/ skip=/\v\\./ end=/\v'/

" Set highlights
highlight default link excTodos Todo
highlight default link excKeywords Keyword
" comments are highlighted as strings in this lang
" TODO: only some strings treated as values highlighted this way
" highlight default link excCommentLine Comment
highlight default link excDirective PreProc
highlight default link excLoopKeywords Repeat
highlight default link excDecInt Number
"highlight default link excHexInt Number
"highlight default link excFloat Float
highlight default link excCommentLine Comment
highlight default link excIdent Identifier
highlight default link excCommentIdent Special
highlight default link excFunction Function

let b:current_syntax = "exc"
