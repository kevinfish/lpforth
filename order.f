\ vocabulary search order specification
\ save as order1.f before unifying word list 2/6/99
 
cr .( Loading vocabulary...)

marker orderf


: find-a-voc ( adr voc-addr -- adr false | nfa true)
        swap dup c@ tmp ! 1+ swap
        begin @ dup  ( adr+1 lfa adr+1 lfa )
        while 2dup link>n dup 1+ swap c@ $1F and dup tmp @ =
                if same? 0=
                        if nip link>n true exit
                        then
                else drop 2drop
                then
        repeat drop 1- false ;


: (find) ( adr -- adr false | nfa true )
        context
        begin 2dup @ dup
        while find-a-voc
                if >r 2drop r> true exit
                else drop
                then cell+
        repeat 2drop drop false ;

' (find) 'find !


16 constant #vocs

variable last-link              \ address of last link for last header created
\ variable voc-link               \ linked list of vocabularies, in kernel
\ 0 voc-link !

\ voc structure 
\ 1 cell            store last lfa of this vocubalary
\ 1 cell            voc link


forth \ old forth, variable, keep lfa of last word

vocabulary forth
@ ' forth 9 + !  \ replace the old forth with new one

forth definitions

vocabulary root

: also          ( -- )
                context dup cell+  #vocs 1- cells cmove>  ;

defer voc-also  ' noop is voc-also

: only          ( -- )
                context #vocs cells erase  root also voc-also ;

: previous      ( -- )
                context dup cell+ swap  #vocs cells cmove>
                context @ 0=
                if      root
                then    voc-also ;

: vcfa>voc   ( vocabulary-cfa -- voc-address ) \ voc-address keep last link
        1+ 2 cells+ ;

: voc>vcfa   ( voc-address -- vocabulary-cfa )
        2 cells- 1- ;
: forth-wordlist ( -- wid )
                ['] forth vcfa>voc ;

: get-current   ( -- wid )
                current @ ;

: set-current   ( wid -- )
                current ! ;

: get-order     ( -- widn .. wid1 n )
                depth >r
                0 #vocs 1-
                do      context i cells+ @
                        dup 0=
                        if      drop
                        then
            -1 +loop    depth r> - ;

: set-order     ( widn .. wid1 n -- )
                dup 0<
                if      drop only
                else    context #vocs cells erase
                        0
                        ?do     context i cells+ !
                        loop    voc-also
                then    ;

: +order        ( wid - )       \ add wid to search order
                >r get-order 1+ r> swap set-order ;

: swap-current  ( wid1 - wid2 ) \ change current to wid1, return old wid2
                get-current swap set-current ;

: voc:          ( wid 'word' - ) \ define 'word' in vocabulary wid
                swap-current >r : r> set-current ;
                
: ?cr ( n -- )
        drop ;
        
: order         ( -- )
                cr ." context: " context
                #vocs 0
                do      dup @ ?dup
                        if      voc>vcfa .name space 14 ?cr
                        then    cell+
                loop    drop
                cr ." current: " current @  voc>vcfa .name    ;

: vocs          ( -- )  
        cr voc-link
        begin @ ?dup
        while dup cell- voc>vcfa .name space 
        repeat ;

context @ context cell+ !

: (set-context) ( -- )
        forth context @ current !  ;

' (set-context) 'set-context ! \ new context setting

root definitions

: forth             forth ;
: forth-wordlist    forth-wordlist ;
: set-order         set-order ;
: also  also ;

only forth also definitions

