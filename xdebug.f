\ $id: debug.f 1.1 1994/04/01 07:52:43 andrew exp $
\ 'ip' store the address that you are tracing
\ 'ip cell+' store the original cfa of the tracing address
\ good debuger now, big lesson: no higher level word in trace before restore
\ ip, save as xdebug1.f April 20th, 1998 - 19:57
\ debugger running well, with identation right, does>, variable, colon,
\ and code can be told. save as xdebug2.f April 21st, 1998 - 0:28
\ save as xdebug3.f before add verbal mode to (z") 10/23/98
\ save as xdedbug4.f before fixing brnext 12/31/98
\ save as xdebug5.f before change to no ncall version 2/7/99
\ fix (loop) problem Fri Mar 31 18:56:05 2000
\ save as xdebug6.f adding more features Mon Apr 3 02:12:04 2000
\ save as xdebug7.f for soft link Wed Apr 26 00:25:24 2000
\ save as xdebug8.f before adding name after dovalue

\ only forth definitions

cr .( Loading xdebugger. )

decimal

9 value var-diff \ the distance between cfa and variable value

\ : cell+ 4 + ;

: dd ." dd" .s key drop ." ee " ;

: +ov? ( n1 n2 -- f ) \ when change from positive to negative, it means overflow
        um+ drop      \ from 7fffffff (+) to 80000000 (-)
\ dup . 
	0 < ;

: .()  ( n -- )  ." (" 0 .r ." ) " ;


\ vocabulary bug          also bug also definitions


\ -------------------- variables --------------------

variable ip  0 ,        \ ip & contents of current breakpoint
variable ip0            \ ip at start of word
variable rtop           \ top of return stack, return op, in normarl condition
variable nesting        \ nesting level
variable watching       \ watch word

\ : tt ." ip@=" ip @ . ." |" .rs ;

: patch  ( cfa -- )
        ip @ @ ip cell+ !       \ save old word
        ip @ ! ;                \ patch in trace word


: call-dest ( adr -- adr' )
        1+ dup cell+ swap @ + ;

\ -------------------- advance ip --------------------

: colon?        ( cfa -- f ) call-dest ['] dolist = ;

: code?         ( cfa -- f ) c@ $E8 <> ;

\ : does>?         ( cfa -- f ) call-dest cell- @ ['] (;code) = ;

: does>?         ( cfa -- f ) dup colon? not swap c@ $E8 = and ;

: variable?     ( cfa -- f ) 1+ cell+ @ ['] dovar = ;

: ?jump  ( ip f -- ip' ) if  cell+ @ else  2 cells +  then ;

: <string0>  ( ip -- ip' )   cell+ count + 1+ ;

: <string>  ( ip -- ip' )   cell+ count + ;

: <exit>  ( ip -- ip' )
        drop nesting @ 0>
        if      rtop @ ( unnest )
                -1 nesting +!
        else    ip0 @   ( done, reset ip for next time )
                nesting off
        then ;

: dnext  ( -- )  \ 'ip @' point to the word is debuging
                \ advance 'ip @' to next word
   ip @   dup @
   case
     ['] >r       of 1 nesting +! cell+                 endof
     ['] r>       of -1 nesting +! cell+                endof
     ['] dovar    of  drop rtop @  -1 nesting +!        endof
     ['] dolit    of  2 cells +                         endof
     ['] dovalue  of  2 cells +                         endof
     ['] compile  of  2 cells +                         endof
\     ['] (ncall)  of  2 cells +         		endof
     ['] branch   of  true ?jump                        endof
     ['] ?branch  of  over 0= ?jump                     endof
     ['] brnext   of  -1 rtop @ + 0<> ?jump          	endof
     ['] (do)     of  2 cells +                         endof
     ['] (?do)    of  over 3 pick = ?jump               endof
     ['] (loop)   of  1 rtop @ +ov? not ?jump           endof
     ['] (+loop)  of  over rtop @ +ov? not ?jump        endof
     ['] _of      of  over 3 pick <> ?jump              endof
     ['] _endof   of  true ?jump                        endof
     ['] (")      of  <string0>                         endof
     ['] (.")     of  <string0>                         endof
     ['] (z")     of  <string0>                         endof
     ['] (abort") of  <string>                          endof
     ['] ."|      of  <string>                          endof
     ['] exit     of  <exit>                            endof
     swap cell+ swap
   endcase
   ip ! ;

\ dovar is a special case

\ -------------------- trace commands --------------------
 
-1 value nextbreak 
 0 value stack-top 
 0 value return-top 
 
defer debug-entry       ' noop is debug-entry   \ application init stuff 
defer debug-exit        ' noop is debug-exit    \ application un-init stuff 
 
create tib-save         MAXSTRING allot 
create pocket-save      MAXSTRING allot 
create here-save        MAXSTRING allot 
create watch-buf        MAXSTRING allot 
       watch-buf OFF                            \ empty to start 
 
: evaluate ( adr len -- )
	dup #tib !
	tib swap cmove
	>in off
	interp ;

: perform-watch ( -- ) 
                state @ >r state off 
                watch-buf count evaluate 
                r> state ! ; 
 

: do-watch      ( -- ) 
                watch-buf c@ 0= if exit then
                cr ." Watch-[" watch-buf count type ." ]: " 
                ['] perform-watch catch drop ; 

: dbg-watch     ( -- ) 
        cr ." Enter a line to interpret after each instruction step is performed:" 
        cr watch-buf 1+ MAXCOUNTED accept watch-buf c! drop ; 

: beep bell emit ;

: to-nextbreak ( -- )
	nextbreak ip ! 
        -1 to nextbreak ;

: dbg-jump      ( -- )          \ set breakpoint beyond following branch word 
                ip @ @ 
                case    ['] branch   of TRUE    endof 
                      \  ['] _repeat  of TRUE    endof 
                        ['] ?branch  of TRUE    endof 
                      \  ['] _until   of TRUE    endof 
                      \  ['] _while   of TRUE    endof 
                        ['] (loop)   of TRUE    endof 
                        ['] (+loop)  of TRUE    endof 
                      \  ['] _of      of TRUE    endof 
                      \  ['] _EXIT    of TRUE    endof 
                                        FALSE swap 
                endcase  
                if      ip @ 2 cells+ to nextbreak 
                        nesting off 
                      \  dnext 
			to-nextbreak
               else    beep 
                then    ; 
 

: run-forth
        begin   cr ." forth>  " query #tib @
        while   interpret
        repeat ;

: dbgnest ( -- )
        ip @ @ dup colon?
        if >body ip !
                1 nesting +!
        else    dup code?
                if  drop ." Can't nest code"
                else dup does>?
                        if ." does> nesting " call-dest >body ip !
                                1 nesting +!
                        else drop ." What is this?"
                        then
                then
        then ;

: dbgunnest ( -- )        \ not valid inside a loop or if >r has been used!
        rtop @ ip !
        -1 nesting +! ;

: help  cr
." c-continue,  d-done,  n-nest,  u-unnest,  l-locate,  f-forth,  j-jump, w-watch, q-quit " ;

: locate  ( where am i? )
        ip @ dup
        begin   dup body> colon?
                if  dup body> >name .id ( .name ) - cell / .()  exit then
                cell -
                over 50 cells - over =
        until
        2drop ." don't know! " ;

: ident ."        " ;

: .wordtype     ( -- ) \ checking order is important
        nesting @ 0 ?do ident loop
        ip @ @
        dup does>?
        if      drop ." (does) " exit
        then
        dup variable?
        if      drop ." ( var) " exit
        then
        dup colon?
        if      drop ." (   :) " exit
        then
        dup code?
        if      drop ." (code) " exit
        then
        drop 5 spaces ;

\ -------------------- trace breakpoint --------------------

: seperator ( -- adr ) 
	." : "  ip @ cell+ ;

: .proc ( adr -- )
	2 cells+ count type ;
	
: look-ahead ( cfa -- )
   ip @ @
   case
     ['] dovar    of  seperator @ . 		endof
     ['] dolit    of  seperator @ .            endof
     ['] compile  of  seperator @ .name          endof
 \    ['] (ncall)  of  seperator @ .proc      endof
     ['] branch   of  seperator @ @ .name       endof
     ['] ?branch  of  seperator @ @ .name              endof
     ['] brnext   of  seperator @ @ .name           endof
     ['] (do)     of  seperator @ @ .name                  endof
     ['] (?do)    of  seperator @ @ .name          endof
     ['] (loop)   of  seperator @ @ .name         endof
     ['] (+loop)  of  seperator @ @ .name      endof
     ['] _of      of  seperator @ @ .name     endof
     ['] _endof   of  seperator @ @ .name        endof
     ['] dovalue  of  seperator @ var-diff - .name         endof
     ['] (")      of  seperator count type      endof
     ['] (.")     of  seperator count type       endof
     ['] (z")     of  seperator count type     endof
     ['] (abort") of  seperator count type         endof
     ['] ."|      of  seperator count type       endof
\     ['] exit     of  <exit>                            endof
   endcase ;
	
: trace  ( -- )
        r> -4 um+ drop dup >r  \ run trace then run real word
        [ ' ip var-diff + ] literal
        dup 4 um+ drop @ swap @ !
                 \ any colon words used before this line cannot be debugged
        1 nr@ rtop !
        ip @ <> abort" trace error"
\        ip 2@ !  ( restore )
        .s
\        watching @ ?dup if  5 spaces execute then cr
	do-watch cr
        .wordtype
        ip @ @ .name look-ahead
        ."   -->  " key upc
        case
          ascii C of  ip0 @ ip ! nesting off    endof
          ascii D of  ip off  exit              endof
          ascii N of  dbgnest                   endof
          ascii U of  dbgunnest                 endof
          ascii F of  run-forth                 endof
          ascii L of  locate                    endof
          ascii Q of  ip off  ." unbug" abort   endof
          ascii ? of  help                      endof
	  ascii W of  dbg-watch			endof
	  ascii J of  dbg-jump			endof
          ascii P of  ip0 @ ip ! nesting off            endof 
\          ascii R of  ip @ to nextbreak nesting off ." R" 
\                     dnext to-nextbreak           endof 

          >r dnext ( default )
\                      nextbreak -1 <> 
\                      if        nextbreak ip ! 
\                                -1 to nextbreak 
\				." Enter to advance!"
\                      then 
 	  r>
        endcase
        [ last @ name> ]
        literal
        patch ;        \ replace next word with trace

\ last keep lfa
\ -------------------- initialize debugger --------------------

\ forth definitions

: watch   ' watching ! ;
: nowatch   watching off ;

: unbug  ( -- )
        ip @ if  ip 2@ !  ip off  then ;



: debug ( -<name>- )    \ set a breakpoint
        unbug
        '
        dup  colon?
        over does>? or 0= abort" must be a : or does> definition"
        dup  colon?
        if      >body
        else    ." does> nesting "
                call-dest >body
        then    dup ip0 ! ip !
        ['] trace patch
        nesting off ;


: dbg   ( -<name>- )    \ debug a word now
        >in @ debug >in ! ;

\ only forth also definitions

: tt 100 0 do i . loop 10 . ;


