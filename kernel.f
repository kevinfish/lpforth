1 loadfile !

hex

: immediate
        80 last @ @ or
        last @ ! ;

decimal

: (
        41 parse 2drop ;  immediate


: \  ( -- )
        #tib @ >in ! ;  immediate


\ start to use \ for comment
\ include file for pforth
\ save as ind1.seq function pretty well  08/23/97 19:03
\ working version, save as ind2.seq
\ words can work on substring  08/28/97, save as ind3.seq
\ do loop, ?do loop, and do +loop are now working  09/17/97 08:59
\ change same? to 'same? deferred word so I can do caps insensitive version
\  09/17/97 09:06

\ modify it for win32 version of pforth, save it as pind.f
\ this file is just to make it to full eforth, save as pind1.f
\ April 13th, 1998 - 22:46
\ do loop leave are done the same way fpc does
\ save as pind2.f April 17th, 1998 - 21:15
\ save as pind8.f before emerge and automation
\ save as kernel9.f before change >name
\ save as kernel10.f before change >name to find all vocs
\ save as kernel11.f before change value, to, +to, change it back later

: .(  ( -- )
        41 parse type ;   immediate


cr .( Loading words to make system complete. )

: ? ( adr -- )
        @ . ;


: $,"  ( -- )
        34 word
        count + aligned
        cp ! ;


: recurse ( -- )
        last @ name> , ;    immediate

: 1- 1 - ;
: 1+ 1 + ;

: 4+ 4 + ;

\ flow control

: for ( -- adr ) \ leave the address on stack for next to resolve
        compile 1- compile >r here ;   immediate

: next ( n -- )
        compile brnext , ;    immediate

: begin ( -- n ) \ leave the adr on stack for until and again to resolve
        here ;   immediate


: until ( n -- )
        compile ?branch  , ;   immediate

: again ( n -- )
        compile branch , ;   immediate

: if ( -- adr )
        compile ?branch here
        0 , ;    immediate

: ahead ( -- n )
        compile branch here
        0 , ;    immediate

: repeat ( adr -- )
        [compile] again here swap ! ;    immediate

: then ( adr -- )
        here swap ! ;     immediate

: else ( adr -- adr' )
        [compile] ahead swap [compile] then ; immediate

: while ( adr -- adr' )
        [compile] if swap ;   immediate


\ string

: $" ( -- \ <string> )
        compile $"| $," ;      immediate

: ." ( -- \ <string> )
        compile ."| $," ;    immediate

: ['] ' [compile] literal ; immediate

\ create does>

: create ( -- \ <string> )
        token $,n overt
        ['] dolist call,
        ['] dovar , ;


: (;code)  ( -- ) \ change dovar to dolist, real execution code at r
        last @ name> 1+ \ address for offset to dolist
        r> over cell+ - swap !  \ store the offset to codes after (;code)
        ;

\ (;code) call-dolist
: does> ( -- )
        compile (;code) ['] dolist call,
        compile cell+ ;  immediate   \ because of dovar, I need to add a cell


: variable create 0 , ;

: constant create , does> @ ;

: abort" ( -- \ <string> )
        compile (abort") $," ;  immediate


4 constant cell
0 constant false
-1 constant true

: nr@ ( n -- n' )  \ take nth value from return stack
        1+ cell * rp@ + @ ;

: nr! ( n1 n2 -- )
        1+ cell * rp@ + ! ;

\  structure: (loop) addr-if-loop
\ return stack: high-limit current-count next-addr
\               2 nr@      1 nr@         r@
: sign-change? ( n1 n2 -- f )
        0< swap 0< xor ;

: (loop)  \ run time code for loop
        1 nr@ dup 1+ dup 1 nr! sign-change?   \ compare current with high limit
        if r> cell+ r> drop r> drop r> drop >r
        else r> @ >r
        then ;

: (+loop) ( n -- )
        1 nr@ swap over + dup 1 nr! sign-change?
        if r> cell+ r> drop r> drop r> drop >r
        else r> @ >r
        then ;



variable ?do-f

: ?condition    ( f -- )
                not abort" conditionals wrong"   ;

: >mark         ( -- addr ) \ forward jump
                here 0 , ;

: >resolve      ( addr -- ) \ fulfil forward jump
                here swap ! ;

: <mark         ( -- addr )
                here ;

: <resolve      ( addr -- )
                , ;

: ?>mark        ( -- f addr )
                true >mark   ;

: ?>resolve     ( f addr -- )
                swap  ?condition >resolve ;

: ?<mark        ( -- f addr )
                true <mark   ;

: ?<resolve     ( f addr -- )
                swap ?condition <resolve  ;

\ do loop structure
\ return stack: (do)+cell h+80000000 l-(h+80000000)
\ (do) adr-(loop)+2cells ... (leave) ...... (loop) adr-(do)+2cells
hex

: (do) ( h l -- )
        r@ cell+ -rot ( next+cell h l )
        swap 80000000 + dup >r  ( next+cell l h+80000000)
        - >r
        >r ;

: do    ( -- f addr )
        compile (do) ?>mark ; immediate


: (?do) ( h l -- )
        2dup =
        if 2drop r> @ >r
        else  r@ cell+ -rot ( next+cell h l )
                swap 80000000 + dup >r  ( next+cell l h+80000000)
                - >r
                >r
        then ;

: ?do ( --  )
        compile (?do) ?>mark ; immediate

: (leave) ( -- )
        r> drop r> drop r> drop r> @ >r ;

: (?leave) ( f -- )
        if r> drop r> drop r> drop r> @ >r
        then ;

: leave ( -- )
        compile (leave) ; immediate

: ?leave ( -- )
        compile (?leave) ; immediate

: loop ( -- )
        compile (loop) 2dup cell+ ?<resolve ?>resolve ; immediate

: +loop
        compile (+loop) 2dup cell+ ?<resolve ?>resolve ; immediate

: i ( -- n )
        1 nr@ 2 nr@ + ;

: j ( -- n )
        4 nr@ 5 nr@ + ;

: k ( -- n )
        7 nr@ 8 nr@ + ;

decimal

: _type ( adr len -- )
        for dup c@ >char emit 1 +
        next
        drop ;



\ see tool

: .s ( -- )
        ." [" depth . ." ]"
        depth ?dup
        if      for r@ pick .
                next
        then ."  <sp" ;

: !csp ( -- )
        sp@ csp !   ;

: ?csp ( -- )
        sp@ csp @ xor abort" stacks" ;

: (>name) ( cfa voc -- nfa f )
        @
        begin ?dup
        while 2dup cell+ name> =
                if cell+ swap drop true exit
                then @
        repeat
        0 swap drop false ;

variable voc-link 

: >name ( cfa -- nfa | 0 ) \ search through all vocabularies
        voc-link ( cfa adr )
        begin @ ?dup 
        while over over cell- (>name) 
                if >r drop drop r> exit
                else drop
                then
        repeat drop 0 ;


variable word-count

: .id ( nfa -- )
        ?dup
        if count 31 and _type 
                word-count @ 1+ word-count !
        else ." {noName}"
        then ;

: .adr ( adr -- )
        ." [" u. ." ]" 58 emit ;

: asee  ( adr -- )
        cr
        dup .adr space dup c@ . space  ( adr )
        dup 1+ dup cell+ swap @ + dup u. >name .id
        1+
        begin cell+ dup space .adr dup @ >name dup
                if .id
                else drop dup @ u.
                then nuf?
        until drop ;

: see  ( -- \ <word> )
        ' cr asee ;

\ words utility

create wpad 33 allot

: ?.id ( nfa -- )
        wpad c@ 0 =
        if .id space
        else dup c@ 31 and wpad c@ dup 0 =
            if .id
            else - dup 0 < not \ nfa n f
                if 0 swap 1+ \ nfa 0 n
                        for over 1+ r@ + wpad count 'same? @execute 0 = or \ nfa 
                        next
                        if .id space
                        else drop
                        then
                else drop drop
                then
            then
        then ;

: words ( -- )
        bl word count dup wpad c! wpad 1+ swap cmove
        cr context @
        begin @ dup
                if dup cell+ ?.id
                else drop exit
                then nuf?
        until  drop ;

\ common kernel words

: between ( n1 n2 n3 -- f ) \ n2 <= n1 <= n3
        >r over > not
        swap r> > not and ;

: ascii ( -- n )
        bl word 1+ c@
        state @
        if [compile] literal
        then ; immediate

: close-all 10 0 do i fclose . loop ;

: place ( from cnt to -- )
        >r
        dup r@ c!
        r> 1+ swap cmove ;

: +place ( from cnt to -- )
        dup c@ over 1+ + >r  ( f c t ) \ new start position
        >r
        dup r@ c@ + r> c! ( f c ) \ store new length
        r> swap cmove ;

hex
8050000 constant base-addr
decimal
create fpad 32 allot
create "wb" ascii w c, ascii b c, 0 c,
0 variable file-h

: simage ( -- )
        pad 80 0 fill
        bl word count pad place
        "wb" pad 1+ 11 xcall file-h ! drop drop
        file-h @ here base-addr - 1 base-addr 14 xcall . drop drop drop drop
        file-h @ 12 xcall . drop ;

\ v12 {
\ file link format
\       cell    linked list of file
\       cell    file number
\       counted name (counted string)

: link,  ( addr -- ) here over @ , swap ! ;

variable file-link
0 file-link !

file-link link,
0 ,
$," lpforth.f"

: .files ( -- )
                cr
                file-link
                begin   @ dup
                        nuf? 0 = and
                while   dup 2 cell * + count type space
                        ." [" dup cell + @ . ." ]" space
                repeat  drop cr ;

: trim-files    ( a1 -- )    \ remove any procs above forget address
                @ \ back one more word, so if file is right in front of words
                  \ to be forget, trim it down, too
                >r file-link 
                begin @ dup r@ <
                until r> drop
                file-link ! ;


\ v12 }





\ kernel1.seq

\ create t1 100 allot
\ create t2 100 allot
\ save as kernel8.f before automation

cr .( Loading kernel words. )

1 loadfile !
file-link link,
loadfile @ ,
$," kernel.f"

260 constant maxbuffer      \ size of any string buffer, must match the
                            \ size of a windows maximum path string,
                            \ which is 260 bytes. ** don't change this **

maxbuffer constant maxstring    \ maximum length of a counted string
maxbuffer constant max-path     \ maximum length of a filename buffer
      255 constant maxcounted   \ maximum length of contents of a counted string

create pocket           maxstring allot
create cur-file         maxstring allot
create temp$            maxstring allot


decimal

: cmove> ( from to count -- )
        >r
        over - ( from to diff ) \ difference between from and to
        r> swap >r
        over + 1-     ( from from+count-1 )
        r> -rot
        ?do i c@ over i + c! -1 
        +loop
        drop ;

: move cmove ;

: pluck ( n1 n2 n3 -- n1 n2 n3 n1 )
        2 pick ;

: tuck ( n1 n2 -- n2 n1 n2 )
        swap over ;

: nip ( n1 n2 -- n2 )
        swap drop ;

hex

: flip ( n -- n' )
        dup FFFF and 10000 *
        swap FFFF0000 and 10000 /
        + ;

: split ( n -- l h )
        dup FFFF and
        swap FFFF0000 and ;

: join ( l h -- n )
        10000 * + ;

decimal

: ?drop ( n false -- false | n true -- n true )
        dup >r
        if drop
        then r> ;

: r>drop ( -- )
        r> r> drop >r ;

: dup>r ( n -- n )
        r> over >r >r ;

: 2r> ( -- n1 n2 )
        r> r> r> rot >r ;

: 2>r ( n1 n2 -- )
        r> -rot >r >r >r ;

: 2r@ ( -- n1 n2 )
        2r> 2dup 2>r ;

: 2** ( n -- 2^n )
        1 swap 0
        ?do 2 *
        loop ;

: cset ( b addr -- )
        >r 2** r@ @ or
        r> ! ;

: creset ( b addr -- )
        >r 2** not r@ @ and
        r> ! ;

: ctoggle ( b addr -- )
        >r 2** r@ @ xor
        r> ! ;

: on ( addr -- )
        true swap ! ;

: off ( addr -- )
        false swap ! ;


: incr ( addr -- )
        dup @ 1+ swap ! ;

: decr ( addr -- )
        dup @ 1- swap ! ;

: 0= 0 = ;

: 0> 0 > ;

: 0<> 0= not ;

: 0dec ( addr -- )
        dup @ dup 0=
        if drop 0
        else 1-
        then swap ! ;

: c+! ( n addr -- )
        dup>r
        c@ + r> c! ;

: 2* ( n -- n' )
        2 * ;

: 2/ ( n -- n/2 )
        2 / ;

: 2+ 2 + ;

: 2- 2 - ;

: <> = not ;

: u> swap u< ;

: 0max ( n -- n' )
        0 max ;

: roll          ( n1 n2 .. nk k -- n2 n3 .. nk n1 )
\  rotate k values on the stack, bringing the deepest to the top.
                >r r@ pick   sp@ dup 2+   r> 1+ 2* cmove>  drop  ;

: 2rot          ( a b c d e f - c d e f a b )
\  rotate the top three double numbers, bringing the deepest pair to top.
                5 roll  5 roll  ;


\ kernel2.seq   more kernel stuff
\ number is now good for all kind of numbers April 18th, 1998 - 1:56


 8 constant bs          \ ascii backspace
 7 constant bell        \ ascii bell

variable case-sen           \ flag: if true, case sensitive
false case-sen !
variable >in_word       \ offset in line to word just parsed out with word


: erase         ( addr len -- )   \ put zeros in the area at addr.
                0 fill   ;
: blank         ( addr len -- )   \ put ascii spaces in the area at addr.
                bl fill   ;

decimal

create atbl     \ uppercase translation table
 0  c,   1  c,   2  c,   3  c,   4  c,   5  c,   6  c,   7  c,
 8  c,  32  c,  10  c,  11  c,  12  c,  13  c,  14  c,  15  c,
16  c,  17  c,  18  c,  19  c,  20  c,  21  c,  22  c,  23  c,
24  c,  25  c,  26  c,  27  c,  28  c,  29  c,  30  c,  31  c,
32  c,  33  c,  34  c,  35  c,  36  c,  37  c,  38  c,  39  c,
40  c,  41  c,  42  c,  43  c,  44  c,  45  c,  46  c,  47  c,
48  c,  49  c,  50  c,  51  c,  52  c,  53  c,  54  c,  55  c,
56  c,  57  c,  58  c,  59  c,  60  c,  61  c,  62  c,  63  c,
64  c,  65  c,  66  c,  67  c,  68  c,  69  c,  70  c,  71  c,
72  c,  73  c,  74  c,  75  c,  76  c,  77  c,  78  c,  79  c,
80  c,  81  c,  82  c,  83  c,  84  c,  85  c,  86  c,  87  c,
88  c,  89  c,  90  c,  91  c,  92  c,  93  c,  94  c,  95  c,
96  c,  65  c,  66  c,  67  c,  68  c,  69  c,  70  c,  71  c,
72  c,  73  c,  74  c,  75  c,  76  c,  77  c,  78  c,  79  c,
80  c,  81  c,  82  c,  83  c,  84  c,  85  c,  86  c,  87  c,
88  c,  89  c,  90  c, 123  c, 124  c, 125  c, 126  c, 127  c,
\ Characters above 127 are translated to below 127
 0  c,   1  c,   2  c,   3  c,   4  c,   5  c,   6  c,   7  c,
 8  c,  32  c,  10  c,  11  c,  12  c,  13  c,  14  c,  15  c,
16  c,  17  c,  18  c,  19  c,  20  c,  21  c,  22  c,  23  c,
24  c,  25  c,  26  c,  27  c,  28  c,  29  c,  30  c,  31  c,
32  c,  33  c,  34  c,  35  c,  36  c,  37  c,  38  c,  39  c,
40  c,  41  c,  42  c,  43  c,  44  c,  45  c,  46  c,  47  c,
48  c,  49  c,  50  c,  51  c,  52  c,  53  c,  54  c,  55  c,
56  c,  57  c,  58  c,  59  c,  60  c,  61  c,  62  c,  63  c,
64  c,  65  c,  66  c,  67  c,  68  c,  69  c,  70  c,  71  c,
72  c,  73  c,  74  c,  75  c,  76  c,  77  c,  78  c,  79  c,
80  c,  81  c,  82  c,  83  c,  84  c,  85  c,  86  c,  87  c,
88  c,  89  c,  90  c,  91  c,  92  c,  93  c,  94  c,  95  c,
96  c,  65  c,  66  c,  67  c,  68  c,  69  c,  70  c,  71  c,
72  c,  73  c,  74  c,  75  c,  76  c,  77  c,  78  c,  79  c,
80  c,  81  c,  82  c,  83  c,  84  c,  85  c,  86  c,  87  c,
88  c,  89  c,  90  c, 123  c, 124  c, 125  c, 126  c, 127  c,


: >body ( cfa -- cfa+5 )
        1+ cell+ ;

: body> ( body -- cfa )
        cell- 1- ;


: [char] char [compile] literal ; immediate

: >lower  ( c -- c' )
   dup [char] A [char] Z between bl and xor ;

: >upper  ( c -- c' )
   dup [char] a [char] z between bl and xor ;

: upc        ( char -- char' )
\ convert a character to upper case.
                atbl + c@ ;

: upper      ( addr len -- )
\ convert a string to upper case.
        over + swap
        ?do i c@ upc i c!
        loop ;

: ?uppercase ( a1 -- a1 )
\ conditionally convert a counted string to upper case
        case-sen @ not
        if dup count upper
        then ;

: lower ( adr len -- ) \ make it lower case
        over + swap
        ?do i c@ dup 65 90 between
               if 32 + i c!
               else drop
               then
        loop ;

: ?lowercase ( a1 -- a1 )
\ conditionally convert a counted string to lower case
        case-sen @ not
        if dup count lower
        then ;


: str-lower ( adr -- )
        count lower ;

' str-lower 'lower !

: ncaps-same? ( adr1 adr2 len -- adr1 adr2 f )
        >r over r@ lower
        dup r@ lower r>
        same? ;

: comp       ( addr1 addr2 len -- -1 | 0 | 1 )
\ compare two strings.  if equal, return 0.  if str1 < str2, return -1.
\ if str1 > str2, return 1 .
        >r 0 -rot
        over - ( 0 a1 d )
        swap r> ( 0 d a1 l )
        over + swap ( 0 d a1+l a1 )
        ?do i c@ over i + c@ 2dup <>  ( 0 d c1 c2 f )
                if <
                        if -1 
                        else 1
                        then rot drop swap leave
                else 2drop
                then
        loop drop ;


: case-comp  ( addr1 addr2 len -- -1 | 0 | 1 )
\ perform a comparison of two strings, but ignore case differences.
        >r 0 -rot
        over - ( 0 a1 d )
        swap r> ( 0 d a1 l )
        over + swap ( 0 d a1+l a1 )
        ?do i c@ upc over i + c@ upc 2dup <>  ( 0 d c1 c2 f )
                if <
                        if -1
                        else 1
                        then rot drop swap leave
                else 2drop
                then
        loop drop ;

: compare       ( addr1 addr2 len -- -1 | 0 | 1 )
\ compare two strings.  if case sensitive is false, ignore case.
                case-sen @ not if   case-comp   else   comp   then   ;


: defer ( ct: -- ) ( rt: -- )
        create ['] noop ,
        does> @execute ;

: cells+ cells + ;

: cells- cells - ;

: to            ( n -<value_name>- )
                ' >body cell+
                state @ if [compile] literal compile ! else ! then ; immediate

: (+to) ( n adr -- )
        dup @ rot + swap ! ;

: +to ( n -<value_name>- )
        ' >body cell+
        state @ if [compile] literal compile (+to) else (+to) then ; immediate

: is [compile] to ; immediate


: 3dup 2 pick 2 pick 2 pick ;


decimal


: octal         ( -- )
\ set the contents of base to 8 (i.e., octal)
                8 base !  ;

: binary        ( -- )
\ set the contents of base to 2 (i.e., binary)
                2 base !  ;
variable dpl

: %$num         ( a1 -- d1 f1 )         \ process as a hex number $a123
                dup >r dup count 1- 0max >r
                dup 1+ swap r> cmove    \ extract the $.
                dup c@ 1- over c!       \ shorten count by 1.
                bl over count + c!      \ append a blank to string.
                base @ >r               \ save the base for later restoral.
                hex number?             \ try to convert the number in hex
                r> base !               \ restore the base.
                dup 0=                  \ if its not a number, restore the $.
                if   r@ count >r dup 1+ r> cmove>
                        1 r@ c+!
                        ascii $ r@ 1+ c!
                then    r> drop ;

: %'num         ( a1 -- d1 f1 )         \ process as an ascii char 'a'
                2+ c@         0 true dpl on ;

: %^num         ( a1 -- d1 f1 )         \ process as a control char ^a
                2+ c@  31 and 0 true dpl on ;

: %numh         ( a1 -- d1 f1 )         \ process as a hex number a123h
                dup count + 1- >r       \ save addr of end last char
                bl r@ dup c@ >r  c!     \ save last char of string & set to bl
                dup c@ 1- over c!       \ shorten the count
                base @ >r               \ save the base to restore later
                hex                     \ set the base to hex
\                count pad place
\                pad count upper         \ case senseless
\                pad
                number?             \ convert the number in hex
                r> base !               \ restore the base
                r> r> c! ;              \ restore trailing h

defer $num      ' %$num   is $num       \ hex
defer 'num      ' %'num   is 'num       \ ascii
defer ^num      ' %^num   is ^num       \ control
defer #num      ' number? is #num       \ a number
defer numh      ' %numh   is numh       \ hex

: %numb         ( a1 -- d1 f1 )         \ process as a binary number 10101b
                base @ 10 =            \ but only if in decimal number base
        if      dup count + 1- >r       \ save addr of end last char
                bl r@ dup c@ >r  c!     \ save last char of string & set to bl
                dup c@ 1- over c!
                2 base !                \ set base=2 (binary)
                number?                 \ convert the number in binary
                r> r> c!                \ restore trailing b
                decimal                 \ return to decimal
        else    #num                    \ else convert as normal number
        then    ;

defer numb      ' %numb   is numb

\ Extend the special number handling done by F-PC to include
\ HEX numbers entered with an 'H' or 'h' postfix character
\ and binary numbers entered with a '&' postfix char.

: %number    ( a1 -- d1 f1 )
\ Convert count delimited string at a1 into double number.  Special
\ prefixes and sufixes allowed.
        count wpad place wpad count upper wpad \ added by pai v8
        dup 1+ c@ ascii $ = \ v8
        if $num
        else dup 1+ c@ ascii ' = \ v8
                if 'num
                else dup 1+ c@ ascii ^ = \ v8
                        if ^num
                        else dup count + 1- c@ dup ascii h = swap ascii H = or
                            if numh
                            else dup count + 1- c@ dup ascii b = swap ascii B =
                                        or
                                        if numb
                                        else #num
                                        then
                            then
                        then
                then
        then ; \ v8

: ?missing      ( f -- )
                if      space here count type
                        true abort"  <- what? "
                then    ;

: (number)      ( a1 -- d1 )
\ convert count delimited string at a1 into a double number.
                %number 0= ?missing ;

defer number    ' (number) is number
\ convert count delimited string at a1 into a double number.

' %number 'number !

: skip ( adr len char -- adr' len' )
\ Skip char through addr for len, returning addr' and len' of char+1.
        >r 2dup r> -rot
        over + swap
        ?do dup i c@ <>
                if leave
                else >r 1 -1 d+ r>
                then
        loop drop ;



: scan ( adr len char -- adr' len' )
\ Scan for char through addr for len, returning addr' and len' of char.
        >r 2dup r> -rot
        over + swap
        ?do dup i c@ =
                if leave
                else >r 1 -1 d+ r>
                then
        loop drop ;

: source ( -- adr len )
\ Return address and count of the input string in the Text input buffer.
        tib #tib @ ;

\ kernel3.seq   more kernel stuff

: >type         ( adr len -- )
                tuck pad swap cmove   pad swap type  ;

: no-name       ( -- )
                ;

: defined       ( -- here 0 | nfa [ -1 | 1 ] )
                bl word count pocket place
                pocket count lower 
                pocket 'find @execute  ;

: stackunder    ( -- )
                true abort" stack underflow" ;

: stackover     ( -- )
                true abort" stack overflow" ;

: warnover      ( -- )
                cr ."  running out of code memory! " ;

\ : dd .s key drop ;

: interp        ( -- )
                begin  >in @ #tib @ <
                while  ?stack defined
                        if  state @
                            if dup c@ $80 and
                                if name> execute
                                else name> ,
                                then
                            else name> execute
                            then
                        else dup c@ 0=
                            if drop
                            else 'number @execute 0= 
                                if count type true abort" unknown" 
                                then
                                state @
                                if double? @
                                    if swap [compile] literal
                                    else drop
                                    then [compile] literal
                                else double? @ not
                                    if drop
                                    then
                                then
                            then 
                        then 
                repeat   ;

defer status    ( -- )

defer interpret ' interp is interpret

variable printing

: print         ( -- ) printing on interpret printing off ;


: control       ( -- n )   
                bl word   1+ c@  31 and
                state @ if   [compile] literal   then   ; immediate

variable  "buf 132 allot

\ (") string

: (")           ( -- addr len )
                r@ count dup r> + 2+ >r ;

: (.")         ( -- )
                r@ count dup r> + 2+ >r type ;

: ,"            ( -- )
                ascii " parse tuck here place ( len )
                0 here count + c!
                2+ allot ;

: ."            ( -- )          compile (.") ,"   ;   immediate

: "             ( -- )          compile (")  ,"   ;   immediate

: ">$           ( a1 n1 -- a2 )
                drop 1- ;

: >link ( cfa -- lfa )
        >name cell- ;

: n>link ( nfa -- lfa )
        cell- ;

: link>n ( lfa -- nfa )
        cell+ ;

: link> ( lfa -- cfa )
        link>n name> ;

: >view ( cfa -- vfa )
        >name 2 cells - ;
        
: view> ( vfa -- cfa )
        2 cells+ name> ;        

variable fence
defer other-forget ( adr -- )
' drop is other-forget

: delete-voc ( voc-l-addr -- ) \ unlink a vocabulary
        voc-link
        begin dup @ ?dup
        while dup 3 pick = 
                if @ swap ! drop exit
                else nip
                then
        repeat 2drop ;

: forget-a-voc ( lfa voc -- )
        tuck ( voc lfa voc )
        begin @ ?dup
        while 2dup >
                if nip swap ! exit
                then
        repeat drop cell+ delete-voc ; \ voc above forget addr
        
: forget-above ( lfa -- )
                dup cell - cp ! 
                dup voc-link
                begin @ ?dup
                while 2dup cell- forget-a-voc
                repeat drop
                other-forget ;
       
: forget        ( -- )
                ' >link 
                dup fence @ > not abort" Cannot forget!"
                forget-above ;

: forward       ( -- )
                compile branch ?>mark                           ; immediate

: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
        3 pick 3 pick ;

: 2swap ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
        rot >r rot r> ;
        
: continue      ( -- )
                2over [compile] repeat                          ; immediate

: aft           ( -- )
                2drop [compile] forward ?<mark 2swap            ; immediate

: ,jump         ( -- )
                233 c, 0 here cell+ - , ;

: hide          ( -- ) \ last contain last nfa
                last @ cell- @ current @ ! ;

: reveal        ( -- )  
                last @ cell- current @ ! ;


: recursive     ( -- )
                reveal ;   immediate

: value         ( n -- )
                create , does> @ ;

: array         ( n1 -- )
                create allot does> ;

\ vocabulary: last-link adr-to-previous-voc
\ voc-link keeps last voc-link-adr
\ if 0, means end of link

variable dp                             \ application dictionary pointer
variable sdp                            \ system dictionary pointer


: <run>         ( -- )
        state @ if      ]
                        state @ 0=
                        if   interpret   then
                else    interpret   then   ;

defer run       ' <run> is run

defer errfix    ' noop is errfix

defer ?error

: (?error)      ( adr len f -- )
                if      ['] <run> is run errfix
                        2>r sp0 @ sp!   printing off
                        2r> space type space   quit
                else    2drop  then  ;

' (?error) is ?error


\ kernel4.seq   last part of the kernel file, finishes up the compile.

\ link this file into the filelist chain.



: warmstrt      ( --- )
\ the default function to be performed on a warm start.
                forth
                true abort" warm start" ;

defer warmfunc  ' warmstrt is warmfunc
\ a defered word that is invoked when a warm start occurs.
\ this function is also called whenever the control break key is pressed.

true value 1stcold
\ a flag to tell if cold has been called yet.


: h.            ( n1 --- )
\ display the unsigned number in hex, with trailing blank. does not
\ change the number base.
                base @ >r hex u. r> base ! ;


\ simage pforth.img

\ kernel.fth  - win32forth fkernel
\ andrew mckewan
\ march 1994
\ given to tom zimmer 05/13/94, assemblable with tasm32
\ metacompiler version 11/95 andrew mckewan
\ separated heads version started december 19th, 1995 tjz
\ added user variables for multi-tasking august 29th, 1996 bee/tjz

: equ create , does> @ ;

decimal

\        sp     equ     <esp>   \ stack pointer for forth, the hardware stack
\        rp     equ     <ebp>   \ return pointer, forth's subroutine stack
\        ip     equ     <esi>   \ "absolute" instruction pointer for forth 
\        bp     equ     <edi>   \ "absolute" base of forth image
\        up     equ     <edx>   \ "absolute" user pointer 
\        tos    equ     <ebx>   \ top of stack is in ebx

16       equ numvocs        \ maximum number of vocabularies in search order

 -1 equ throw_abort
 -2 equ throw_abortq
 -3 equ throw_stack_overflow
 -4 equ throw_stack_underflow
-13 equ throw_undefined





: bounds ( adr len -- lim first )
        over + swap ;

: w@         ( a1 -- w1 )    \ fetch the word (16bit) w1 from address a1
        @ $FFFF and ;

: sw@        ( a1 -- w1 )    \ fetch the sign extended word (16bit) w1
                             \ from address a1
        w@ ;

: w!         ( w1 a1 -- )    \ store word (16bit) w1 into address a1
        dup @ $FFFF0000 and rot + swap ! ;

: w+!        ( w1 a1 -- )    \ add word (16bit) w1 to the contents of address a1
        dup w@ rot + swap w! ;


\ -------------------- cell operators --------------------


: +cells     ( n1 a1 -- n1*cell+a1 ) \ multiply n1 by the cell size and add
                                        \ the result to address a1
        swap cells + ;

: -cells     ( n1 a1 -- a1-n1*cell ) \ multiply n1 by the cell size and
                                        \ subtract the result from address a1
        swap cells - ;

\ -------------------- char operators --------------------

: chars      ( n1 -- n1*char )       \ multiply n1 by the character size (1)
                ;

: char+      ( a1 -- a1+char )       \ add the characters size in bytes to a1
        1+ ;


: invert     ( n1 -- n2 )    \ perform a bitwise -1 xor on n1, return result n2
        -1 not ;

: lshift     ( u1 n -- u2 )  \ shift u1 left by n bits (multiply)
        0
        ?do 2 *
        loop ;

: rshift     ( u1 n -- u2 )  \ shift u1 right by n bits (divide)
        0
        ?do 2 /
        loop ;

: cincr      ( addr -- )     \ increment the byte contents of addr
        dup c@ 1+ swap c! ;

: cdecr      ( addr -- )     \ decrement the byte contents of addr
        dup c@ 1- swap c! ;


: under+     ( a x b -- a+b x ) \ add top of stack to third stack item
        swap >r + r> ;


: d2*        ( d1 -- d2 ) \ multiply the double number d1 by two
        swap dup 2* dup rot over u<
        if 1
        else 0
        then rot 2* + ;

: d2/        ( d1 -- d2 ) \ divide the double number d1 by two
        dup 2/ dup rot u<
        if 0
        else -1
        then rot 2/ + swap ;

\ -------------------- unsigned multiply & divide --------------------


: word-split ( u1 -- low high ) \ split the unsigned 32bit u1 into its high
                                   \ and low 16bit quantities.
        dup $FFFF and swap $10000 / ;

: word-join  ( low high -- n1 ) \ join the high and low 16bit quantities
                                   \ into a single 32bit n1
        $10000 * + ;

\ -------------------- comparison operators --------------------

: du<        ( ud1 ud2 -- f1 ) \ return true if unsigned double ud1 is
                                  \ less than undigned double ud2
        rot swap u<
        if 2drop true
        else u<
        then ;

: umin       ( u1 u2 -- n3 ) \ return the lesser of unsigned u1 and
                                \ unsigned u2
        2dup u<
        if drop
        else swap drop
        then ;


: umax       ( u1 u2 -- n3 ) \ return the greater of unsigned u1 and
                                \ unsigned u2
        2dup u<
        if swap drop
        else drop
        then ;

\ -------------------- double stack operators --------------------

: 4drop      ( n1 n2 n3 n4 -- ) \ discard four items from the data stack
        drop drop drop drop ;

: 4dup          ( a b c d -- a b c d a b c d )
\ duplicate top 4 single numbers (or two double numbers) on the stack.
                2over 2over   ;


\ -------------------- system variables --------------------



0 value source-id
0 value source-position

create .smax   8 ,              \ max number of stack entries to show

\ -------------------------------------------------------
\ vocabulary dictionary structure
\
\       [ cfa field        ] +0           vcfa = vocabulary cfa
\       [ voc link         ] +4           vlink

63 value name-max-chars                 \ function names can be this long


\ warning: (find) is a case sensitive find.  if you need to be able to find
\ words in the dictionary that have not already been passed through ?uppercase,
\ then you should use caps-find which will uppercase the string before trying
\ to find it in the dictionary.


: ?comp  state @ 0= abort" compilation only"  ;

: ?pairs        ( n1 n2 -- )  xor abort" conditionals not paired"  ;

: _case     ( -- )          \ "runtime" marker for the decompiler, a noop
        ;

: _of       ( n1 n2 -- [n1] ) \ "runtime"
                                  \ if n1<>n2 branch to after endof, return n1
                                  \ else continue and don't return n1
        over =
        if drop r> cell+ >r
        else r> @ >r
        then ;
: _endof    ( -- )          \ "runtime" branch to after endcase
        r> @ >r ;

: _endcase  ( n1 -- )       \ "runtime" discard n1 and continue
        drop ;

: case   ?comp  compile _case  0 ; immediate
: of     ?comp  compile _of  >mark 4 ; immediate
: endof  ?comp  4 ?pairs  compile _endof  >mark  swap >resolve  5 ; immediate

: endcase  ?comp  compile _endcase
           begin  ?dup
           while  5 ?pairs  >resolve
           repeat ; immediate

\ -------------------- string literals --------------------

\ convert occurances of \n within string a1,n1 to the crlf pairs.
\ useful mostly for strings that will get passed to the operating system.

defer \n->crlf  ( a1 n1 -- )    ' 2drop is \n->crlf

: 2constant     ( n1 n2 -- )
                create , ,
                does> dup cell+ @ swap @ ;

: 2variable     ( -<name>- )
                variable 0 , ;


: wait          ( -- )
                key 27 = if abort then ;


: h.r           ( n1 n2 -- )    \ display n1 as a hex number right
                                \ justified in a field of n2 characters
                base @ >r hex >r
                0 <# #s #> r> over - spaces type
                r> base ! ;

: h.n           ( n1 n2 -- )    \ display n1 as a hex number of n2 digits
                base @ >r hex >r
                0 <# r> 0 ?do # loop #> type
                r> base ! ;

: h.2           ( n1 -- ) 2 h.n ;               \ two digit hex number
: h.4           ( n1 -- ) 4 h.n ;               \ four digit hex number
: h.8           ( n1 -- ) 8 h.n ;               \ eight digit hex number


: nxcall ( [...] n x -- return )
        swap >r xcall
        r> swap >r
        0
        ?do drop
        loop
        r> ;

: s"            ( -<string">- )
                state @
                if      compile (")  ,"
                else    ascii " word
                        temp$ dup>r over c@ 1+ cmove
                        0 r@ count + c!
                        r> count
                then ; immediate

: (z") ( -- adr )
         r@ count r> + 2+ >r ;

: z"            ( -<text">- )
                state @
                if      compile (z") ,"
                else    ascii " word
                        temp$ dup>r over c@ 1+ cmove
                        0 r@ count + c!
                        r> 1+
                then ; immediate

: marker ( -- )
        create last @ ,
        does> @ n>link forget-above ;

: s>d 0 ;

: d>s drop ;

: align ;

\ to complete double word set

cr .( Loading double words. )

: 2literal ( l h -- )
        swap compile dolit ,
        compile dolit , ; immediate
        
: d- ( d1|ud1 d2|ud2 -- d3|ud3 )
        dnegate d+ ;

: d. ( d -- )
        <# #s #> type ;

: d.r ( d n -- )
        >r <# #s #>
        r> over - spaces type ;
        
: d0< ( d -- f )
        nip 0< ;
        
: d0= ( d -- f ) or 0= ;

: d< ( d1 d2 -- f ) rot 2dup >
        if 2drop 2drop false
        else =
                if < 
                else true 
                then
        then ;

: d= ( d1 d2 -- )
        rot = -rot = and ;
: d> ( d1 d2 -- f )
        4dup d=
        if 4drop false
        else d< not
        then ;
        
\ : d>s ( d -- n ) drop ;

: dabs ( d -- ud ) 
        dup 0<
        if dnegate
        then ;

\ : 4dup 2over 2over ;
                
: dmax ( d1 d2 -- d3 ) 4dup d< if 2swap then 2drop ;

: dmin ( d1 d2 -- d3 ) 4dup d> if 2swap then 2drop ; 

: m*/ ( d1 n1 +n2 -- d2 ) >r 0 d* r> nip / ; \ still wrong

: m+ ( d n -- d ) 0 d+ ;

                                                                                
\ linux specific file
\ the differences between win32 and linux are two files: lpforth.f and
\  extra linux.f file
\ make linux forth non-canon, add a 'scall' for linux function call
\ to dynamic linked library, return on top of stack
\ save as linux1.f
\ save as linux8.f before automation

cr .( Loading linux specific words. )

 15    constant dlopen-x
 16    constant dlsym-x
 17    constant dlclose-x
.( mm2 )
: dlopen ( flag  z" -- lib )
        2 dlopen-x nxcall ;

: dlsym ( z" lib -- adr )
        2 dlsym-x  nxcall ;

: dlclose ( lib -- n )
        1 dlclose-x nxcall ;

\ solib.f modified from winlib.f from win32for
\ **********************************************************************
\
\ library format:
\       cell    linked list of libraries    
\       cell    handle    --------------- lib
\       cell    library type 0=normal, -1=system (no load for turnkey)
\       counted name (counted string)
\
\ proc format:
\       cell    link to next proc  
\       cell    procedure address   ---------- proc
\       cell    pointer to library
\       name    (counted string)
\
\ **********************************************************************
.( mm1 )

marker lib

: place0 ( from cnt to -- )
        >r
        dup r@ c!
        r@ 1+ swap cmove 
        0 r> count + c! ;

variable lib-link            \ linkage for libraries
         lib-link off

variable proc-link           \ linkage for procedures
         proc-link off

 
: load-library  ( lib -- err? )        \ f1=FALSE if all is ok
        dup @ 0=                        \ library address NULL?
        if      dup 2 cells+            \ step from PFA to counted library name
                1+ 0 swap dlopen  ( LoadLibrary )
                dup>r r@
                if      swap !          \ install library address
                else    2drop           \ discard and leave NULL
                then    r> 0=           \ -- f1
        else    drop    FALSE           \ if not, then return FALSE
        then    ;

: free-library  ( lib -- )
        dup @ dlclose drop 
        off ;

: trim-procs    ( a1 -- )    \ remove any procs above forget address
                >r proc-link 
                begin @ dup r@ <
                until r>drop
                proc-link ! ;

: trim-libs    ( a1 -- )    \ remove any procs above forget address
                >r lib-link 
                begin @ dup r@ >
                while dup cell+ free-library
                repeat r>drop
                lib-link ! ;

: trims ( adr -- )
	dup trim-procs
	dup trim-libs
	trim-files ;
	
' trims is other-forget	
.( mm3 )
		
	                                                                

\ : link,  ( addr -- ) here over @ , swap ! ;

create lib$ maxstring allot

: library-exist? ( z" -- f )
        0 swap dlopen 0= not ;

: library ( \ lib$ 'name.so' -- )  \ usage: library libc.so
        bl word count lib$ place0
        lib-link
        begin  @ ?dup
        while   dup 3 cells+ count lib$ count rot max compare 0=
                if      drop EXIT   \ already in list, EXIT
                then
        repeat
        lib$ 1+ library-exist?
        if  lib-link link,     \ the link of all libraries
            0 lib$ 1+ dlopen , \ the library handle
            0 ,                \ mark as application library
            lib$ count here dup>r over 2+ allot place0 
            0 r> count + c! 
        else abort" Cannot find library."
        then ;

: load-proc ( proc -- err? )
        dup @ 0=                        \ proc address NULL?
        if      dup cell+ @ load-library 0=
                if dup 2 cells+ 1+ over cell+ @ @ dlsym swap ! false
                else drop true
                then
        else    drop false  \ already exist 
        then    ;

create proc-name MAXSTRING allot
       proc-name off

\ -------------------- Calling Procedures --------------------

\ Look for proc name. 
: "find-proc    ( a1 n1 -- addr-proc-addr TRUE | FALSE )
                proc-name place0
                proc-link
                begin   @ ?dup
                while   dup 3 cells+ count proc-name count rot max 
                        compare 0=
                        if      cell+   \ addr of procedure addr
                                TRUE
                                EXIT    \ leave with the CFA
                        then
                repeat  FALSE ;

: +null ( addr -- )
        count + 0 swap c! ;                                

0 constant null

: exist-in-lib? ( -- false | lib ) \ string in proc-name
        lib-link
        begin   @ ?dup
        while   proc-name 1+ over cell+ @ dlsym
                if      cell+    \ if so, discard linkage
                        EXIT                \ got it
                then
        repeat  \ no found
        drop false ;
 
: ?proc-error   ( f1 -- )       \ f1=error if true
                if      proc-name count pocket place0
                        TRUE abort" Couldn't find procedure"
                then    ;
                                
: "proc ( a1 n1 -- ) \ try to define proc if can
        proc-name place0   \ get the proc name
        proc-link
        begin   @ ?dup
        while   dup 3 cells+ count                \ next proc name in list
                proc-name count rot max 
                compare 0=      \ is this name already defined
                if      drop true               \ if so, discard linkage
                        EXIT                    \ got it
                then
        repeat  \ no found
        exist-in-lib? dup ( false | lib )
        if  proc-link link,
                here 0 ,
                swap ,
                proc-name count here over 2+ allot place0 
                load-proc
                if      TRUE ?proc-error
                then
        else true ?proc-error
        then
        ;                          \ re-select system memory


: ndrop ( n1 n2 n3 ... nn n -- ) sp@ swap 1+ cells + sp! ;

: perform-ncall ( [args..] n proc-addr -- result )
        swap >r call
        r> swap >r ndrop r> ;

: (ncall) ( -- )
        r> dup cell+ >r @ ( addr-proc-addr ) \ the real proc addr may vary each time system run
        @ perform-ncall ;


: set-proc ( -- ) \ set an proc available for compling
                bl word count 2dup pocket place0
                "find-proc ( proc-addr true | false )
                if drop
                else pocket count "proc
                     pocket count "find-proc 0= ?proc-error
                     drop
                then ;

 : ncall          ( [args..] n -<proc>- result ) \ compile or execute a so proc
                bl word count 2dup pocket place0
                "find-proc ( addr-proc-addr true | false ) \ try to find the call
                if                              \ if we found it, then
                        state @                 \ if we are compiling
                        if  compile (ncall) ,  \ then compile the call
                        else  @ perform-ncall   \ if interpreting, execute it
                        then
                else                            \ if we didn't find the call
                        state @                 \ and we are compiling, then
                        if  
      abort" Need to make proc available by 'set-proc' first!"
                        else                    \ otherwise, define the call
                             pocket count "proc  \ define proc
                             pocket count "find-proc 0= ?proc-error
                             @ perform-ncall      \ and then just doit
                        then
                        
                then    ; IMMEDIATE
 
: .libs         ( -- )
                cr
                lib-link
                begin   @ dup
                        nuf? 0= and
                while   dup 3 cells+ count type space 
                        ." [" dup cell+ @ . ." ]" space
                repeat  drop cr ;

: .procs        ( -- )
                cr
                proc-link
                begin   @ dup
                        nuf? 0= and
                while   dup 3 cells+ count type space
                        ." [" dup cell+ @ . ." ]" space
                repeat  drop cr ;

: init-libs ( -- )
                lib-link
                begin   @ ?dup
                while   0 over 3 cells+ 1+ dlopen 
                        over cell+ !
                repeat ;


: init-procs ( -- )
                proc-link
                begin   @ ?dup
                while   dup 3 cells+ 1+ over 2 cells+ @ @ dlsym
                        over cell+ ! 
                repeat ;
.( mm5 )
library libm.so

set-proc tcgetattr
set-proc tcsetattr

create save-termios 80 allot

: non-canon ( -- )
      pad 1 2 ncall tcgetattr abort" tcgetattr error"
      pad save-termios 80 cmove
      0 pad 23 + c!
      $1 $2 $8 $8000 or or or not
      pad $3 cells + dup >r @ and r> ! 
      pad 2 1 3 ncall tcsetattr abort" tcsetattr error" ;


.( mm4 )
non-canon

: reset-termios ( -- )
      save-termios 2 1 3 ncall tcsetattr abort" reset error" ;

: bye reset-termios bye ;

set-proc fflush

: ioflush ( -- )
        0 1 ncall fflush drop ;

: (boot) 
        init-libs 
        init-procs 
        ioflush 
        non-canon 
        hi ;

' (boot) 'boot !

: tab space ;

variable lmargin    0 lmargin !
\ the left margin setting used by ?line, ?cr.

variable rmargin   70 rmargin !
\ controls the right margin, used by ?line, ?cr.

variable tabsize    8 tabsize !
\ controls the tab increment for tab. default is 8.

variable #out

: ?line         ( n -- )
\ break the line at the cursor if there are less than n1 characters
\ till rmargin is encountered.
                #out @ +  rmargin @ > if cr lmargin @ spaces then ;

: ?cr           ( -- )
\ break the line at the cursor, if we have reached the right margin
\ as specified by rmargin.
                0 ?line  ;

: tab           ( --- )
\ print spaces to get to the next tab increment as specified by
\ tabsize.
                #out @ tabsize @ mod tabsize @ swap - spaces ;

: tab space ;
\ words utility

: ?.id ( nfa -- )
        wpad c@ 0 =
        if .id tab
        else dup c@ 31 and wpad c@ dup 0 =
            if .id tab
            else - dup 0 < not \ nfa n f
                if 0 swap 1+ \ nfa 0 n
                        for over 1+ r@ + wpad count 'same? @execute 0 = or \ nfa 
                        next
                        if .id tab
                        else drop
                        then
                else drop drop
                then
            then
        then ;

: .word-displayed ( -- )
        cr ." Displayed" word-count @ . ."  word(s)." ;

: words ( -- )
        bl word count dup wpad c! wpad 1+ swap cmove
        0 word-count !
        cr context @
        begin @ dup nuf? not and
        while dup cell+ ?.id 
        repeat drop 
        .word-displayed ;

\ dump utility

.( mm5 )
: dm+ ( adr len -- adr'  )
        over 4 u.r space
        for dup c@ h.2 space 1 +
        next ;

: du-head ( addr -- ) \ print a address header
        16 mod 
        cr ." Addr    " 
        16 0 
        do dup i + 16 mod h.2 space
        loop drop ;

: dump ( b u -- )
        base @ >r hex
        over du-head
        16 / 1+ 
        for cr 16 2dup dm+ rot rot 2 spaces _type false
                if r> drop drop r> base ! exit
                then
        next drop r> base ! ;

\ nxcall version save as file1.f before change to solib version
\ file.f, try to conform ANS forth file word sets
\ file words, resize-file and file-status not implemented
\ this version can view source, save as file7.f
\ view, read-line version, save as file8.f

cr .( Loading file words. )

set-proc fopen
set-proc fclose
set-proc fwrite
set-proc fread
set-proc fseek
set-proc fgets
set-proc fputs
set-proc strlen
set-proc ftell
set-proc fseek
set-proc rewind
set-proc remove
set-proc rename

0 constant SEEK-SET
1 constant SEEK-CUR
2 constant SEEK-END

: open-file    ( addr len fmode -- fileid ior )
                -rot pocket place0 pocket 1+ 
                2 ncall fopen dup 0= ;

create r/o$ ," r"
create r/w$ ," r+"
create w/o$ ," w"

: r/o ( -- fmode )
        r/o$ 1+ ;

: r/w ( -- fmode )
        r/w$ 1+ ;

: w/o ( -- fmode )
        w/o$ 1+ ;

: close-file ( fileid -- ior )
        1 ncall fclose 0= not ;

: create-file ( adr len fmode -- fileid ior )
        drop z" w+" open-file ;


: read-file ( adr len fileid -- ior )
        -rot swap 1 swap 4 ncall fread dup 0= ;

: reposition-file ( ud fileid -- ior )
        seek-set -rot 3 ncall fseek ;

: DELETE-FILE ( c-addr u -- ior )
         pocket place0 pocket 1+ 1 ncall remove ;

: FILE-POSITION ( fileid -- ud ior )
        1 ncall ftell dup -1 = ;

: FILE-SIZE ( fileid -- ud ior )
        >r
        r@ file-position drop 
        seek-end 0 r@ 3 ncall fseek drop
        r@ file-position 
        rot r> reposition-file drop ;

0 value input-file
0 value read-line-len
0 value input-buf

: READ-LINE ( c-addr u1 fileid -- u2 read-ok? ior )
        to input-file
        to read-line-len
        to input-buf 
        input-file read-line-len 1+ input-buf 
        3 ncall fgets dup 0=  \ something weird
        if drop
            input-file file-position drop input-file file-size drop =
            if 0 false false  \ end of file
            else 0 false true \ file error
            then
        else 1 ncall strlen 
             dup read-line-len = 
             input-buf read-line-len + c@ $a <> and not
             if 1-          \ normal, otherwise full read
             then true false
        then ;

       
: INCLUDE-FILE ( i*x fileid -- j*x ) 
        >r 1 loadline !
        begin tib maxstring r@ read-line abort" read error"
        while #tib ! 
              >in off
              interp 
            \  ascii . emit
              1 loadline +!
        repeat r>drop ;

: set-file-link ( adr len -- )
        file-link @ cell+ @ 1+ loadfile ! 
        file-link link,
        loadfile @ ,
        dup>r
        dup c, here swap cmove
        r> allot ;
        
: INCLUDED ( i*x c-addr u -- j*x )
        2dup pocket place0
        r/w open-file abort" file not found"
        pocket count set-file-link  
        include-file ;

: fload ( | file -- )
        loadfile @ loadline @
        bl word count included drop 
        loadline ! loadfile ! ;
          
: RESIZE-FILE ( ud fileid -- ior ) ;

: write-file ( c-addr u fileid -- ior )
        -rot 1 -rot dup>r swap 4 ncall fwrite r> <> ;
        
: WRITE-LINE ( c-addr u fileid -- ior )
        >r pocket place0 $a pocket count + c!
        r> 1 pocket count 1+ dup>r swap 4 ncall fwrite 
        r> <> ;

: FILE-STATUS ( c-addr u -- x ior ) ;

: FLUSH-FILE ( fileid -- ior )
        1 ncall rewind  ;

: RENAME-FILE  ( c-addr1 u1 c-addr2 u2 -- ior ) 
        pocket place0 cur-file place0
        pocket 1+ cur-file 1+ 2 ncall rename ;
  
\ memory

cr .( Loading memory words. )

set-proc malloc
set-proc free
set-proc realloc
set-proc system

: ALLOCATE ( u -- a-addr ior ) 
        1 ncall malloc dup 0= ;

: FREE ( a-addr -- ior ) 
        1 ncall free drop 0 ;

: RESIZE ( a-addr1 u -- a-addr2 ior ) 
        swap 2 ncall realloc dup 0= ;

: sh ( z"  -- )
       cr 1 ncall system drop ;
        

\ following is utilities for viewing source

cr .( Loading view words. )

decimal


25 value screen-rows
    
0 value cur-first-line# \ current first line position
0 value last-top-line#

create hor-line-t 80 allot
hor-line-t 80 ascii - fill
create line-buf maxstring allot
0 value cur-line

: horizontal-line ( -- )
         cr hor-line-t 80 type ;

16 value locate-height 

: locate-header ( -- n1 )
                locate-height 4 / ;

-1 value orig-loc
0 value lcnt
0 value lochdl

: $locate       ( line# filename | dummy -1 -- )
                count 2dup cur-file place0 
                r/o open-file abort" Couldn't open source file!"
                to locHdl
                0 to lcnt
                to cur-line
                base @ >r decimal
                cr ." From file: " cur-file count type 2 spaces 
                ."  Starting line: " cur-line locate-header - .
                horizontal-line
                cur-line locate-header - 0max 0
                ?do     line-buf maxstring lochdl read-line
                        abort" Read Error"
                        nip 0= if leave then
                        1 +to lcnt
                loop
                locate-height 0
                do      line-buf maxstring locHdl read-line
                        abort" Read Error"
                        if  1 +to lcnt
                                lcnt orig-loc =
                                if      horizontal-line
                                        line-buf swap cr type
                                        horizontal-line
                                else    line-buf swap cr type
                                then
                        else    drop leave
                        then
                loop    horizontal-line
                locHdl close-file drop
                r> base ! 
                cr ;

: .viewinfo     ( -<name>- line filename )
        ' >view @ $10000 /mod loadfile ! to orig-loc 
                file-link
                begin   @ dup
                        0 = not
                while   dup cell+ @ loadfile @ =
                        if 2 cells+ orig-loc swap exit
                        then
                repeat  drop true abort" Cannot find file" ;

: ll       ( -<name>- )    \ show some source lines of word
                .viewinfo $locate ;

: n             ( -- )          \ show the next bunch of lines
                cur-line locate-height 4 - + cur-file $locate ;

: b             ( -- )          \ show the previous bunch of lines
                cur-line locate-height 4 - - 0 max cur-file $locate ;

cr .( Loading facility words. )

: key? ?key ;

: ekey ( -- n ) key ;

: ekey>char ( u -- u flag ) key dup 128 < ;

: emit? ( -- flag ) true ;


set-proc gettimeofday
set-proc localtime

create timeval 4 cells allot
create timezone 2 cells allot
0 value tm

: time&date 
        timezone timeval 2 ncall gettimeofday abort" Error"
        timeval 1 ncall localtime to tm
        tm @ tm cell+ @ tm 2 cells+ @
        tm 3 cells+ @ tm 4 cells+ @ 1+ tm 5 cells+ @ 1900 + ;
        
set-proc usleep

: ms ( n -- )
        1000 * 1 ncall usleep drop ;                
                                      
' b fence !

fload inbuf.f
fload xdebug.f
fload float.f
fload order.f

\ simage order.img

fload asm386.f
fload dis486.f

\ simage dis.img

simage pforth.img



