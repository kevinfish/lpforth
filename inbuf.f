\ input buffer by pai
\ working pretty well April 25th, 1998 - 1:25
\ up and down array works
\ save as inbuf2.f before adding pushkey Thu Apr 6 22:23:29 2000
\ save as inbuf3.f for solft link

cr .( Loading input buffer words. )

decimal

maxstring constant b/accept     \ each commandline is maxstring bytes
       31 constant n/accept     \ save 31 previous command lines
                                \ use 31 to make it fit in 8k bytes
  0    value accept#
  0    value accepted?
create prev-accept-buf b/accept n/accept * allot

: accept-init   ( -- )
                0 to accept#
                prev-accept-buf b/accept n/accept * erase ;

\ initialization-chain chain-add accept-init      \ add to init chain

: +accept#      ( n1 -- )
                accept# + n/accept mod to accept# ;

: prev-accept-buf" ( -- a1 )
                prev-accept-buf accept# b/accept * + ;

: replace-buf ( bt et pt -- bt et pt' )
                prev-accept-buf" count 2dup + 4 pick >
                if 2drop bell emit exit
                then >r 3 pick r@ cmove
                drop over r> +
                13 emit 70 spaces
                13 emit 2 pick 2dup - type ;

: accept-lup    ( bt et pt c -- bt et pt' )
                false to accepted?
                -1 +accept#
                replace-buf ;

: accept-ldown  ( bt et pt c -- bt et pt' )
                accepted? 0=
                if 1 +accept#
                then false to accepted?
                replace-buf ;

: store-buf ( bt et pt c -- bt pt pt )
        true to accepted?
        nip
        2dup swap - 0>
        if 2dup over - prev-accept-buf" place
                1 +accept#
        then
        dup ;

: esc-seq ( c -- )
    	case 
    		91 of true in-esc? ! endof
    		65 of accept-lup endof
    		66 of accept-ldown endof
    		67 of endof
    		68 of endof
    		dup >r ktap r>
    	endcase ;
	
: non-esc-seq ( c -- )
        case
                9 of accept-lup endof
                27 of true in-esc? ! endof
                11 of accept-ldown endof
                crlf @ of store-buf endof
                dup >r ktap r>
        endcase ;
 
\ create push-buf 5 allot
\ 0 push-buf c!

\ :  pushkey ( c -- )
\	push-buf c+place ;
      	
: buf-ktap ( bt et pt c -- bt et pt' )
\    >r push-buf count >r over r@ cmove r> + r>  \ may need these for pushkey later
\    0 push-buf c!
    in-esc? @ 
    if false in-esc? ! esc-seq
    else non-esc-seq
    then ;


: buf-accept ( adr len -- adr len' )
        'tap @ >r
        ['] buf-ktap 'tap !
        accept
        r> 'tap ! ;

accept-init

' buf-accept dup 'expect ! 'default-accept !


