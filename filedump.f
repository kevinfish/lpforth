\ improve filedump October 5th, 1996 - 2:37
\ recheck everything to make scroll bar in dump-window
\ take care of last line October 6th, 1996 - 14:22
\ add print function November 23rd, 1996 - 12:23
\ fix bug for big file February 25th, 1997 - 20:39
\ modify it to use in lpforth 5/10/98
\ save as filedump1.f before changing it to no memory buffering version Mon Nov 6 11:28:34 2000, OK


marker dumpf

\ fload memory.f

decimal


76 value screen-cols
24 value screen-rows
0 value inf
    

0 value cur-first-line# \ current first line position
0 value first-line#    \ first line number
200 value last-line#   \ last line number
0 value last-line-len  \ chars length of the last line
0 value last-top-line#

create cur-filename max-path allot
16 value bytes/line
0 value file-len \ length of the whole file
0 value file-ptr \ address of the memory for file

create line-buf 80 allot

: H.N.str       ( n1 n2 -- adr len )    \ display n1 as a hex number of n2 digits
                base @ >r hex >r
                0 <# r> 0 ?do # loop #>
                r> base ! ;

create spcs 80 allot
spcs 80 bl fill

: prepare-last-line ( line# -- adr len )
        bytes/line *
        dup 8 h.n.str line-buf place
        spcs 2 line-buf +place 
	cur-first-line# bytes/line * - \ calculate the position in buffer
        file-ptr + dup dup last-line-len + swap
        ?do i c@ 2 h.n.str line-buf +place
                spcs 1 line-buf +place
        loop spcs bytes/line last-line-len - 3 * line-buf +place
        last-line-len line-buf +place
        spcs bytes/line last-line-len - line-buf +place
        line-buf count ;

: dump-line ( line# -- adr len )  \ prepare a line
        dup last-line# =
        if prepare-last-line
        else bytes/line *
               dup 8 h.n.str line-buf place
               spcs 2 line-buf +place
		cur-first-line# bytes/line * - \ calculate the position in buffer
               file-ptr + dup dup bytes/line + swap
               ?do i c@ 2 h.n.str line-buf +place
                        spcs 1 line-buf +place
               loop bytes/line line-buf +place
               line-buf count
        then ;

3000 value buf-len

: read-to-buffer ( -- )
	cur-first-line# bytes/line um* inf reposition-file drop
	file-ptr buf-len inf read-file 2drop ;

: show ( -- )
	read-to-buffer
                        screen-rows 0
                        ?do 
                                i cur-first-line# + dup last-line# >
                                if  screen-cols spaces
                                else cr dump-line _type 
                                then 
                        loop ;

                        
: "open-file    ( a1 n1 -- )  \ open file, read to memory, set last-line#
		inf ?dup if close-file drop then
                2dup r/o open-file abort" open error" to inf
                        127 min cur-filename place
                        \ release/allocate the text buffer
                        file-ptr ?dup if free drop then
                        inf file-size 2drop to file-len
                        buf-len allocate drop to file-ptr \ read 1000 bytes at most
                        \ read the file into memory
                        0 to cur-first-line#
                        file-len bytes/line /mod to last-line# to last-line-len
                        last-line# screen-rows - 1+ 0max to last-top-line# ;
  


: filedump ( s" -- )
        "open-file show ;


: de. base @ swap decimal . base ! ;

: n ( -- )
       screen-rows cur-first-line# + last-top-line# min to cur-first-line# 
       show ;

: b ( -- )
       cur-first-line# screen-rows - 0max to cur-first-line# 
       show ;
       
: adr-dump ( adr -- )
	16 / last-top-line# min 0max to cur-first-line# 
	show ;
	
: %-dump ( -- )
	last-top-line# 100 */ last-top-line# min 0max to cur-first-line#
	show ;
	
: all-dump ( -- )
	last-line# 0
	?do i cr dump-line _type nuf? if leave then
	loop ;
		
		          
