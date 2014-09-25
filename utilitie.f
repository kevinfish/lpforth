\ save as utilitie1.f for solft link Wed Apr 26 00:38:35 2000

cr .( Loading Utilities)

marker t-m

decimal

1 s" chdir" proc chdir

: cd ( -- ) 
	bl word count pad place0
	pad 1+ chdir abort" Directory error" ;
	
: dir ( -- )
	z" ls -l"  system drop ;
	
: ls ( -- )
	z" ls"  system drop ;
	
: os ( z" -- )
	 system drop ;

: asciiz->asc-len ( adr -- adr' len )
	maxstring 2dup 0 scan nip - ;
				
: new-file ( adr len -- handle rio )
	2dup delete-file drop 
	r/w create-file ;
	
: fast 0 0 min-time ;

: slow 0 1 min-time ;
		
: ?keypause ( -- )
	nuf? abort" interrupted!" ;

: .proc ( adr -- )
	cr 
	dup @ 10 u.r 
	dup cell+ @ 10 u.r
	dup 2 cells+ @ dup 10 u.r 2 cells+ count space type space
	dup 3 cells+ @ 3 u.r
	link>pname count space type space ;

: .procs        ( -- )
	cr ."  Link      Proc-addr Library   Library-name  Para-no   Proc-name"                 
		cr
                proc-link
                begin   @ dup
                        nuf? 0= and
                while   dup .proc 
                repeat  drop cr ;

: t-words ( link -- )
	s" " wpad place
        begin dup dup nuf? not and
        while link>n ?.id @
        repeat 2drop ;                                                                      

: f-words ( -- )
	['] forth 17 + 
	max-thread 0
	do cr cr dup i cells+ @ t-words
	loop drop ;

\ these callback words are for gtk to do callback
\ callback word structure
\ call docallback high-level-word c-ebp 

variable return-p

create ebp-stack 40 cells allot
here value ebp-top
ebp-stack 40 cells erase
		
code docallback ( para return-addr pfa -- )
	ebx pop \ put pfa in ebx to index high level word and c's ebp

	ebp cell [ebx] mov \ save c's ebp
	
	ebp-top # ebp mov \ set up new ebp so it won't mess up c's ebp
			\ this ebp will be the index for return-addr 

	cell # ebp sub
	eax pop
	eax 0 [ebp] mov \ save return address so the para will be on top 

	cell # ebp sub
	ebx 0 [ebp] mov \ save pfa for restoring c's ebp later
	
	return-p # esi mov  \ put return-point adr in esi, so "next" jump to 
			\ return-p after excute high level word

	0 [ebx] eax mov \ high level word cfa
	eax jmp
	
	here return-p ! \ return point after high level word
	
	0 [ebp] ebx mov \ get pfa to ebx 
	cell # ebp add
	
	0 [ebp] eax mov \ get return address back to stack
	eax push  
	cell # ebp add  
	
	cell [ebx] ebp mov \ restore c's ebp

	ret
	c;

: callback: ( cfa | name -- )
	token $,n ['] docallback call, , 0 , overt ; 

code docb
        lods
        eax push
        lods
        eax jmp
        c;
                                          
: [cb'] ( -- )
	' compile docb , ; immediate 
	

\ *******************

\ to conform ans forth

: reposition-file ( ud fileid -- ior )
        nip reposition-file ;

: FILE-POSITION ( fileid -- ud ior )
         file-position 0 swap ;

: FILE-SIZE ( fileid -- ud ior )
	file-size 0 swap ;

: .time cr time&date . . . . . . ;


: fill ( adr n char -- )
	-rot
	bounds
	?do dup i c!
	loop drop ;

: erase ( adr n -- )
	0 fill ;

	 	
