\ procedure to produce new lpforth
\ 1. In working lpforth, run "fload meta.f". It will produce a file called "lpforth.new". 
\    Run "chmod 755 lpforth.new" to make it executable.
\ 2. In shell, run "lpforth.new ", then "fload mkernel.f".
\ 3. Run "fsave lpforth" to produce new working lpforth, or "fsave new-name" to produce a new forth.
\    Again, run "chmod 755 <name>" if necessary.


\ meta compiler, working version save as meta1.f
\ structural conditional version, save as meta2.f, see jmp.f for relative jmp
\ rescued version, save as meta3.f 10/23/98, before change to tk version
\ save as meta4.f before adding callback 10/28/98
\ callback working with error message save as meta5.f 10/29/98
\ save as meta7.f 12/29/98
\ save as meta8.f before add all the stack comment 1/17/99
\ save as meta9.f before making stand alone possible 1/19/99
\ save as meta10.f before strip down lpforth.c 2/4/99
\ save as meta11.f before get rid of open and close 2/4/99
\ save as meta12.f before change to thread versio 2/10/99
\ fail to change to thread version, save as metath.f 2/10/99
\ need to also do threading in metacompiling time
\ revert meta12.f to meta.f
\ thread-no cannot certain number, why?
\ cmove cannot move more than 233 bytes is the reason, how to correct it?
\ thread version ok except above 2/12/99 save as meta13.f
\ try to fix cmove by change brnext, save as meta14.f 2/17/99
\ give up the attempt to keep low level words minimal
\ add low level words to increase efficiency
\ save as meta16.f before adding low level words Sat Apr 8 23:45:15 2000
\ save as meta17.f for soft link Thu Apr 27 00:42:33 2000
\ meta18.f to change i, j, and k to lowlevel
\ meta19.f to change cmove and cmove> to low level
\ add (+loop) to low level words Wed Mar 7 00:34:03 2001


marker meta-m

\ some words to make compatiability, and to use in metacompiling stage 
\ to avoid mixup with meta words

\ some xcall no

1 constant bye-x
2 constant key2-x
3 constant open-x
4 constant close-x
5 constant fopen-x
6 constant fclose-x
7 constant fwrite-x



32 value gap-to-st

: ?keypause nuf? drop ;
: release free ;
assembler definitions
: int $cd c, c, ; \ not in the assembler
: hlt $f4 c, ;
: byebye bye ;
: t' ' ;
: thex hex ;


forth definitions

: h_cells cells ; \ from host, to distinguishf rom target words
: h_cell+ cell+ ;
: h_' ' ;
: h_+ + ;
: h_- - ;
: h_hex hex ;
: toforth forth ;



\ ***************************************************



\ build a elf file to do just two things.
\ 1. emit X to standard output
\ 2. exit the program
\ modify it so it will be similar to pmeta.seq
\ response, but not correct execute version, save as eforth3.f
\  09/04/97 16:21, before change to no offset version
\ save as eforth4.f  09/06/97 01:23, . um+, um* now ok
\ number input and dictionary finding not good yet
\ set a new constant bits-1-t = 31
\ save as eforth5.f, working well except
\ 1. nuf? not functioning normally
\ 2. echo input when it's not necessary
\ 3. backspace working, but visual feedback is wrong
\ three bugs fixed, return send 0xa instead of 0xd, um+ and um* need bits-1-t
\ which is 31 instead of 15, 0< is sar eax, # 31 instead of shr eax, #
\ seperate data stack and return stack
\ try to adapt it to win32 version April 11th, 1998 - 0:40
\ problem with xchg, save as pforth1.f before change dolist April 11th, 1998 - 11:45
\ tx! is working, no good ?rx save as pforth2.f April 11th, 1998 - 22:28
\ c function call works in a way that inout paramater are in the stack
\ and return is in eax, I change xcall to always return eax to stack
\ but I still need to clean the input paramater.
\ now this version is working, interpreter is running.
\ but file i/o is not working well, I can not input pind.f file.
\ save as pforth3.f April 12th, 1998 - 15:03
\ for keyboard _getch() return 13 when RETURN key kit
\ for _putch(), need 10 and 13 to perform
\ for putchar(), 10 is enough to to carriage return
\ use two version of ?rx April 12th, 1998 - 23:44
\ file version, at running well save as pforth4.f April 13th, 1998 - 0:55
\ recover from old version, save as pforth8.f before add 'find
\ April 24th, 1998 - 0:43
\ modify <# # #> to process double word to fit ANSI standard
\ add a lower in token to make every definition lower case
\ save as pforth9.f April 25th, 1998 - 23:37
\ same as win32 version except linux cr-t and ?rx save as lpforth3.f
\ this version now use lowlevel >rx and tx! and works well save as lpforth4.f
\ save as lpforth6.f before change to double number version
\ view field version, save as lpforth7.f
\ save as lpforth8.f before automation
\ order is now working, save as lpforth9.f
\ no ph header version, depend on c wraper, save as lpforth10.f
\ attempt to have metacompiling version
\ produce "lpforth.new" as new execution file

decimal

$805E000 value sys-base   \ starting address  \ ***
\ the address that will exist at run time

: >abs-addr sys-base + ;

0 value out-h

create out-file-str maxstring allot

: create-out-file ( -- )
        s" lpforth.img" delete-file drop
        s" lpforth.img" r/w create-file abort" Error" to out-h ;


0 value target-ptr
$100000 value code-mem-size

: allocate-memory ( -- )
        code-mem-size allocate abort" Error" to target-ptr ;

allocate-memory

: find-pattern ( n -- )
        target-ptr dup code-mem-size + swap
        do i @ over =
                if cr i 10 u.r
                        i target-ptr - sys-base + 10 u.r
                then
        loop drop ;
        

       

: kill target-ptr free . ;

: >t sys-base - target-ptr + ;

: t! ( n adr -- )
        >t ! ;

: t@ ( adr -- n )
        >t @ ;
: tc@ ( adr -- n )
        >t c@ ;


: t_@ ( adr -- n )
        >t @ ;

: t_! ( n adr -- )
        >t ! ;

: t_c@ ( adr -- c )
        >t c@ ;

: t_c! ( c adr -- )
        >t c! ;

: t_w@ ( adr -- w )
        >t w@ ;

: t_w! ( w adr -- )
        >t w! ;

sys-base value t_here

\ extra ************************

: diff ( -- )
	t_here sys-base gap-to-st +
	?do i c@ i >t c@ = not
	     if i . leave
	     then
	loop ;
	
\ *****************************
 

0 value t_last


: t_align ( -- )
        t_here cell mod
        if t_here cell / 1+ cell * to t_here
        then ;

: t_allot ( n -- )
        +to t_here ;

: t_, ( n -- )
        t_here t_!
        cell +to t_here ;

0 value dolit-t

: t_n, ( n -- ) \ put dolit and number
        dolit-t t_,
        t_, ;

: t_c, ( n -- )
        t_here t_c!
        1 +to t_here ;

: t_w, ( w -- )
        t_here t_w!
        2 +to t_here ;

: cmove>t ( from to count -- ) \ move from host to target
        swap >t swap cmove ;

: t_s, ( adr len -- ) \ compile string to target
        t_here swap
        dup t_allot cmove>t ;

\ 0 value link

variable loadfile
0 loadfile !

\ word structure
\ view 4 bytes
\ link 4 bytes
\ name counted string
\ code

\ this view part is only for targeted words

50 constant thread-no

create link-table 0 , thread-no , thread-no cells allot
link-table 2 cells+ thread-no cells erase

: "#hash ( adr len thread-no -- n )
	>r 2dup + 1- c@ 2* + 2* swap c@ + r> mod ;
	
: thread ( adr len voc -- thread-adr )
	cell+ dup @ swap cell+ >r "#hash cells r> + ;

: "head ( addr -- )
        loadfile @ $10000 * loadline @ + t_,  \ view field
        t_here 
        over count link-table thread dup @ t_, 
        !
        t_here to t_last      \ t_last keep last nfa
        count                 \ adr cnt
        dup t_c, 
        dup >r 			\ str length at two end
        t_s, 
        r> t_c, ;


: t_immediate ( -- ) \ set immediate bit to last compiled word
        t_last t_c@ $80 or t_last t_c! ;

: t_compile-only ( -- )
        t_last t_c@ $40 or t_last t_c! ;

0 value start

: begin-cdef here to start ;

: end-cdef ( -- )
        start here over - t_s, ;

: fill-code ( -- )
        target-ptr t_here sys-base - 
        out-h write-file drop ;

: main ( -- )                  \ make final execute file
        create-out-file
        fill-code
        out-h close-file abort" Error" 
	z" cat lpforth.bin lpforth.img > lpforth.new" system drop ;

: check-stack ( -- )
        depth 2 = not
        if release true abort" something wrong"
        then ;

: tname ( name | -- )  \ make link field, fill name, leave cfa as a value
        check-stack
        >in @
        bl word "head
        >in !
        t_here
        create ,
        does> @ t_, ;

: @value ( -- ) \ get the cfa of following word
        ' 9 + @ ;


: t_,call ( -- )   \ ** need tinker
                232 t_C, @value t_HERE cell+ - t_, ;

0 value dolist-t        \ defer this value, fill it after it is defined
0 value exit-t

: t_: ( name | -- )  \ make link field, fill name, leave cfa as a value
        check-stack
        >in @
        bl word "head
        >in !
        t_here
        create , 232 t_C, dolist-t t_HERE cell+ - t_,
        does> @ t_, ;

: t_; ( -- )
        exit-t t_, ;


: offset ( n -- )
        t_here + ;

: t_," ( <string> | -- )
        ascii " parse dup t_c, t_s, ;

: ttype ( nfa  -- )
        dup
        dup tc@ $1f and  \ nfa len
        >r
        1+ >t pad r@ cmove
        1+ r@ + 8 u.r space
        pad r> type ;


\ : twords ( -- )
\        link
\        begin dup 0= not   \ lfa
\        while dup cell+ cr ttype ?keypause t@
\        repeat drop  ;

0 value cfa

\ : t.id ( cfa -- )
\        to cfa
\        link
\        begin dup 0= not   \ lfa
\        while dup cell+ dup dup t_c@ $1f and +  1 + cfa =
\                if ttype drop exit
\                else drop
\                then t_@
\        repeat drop  cfa 8 u.r ;

\ : tsee ( cfa -- )
\        ' 9 + @ 
\        5 +
\        begin cr dup 8 .r space dup t_@ t.id cell+ key 27 =
\        until drop ;

: tvar! ( adr adr' -- )   \ 232(1 byte) dolist(4 bytes) dovar(4 bytes) #
        9 + t! ;


: >marker ( -- | -- )
        create t_here , 0 t_,
        does> @ t_here swap t! ;

: marker ( -- )
        create t_here ,
        does> @ t_, ;

0 value ?branch-t
0 value branch-t
0 value brnext-t


: t_if ( -- adr )  \ leave the address to be filled
        ?branch-t t_, t_here 0 t_, ;   immediate

: t_then ( adr -- ) \ calculate the offset and fill to the adr
        t_here swap t! ;  immediate


: t_else ( adr -- adr' )
        branch-t t_, t_here 0 t_,  \ adr adr'
        swap t_here swap t! ;  immediate


: t_begin ( -- adr )
        t_here ;  immediate

: t_while ( adr -- adr adr ' )
        ?branch-t t_, t_here 0 t_, ; immediate

: t_repeat ( adr adr' -- )
        branch-t t_, swap t_,  \ adr'
        t_here swap t! ;  immediate

: t_again ( adr -- )
        branch-t t_, t_, ; immediate

: t_until ( adr -- )
        ?branch-t t_, t_, ;  immediate

: t_for ( -- adr )
        t_here ;  immediate

: t_next ( adr -- )
        brnext-t t_, t_, ;  immediate


: t_( [compile] ( ; immediate


: t_\ [compile] \ ; immediate



t_\ compiled ok  12/27/96 21:52
t_\ this version is good for simple commands, numbers.
t_\ questions remained for . u. etc  12/30/96 00:50
t_\ . .s solved.
t_\ bug: no value in stack, type . crash system  12/30/96 02:37
t_\ . bug fixed  12/30/96 22:39
t_\ try to fix throw and catch  12/30/96 23:35
t_\ get rid of t_,, include it in tname  01/11/97 14:46
t_\ compile ok and running  01/11/97 14:46 work with pmeta1.seq
t_\ add some t_if, t_then, and t_else  01/11/97 22:57 use pmeta2.seq
t_\ use pmeta3.seq
t_\ use pmeta4.seq, t_:, t_; have been defined. t_,call dolist eliminated
t_\ 01/12/97 14:54
t_\ no user area version, no multiuser design
t_\ name and code are together
t_\ lfa       nfa                          cfa            cfa-list if :
t_\ 2 bytes   length+name (legth+1 bytes)  direct thread
t_\ to show the diferences between fpc and target compiler, replace \ with t_\
t_\ add file redirection ability to load file
t_\ add asciiz to get a standard asciiz string  01/20/97 12:30
t_\ modify .
t_\ save before minimize it, working with redirection to import file
t_\ August 22nd, 1997 - 13:48
t_\ pforth release save as pforth?.seq  08/23/97 21:22
t_\ modified to eforth.f August 30th, 1997 - 1:12
t_\ working well save as pforth5.f April 15th, 1998 - 22:01
t_\ ?key version, works well, I have to rerrange '=' 'not' in order
t_\ for it to work, save pforth6.f April 15th, 1998 - 22:34


vocabulary target

only forth also target also definitions
: to-forth forth ;
: in-dis only forth also target also forth also disassembler also hex ;

$40 value compile-only-t   t_\ all the values end with -t are used for target
$80 value immediate-t

4 value cell-t
10 value base-t
16 value vocs-t
8 value bksp-t
10 value lf-t
10 value cr-t   t_\ for linux, enter return 0xa, strange!
7 value err-t
39 value tic-t
31 value bits-1-t  t_\ bits number minus 1

$e8 value call-t

code-mem-size >abs-addr value em-t      t_\ end of memory
128 cell-t * value rts-t                t_\ return stack size

em-t value up-t                         t_\ user point = end of memory
up-t 8 cell-t * - value rp-t            t_\ return stack pointer
rp-t rts-t - value tib-t                t_\ temperatory input buffer
tib-t 8 cell-t * - value sp-t           t_\ stack pointer

\ $100 value cold-t                     t_\ starting point for com file

0 value h-cold-t

sys-base gap-to-st + t_\ for sp0, rp0, and jump, also some other messages like argc and argv
$2d ( $22 w/o "x" ) +  4 - \ need 4 - in 386
value offset-cold-t  \ *** manually calculated

target-ptr gap-to-st + value orig-addr



t_\ the address for storing the address to high level cold word, manually calculate

t_\ tlabel will compile low level word in fpc then end tlable will move the
t_\ compiled code to target area and update t_here

gap-to-st t_allot                                              \ ***
sys-base value sp-addr     \ hold the original esp       \ ***
4 sys-base + value rp-addr                                   \ ***
8 sys-base + value jmpt-addr
\ 12 sys-base +      is for argc
\ 16 sys-base +      is for argv
\ 20 sys-base + value ebp-addr

allocate-memory

assembler also disassembler also target definitions

\ debug main

: '' ' 9 + @ >t dis ;


code l-orig         t_\ where the forth start, use old esp, relocate ebp
        begin-cdef
        eax pop
        eax pop
        eax pop
        eax pop
        jmpt-addr # ebx mov   t_\ jump table for a few words in c, dlopen, dlsym...
        eax 0 [ebx] mov
        
        esp eax mov
        $e0 # eax add   \ change to $e0 so it won't disturb argv[]
        eax ebp mov
        
        sp-addr # ebx mov
        esp eax mov
        eax 0 [ebx] mov
        rp-addr # ebx mov
        ebp eax mov
        eax 0 [ebx] mov
        cld
        
        h-cold-t # esi mov
        lods
        eax jmp
        0 # ebx mov
        1 # eax mov
        $80 int
        hlt
        c; end-cdef


tname xcall t_( paras... n -- paras... n return )
code l_xcall  
        begin-cdef
        jmpt-addr # ebx mov
        0 [ebx] eax mov
        eax ebx mov
        
        eax pop
        eax 2 shl
        eax ebx add    	t_\ calculate position in jump table

        0 [ebx] eax mov
        eax call	t_\ call it
        
        eax push
        
        lods
        eax jmp
        c; end-cdef
        
        

t_\ tname make a head of following word in the target area,
t_\ and fill the link field

tname noop  t_( -- ) 
code l_noop 
        begin-cdef
        lods
        eax jmp 
        c; end-cdef



tname call t_( para... adr -- para... return ) t_\ the calling engine to c routine
code l_call		t_\ c routine will preserve ebp
        begin-cdef
        
        eax pop
        eax call
        eax push
 
        
        lods
        eax jmp
        c; end-cdef



t_\ delete !io here, because it does not do anything for now

tname dolit t_( -- n )
code l_dolit
        begin-cdef
        lods
        eax push
        lods
        eax jmp
        c; end-cdef    t_compile-only

@value dolit to dolit-t

tname dovalue t_( -- n ) t_\ for "to" to use
code l_dovalue
        begin-cdef
        lods
        eax push
        lods
        eax jmp
        c; end-cdef    t_compile-only


tname dolist t_( addr -- )
code l_dolist
        begin-cdef
        4 # ebp sub
        esi 0 [ebp] mov
        esi pop
        lods
        eax jmp
        c; end-cdef    t_compile-only

@value dolist to dolist-t    t_\ fill it the dolist-t with the cfa of dolist

tname brnext t_( -- )
code l_next
        begin-cdef
       $80 c, $6d c, $00 c, $01 c, \ sub     0 [ebp], # $1
\        $0f c, $82 c, $5 ,          \ jc      @@1
        u< not           
  if    0 [esi] esi mov
        lods
        eax jmp
  then  cell-t # ebp add
        cell-t # esi add
        lods
        eax jmp
       c; end-cdef   t_compile-only

@value brnext to brnext-t

tname ?branch t_( f -- )
code l_?branch
        begin-cdef
        ebx pop
        ebx ebx or
        t_\ $0f c, $84 c, $9 , \ je
        0= not
  if    cell-t # esi add
        lods
        eax jmp
  then  0 [esi] esi mov
        lods
        eax jmp
        c; end-cdef   t_compile-only

@value ?branch to ?branch-t
              
tname branch t_( -- )
code l_branch
        begin-cdef
        0 [esi] esi mov
        lods
        eax jmp
        c; end-cdef   t_compile-only

@value branch to branch-t

tname execute t_( cfa -- )
code l_execute
        begin-cdef
        ebx pop
        ebx jmp
        c; end-cdef

tname exit t_( -- )
code l_exit
        begin-cdef
        0 [ebp] esi mov
        cell-t # ebp add
        lods
        eax jmp
        c; end-cdef

@value exit to exit-t


tname ! t_( n adr -- )
code l_!
        begin-cdef
        ebx pop
        eax pop
        eax 0 [ebx] mov
        lods
        eax jmp
        c; end-cdef

tname @ t_( adr -- n )
code l_@
        begin-cdef
        ebx pop
        0 [ebx] push
        lods
        eax jmp
        c; end-cdef

tname c! t_( c adr -- )
code l_c!
        begin-cdef
        ebx pop
        eax pop
        al 0 [ebx] mov
        lods
        eax jmp
        c; end-cdef

tname c@ t_( adr -- c )
code l_c@
        begin-cdef
        ebx pop
        eax eax xor
        0 [ebx] al mov
        eax push
        lods 
        eax jmp
        c; end-cdef

tname rp@ t_( -- adr )
code l_rp@
        begin-cdef
        ebp push
        lods
        eax jmp
        c; end-cdef

tname rp! t_( adr -- )
code l_rp!
        begin-cdef
        ebp pop
        lods
        eax jmp
        c; end-cdef t_compile-only

tname r> t_( -- n )
code l_r>
        begin-cdef
        0 [ebp] push
        cell-t # ebp add
        lods
        eax jmp
        c; end-cdef t_compile-only

tname r@ t_( -- n )
code l_r@
        begin-cdef
        0 [ebp] push
        lods
        eax jmp
        c; end-cdef

tname >r t_( n -- )
code l_>r
        begin-cdef
        cell-t # ebp sub
        0 [ebp] pop
        lods
        eax jmp
        c; end-cdef  t_compile-only

tname sp@ t_( -- adr )
code l_sp@
        begin-cdef
        esp ebx mov
        ebx push
        lods
        eax jmp
        c; end-cdef

tname sp! t_( adr -- )
code l_sp!
        begin-cdef
        esp pop
        lods
        eax jmp
        c; end-cdef

tname drop t_( n -- )
code l_drop
        begin-cdef
        cell-t # esp add
        lods
        eax jmp
        c; end-cdef

tname  dup t_( n -- n n )
code l_dup
        begin-cdef
        esp ebx mov
        0 [ebx] push
        lods 
        eax jmp
        c; end-cdef

tname swap t_( n1 n2 -- n2 n1 )
code l_swap
        begin-cdef
        ebx pop
        eax pop
        ebx push
        eax push
        lods
        eax jmp
        c; end-cdef

tname over t_( n1 n2 -- n1 n2 n1 )
code l_over
        begin-cdef
        esp ebx mov
        4 [ebx] push
        lods
        eax jmp
        c; end-cdef

tname 0< t_( n -- f )
code l_0<
        begin-cdef
        eax pop
        eax 31 sar
        eax push
        lods
        eax jmp
        c; end-cdef

tname and t_( n1 n2 -- f )
code l_and
        begin-cdef
        ebx pop
        eax pop
        eax ebx and
        ebx push
        lods
        eax jmp
        c; end-cdef

tname or t_( n1 n2 -- f )
code l_or
        begin-cdef
        ebx pop
        eax pop
        eax ebx or
        ebx push
        lods
        eax jmp
        c; end-cdef

tname xor t_( n1 n2 -- f )
code l_xor
        begin-cdef
        ebx pop
        eax pop
        eax ebx xor
        ebx push
        lods
        eax jmp
        c; end-cdef

tname um+ t_( u1 u2 -- d )
code l_um+
        begin-cdef
        eax pop
        ebx pop
        ebx eax add
        eax push
        0 # ebx mov
        ebx ebx adc
        ebx push
        lods
        eax jmp
        c; end-cdef

tname sp0 t_( -- adr )
code l_sp0
        begin-cdef
        sp-addr # eax mov
        eax push
        lods 
        eax jmp
        c; end-cdef

tname rp0 t_( -- adr )
code l_rp0
        begin-cdef
        rp-addr # eax mov
        eax push
        lods 
        eax jmp
        c; end-cdef


t_: bye t_( code -- )
        bye-x t_n, xcall drop drop t_;


tname tx! t_( c -- )  t_\ send char to output device
code l_tx!
        begin-cdef
        4 # eax mov
        1 # ebx mov
        esp ecx mov
        1 # edx mov
        $80 int
        cld
        eax pop
        lods
        eax jmp
        c; end-cdef


t_: = t_( n1 n2 -- f )
	xor
        t_if dolit   0 t_,  t_;
        t_then dolit   true t_,  t_;

t_: not t_( n1 n2 -- f )
	dolit   -1 t_,  xor  t_;

tname ?rx  t_( -- c T | F )     t_\ return char and true, or false if no input
code l_?rx
        begin-cdef
        3 # eax mov
        0 # ebx mov
        ebx push
        esp ecx mov
        1 # edx mov
        $80 int
        cld
        eax neg
        0=
  if    ebx pop
  then  eax push
        lods
        eax jmp
        c; end-cdef

tname (loop) t_( -- )
code l_(loop) 
	begin-cdef
	0 [ebp] inc
	ov not 
	if	
		0 [esi] esi mov
 	else   
		3 cells # ebp add
		cell # esi add
 	then 
	lods
	eax jmp 	
	c; end-cdef

tname (+loop) t_( -- )
code l_(+loop) 
	begin-cdef
	eax pop
	eax 0 [ebp] add
	ov not 
	if	
		0 [esi] esi mov
 	else   
		3 cells # ebp add
		cell # esi add
 	then 
	lods
	eax jmp 	
	c; end-cdef

tname i t_( -- n )
code l_i 
	begin-cdef
	0 [ebp] ebx mov
	cell [ebp] ebx add
	ebx push
	lods eax jmp c; end-cdef

tname j t_( -- n )
code l_j 
	begin-cdef
	3 cells [ebp] ebx mov
	4 cells [ebp] ebx add
	ebx push
	lods 
	eax jmp 
	c; end-cdef

tname k t_( -- n )	
code l_k 
	begin-cdef
	6 cells [ebp] ebx mov
	7 cells [ebp] ebx add
	ebx push
	lods 
	eax jmp 
	c; end-cdef

tname /mod t_( n1 n2 -- rem quot )		
code l_/mod 
	begin-cdef
	ebx pop
	ecx pop
	ecx eax mov
	ebx eax xor
	0<
	if
		ecx eax mov
		edx ecx mov
		cdq
		ebx idiv
		$85 c, $d2 c, \ edx edx test
		0= not 
		if		
			ebx edx add
			eax dec
		then
	else 
		ecx eax mov	
		edx ecx mov
		cdq
		ebx idiv

	then
	edx push
	eax push
	lods 
	eax jmp 
	c; end-cdef


tname compare t_( adr1 len1 adr2 len2 -- f )
code l_compare
	begin-cdef
	2 cells # ebp sub
	edi 0 [ebp] mov
	esi cell [ebp] mov
	ebx pop
	edi pop
	ecx pop
	esi pop
	eax eax sub
	ebx ecx cmp \ compare length
	0= not
	if 	u> not
		if 	eax dec
		else	eax inc
			ebx ecx mov
		then
	then
	repz	
	$a6 c, \ cmpsb
	0= not
	if	0< 
		if 	-1 # eax mov
		else	 1 # eax mov
		then
	then
	eax push
	0 [ebp] edi mov
	cell [ebp] esi mov
	2 cells # ebp add
	lods 
	eax jmp 
	c; end-cdef



t_: dovar t_( -- adr )
	r> t_;         t_compile-only


t_:  break t_( -- )
	t_;  t_\ for debugging


t_: up t_( -- adr )
	dovar up-t t_,

t_: '?key t_( -- adr )
	dovar ?rx

t_: 'emit t_( -- adr )
	dovar tx!

t_: 'expect t_( -- adr )
	dovar 0 t_,     t_\ deferred, accept

t_: 'tap t_( -- adr )
	dovar 0 t_,        t_\ deferred, ktap

t_: 'echo t_( -- adr )
	dovar 0 t_,

t_: 'prompt t_( -- adr )
	dovar 0 t_,    t_\ deferred, usually .ok

t_: base t_( -- adr )
	dovar base-t t_,

t_: tmp t_( -- adr )
	dovar 0 t_,        t_compile-only

t_: span t_( -- adr )
	dovar 0 t_,

t_: >in t_( -- adr )
	dovar 0 t_,

t_: #tib t_( -- adr )
	dovar 0 t_,  tib-t t_,

t_: csp t_( -- adr )
	dovar 0 t_,

t_: 'eval t_( -- adr )
	dovar 0 t_,  t_\ ways to evaluate a token, could be $interpret or $compile

t_: 'number t_( -- adr )
	dovar 0 t_,

t_: hld t_( -- adr )
	dovar 0 t_,

t_: handler t_( -- adr )
	dovar 0 t_,

t_: context t_( -- adr )
	dovar 0 t_, 0 t_, 0 t_, 0 t_, cell-t vocs-t 4 - * t_allot

t_: current t_( -- adr )
	dovar 0 t_,

t_: cp t_( -- adr )
	dovar 0 t_,

t_: last t_( -- adr )
	dovar 0 t_,

t_: #thread t_( -- adr )
	dovar thread-no t_,

t_: ?dup t_( n -- n n | n )
	dup 
	t_if dup 
	t_then  t_;

t_: @execute t_( adr -- )
        @   ?dup
        t_if execute
        t_then  t_;

t_\ added by pai **********

t_: 'hand t_( -- adr )
	dovar 0 t_,

t_: filep t_( -- adr )
	dovar 0 t_,

t_: ?rx2 t_( -- c T | F ) t_\ read() version
        filep @ key2-x t_n, xcall
        swap drop t_\ clear input
        dup 0<
        t_if drop dolit 0 t_,
                'hand @execute
        t_else dolit -1 t_,
        t_then
        t_;

t_: crlf t_( -- adr )
	dovar cr-t t_,

t_: v?rx t_( -- adr )
	dovar ?rx

t_: v?rx2 t_( -- adr )
	dovar ?rx2

t_\ *************

t_: dovoc t_( -- )
	r> context ! t_;     t_compile-only

t_\ link thread link-table
t_: forth t_( -- ) 
	dovar 0 t_, thread-no t_, thread-no h_cells 
	t_allot t_\ forth vocabulary, simple version

t_: rot t_( n1 n2 n3 -- n2 n3 n1 )
	>r swap r> swap t_;

t_: 2drop t_( n1 n2 -- )
	drop drop t_;

t_: 2dup t_( n1 n2 -- n1 n2 n1 n2 )
	over over t_;

t_: + t_( n1 n2 -- n3 )
	um+ drop t_;

t_: negate t_( n -- n' )
	not dolit 1 t_, + t_;

t_: dnegate t_( d -- d' )
        not >r not dolit 1 t_, um+
        r> + t_;

t_: - t_( n -- n' )
	negate   +   t_;

t_: abs t_( n -- n' )
	dup 0<
        t_if negate
        t_then t_;


t_: u< t_( n1 n2 -- f )
	2dup   xor   0<
        t_if swap   drop   0< exit
        t_then -   0<   t_;



t_: < t_( n1 n2 -- f )
	2dup xor 0<                   t_\ equal?
        t_if drop   0<  exit            t_\ if equal, then false
        t_then -   0<   t_;             t_\ if not equal, check diff 0< or not

t_: max t_( n1 n2 -- n )
	2dup   <
        t_if swap
        t_then drop   t_;

t_: min t_( n1 n2 -- n )
	2dup   swap   <
        t_if swap
        t_then drop t_;

t_: within t_( n1 n2 n3 -- f )
	over - >r - r> u< t_;

t_: um/mod t_( ud1 udh un -- ur uq )
        2dup   u<
        t_if negate   dolit bits-1-t t_,  >r
                t_for >r   dup   um+
                        >r   >r   dup   um+
                        r>   +   dup
                        r>   r@   swap   >r
                        um+   r>   or
                        t_if >r   drop   dolit   1 t_,  +   r>
                        t_else drop
                        t_then
                        r>
                t_next drop   swap exit
        t_then drop   2drop dolit   -1 t_,  dup   t_;

t_: m/mod t_( d n -- r q )
        dup   0<   dup   >r
        t_if negate   >r   dnegate   r>
        t_then >r   dup   0<
        t_if r@   +
        t_then r> um/mod   r>
        t_if swap   negate   swap
        t_then t_;


t_: mod t_( n1 n2 -- r )
	/mod drop t_;

t_: / t_( n1 n2 -- q )
	/mod swap drop t_;

t_: um* t_( u1 u2 -- ud )
	0 t_n, swap bits-1-t t_n, >r
        t_for dup um+ >r >r
                dup um+ r> + r>
                t_if >r over um+ r> +
                t_then
        t_next
        rot drop t_;

t_: * t_( n1 n2 -- n3 )
	um* drop t_;

t_: m* t_( n1 n2 -- d )
	over over xor 0< >r
        abs swap abs um* r>
        t_if dnegate
        t_then t_;

t_: */mod t_( n1 n2 n3 -- r q )
	>r m* r> m/mod t_;

t_: */ t_( n1 n2 n3 -- q )
	*/mod swap drop t_;

t_: cell+ t_( adr -- adr' )
	cell-t t_n, + t_;

t_: cell- t_( adr -- adr' )
	-4 t_n, + t_;

t_: cells t_( adr n -- adr' )
	cell-t t_n, * t_;

t_: aligned t_( -- ) t_\ not used in this version
	t_;

t_: bl t_( -- n )
	32 t_n, t_;

t_: >char t_( n -- n' )
	$7f t_n, and dup
        127 t_n, bl within
        t_if drop ascii . t_n,
        t_then t_;

t_: depth t_( -- n )
	sp@ sp0 @ swap -
        cell-t t_n, / t_;

t_: pick t_( n -- n' )
	1 t_n, + cells sp@ + @ t_;

t_: +! t_( n adr -- )
	swap over @ +
        swap ! t_;

t_: 2! t_( d adr -- )
	swap over !
        cell+ ! t_;

t_: 2@ t_( adr -- d )
	dup cell+ @
        swap @ t_;

t_: count t_( adr -- adr+1 len )
        dup 1 t_n, + swap c@ t_;

t_: here t_( -- adr )
	cp @ t_;

t_: pad t_( -- adr )
	here $80 t_n, + t_;

t_: tpad t_( -- adr )
	here $40 t_n, + t_;

t_: tib t_( -- adr )
	#tib cell+ @ t_;

tname cmove t_( from to cnt -- )
code l_cmove
	begin-cdef
	ecx pop
	esi eax mov
	edi ebx mov
	edi pop
	esi pop
	$f2 c,  t_\ repnz
	$a4 c,  t_\ movsb
	eax esi mov
	ebx edi mov
	lods
	eax jmp
	c; end-cdef

tname cmove> ( from to cnt -- )
code l_cmove>
	begin-cdef
	ecx pop
	esi eax mov
	edi ebx mov
	ecx dec
	edi pop
	esi pop
	ecx edi add
	ecx esi add
	ecx inc
	std
	$f2 c,
	$a4 c,
	cld
	eax esi mov
	ebx edi mov
	lods
	eax jmp
	c; end-cdef


t_: fill t_( adr n char -- )
        swap >r swap
        branch >marker fill1:
        t_for 2dup c! 1 t_n,  +
        fill1:
        t_next 2drop t_;

t_: -trailing t_( adr len -- adr' len' )
        >r
        branch >marker tr1:
        t_for bl over r@ + c@ <
                t_if r> 1 t_n, + exit
                t_then
        tr1:
        t_next 0 t_n, t_;

t_: pack$ t_( adr len adr' -- adr' )
        >r dup r@ c!
        r@ 1 t_n, +
        swap cmove r> t_;

t_: digit t_( n -- char )
        9 t_n, over <  t_\ if > 9 then skip 7 ascii code between 9 and A
        7 t_n, and +
        ascii 0 t_n, + t_;

t_: extract  t_( d base -- d' char )
        um/mod
        0 t_n, rot digit t_;

t_: <#  t_( -- )
        pad hld ! t_;

t_: hold t_( char -- ) t_\ put char to hld
        hld @ 1 t_n, -
        dup hld ! c! t_;

t_: #  t_( d -- d' )  t_\ extract one digit
        base @ extract hold t_;

t_: #s t_( d -- d' )
        t_begin # over 0 t_n, =
        t_until t_;

t_: sign t_( d -- d' )
        0<
        t_if ascii - t_n, hold
        t_then t_;

t_: #> t_( d -- adr len )
        drop drop hld @
        pad over - t_;

t_: str t_( n -- adr len )
        dup >r abs 0 t_n,
        <# #s r> sign #> t_;

t_: hex t_( -- )
        16 t_n, base ! t_;

t_: decimal t_( -- )
        10 t_n, base ! t_;

t_\  DIGIT?      ( c base -- u t )
t_\  um*onvert a character to its numeric value. A flag indicates success.

t_: digit? t_( c base -- u t )
        >r ascii 0 t_n, -
        9 t_n, over <
        t_if 7 t_n, -
                dup 10 t_n, < or
        t_then dup r> u< t_;


t_\ v8 {
t_: d* t_( l h n -- d' )
        dup >r swap >r 
        um* r> + r> um* drop t_;

t_: -rot t_( a b c -- c a b )
	swap rot swap t_;

t_: /string t_( adr len n -- adr' len' )
        dup >r - swap r> + swap t_;

t_: d+ t_( d1 d2  -- d3 )
	>r rot + swap r> + t_;

t_: >number t_( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
        t_begin over c@ base @ digit? >r over r> and
        t_while -rot >r >r >r base @ d* r> 0 t_n, d+ r> r> 1 t_n, /string
        t_repeat drop t_;

t_: dp-location t_( -- adr )
	dovar 0 t_,

t_: double? t_( -- adr )
	dovar 0 t_,

t_: > t_( n1 n2 -- f )
	negate swap negate swap < t_;
       
t_: (number?) t_( addr len -- d1 f1 )
       	0 t_n, double? !
        -1 t_n, dp-location !
        over c@ 45 t_n, = t_\ ascii - =
        over 0 t_n, > and dup >r
        t_if 1 t_n,  /string
        t_then
        dup 0 t_n, = 
        t_if r> drop 0 t_n, t_;
        t_then
        0 t_n, 0 t_n, rot >r rot r> >number
        over c@ 46 t_n, = t_\ ascii . =
        over 0 t_n, > and
        t_if dup 1 t_n, - dp-location !
                 1 t_n, /string >number
                 dup 0 t_n, =
                 t_if -1 t_n, double? !
                 t_then
        t_then swap drop 0 t_n, =
        r> 
        t_if >r dnegate r>
        t_then t_;

t_: number? t_( adr -- d1 true | adr false )
        dup >r
        count (number?)
        t_if r> drop -1 t_n,
        t_else drop drop r> 0 t_n,
        t_then t_;

t_\ v8 }
       
t_: 'also-do t_( -- adr ) t_\ buries in ?key to take the ride
	dovar noop

t_: ?key t_( -- char true | false )
        '?key @execute 'also-do @execute t_;

t_: key t_( -- char )
        t_begin ?key
        t_until t_;

t_: emit t_( char -- )
        'emit @execute t_;


t_: nuf? t_( -- f )
        ?key dup
        t_if 2drop key cr-t t_n, =
        t_then t_;

t_: pace t_\ for file use
        t_;
        
t_\        dolit 46 t_, emit t_; t_\ show progress

t_: space t_( -- )
        bl emit t_;

t_: spaces t_( n -- )
        0 t_n, max >r
        branch >marker spaces1:
        t_for space
        spaces1:
        t_next t_;

t_: type t_( adr len -- )
        dup
        t_if >r branch >marker type1:
                t_for dup c@ emit 1 t_n,  +
                type1:
                t_next drop t_;
        t_then 2drop t_;

t_: cr t_( -- )
        lf-t t_n, emit   t_\ in linux $a is enough to produce cr
t_\        dolit cr-t t_,  emit
        t_;

t_: do$ t_( -- )
        r> r@ r> count +
        aligned >r swap >r t_;   t_compile-only

t_: $"| t_( -- )
        do$ t_;     t_compile-only

t_: ."| t_( -- )
        do$ count type t_;   t_compile-only

t_: .r t_( n1 n2 -- )
        >r str r> over -
        spaces type t_;

t_: u.r t_( u n -- )
        >r 0 t_n, <# #s #>
        r> over -
        spaces type t_;

t_: u. t_( n -- )
        0 t_n, <# #s #>
        type space t_;

t_: . t_( n -- )
        base @ 10 t_n, xor t_\ if not decimal
        t_if u.
        t_else str type space
        t_then t_;

t_: (parse) t_( adr len c -- adr' len' delta | <string> )
        tmp ! over >r dup       t_( adr len len )
        t_if 1 t_n, - tmp @ bl =  t_( adr len-1 f )
                t_if >r marker (parse)1: bl over c@
                        - 0< not
                        t_if 1 t_n, +
                                brnext (parse)1:  t_\ take out leading space
                                r> over swap -  0 t_n, swap t_;
                        t_then r>
                t_then over swap
                >r
                marker (parse)2: tmp @ over c@ -
                tmp @ bl =
                t_if 0<
                t_then
                t_if 1 t_n, +
                        brnext (parse)2:
                        dup >r
                        branch >marker (parse)3:
                t_then r> drop dup
                1 t_n, + >r
                (parse)3: over -
                r> r> -  t_;
        t_then over r> - t_;

t_: parse t_( c -- adr len | <string> )
        >r tib >in @ +
        #tib @ >in @ - r>       t_( adr len c )
        (parse) >in +! t_;

t_: char t_\ similar to ascii
        bl parse drop c@ t_;

t_: 'lower dovar drop

t_: token t_( -- adr | <string> )
        bl parse dolit 31 t_, min
        tpad pack$
        tpad 'lower @execute
        t_;

t_: word t_( char -- adr )
        parse here pack$ t_;

t_: name> t_( nfa -- cfa )
        dup c@ $1f t_n,  and
        2 t_n, + + t_;

t_: same? t_( adra adr2 len -- adr1 adr2 f )
        >r
        branch >marker same?1:
        t_for over r@ + c@
                over r@ + c@
                -  ?dup
                t_if >r 2drop r> r> drop t_;
                t_then
        same?1:
        t_next 2drop 0 t_n, t_;

t_: 'same? t_( -- adr ) \ -1 | 0 | 1
        dovar same?
        
t_: "#hash ( adr len thread-no -- n )
	>r 2dup + 1 t_n, - c@ 2 t_n, * + 2 t_n, * swap c@ + r> mod t_;

t_: thread t_( adr len voc -- thread-adr )
	cell+ dup @ swap cell+ >r "#hash cells r> + t_;

t_: find-a-voc t_( adr voc-addr -- nfa true | adr false )
	over count rot thread 
	swap dup c@ tmp ! 1 t_n, + swap
	t_begin @ dup
	t_while 2dup
		cell+ dup 1 t_n, + swap c@ $1f t_n, and dup tmp @ =
		t_if 'same? @execute 0 t_n, =
			t_if swap drop cell+ -1 t_n, t_;
			t_then
		t_else drop 2drop
		t_then
	t_repeat drop 1 t_n, - 0 t_n, t_;
	
	 
t_: find t_( adr -- nfa true | adr false )
        context 
        t_begin 2dup @ dup
        t_while find-a-voc 
       		t_if >r 2drop r> -1 t_n, t_;
        	t_else drop
        	t_then cell+
        t_repeat 2drop drop 0 t_n, t_;

t_: 'find t_( -- adr )
	dovar find

t_: name? t_( adr -- nfa true | adr false ) t_\ in dictionary?
        'find @execute t_;

t_: ^h t_( bt et pt -- bt et pt' )               t_\ backspace
        >r over r> swap over xor                 t_\ bt <> pt, -- bt et pt
        t_if bksp-t t_n, 'echo @execute     t_\ perform backspace
                1 t_n, - bl 'echo @execute  t_\ erase
                bksp-t t_n, 'echo @execute  t_\ back again
        t_then t_;

t_: tap t_( bt et pt c -- bt et pt' )  t_\ accept and echo the key and bump cursor
        dup 'echo @execute             t_\ emit it out
        over c! 1 t_n, + t_;      t_\ store to tib and add pointer

t_: ktap t_( bt et pt c -- bt et pt' ) t_\ special keys
        dup crlf @ xor         t_\ not cr, ** special place for ?rx to work
        t_if 127 t_n, xor     t_\ not backspace, here is 127 from linux 2.0
                t_if bl tap t_;
                t_then ^h t_;          t_\ backspace
        t_then drop swap drop dup t_;  t_\ cr, -- bt pt pt

t_: in-esc? t_( -- adr )
	dovar 0 t_,

t_: accept t_( adr len -- adr len' )
        over + over
        t_begin 2dup xor
        t_while key dup 
                bl 127 t_n, within in-esc? @ not and
                t_if tap                t_\ if printable
                t_else 'tap @execute    t_\ deferred word, usually ktap
                t_then
        t_repeat
        drop over - t_;

t_: expect t_( adr len -- )             t_\ store input to adr, len to span
        'expect @execute span ! drop t_;

t_: query t_( -- ) t_\ accept input to tib
        tib 270 t_n, 'expect @execute #tib !
        drop 0 t_n, >in ! t_;        t_\ put index >in to 0
                                          t_\ len to #tib

t_: catch t_( cfa -- 0 | err# )
        sp@ >r handler @ >r rp@ handler ! t_\ set up frame
        execute
        r> handler !                      t_\ restore frame, no error
        r> drop dolit 0 t_, t_;

t_\ throw will restore the frame setup by catch
t_\ if throw happen, leave an err# for quit to process

t_: throw  t_( err# -- err# ) t_\ reset system to frame setup before
        handler @ rp!
        r> handler !
        r> swap >r  t_\ keep the error code, because going to reset sp
        sp!         t_\ get back the original cfa
        drop r> cr t_;  t_\ drop cfa, keep the error code

t_: null$ t_( -- adr )
        dovar 
        6 t_c, 99 t_c, 111 t_c, 121 t_c, 111 t_c,
        116 t_c, 101 t_c,

t_: abort t_( -- )
        null$ throw

t_: (abort") t_( f -- )
        t_if do$ throw
        t_then do$ drop t_;    t_compile-only

t_: $interpret t_( adr -- )
        name?  t_\ ( nfa true | adr false ) in dictionary?
        t_if dup c@ compile-only-t t_n, and
                (abort") t_," compile only "
                name> execute t_;
        t_then 'number @execute
        t_if double? @ not  t_\ v8
                t_if drop   t_\ v8
                t_then t_;  t_\ v8
        t_then throw   t_\ go back to the frame setup in quit

t_: state t_( -- adr )
	dovar 0 t_,

t_: [ t_( -- )
        0 t_n, state ! 
        dolit $interpret 'eval ! t_;    t_immediate

t_: .ok t_( -- )
        state @ 0 t_n, =
        t_if ."| t_," ok"
        t_then 
        cr
        t_;

t_: ?stack t_( -- )
        depth 0<
        (abort")
        t_," underflow "
        t_;

t_: loadfile t_( -- adr )
	dovar 0 t_,  t_\ v12
t_: loadline t_( -- adr )
	dovar 0 t_,  t_\ v12

t_: eval t_( -- )   t_\ evaluate the input stream in tib
        t_begin token dup c@          t_( adr n )
        t_while 'eval @execute ?stack
        t_repeat drop 'prompt @execute 
        1 t_n, loadline @ + loadline !   t_\ v12
        t_;

t_: preset t_( -- )
        sp0 @ sp!
        tib-t t_n, #tib cell+ !
        t_;

t_: 'default-accept t_( -- adr )
	dovar accept

t_: xio t_( prompt echo tap -- )
        'default-accept @ 'expect !
        'tap ! 'echo ! 'prompt !
        t_;    t_compile-only

t_: file t_( -- )                      t_\ prompt pace, no echo, ktap normal
        dolit pace dolit drop
        dolit ktap xio
        lf-t t_n, crlf !
        v?rx2 @ '?key !
        t_;

t_: hand t_( -- )               t_\ set up i/o vectors
        dolit .ok dolit emit    t_\ prompt ok, do echo, ktap normal
        dolit ktap xio
        cr-t t_n, crlf !
        v?rx @ '?key !
        filep @ ?dup 
        t_if close-x t_n, xcall drop drop 0 t_n, filep ! cr
        t_then t_\ somehow in linux if try to close 0, it produce some keyboard input
        t_;

t_: i/o t_( -- adr )
        dovar ?rx tx!

t_: console t_( -- )
        i/o 2@ '?key ! 'emit !
        hand t_;

t_: quit t_( -- ) t_\ start every thing all over
        rp0 @ rp!
        t_begin [                          t_\ change state to interpreter
                t_begin query space             t_\ to tib
                        dolit eval
                        catch ?dup         t_\ error?
                t_until
                'prompt @ >r               t_\ if error happened
                console null$ over xor     t_\ err# <> null$ addr
                t_if space count type ."| t_,"  ?"
                t_then r> dolit .ok xor    t_\ prompt <> .ok, means file condition
                t_if err-t t_n, emit  t_\ ring bell
                t_then preset
        t_again

t_: ' t_( | name --  cfa )
        token name?
        t_if dup c@ 31 t_n, and 2 t_n,
                + + t_;
        t_then throw

t_: allot t_( n -- )
        cp +! t_;

t_: , t_( n -- )
        here dup cell+
        cp ! ! t_;

t_: c, t_( c -- )
        here dup 1 t_n,  +
        cp ! c! t_;

t_: [compile] t_( -- )
        '   ,   t_;       t_immediate

t_: compile t_( -- )
        r> dup @ ,
        cell+ >r t_;   t_compile-only

t_: literal t_( n -- )
        compile dolit , t_;     t_immediate

t_: ?unique t_( adr -- adr )
        dup name?
        t_if ."| t_," reDef "
                over count type space space
        t_then drop t_;

t_\ for view, all the new defined words

t_: view, t_( -- )
        loadfile @ $10000 t_n, * loadline @ + , t_;
        
t_: last-thread-adr dovar 0 t_,        

t_: $,n t_( adr -- )
        dup c@
        t_if ?unique
                view, \ for view
                dup count current @ thread dup last-thread-adr ! @ ,
                here last !
                dup c@ dup >r 1 t_n, + >r
                here r@ cmove r> allot
                r> c,
                t_;
        t_then $"|
        t_," name"
        throw

t_: $compile t_( adr -- )
        name?
        t_if dup name> swap c@
                immediate-t t_n, and
                t_if execute t_;
                t_else , t_;
                t_then
        t_then
        'number @execute
        t_if double? @
                t_if swap literal   t_\ v8
                t_else drop    t_\ v8
                t_then literal t_;
        t_then throw


t_: overt t_( -- )
        last @ cell- last-thread-adr @ ! t_;

t_: ; t_( -- )
        compile t_; [ overt t_;  t_compile-only t_immediate

t_: ] t_( -- )
        -1 t_n, state !
        dolit $compile 'eval ! t_;

t_: call, t_( cfa -- )
        call-t t_n, c, here
        cell+ - , t_;

t_: : t_( -- )
        token $,n dolit dolist
        call, ] t_;


t_: hi t_( -- )
        cr ."|
        t_," lpforth by Pai"
        cr t_;

t_: 'boot t_( --  adr )
        dovar hi

t_: set-context t_( -- )
	forth context ! context @ current ! 
	t_;

t_: 'set-context t_( -- adr )
	dovar set-context

t_: cold t_( -- )               t_\ high level cold word
        t_here to h-cold-t
        dolit hand 'hand !
        t_begin
                preset
                'set-context @execute 
                rp0 @ rp!
                'boot @execute 
                quit
        t_again

t_: fopen t_( para r/w adr -- filep )
        open-x t_n, xcall >r drop drop drop r> t_;

t_: fclose t_( filep -- f )
        close-x t_n, xcall swap drop t_;

t_: fload t_( | file-name -- )
        bl word
        dup count + 0 t_n, swap !
        count drop >r 0 t_n, 0 t_n, r> fopen filep !
        0 t_n, loadline ! t_\ for view
        file t_;

h-cold-t offset-cold-t t!  t_\ fill the address for high level cold word

@value accept @value 'expect tvar!
@value ktap @value 'tap tvar!
@value tx! @value 'echo tvar!  t_\ change for win32
@value .ok @value 'prompt tvar!
@value $interpret @value 'eval tvar!
@value number? @value 'number tvar!
t_here @value cp tvar!
t_last @value last tvar!

link-table @value forth 9 h_+ thread-no h_cells h_cell+ h_cell+ cmove>t

main

byebye


