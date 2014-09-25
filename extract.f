\ extract.f
\ This utility will let you make a smaller executable which contains only the words needed for your program.
\ It should work in normal cases. However, if you have some cleaver implementation it didn't take care, you 
\ need to modify this program to make it work for you. Source is all here.



\ save as extract1.f before chenge to no ncall version
\ the way it works:
\ 1. Make a table of all the words with starting address and end address
\ 2. Mark only the words used.
\ 3. Move the used word to a target space, replace old cfas with new cfas.
\ 4. Strings and condition jumps need to be done differently.
\ ." Hello world!!" working, save as extract2.f Mon Mar 27 01:40:46 2000
\ working for number type out. create does> problem need to be solved
\ save extract3.f Tue Mar 28 14:10:40 2000
\ can mark does> target save as extract4.f Wed Mar 29 13:32:33 2000
\ working even when there is create does>, save as extract5.f Thu Mar 30 18:54:12 2000
\ the execute-special is ugly, need to improve it. Done, save as extract6.f Thu Mar 30 20:34:06 2000
\ somehow calling to .so files is working without initialization of libraries and procs
\ save as extract7.f, Fri Mar 31 12:19:42 2000
\ library and proc have been worked out, need to init both before it can run
\ also callback is now working
\ working with gtk2.1.f, save as extract8.f
\ try to clean up extract. Make it a one word utility.
\ indicate target name to save 
\ save as extract9.f before change Sun Apr 30 11:50:27 2000
\ save as extract10.f before adding xref Sun Apr 30 18:32:21 2000
\ xref ok, save as extract11.f before adding defer ability Mon May 1 01:11:13 2000
\ fix list-marked2 bug, because forget to allot after command-buf
\ save as extract12.f Sat May 6 17:29:29 2000
\ multiple targets ok, save as extract13.f Sat May 6 17:52:23 2000
\ this version can target a word without lbye, lbye is offered by target-main
\ save as extract14.f Sat May 6 18:54:40 2000
\ to do:  
\ 	1. Setup a template for the word need to init libraries.
\ 	2. Make a target without c wrapper if no library call is used.
\	3. Integrate extract.f as a part of system.
\ ok to automaticly determine if libraries are used, make different targets
\ save as extract15.f Sat May 6 19:16:17 2000 
\ save as extract16.f before trying to get rid of c wrapper Sun May 7 23:46:23 2000




marker e-m

: int $cd c, c, ; \ not in the assembler

code lbye
	0 # ebx mov
	1 # eax mov
	$80 int
	next
	c;


hex


: list ( -- )
	0 0 min-time
        cr context @
        begin @ dup 
        while cr dup cell- 9 .r space dup cell+ .id  
        repeat drop 
        0 1 min-time 
	;


: marked? ( cfa -- f )
	>view @ $80000000 and 0= not ;	  

: mark-used ( cfa -- )
	dup marked? not
	if dup >name space .id
		>view dup @ $80000000 or swap ! 
	else drop
	then ;
	

: list? ( cfa -- f )
	1+ dup cell+ swap @ + ['] dolist = ;

0 value inf
0 value cfa-table
0 value total-cfa
0 value cfa-index
0 value total-words

: count-lines ( -- n )
	0 to total-cfa
	0 0 inf reposition-file drop
	begin pad maxstring inf read-line abort" count-lines" nip
	while 1 +to total-cfa
	repeat ;

	
: fill-table ( -- )
	hex 0 to cfa-index
	0 0 inf reposition-file drop
	begin tib maxstring inf read-line abort" count-lines" 
	while #tib ! 0 >in ! bl word number? 
		if drop cfa-table cfa-index cells+ ! 1 +to cfa-index
		else 2drop
		then
	repeat 
	here cfa-table cfa-index cells+ ! 
	1 +to cfa-index 
	cfa-index to total-cfa cfa-index to total-words
	#tib @ >in ! ;

: refresh ( ptr -- )
	?dup 
	if free drop
	then ;	
	
: build-table ( -- )
	s" allword.srt" r/w open-file drop to inf
	count-lines
	cfa-table refresh
	total-cfa cells allocate abort" build-table" to cfa-table
	fill-table 
	inf close-file drop ;
	
0 value end-adr

: get-end-adr ( cfa -- adr )
        false swap
	total-words 0
        ?do cfa-table i cells+ @ over >
        	if swap drop cfa-table i cells+ @ swap leave
        	then
        loop drop ;

0 value cur-list
create @execute-list 10 cells allot
0 value @execute-cnt

: add-@execute ( cfa -- )
	@execute-list @execute-cnt cells+ !
	1 +to @execute-cnt ;

: in-@execute-list? ( cfa -- f ) 
	false swap
	@execute-cnt 0
	?do @execute-list i cells+ @ over = 
		if nip true swap leave
		then
	loop drop ;

defer @execute-alert

: in-defer? ( ip -- f )
	['] defer dup get-end-adr between ;
 
: @execute-special-cfa ( ip -- cfa ip ) \ leave cfa if necessaty
     	cr ." ****** @execute alert in " cur-list >name .id cr 
	dup in-defer? not
	if dup 2 cells- @ \ get cfa of the variable
     		>body cell+ @  \ get the content of variable
		dup add-@execute
     		swap 
	then ;

: @execute-special ( ip -- ip ) \ for just checking if (;code) exist
     	cr ." ****** @execute alert in " cur-list >name .id cr ;

defer execute-special

: (execute-special) ( ip -- ip )
	cr ." ****** execute alert in " cur-list >name .id cr ;


disassembler
\ for marking of cb-delete, don't forward, so cb-delete will be marked

\ ' (loop) value old-(loop)
\ this is for adding low level (loop)

				
: forward-ip ( ip cfa -- ip' ) \ for marking
	case
     		['] dovar    of  drop end-adr endof
     		['] dolit    of  cell+  endof
		['] dovalue  of  cell+  endof 
     		['] compile  of  cell+  endof
\     		['] (ncall)  of  cell+  endof
     		['] branch   of  cell+  endof
     		['] ?branch  of  cell+  endof
     		['] brnext   of  cell+  endof
     		['] (do)     of  cell+  endof
     		['] (?do)    of  cell+  endof
     		['] (loop)   of  cell+  endof
\     		old-(loop)   of  cell+  endof
     		['] (+loop)  of  cell+  endof 
     		['] _of      of  cell+  endof
     		['] _endof   of  cell+  endof
     		['] (")      of  count + 1+ endof
     		['] (.")     of  count + 1+ endof
     		['] (z")     of  count + 1+ endof
     		['] (abort") of  count + endof
     		['] ."|      of  count + endof
     		['] $"|      of  count + endof
     		['] (.s")     of count + 1+ endof 
     		['] @execute of @execute-alert endof
     		['] execute of  execute-special endof
   	endcase ;

: forward-ip-mark ( ip cfa -- ip' )
	['] @execute-special-cfa is @execute-alert 
	['] (execute-special) is execute-special
	forward-ip ; 
	
: forward-ip-mark-does> ( ip cfa -- ip' ) 
	['] @execute-special is @execute-alert 
	['] (execute-special) is execute-special
	forward-ip ; 

: forward-ip-look ( ip cfa -- ip' ) 
	['] noop is @execute-alert 
	['] noop is execute-special
	forward-ip ; 


  	
forth   	

: breaker ;

: mark-list ( cfa -- cfa1 cfa2 ... )
	['] dolist mark-used
	dup to cur-list
	dup get-end-adr to end-adr
	>body 
	begin dup end-adr = not
	while dup @ 
\ dup $8080419 = if ." got you in mark-list " breaker then
	swap cell+ over forward-ip-mark
	repeat drop ;

: callback-word? ( cfa -- f )
	call-dest ['] docallback = ;

: mark-callback-word ( cfa -- cfa1 cfa2 ... ) \ take case of marking delete, and docallback
	['] docallback mark-used \ take case of docallback
	>body @ ;

: create-does>? ( cfa -- f )
	dup to cur-list
	dup get-end-adr to end-adr
	>body 
	begin dup end-adr = not
	while dup @ ['] (;code) =
		if drop true exit
		else dup @ swap cell+ swap forward-ip-mark-does>
		then
	repeat drop false ;

: does>cfa ( does -- cfa )
        total-cfa 0
        ?do cfa-table i cells+ @ over >
        	if cfa-table i 1- cells+ @ view> leave
        	then
        loop nip ;


: mark-create-does> ( cfa -- cfa1 cfa2 ... )
	dup to cur-list
	['] dovar mark-used
	1+ dup cell+ swap @ + \ call destination
	dup does>cfa mark-used
	dup get-end-adr to end-adr
	>body 
	begin dup end-adr = not
	while dup @ 
\ dup $8080419 = if ." got you in mark-create-does> " breaker then
 	swap cell+ over forward-ip-mark
	repeat drop ;

: does>product? ( cfa -- f )
	call-dest dup sys-base here between
	if cell- @ ['] (;code) =
	else drop false
	then ;

defer mark

: defer? ( cfa -- f )
	call-dest ['] mark call-dest = ;
	
: (mark) ( cfa -- )
\ dup sys-base < if .s cur-list >name .id true abort" small cfa" then
\ cr .s
	dup marked? 
	if drop exit then
	dup mark-used
	dup list?
	if mark-list exit then
	dup does>product? \ if there is does> word product, need to mark the original does> word
	if dup>r mark-create-does> 
		r> dup defer?
		if 
\ ." mark defer" key drop
			9 + @ mark 
		else drop
		then
		exit 
	then
	dup callback-word?
	if mark-callback-word exit then \ go ahead and mark docallback and delete
	drop 
	;

' (mark) is mark

: iterate ( ... cfa -- )
	begin dup
	while mark
	repeat drop ;

: handle>cfa ( adr -- adr' )
	3 cells- 1- ;

: link>libpointer ( adr -- adr' )
	2 cells+ ;

: link>cfa ( adr -- adr' )
	2 cells- 1- ;

: mark-library ( -- ) \ mark all library if any library is used
	['] link>content marked?
	if
		['] library mark-used
                cr
                lib-link
                begin   @ dup
                while   dup link>cfa dup >name .id mark-used
                repeat  drop cr 
	then ;

: docallback-special ( -- )
	['] docb marked?
	if 
		['] ebp-top mark-used
		['] ebp-stack mark-used
		['] return-p mark-used 
	then ;

	
: textract ( | name -- )
	0 ' iterate 
	mark-library 
	docallback-special ;

defer do-word ( lfa -- )

: scan-thread ( thread -- )
	begin @ ?dup
	while dup do-word ?keypause
	repeat ;
	
: scan-voc ( voc -- )
	dup link>thread-no @ 0
	do dup link>table i cells+ scan-thread
	loop drop ;
	

: scan-words ( -- )
	fast
	voc-link
	begin @ ?dup
	while dup scan-voc
	repeat
	slow ;


0 value total-space

: list-marked-word ( lfa -- )
	dup link> marked? 
	if cr \ dup cell- 9 .r space \ vfa
        		dup link> 9 .r space \ cfa
        		dup link> get-end-adr 9 .r space \ end-adr
        		dup link> get-end-adr \ end-adr
         		over link> \ cfa
       			- dup 5 .r space 
       			+to total-space
        		dup cell+ .id 
 	then drop ;
	

: list-marked ( -- )
cr ." *********************"
cr ." List used words" cr
	fast
	0 to total-space
	['] list-marked-word is do-word
	scan-words
        slow 
        cr ." Total space needed: " total-space . ;


0 value outf
\ create file-pad 1 allot

: file-emit ( c -- )
	dup tx!
	file-pad c! 
	file-pad 1 outf write-file abort" file-emit" ;
                          
: lm list-marked ;

: list-word ( lfa -- )
	cr dup link>v 9 .r space link>n .id ;
	 
: list-all ( -- )
	base @ >r hex
	s" allword.out" new-file abort" list-all" to outf
	fast 0 to word-count
	'emit @ >r ['] file-emit 'emit !
	['] list-word is do-word
	scan-words
       	r> 'emit !
	.word-displayed
	slow 
	outf close-file abort" close- error" 
	z" sort <allword.out >allword.srt" os 
	build-table 
	r> base ! ;

0 value xref-cfa

: cfa-exist? ( lfa -- )
	false over
	link> dup to cur-list
	dup get-end-adr to end-adr
	>body 
	begin dup end-adr <
	while dup @ dup xref-cfa =
		if rot drop true -rot
		then swap cell+ swap forward-ip-look
	repeat drop 
\ cr .s
	if link> >name .id space
	else drop
	then
	;


: (xref) ( cfa -- )
	to xref-cfa
	cfa-table 0=
	if list-all
	then 
	['] cfa-exist? is do-word
	fast cr
	scan-words
        slow ;

: xref ( -- )
	' (xref) ;

\ debug list-all
 hex  
\ list-all
 
decimal

: unmark ( lfa -- )
	link>v dup @ $80000000 not and swap ! ;
	
: clear-mark ( -- )
	['] unmark is do-word
	scan-words ;

: cm clear-mark ;


: list-marked2 ( -- )
	s" aneeded.out" new-file abort" open error" to outf
	fast
	'emit @ >r ['] file-emit 'emit !
	['] list-marked-word is do-word
	scan-words
       	r> 'emit !
	slow 
	outf close-file abort" close- error" 
	z" sort <aneeded.out >aneeded.srt" os 
	;

: lm2 list-marked2 ;

\ purify table 
\ cell 		old cfa
\ cell 		old end adr
\ cell 		new cfa

3 constant pu-cells

: >old-end cell+ ;
: >new-cfa 2 cells+ ;
create out-pad maxstring allot

0 value purify-table

: fill-purify-table ( -- )
	hex 0 to cfa-index
	0 0 inf reposition-file drop
	begin tib maxstring inf read-line abort" fill-table" 
	while #tib ! 0 >in ! bl word number? 
		if drop purify-table cfa-index pu-cells * cells+ dup>r ! 
			bl word number? 
			if drop r@ >old-end ! 0 r> >new-cfa !
			else 2drop
			then
			1 +to cfa-index
		else 2drop
		then
	repeat 
	cfa-index to total-cfa 
	#tib @ >in ! ;

: build-pu-table ( -- )
	s" aneeded.srt" r/w open-file drop to inf
	purify-table refresh
	count-lines
	total-cfa pu-cells * cells allocate abort" build-table" to purify-table
	fill-purify-table 
	inf close-file drop ;


0 value target-ptr

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

: build-head ( -- )
	sys-base $5c t_s, ;

: old-cfa ( i -- cfa )
	pu-cells * cells purify-table + @ ;

: old-end ( i -- cfa )
	pu-cells * cells purify-table + >old-end @ ;

: new-cfa-adr ( i -- adr )
	pu-cells * cells purify-table + >new-cfa ;

0 value unsolved-table
0 value unsolved-cnt

: unsolved-old-cfa ( i -- adr )
	2* cells unsolved-table + ;

: unsolved-new-adr ( i -- adr )
	2* cells unsolved-table + cell+ ;

: add-unsolved ( old-cfa -- ) \ put in old cfa, and address to be filled later 
	t_here unsolved-cnt unsolved-new-adr ! \ this adr need to be replace with new cfa
	unsolved-cnt unsolved-old-cfa ! \ old cfa as a tag to recognize later
	1 +to unsolved-cnt ;
	
: old>new ( old-cfa -- new-cfa ) \ cfa can be larger
	0 swap 
	total-cfa 0
	?do i old-cfa over < not
		if i old-cfa over =
			if i
			else i 1 -
			then
			rot drop new-cfa-adr @ swap leave
		then 
	loop 
	over 0=
	if add-unsolved
	else drop
	then ;

0 value old-cfa-saved

: old>new ( old-cfa -- new-cfa ) \ old version, cfa must fit
	dup to old-cfa-saved
	0 swap 
	total-cfa 0
	?do i old-cfa over =
		if swap drop i new-cfa-adr @ swap leave
		then 
	loop 
	over 0=
	if add-unsolved cr ." ************ empty new cfa allert: " old-cfa-saved >name .id
	else drop
	then ;

: old>new-sure ( old-cfa -- new-cfa ) \ if 0, abort
	0 swap 
	total-cfa 0
	?do i old-cfa over =
		if swap drop i new-cfa-adr @ swap leave
		then 
	loop 
	over 0=
	if abort" new cfa is 0"
	else drop
	then ;

: new>old ( new-cfa -- old-cfa )
	0 swap 
	total-cfa 0
	?do i new-cfa-adr @ over =
		if swap drop i old-cfa swap leave
		then 
	loop drop ;

: move-as-it ( ip -- ip+cell )
	end-adr over - t_s,
	end-adr ;

: move-dolit ( ip -- ip+cell )
\ cr ." move-dolit alert" dup @ . dup @ 9 - >name .id cr
	dup @ t_, cell+ ;

: relative-move ( ip -- ip+cell )
	dup @ over - t_here + t_, cell+ ;

: move-string0 ( ip -- ip' )
	dup count dup t_c, 1+ t_s, count + 1+ ;						

: move-string ( ip -- ip' )
	dup count dup t_c, t_s, count + ;						

: move-as-cfa ( ip -- ip+cell )
	dup @ old>new t_, cell+ ;

: move-dovar ( ip -- ip+cell )
	dup @ in-@execute-list?
	if 
cr ." move as cfa******: " dup @ dup . >name .id cr
		move-as-cfa
	else 
\ cr ." move as it is: " dup @ . cr
		move-as-it
	then ;

: move-docb ( ip -- ip+cell ) \ take care of cb-delete address
	move-as-cfa ;

: move-dovalue ( ip -- ip+cell ) 
	dup @ 9 - old>new 9 + t_, cell+ ;
	
disassembler
				
: forward-ip-move ( ip cfa -- ip' )
	case
 \    		['] dovar    of  end-adr over - t_s,  end-adr endof
     		['] dovar    of  move-dovar drop end-adr endof
      		['] dolit    of  move-dolit endof
		['] docb     of  move-docb endof \ can be omitted?
		['] dovalue  of  move-dovalue endof
     		['] branch   of  relative-move endof
     		['] ?branch  of  relative-move endof
     		['] brnext   of  relative-move endof
     		['] (do)     of  relative-move endof
     		['] (?do)    of  relative-move endof
     		['] (loop)   of  relative-move endof
\     		old-(loop)   of  relative-move endof
     		['] (+loop)  of  relative-move endof 
     		['] _of      of  relative-move endof
     		['] _endof   of  relative-move endof
     		['] (")      of  move-string0 endof
     		['] (.")     of  move-string0 endof
     		['] (z")     of  move-string0 endof
     		['] (abort") of  move-string endof
     		['] ."|      of  move-string endof
     		['] $"|      of  move-string endof
     		['] (.s")    of  move-string0 endof 
   	endcase ;

forth

: t_call,  ( cfa -- )
	$e8 t_c, t_here cell+ - t_, ;

	
: move-list ( i --  ) 
	t_here over new-cfa-adr !
	['] dolist old>new t_call,
\ 	dup old-cfa 1+ dup cell+ swap @ + old>new t_call,
	dup old-end to end-adr
	old-cfa 1+ cell+   \ call dolist has been done
	begin dup end-adr = not
	while dup @ dup old>new t_, swap cell+ swap forward-ip-move
	repeat drop ;

: move-callback-word ( i --  ) \ take care of cb-delete moving itself
	t_here over new-cfa-adr !
	['] docallback old>new t_call,
	dup old-end to end-adr
	old-cfa 1+ cell+   \ call docallback has been done
	begin dup end-adr = not
	while dup @ dup old>new t_, swap cell+ swap forward-ip-move
	repeat drop ;
	
: move-assembly ( i -- )
	t_here over new-cfa-adr !
	dup old-cfa swap old-end over - t_s, 
	;

: forward-to-does> ( cfa -- cfa-does> )
	>body 
	begin dup @ ['] (;code) = not
	while dup @ swap cell+ swap forward-ip-mark-does>
	repeat cell+ ;

: move-create-does> ( i --  ) \ move only the part after does>
	t_here over new-cfa-adr !
	['] dolist old>new t_call,
	dup old-end to end-adr
	dup old-cfa forward-to-does> 
	dup rot pu-cells * cells purify-table + ! \ modify old cfa to after (;code)
	1+ cell+   \ call dolist has been done
	begin dup end-adr = not
	while dup @ dup old>new t_, swap cell+ swap forward-ip-move
	repeat drop ;

: move-does>product ( i --  ) 
	t_here over new-cfa-adr !
 	dup old-cfa call-dest old>new t_call,
	dup old-end to end-adr
	old-cfa 1+ cell+   
	begin dup end-adr = not
	while dup @ dup old>new t_, swap cell+ swap forward-ip-move \ should move as it is
	repeat drop ;

: move-defer-product ( i --  ) 
	t_here over new-cfa-adr !
 	dup old-cfa call-dest old>new t_call,
	old-cfa 1+ cell+
	dup @ old>new t_,
	cell+ @ old>new t_, ;
	
: move-code ( i -- )
	dup old-cfa 
dup >name .id space
	create-does>?
	if move-create-does> exit then
	dup old-cfa does>product?
	if dup old-cfa defer?
		if move-defer-product
		else move-does>product 
		then
		exit 
	then
	dup old-cfa list?
	if move-list exit then
	dup old-cfa callback-word?
	if move-callback-word exit then
	move-assembly
	;

: resolve ( i -- ) \ resolve whole unresolved table
	dup>r unsolved-old-cfa @ \ cfa need to be resolved
	old>new r> unsolved-new-adr @ t_! ;


variable run-word

: target-main ( -- )
	run-word @execute lbye ;

: lib-target-main ( -- )
	init-libs 
	init-procs
	run-word @execute 
	bye ;
	
32 constant gap-to-st
sys-base  gap-to-st + $2d + 4 - constant offset-to-main

: start-point ( -- )
	['] library marked?
	if ['] lib-target-main
	else ['] target-main 
	then old>new
	t_here offset-to-main >t !
	t_, ;

: end-point ( -- ) \ to correctly place cp
	['] cp old>new 
	if t_here ['] cp old>new 9 + t_! 
	then
	['] base old>new
	if $a ['] base old>new 9 + t_! 
	then
	;

0 value imagef

: save-image ( -- )
	s" target.img" new-file drop to imagef
	target-ptr t_here sys-base - imagef write-file abort" error"
	imagef close-file drop ;

0 value link-from-adr \ store the address that should link to next node

: cfa>link ( adr -- adr' )
	2 cells+ 1+ ;

: relink-libs ( -- ) \ go through libs, and relink libraries 
                cr
		['] lib-link old>new >body cell+ to link-from-adr
                lib-link 
                begin   @ dup
                while   dup link>cfa marked? \ only when library is used then relink
			if dup link>cfa 
				cr dup >name .id 
				old>new cfa>link dup link-from-adr t_!
				to link-from-adr
			then
                repeat  drop 
		0 link-from-adr t_! \ the end of link list
		;

: relink-procs ( -- ) \ go through procs, and relink them 
                cr
		['] proc-link old>new >body cell+ to link-from-adr
                proc-link 
                begin   @ dup
                while   dup link>cfa marked? \ only when proc is used then relink
			if dup link>cfa 
				cr dup >name .id 
				old>new cfa>link dup link-from-adr t_!
				to link-from-adr
			then
                repeat  drop 
		0 link-from-adr t_! \ the end of link list
		;


: fix-docallback ( -- )
	['] ebp-stack old>new $28 cells+ ['] ebp-top old>new 9 + t_! \ set content of ebp-top
	['] docallback old>new
	dup $25 + ['] return-p old>new 9 + t_! \ set +$25 as return-p content
	dup $1d + ['] return-p old>new 9 + swap t_! \ return-p adr goto +$1d
	dup $5 + ['] ebp-top old>new 9 + t_@ swap t_! \ ebp-top content goto +$5
	drop
	;

: ?fix-docallback ( -- )
	['] docallback marked?
	if fix-docallback
	then ;

: ?relink-lib ( -- ) \ relink library if any library is used
	['] library marked?
	if relink-libs relink-procs
	then ;

: purify ( -- )
	target-ptr refresh
	total-space 500 + allocate abort" space" to target-ptr
	target-ptr total-space 500 + erase
	build-pu-table
	unsolved-table refresh
	total-cfa 2* cells allocate abort" space" to unsolved-table
	unsolved-table total-cfa 2* cells erase
	0 to unsolved-cnt
	build-head
	total-cfa 0
	?do i move-code
	loop 
	cr ." Unsolved words: "  unsolved-cnt .
	unsolved-cnt 0
	?do i resolve
	loop 
	?relink-lib
	start-point 
	end-point
	?fix-docallback
	save-image 
 	;
hex	

: list-purify ( -- )
	s" aneeded.srt" r/w open-file drop to inf
	s" aneeded.sym" new-file drop to outf
	pad maxstring inf read-line 2drop drop 
	total-cfa 0 
	?do i old-cfa 0 <# #s #> out-pad place s"   " out-pad +place
		i new-cfa-adr @ 0 <# #s #> out-pad +place  s"   " out-pad +place
		tib maxstring inf read-line abort" fill-table" drop
		#tib ! 0 >in ! 
		bl word drop bl word drop bl word drop bl word count out-pad +place
		out-pad count outf write-line drop
	loop 
	outf close-file drop
	inf close-file drop 
	;

: str? ( cfa -- f )
	>r
	r@ ['] (.") =
	r@ ['] (") = or
	r> ['] (z") = or
	
	; 

: print-str ( ip cfa -- ip' )
	>name .id
	cell+ dup count 2dup type -1 2 d+ dump
	dup count nip 2 + + \ one for length, one for trailing 0
	;

: asee  ( adr -- )
	cr
        dup .adr space dup c@ . space  ( adr )
        dup 1+ dup cell+ swap @ + dup u. >name .id
        cell+ 1+
        begin dup space cr .adr dup @ dup str? 
		if print-str
		else >name dup
                	if .id
                	else drop dup @ u.
                	then 
			cell+
		then nuf?
        until drop ;
 
: see  ( -- \ <word> )
        ' cr asee ;  


: tprint-str ( ip cfa -- ip' )
	new>old >name .id
	cell+ dup >t count 2dup type -1 2 d+ dump
	dup >t count nip 2 + + \ one for length, one for trailing 0
	;
	
: tasee ( adr -- )
	cr
        dup .adr space dup t_c@ . space  ( adr )
        dup 1+ dup cell+ swap t_@ + dup u. new>old >name .id
        cell+ 1+
        begin dup space cr .adr dup t_@ dup new>old str? 
		if tprint-str
		else new>old >name dup
                	if .id
                	else drop ( ip )
				dup t_@ u.
                	then 
			cell+
		then nuf?
        until drop ;
 

: tsee ( -- )
	' dup create-does>?
	if forward-to-does>
	then old>new dup
	if cr tasee 
	else drop ." Not found in target!"
	then ;


\ : tt 3 0 ?do drop loop ;

create elf-name maxstring allot
create command-buf maxstring allot
0 value target-cfa

: target ( -- | target-word elf-name )
	hex
	' run-word !
	bl word count dup 0= abort" Need a name for execute file : target name1 name2"
	elf-name place
	clear-mark 
	sys-base to t_here
	cr ." ******** list-all ********"
	list-all
	0 ['] target-main iterate 
	['] link>content marked? \ if library is used, use lib-target-main as template
	if 0 ['] lib-target-main iterate
	then
	mark-library 
	docallback-special
	cr ." ******** list-marked2 ********"
	list-marked2 
	cr ." ******** purify ********"
	purify 
	s" cat lpforth.bin target.img > " command-buf place
	elf-name count command-buf +place
	command-buf count pad place0 pad 1+ system drop
	s" chmod 755 " command-buf place
	elf-name count command-buf +place
	command-buf count pad place0 pad 1+ system drop	
	list-purify 
	;


hex
      
: t1 10 0 do i . loop z" ls " os ;
: t2 20 0 do i . loop ;
: t3 ;

cr 
cr .( Usage: target <target-word> <elf-name> )



                                           
