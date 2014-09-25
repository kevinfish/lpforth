\ gtk try

\ save as ghello2.f 1/16/99
\ a callback to forth, the registers to keep unchanged are: esp, ebp, edi
\ the rest: eax, ebx, ecx, edx, and esi can ne changed 
\ hello2 working, low level call high level
\ save as ghello3.f 1/16/99
\ low to hi call working for destroy, too. save as ghello4.f before clean up
\ 1/16/99
\ save as ghello5.f before change 1/17/99
\ save as ghello6.f before add call back kind of word 1/18/99
\ save asd ghello7.f before modify to new library structure Wed Aug 4 14:12:52 1999
\ allot a stack for ebp to prevent stack overrun code
\ save as ghello8.fThu Aug 5 18:41:49 1999
\ modify for release in Apr. 2001, save as ghello9.f Sat Apr 21 16:53:33 2001

marker h-m

\ : asciiz->asc-len ( adr -- adr' len )
\	maxstring 2dup 0 scan nip - ;

library /usr/lib/libglib.so
\ library libglib
library /usr/lib/libgdk.so \ it is necessary for it to work
library /usr/lib/libgtk.so

2 s" gtk_init" proc gtk_init
1 s" gtk_window_new" proc gtk_window_new
4 s" gtk_signal_connect" proc gtk_signal_connect
2 s" gtk_container_set_border_width" proc gtk_container_set_border_width
1 s" gtk_button_new_with_label" proc gtk_button_new_with_label
4 s" gtk_signal_connect_object" proc gtk_signal_connect_object
2 s" gtk_container_add" proc gtk_container_add
1 s" gtk_widget_show" proc gtk_widget_show
0 s" gtk_main" proc gtk_main

1 s" g_print" proc g_print
0 s" gtk_main_quit" proc gtk_main_quit
2 s" gtk_hbox_new" proc gtk_hbox_new
5 s" gtk_box_pack_start" proc gtk_box_pack_start

0 constant GTK_WINDOW_TOPLEVEL

\ : argc sys-base 3 cells+ @ ;

\ : argv sys-base 4 cells+ @ ;

0 value window
0 value button
0 value button2
		
0 value saved-rp

: 5stacks ( -- )
	cr 5 0
	do 4 i - pick .
	loop ;
	
variable return-p

\ callback word structure
\ call docallback high-level-word c-ebp 
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

\ call(e8) docallback cfa-hi-level c's-ebp		


: hi-hello ( data widget -- data widget )
	5stacks
	1 pick asciiz->asc-len type space ;

		
' hi-hello callback: hello2

: hi-destroy ( data widget -- data widget )
	cr ." Destroy event occured" 
	5stacks 
	gtk_main_quit drop ;

' hi-destroy callback: destroy2

: hi-delete ( data event widget -- data event widget )
	cr ." Delete event occured" cr
\	z" Delete event" g_print drop
	5stacks 
	;

' hi-delete callback: delete2
				
\ : [p']	( -- adr )
\	bl word count "find-proc not abort" Cannot find procedure!"
\	@ ; immediate

1 s" gtk_widget_destroy" proc gtk_widget_destroy
\ set-proc gtk_type_check_object_cast
21 constant GTK_TYPE_OBJECT
0 value box1
	
: ghello ( -- )
	argv argc gtk_init drop
\	z" pretest" g_print drop 
	GTK_WINDOW_TOPLEVEL gtk_window_new to window
	null ['] delete2 z" delete_event" window gtk_signal_connect drop
	null ['] destroy2 z" destroy" window gtk_signal_connect drop
	10 window gtk_container_set_border_width drop
	0 false gtk_hbox_new to box1
	box1 window gtk_container_add drop

	z" Button 1" gtk_button_new_with_label to button
	z" Button 1" ['] hello2 z" clicked" button 
	gtk_signal_connect drop
	0 true true button box1 gtk_box_pack_start drop
	button gtk_widget_show drop

	z" Button 2" gtk_button_new_with_label to button2
	z" Button 2" ['] hello2 z" clicked" button2
	gtk_signal_connect drop
	0 true true button2 box1 gtk_box_pack_start drop
	button gtk_widget_show drop

	button gtk_widget_show drop
	button2 gtk_widget_show drop
	box1 gtk_widget_show drop
	window gtk_widget_show drop
	gtk_main drop ;

: gh2 ( -- )
	argv argc gtk_init drop
	z" pretest" g_print drop 
	GTK_WINDOW_TOPLEVEL gtk_window_new to window
	null ['] delete2 z" delete_event" window gtk_signal_connect drop
	null ['] destroy2 z" destroy" window gtk_signal_connect drop
	10 window gtk_container_set_border_width drop
	0 false gtk_hbox_new to box1
	box1 window gtk_container_add drop

	z" Button 1" gtk_button_new_with_label to button
	z" Button 1" ['] hello2 z" clicked" button 
	gtk_signal_connect drop
	0 true true button box1 gtk_box_pack_start drop
	button gtk_widget_show drop


	button gtk_widget_show drop
	box1 gtk_widget_show drop
	window gtk_widget_show drop
	gtk_main drop ;

: simple ( -- )
	argv argc gtk_init drop
	GTK_WINDOW_TOPLEVEL gtk_window_new to window
	null ['] delete2 z" delete_event" window gtk_signal_connect drop
	null ['] destroy2 z" destroy" window gtk_signal_connect drop


	window gtk_widget_show drop
	gtk_main drop ;

: simple2 ( -- )
	argv argc gtk_init drop
	GTK_WINDOW_TOPLEVEL gtk_window_new to window
	null ['] delete2 z" delete_event" window gtk_signal_connect drop
	null ['] destroy2 z" destroy" window gtk_signal_connect drop

	10 window gtk_container_set_border_width drop
	0 false gtk_hbox_new to box1
	box1 window gtk_container_add drop

	z" Button 1" gtk_button_new_with_label to button
	z" Button 1" ['] hello2 z" clicked" button 
	gtk_signal_connect drop
	0 true true button box1 gtk_box_pack_start drop
	button gtk_widget_show drop

	button gtk_widget_show drop
	box1 gtk_widget_show drop

	window gtk_widget_show drop
	gtk_main drop ;
