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
\ change to do gtk turtorial example Thu Aug 5 18:48:12 1999
\ basiclly the showing is working except scrollb ar and button offset address
\ are not right
\ libgtk.so.1 version Thu Sep 9 15:44:03 1999
\ figure out the offset of editable, word wrap, and vscrollbar by trying
\ save as gtk14.4.f2 Fri Sep 10 01:20:39 1999
\ this version work for gtk ver 1.2.6 , the wrap word need to be redone
\ Mon Jan 31 18:09:06 2000
\ save as gtk14.4.v2.f
\ wrap editable and scroll offset are ok

decimal

marker h-m
library libglib.so
library libgdk.so
library libgtk.so

2 s" gtk_init" proc gtk_init
1 s" gtk_window_new" proc gtk_window_new
2 s" gtk_window_set_title" proc gtk_window_set_title
4 s" gtk_signal_connect" proc gtk_signal_connect
2 s" gtk_container_set_border_width" proc gtk_container_set_border_width
1 s" gtk_button_new_with_label" proc gtk_button_new_with_label
2 s" gtk_container_add" proc gtk_container_add
1 s" gtk_widget_show" proc gtk_widget_show
0 s" gtk_main" proc gtk_main
0 s" gtk_main_quit" proc gtk_main_quit
2 s" gtk_hbox_new" proc gtk_hbox_new
5 s" gtk_box_pack_start" proc gtk_box_pack_start
3 s" gtk_table_new" proc gtk_table_new
6 s" gtk_table_attach_defaults" proc gtk_table_attach_defaults
0 constant GTK_WINDOW_TOPLEVEL


2 s" gtk_text_set_editable" proc gtk_text_set_editable
2 s" gtk_text_set_word_wrap" proc gtk_text_set_word_wrap
3 s" gtk_widget_set_usize" proc gtk_widget_set_usize
4 s" gtk_window_set_policy" proc gtk_window_set_policy
2 s" gtk_vbox_new" proc  gtk_vbox_new
3 s" gtk_table_set_row_spacing" proc gtk_table_set_row_spacing
3 s" gtk_table_set_col_spacing" proc gtk_table_set_col_spacing
2 s" gtk_text_new" proc gtk_text_new
\ 2 s" gtk_text_set_editable" proc gtk_text_set_editable
10 s" gtk_table_attach" proc gtk_table_attach
1 s" gtk_vscrollbar_new" proc gtk_vscrollbar_new
0 s" gdk_colormap_get_system" proc gdk_colormap_get_system
2 s" gdk_color_alloc" proc gdk_color_alloc
1 s" gdk_font_load" proc gdk_font_load
1 s" gtk_widget_realize" proc gtk_widget_realize
1 s" gtk_text_freeze" proc gtk_text_freeze
6 s" gtk_text_insert" proc gtk_text_insert
\ 2 s" fopen" proc fopen
\ 4 s" fread" proc fread
\ 1 s" fclose" proc fclose
1 s" gtk_text_thaw" proc gtk_text_thaw
0 s" gtk_hbutton_box_new" proc gtk_hbutton_box_new
2 s" gtk_toggle_button_set_active" proc gtk_toggle_button_set_active
1 s" gtk_check_button_new_with_label" proc gtk_check_button_new_with_label
\ \ 2 s" gtk_toggle_button_set_active" proc gtk_toggle_button_set_active 
\ 2 s" gtk_vbox_new" proc gtk_vbox_new
1 s" gtk_widget_grab_default" proc gtk_widget_grab_default
0 s" gtk_hseparator_new" proc gtk_hseparator_new

: callback ( data widget -- data widget )
	cr ." Hello again - "
	over asciiz->asc-len type 
	."  is pressed" ;

' callback callback: cb-callback

: delete ( data event widget -- data event widget )
	cr ." Delete event occured" ;

' delete callback: cb-delete

: destroy ( data widget -- data widget )
	gtk_main_quit drop ;

' destroy callback: cb-destroy

72 value eo
72 value wo

: text-toggle-editable ( text checkbutton -- text checkbutton )
	over over eo + c@ 1 and swap gtk_text_set_editable drop ;

' text-toggle-editable callback: cb-tte

: text-toggle-word-wrap ( text checkbutton -- text checkbutton )
	over over wo + c@ 1 and swap gtk_text_set_word_wrap drop ;

' text-toggle-word-wrap callback: cb-ttww

0 value window
0 value box1
0 value box2
0 value hbox
0 value button
0 value check
0 value separator
0 value table
0 value vscrollbar
0 value text
0 value cmap
create color 100 allot
0 value fixed-font
0 value infile
0 value nchars

1 constant gtk_expand
2 constant gtk_shrink
4 constant gtk_fill
13 2** constant GTK_CAN_DEFAULT

84 value so

				
: gtkmain ( -- )
	argv argc gtk_init drop
	GTK_WINDOW_TOPLEVEL gtk_window_new to window
	500 600 window gtk_widget_set_usize drop
	false true true window gtk_window_set_policy drop
	null [cb'] cb-delete z" delete_event" window gtk_signal_connect drop
	null [cb'] cb-destroy z" destroy" window gtk_signal_connect drop
	z" Text Widget Example" window gtk_window_set_title drop
	0 window gtk_container_set_border_width drop

	0 false gtk_vbox_new to box1
	box1 window gtk_container_add drop
	box1 gtk_widget_show drop

	10 false gtk_vbox_new to box2
	10 box2 gtk_container_set_border_width drop
	0 true true box2 box1 gtk_box_pack_start drop
	box2 gtk_widget_show drop

	false 2 2 gtk_table_new to table
	2 0 table gtk_table_set_row_spacing drop
	2 0 table gtk_table_set_col_spacing drop
	0 true true table box2 gtk_box_pack_start drop
	table gtk_widget_show drop

	null null gtk_text_new to text
	true text gtk_text_set_editable drop
	0 0 gtk_expand gtk_shrink or gtk_fill or dup
	1 0 1 0 text table gtk_table_attach drop
	text gtk_widget_show drop

	text so + @ dup . gtk_vscrollbar_new to vscrollbar
	\ look up from gdb to see the offset
	0 0 gtk_expand gtk_shrink or gtk_fill or gtk_fill
	1 0 2 1 vscrollbar table gtk_table_attach drop
	vscrollbar gtk_widget_show drop
	
	gdk_colormap_get_system to cmap
	$ffff color cell+ !
	0 color 2 cells+ !
	0 color 3 cells+ !
	color cmap gdk_color_alloc 0= abort" couldn't allocate color"
	z" -misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*" gdk_font_load 
	to fixed-font
	
	text gtk_widget_realize drop
	text gtk_text_freeze drop
	-1 z" Supports " null text $18 + @ $1a4 + null text 
	\ &text->style->black
	gtk_text_insert drop
	-1 z" colored " null color null text 
	gtk_text_insert drop
	-1 z" text and different " null text $18 + @ $1a4 + null text 
	gtk_text_insert drop
	-1 z" fonts" null text $18 + @ $1a4 + fixed-font text 
	gtk_text_insert drop
	
	z" r" z" gtext.c" fopen to infile
	
	infile 
	if 
	    begin infile 1024 1 pad fread to nchars
		nchars pad null null fixed-font text gtk_text_insert drop
	    	nchars 1024 <
	    until 
	    infile fclose drop 
	then 
	
	text gtk_text_thaw drop
	
	gtk_hbutton_box_new to hbox
	0 false false hbox box2 gtk_box_pack_start drop
	hbox gtk_widget_show drop
	
	z" Editable" gtk_check_button_new_with_label to check
       	0 false false check hbox gtk_box_pack_start drop
       	text [cb'] cb-tte z" toggled" check 
       	gtk_signal_connect drop
       	true check gtk_toggle_button_set_active drop
       	check gtk_widget_show drop
       	z" Wrap Words" gtk_check_button_new_with_label to check 
       	0 true false check hbox gtk_box_pack_start drop
       	text [cb'] cb-ttww z" toggled" check 
       	gtk_signal_connect drop
       	false check gtk_toggle_button_set_active drop
       	check gtk_widget_show drop

       	gtk_hseparator_new to separator
       	0 true false separator box1 gtk_box_pack_start drop
       	separator gtk_widget_show drop
       	
       	10 false gtk_vbox_new to box2
       	10 box2 gtk_container_set_border_width drop
       	0 true false box2 box1 gtk_box_pack_start drop
       	box2 gtk_widget_show drop
       
       	z" close" gtk_button_new_with_label to button
       	null [cb'] cb-destroy  z" clicked" button 
	gtk_signal_connect drop        
	0 true true button box2 gtk_box_pack_start drop  
\      GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT); 	
	button cell+ dup @ GTK_CAN_DEFAULT or swap !        
	button gtk_widget_grab_default drop
        button gtk_widget_show drop

        window gtk_widget_show drop
       
        gtk_main drop ;

: wmain ( h l -- )
	do i to wo i . gtkmain ." end" i . 
	loop ;

: emain ( h l -- )
	do i to eo i . gtkmain ." end" i . 
	loop ;

: lbye bye ;

: main init-libs init-procs gtkmain cr lbye ;
 
' main 'boot !



