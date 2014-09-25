\ save as fsave1.f for soft link Wed Apr 26 00:37:27 2000

create file-pad maxstring allot

: fsave ( -- )
	bl word count file-pad place 
	s" temp.img" pad place
	pad count 2dup delete-file drop 
	r/w create-file abort" create error" >r 
	sys-base here sys-base - r@ write-file abort" write error" 
	r> close-file drop
	s" cat lpforth.bin temp.img > " pad place
	file-pad count pad +place
	pad +null
	pad 1+ system drop ;
	 
	
	
