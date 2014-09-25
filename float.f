\ C wrapper version, save before change to .so version 7/30/98
\ still working on it to get rid of all C code
\ no c version 1
\ save as float2.f for soft link Wed Apr 26 00:34:46 2000

cr .( Loading floating point words. )

marker float-m
11 constant tfloat-x
12 constant sfloat-x

code fspop ( -- r ) \ get the top of floating point stack, store top?
	ebp esp xchg
	$dd c, $5d c, $f8 c, \ fstp [ebp-8]   move FPT to [ebp-8]
	8 # ebp sub
	ebp esp xchg
	next c;

code fspush ( r -- ) 
	ebp esp xchg
	$dd c, $45 c, $0 c, \ fld [ebp]   move [ebp] to FPT
	8 # ebp add
	ebp esp xchg
	next c;

code fcompp ( -- f )
	$de c, $d9 c,
	$9b c, $df c, $e0 c,
	$4100 # eax and
	eax push
	next c;
			
code fs+ ( -- ) 
	$de c, $c1 c, 	
	next c;

code fs- ( -- ) 
	$de c, $e9 c, 
	next c;

code fs* ( -- ) 
	$de c, $c9 c, 	
	next c;

code fs/ ( -- ) 
	$de c, $f9 c, 
	next c;
	
: f> ( r1 r2 -- f )
	fspush fspush fcompp 0= ;

: f< ( r1 r2 -- f )
	fspush fspush fcompp $100 = ;

: f= ( r1 r2 -- f )
	fspush fspush fcompp $4100 = ;
	
create float-buf maxstring allot

: >float ( adr len -- r true | false )
        float-buf place 
        float-buf 1+ sp@ cell- sfloat-x xcall 
        if true
        else 2drop false
        then  ;

: fnumber ( adr -- d1 true | adr false )
        dup count >float 
        if rot drop true true double? !
        else false 
        then ;

: f%number ( adr -- d1 true | adr false )
        %number
        if true
        else fnumber
        then ;
        
' f%number 'number !        


: float ( n -- n1 n2 )
        sp@ cell- tfloat-x xcall drop ;


: fdrop ( r -- ) 2drop ;
: fdup ( r -- r r ) 2dup ;
: fswap ( r1 r2 -- r2 r1 ) 2swap ;

3 s" printf" proc printf

: f. ( n1 n2 -- )
        z" %12f" printf drop ioflush ;

: f.s ( -- )
	fspop fdup f. fspop fdup f. fspush fspush ;
		
: f+ ( r1 r2 -- r3 )
	fspush fspush fs+ fspop ;

: f- ( r1 r2 -- r3 )
        fswap fspush fspush fs- fspop ;

: f* ( r1 r2 -- r3 )
	fspush fspush fs* fspop ;
	
: f/ ( r1 r2 -- r3 )
	fswap fspush fspush fs/ fspop ;

2 s" floor" proc  floor
: floor ( r1 -- r2 )
         floor drop \ get rid of returned eax
        fspop ;

2 s" pow" proc pow
: f** ( r1 r2 -- r3 )
        pow drop \ get rid of returned eax
        fspop ;

2 s" fabs" proc  fabs
: fabs ( r1 -- r2 )
         fabs drop \ get rid of returned eax
        fspop ;

2 s" acos" proc acos
: facos ( r1 -- r2 )
        acos drop \ get rid of returned eax
        fspop ;

2 s" acosh" proc acosh
: facosh ( r1 -- r2 )
        acosh drop \ get rid of returned eax
        fspop ;

2 s" asin" proc asin
: fasin ( r1 -- r2 )
        asin drop \ get rid of returned eax
        fspop ;

2 s" asinh" proc asinh
: fasinh ( r1 -- r2 )
        asinh drop \ get rid of returned eax
        fspop ;

2 s" atan" proc atan
: fatan ( r1 -- r2 )
        atan drop \ get rid of returned eax
        fspop ;

4 s" atan2" proc atan2
: fatan2 ( r1 r2 -- r3 )
        atan2 drop \ get rid of returned eax
        fspop ;

2 s" atanh" proc atanh
: fatanh ( r1 -- r2 )
        atanh drop \ get rid of returned eax
        fspop ;

2 s" cos" proc cos
: fcos ( r1 -- r2 )
        cos drop \ get rid of returned eax
        fspop ;

2 s" cosh" proc cosh
: fcosh ( r1 -- r2 )
        cosh drop \ get rid of returned eax
        fspop ;

2 s" exp" proc exp
: fexp ( r1 -- r2 )
        exp drop \ get rid of returned eax
        fspop ;

2 s" log" proc log
: fln ( r1 -- r2 )
        log drop \ get rid of returned eax
        fspop ;

2 s" log10" proc log10
: flog ( r1 -- r2 )
        log10 drop \ get rid of returned eax
        fspop ;

2 s" sin" proc sin
: fsin ( r1 -- r2 )
        sin drop \ get rid of returned eax
        fspop ;

2 s" sinh" proc sinh
: fsinh ( r1 -- r2 )
        sinh drop \ get rid of returned eax
        fspop ;

2 s" sqrt" proc sqrt
: fsqrt ( r1 -- r2 )
        sqrt drop \ get rid of returned eax
        fspop ;

2 s" tan" proc tan
: ftan ( r1 -- r2 )
        tan drop \ get rid of returned eax
        fspop ;

2 s" tanh" proc tanh
: ftanh ( r1 -- r2 )
        tanh drop \ get rid of returned eax
        fspop ;

\ : D>F ( d -- r ) sp@ dtfloat-x xcall 2drop ;

	
: F! ( r f-addr -- ) 2! ;
: F0< ( r -- flag ) d0< ;
: F0= ( r -- flag ) d0= ;
: f>d ( f -- d ) ;
: f@ ( addr -- r ) 2@ ;
: falign ( -- ) ;
: faligned ( -- ) ;
: fconstant create , , 
        does> dup cell+ @ swap @ ;
: fdepth ( -- n ) depth 2/ ;
: fliteral ( f -- ) [compile] 2literal ; immediate 
: float+ ( addr -- addr' ) 2 cells+ ;
: floats ( n1 -- n2 ) 2* cells ;
: fmax ( r1 r2 -- r3 ) dmax ;
: fmin ( r1 r2 -- r3 ) dmin ;
: fnegate ( r1 -- r2 ) ;
: fover ( r1 r2 -- r1 r2 r1 ) 2over ;
: frot ( r1 r2 r3 -- r2 r3 r1 ) 2rot ;
: fround ( r1 -- r2 ) ;
: fvariable 2variable ;
: represent ( r c-addr u -- n flag1 flag2 ) ;
: df! ;
: df@ ;
: dfalign ;
: dfaligned ;
: dfloat+ ( addr -- addr' ) 4 cells+ ;
: dfloats ( n1 -- n2 ) 4 * cells ;
: fexpm1 ;
: flnp1 ;
: fs. ;
: fsincos ;
: f~ ;
: precision ;
: set-precision ;
: sf! ;
: sf@ ;
: sfalign ;
: sfaligned ;
: sfloat+ ( addr -- addr' ) cell+ ;
: sfloats ( n1 -- n2 ) cells ;

