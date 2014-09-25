/* 
   Linux works in such a way that the address of eforth[] is always the same. 
   Program start at 0x80486d0  ????
   So once this file is complete we can run it, get the address of eforth[]
   and recompile eForth for this address.
   --- Pai's log
   I try to convert this forth to win32, eventually I plan to port
   a F-PC like environment to linux. First, I want to try to convert my
   previous eforth to win32 version with C wrapper. pforth.c will be compiled
   to pforth.exe. Upon execution, it will load pforth.img into the memory and
   jump into the 'COLD' in pforth.com. pforth.img is the image of pforth.com
   4/10/98

compiling method

gcc -g -rdynamic -o lpforth lpforth.c -ldl -lm -lX11 `gtk-config --cflags` `gtk-config --libs`
for include gtk ability

gcc -g -rdynamic -o lpforth lpforth.c -ldl -lm 

gcc -o lpforth lpforth.c -ldl \ this one is good enough

see script "meta" for how to make a single execution file


floating in C version
save as lpforth1.c

save as lpforth2.c before strip down 2/4/99,
strip down worked 2/4/99
open close and two floats should be out, too.
save as lpforth3.c before get rid of open and close 2/4/99
save as lpforth6.c before change to one file version without image file Fri Mar 24 00:58:40 2000
single execute file ok, refer to "meta" script file Fri Mar 24 02:08:43 2000


*/   
#include <stdio.h>  // needed for fopen etc
#include <stdlib.h>

#include <fcntl.h>  // needed for open

#include <unistd.h> // needed for close
#include <dlfcn.h>  // needed for dlopen etc
#include <math.h>   // for floating point

/* For debugging, until the C IO-lib is stable */
#define EFORTH  0x805E000  /* img start point in my system eforth[] is 0x8049c00 */
#define MEMST   0x805E000 /* start addr to put image so it will fit page */
#define STPOINT 0x805E020  /* program start point */
#define ESIZE   15192 /* look up lpforth.bin size, then fill this in */


typedef int FUNC();		/* array with I/O functions */


int bye(int exit_code)
{
printf("bye\n");
exit(exit_code);
return 0;
}


int key2(int filep)
{
	char c;
	return (read(filep, &c, 1)>0) ?  (unsigned char) c : EOF ;
}


// double sin(double x) { return sin(x); }
void tfloat(double *sp1, int n){*sp1=n;}
void sfloat(double *sp1, char *adr){sscanf(adr, "%lf", *sp1);}

FUNC *call[256]={ 
		0,		/* eForth itself */
		bye,		// 1
		key2,		// 2 needed in ?rx2
		open,		// 3 needed in fopen
		close,		// 4 needed in hand fclose
		fopen,		// 5 needed in simage
		fclose,		// 6
		fwrite,		// 7 needed in simage
		dlopen,         // 8
		dlsym,          // 9
		dlclose,        // 10
		tfloat,		// 11 needed for float.f
		sfloat,		// 12
		dlerror,        // 13
		};

char *eforth[1000000];		/* array with eForth binary */


int main(int argc, char *argv[]) 
{ FILE *in; 
  char imgfile[256]="";
  int x;
  strcat(imgfile, argv[0]);
//  printf(imgfile);
// printf(imgfile);

  if ((in=fopen(imgfile,"rb")) == NULL) 
    { printf("Problem opening pforth.img.\n"); exit(1); } 
  if ( ((char *)EFORTH < eforth) || ((char *)EFORTH > eforth+0x10000) ) {
     fprintf(stderr,"\n** eforth = %p, but EFORTH = %x **\n", eforth, EFORTH); exit(1); }
  fseek(in, ESIZE, SEEK_SET);
  fread((char *)MEMST, 1, 1000000, in); 
//  printf("%d\n", x);
  fclose(in);
  printf("[0x%x]\n ",eforth);
//  printf("[0x%x]\n ",&argc);
//  printf("[0x%x]\n ",&argv);
 
//  printf("[0x%x]\n ",(EFORTH - (int)eforth+16));

//  printf("%d\n", (int)eforth[(int)(EFORTH - (int)eforth+16)/4]);
 
  (int)eforth[(int)(EFORTH - (int)eforth+12)/4]= &argc;
  (int)eforth[(int)(EFORTH - (int)eforth+16)/4]= &argv;
//  printf("%d\n", (int)eforth[(int)(EFORTH - (int)eforth+16)/4]);

  call[0] = (FUNC *)(STPOINT); 
//  printf("[0x%x]\n ", call[0]);

  return call[0](argc, argv, &call); 
}

