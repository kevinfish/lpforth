#!/bin/sh
# expecting lpForth files to reside in the same directory as the script

./lpforth fload meta.f             # metacompile the kernel
mv lpforth lpforth.prev            # keep the previous system 
chmod +x lpforth.new               # make new kernel executable
echo "fload mkernel.f              # load extensions
      fsave lpforth                # write new system
      bye "  | ./lpforth.new       # exit kernel
chmod +x lpforth                   # make new system executable
