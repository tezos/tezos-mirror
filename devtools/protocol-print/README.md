# print-fix

## Why a patch

This allows printing inside the protocol files for debugging purposes.

## How to use

```sh
# apply patch
git apply /dev/patch.diff


# make 
make

# now you can just print in any protocol file by using the command
Hack.print "Hello World!" 


```
