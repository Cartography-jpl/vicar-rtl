#!/bin/csh

cd lib/$VICCPU
rm libmath77.a
foreach i (../../fortran/*.f)
  set j=${i:t}
  if ($j =~ amach*) goto amach
  echo "Compiling " $j
  f77 -c $i
  ar ru libmath77.a ${j:r}.o
  rm ${j:r}.o
amach:
end
echo "Compiling amach.f"
f77 -c ../../fortran/amach.f
ar ru libmath77.a amach.o
rm amach.o

# NOTE: For systems requiring it, manually run ranlib after the build is done.
# (SunOS needs it).

