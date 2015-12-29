sources=$(cat pymod_source.txt)

if [ -v MSYSTEM ]
then
  # Assume this is running on Windows under MSYS2/bash
  $F2PY -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $sources
  $F2PY -c --compiler=mingw32 --fcompiler=gnu95 -lodbc32 ./pyfvs$1.pyf libFVS$1_static.a

else
  $F2PY -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $sources
  $F2PY -c -lodbc ./pyfvs$1.pyf libFVS$1_static.a

fi

