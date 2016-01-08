sources=$(cat pymod_source.txt)
logfile=../f2py_$1.log

if [ -v MSYSTEM ]
then
  # Assume this is running on Windows under MSYS2/bash
  f2py -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $sources > $logfile 2>&1
  f2py -c --compiler=mingw32 --fcompiler=gnu95 -lodbc32 ./pyfvs$1.pyf libFVS$1_static.a >> $logfile 2>&1

else
  
  # Prefer the Python 3 version of F2PY  
  py_ver=$(python -c "import sys;print(sys.version_info[0])")
  if [ $py_ver = 3 ] || [ -e f2py3 ]; then
    f2py=f2py3
  else
    f2py=f2py
  fi
  
  i=$(which python)
  echo Python executable: $i > $logfile
  i=$(which $f2py)
  echo F2PY executable: $i >> $logfile
  
  $f2py -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $sources >> $logfile 2>&1
  $f2py -c -lodbc ./pyfvs$1.pyf libFVS$1_static.a >> $logfile 2>&1

fi

