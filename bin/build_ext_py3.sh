if [ -v MSYSTEM ]
then
  # Assume this is running on Windows under MSYS2/bash
  f2py -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $PROJ_ROOT/api/fvs_step.f90 
  f2py -c ./pyfvs$1.pyf --compiler=mingw32 --fcompiler=gnu95 -L. -lFVS$1_static -lodbc32

else
  f2py -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature $PROJ_ROOT/api/fvs_step.f90 
  f2py -c ./pyfvs$1.pyf -L. -lFVS$1_static -lodbc

fi

