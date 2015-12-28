f2py -h pyfvs$1.pyf -m pyfvs$1 --overwrite-signature ../../../api/fvs_step.f90 
f2py -c ./pyfvs$1.pyf -L. -lFVS$1_static -lodbc
