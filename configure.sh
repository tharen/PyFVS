mkdir build
cd build
cmake -G"Unix Makefiles" ..  -DFVS_VARIANTS="pnc;wcc;soc;cac" -DCMAKE_INSTALL_PREFIX=open-fvs

