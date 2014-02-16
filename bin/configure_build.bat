::Configure build

cmake .. -G"MinGW Makefiles" -DFVS_VARIANTS=pnc -DDEBUG=ON -DWITH_PYMOD=ON -DSTATIC_LINK=ON
