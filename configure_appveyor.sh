cd bin
cmake -G"Unix Makefiles" . \
    -DFVS_VARIANTS=pnc,wcc,op \
    -DCMAKE_SYSTEM_NAME=Windows \
    -DMAKE_JOBS=1
