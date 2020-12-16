cd bin
cmake -G"Unix Makefiles" . \
    -DFVS_VARIANTS=pn,wc,op \
    -DCMAKE_SYSTEM_NAME=Windows \
    -DMAKE_JOBS=2
