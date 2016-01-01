cd bin
cmake -G"Unix Makefiles" . \
    -DFVS_VARIANTS=all \
    -DCMAKE_SYSTEM_NAME=Windows \
    -DMAKE_JOBS=1
