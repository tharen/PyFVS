# Build Open-FVS (trunk) using Travis-CI

cd bin
cmake -G"Unix Makefiles" . \
    -DFVS_VARIANTS=all \
    -DCMAKE_SYSTEM_NAME=Linux \
    -DMAKE_JOBS=1
