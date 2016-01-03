# Build Open-FVS (trunk) using Travis-CI

cd bin
cmake -G"Unix Makefiles" .. \
    -DFVS_VARIANTS=pnc \
    -DWITH_PYMOD=Yes \
    -DCMAKE_SYSTEM_NAME=Linux \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/Open-FVS \
    -DMAKE_JOBS=1
