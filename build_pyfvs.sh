# Build PyFVS using Travis-CI

cd bin
mkdir travis-ci
cd travis-ci

cmake -G"MSYS Makefiles" .. -DFVS_VARIANTS="pnc;wcc" -DWITH_PYMOD=Yes \
    -DPYTHON_EXECUTABLE=python35 \
    -D32BIT_TARGET=No -DNATIVE_ARCH=Yes -DSTATIC_LINK=ON \
    -DENABLE_LTO=OFF -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=(pwd)/Open-FVS

make pnc
