# Build PyFVS using Travis-CI

cd bin
mkdir -p build
cd build

cmake -G"Unix Makefiles" .. -DFVS_VARIANTS="pnc;wcc" -DWITH_PYEXT=ON \
    -D32BIT_TARGET=No -DNATIVE_ARCH=Yes -DSTATIC_LINK=ON \
    -DENABLE_LTO=OFF -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/Open-FVS
