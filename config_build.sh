mkdir -p bin/build
pushd bin/build

cmake -G"Unix Makefiles" .. \
    -DFVS_VARIANTS="pnc;wcc;soc;cac" -DWITH_PYEXT=ON \
    -D32BIT_TARGET=No -DNATIVE_ARCH=Yes \
    -DSTATIC_LINK=ON -DENABLE_LTO=OFF \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=$(pwd)/dist

