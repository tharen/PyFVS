cd ${TRAVIS_BUILD_DIR}

# make the folder to build from, e.g. bin/build
mkdir -p ${BUILD_ROOT}
cd ${BUILD_ROOT}

# Configure the CMake build
cmake -G"Unix Makefiles" .. \
    -DFVS_VARIANTS=${FVS_VARIANTS} \
    -DWITH_PYEXT=Yes \
    -DNATIVE_ARCH=No \
    -DUNIX_TARGET=Yes \
    -DCMAKE_SYSTEM_NAME=Linux \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=Open-FVS    

# Build the Open-FVS binaries
# the install target will copy the Python files over
# FIXME: The install targer should simply put the binaries in the 
#        Python source package, enabling a more simple python setup.py process
cmake --build . --target install 2> build_err.log

# Build the additional Python extensions
cd Open-FVS/python
python setup.py build_ext --inplace

# Create archives
mkdir ${TRAVIS_BUILD_DIR}/artifacts

# Create the wheel
python setup.py bdist_wheel
mv dist/*.whl ${TRAVIS_BUILD_DIR}/artifacts/.

# Create the project zip archive
# FIXME: After fixing the install target, adapt this to leverage the sdist command
cd ${BUILD_ROOT}
7z a -bd ${TRAVIS_BUILD_DIR}/artifacts/${ARCHIVE_NAME} Open-FVS/*

cd ${TRAVIS_BUILD_DIR}
