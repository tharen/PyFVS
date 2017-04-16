source activate pyfvs
conda env list
python -c "import sys;print(sys.executable)"
python --version

source .travis-ci/configure.sh
cd ${BUILD_ROOT} && cmake --build . --target install 2> build_err.log