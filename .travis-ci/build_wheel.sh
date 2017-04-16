
pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python

source activate pyfvs
python setup.py build_ext --inplace
python setup.py bdist_wheel

# Test the shiny new wheel
cd dist
pip install --no-index --find-links . pyfvs
pyfvs --run-tests
mv *.whl ${BUILD_ROOT}/artifacts

popd