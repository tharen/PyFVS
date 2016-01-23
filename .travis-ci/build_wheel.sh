pip install --upgrade nose-parameterized nose2

pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python

python setup.py bdist_wheel
cd dist
pip install --no-index --find-links . pyfvs
fvs --run-tests
mv dist/*.whl ${BUILD_ROOT}

popd