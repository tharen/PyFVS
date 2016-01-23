pip install --upgrade nose-parameterized nose2

export PYTHONPATH=${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python:$PYTHONPATH

python -c "import numpy;print('Numpy version:',numpy.version.version)"

pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python

python setup.py bdist_wheel
pip install --no-index --find-links dist pyfvs
fvs --run-tests
mv dist/*.whl ${BUILD_ROOT}

popd