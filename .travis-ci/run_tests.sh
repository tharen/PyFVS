pip install --upgrade nose-parameterized

export PYTHONPATH=${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python:$PYTHONPATH

python -c "import numpy;print('Numpy version:',numpy.version.version)"

pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python/test
python -m unittest test_variants
popd