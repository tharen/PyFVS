pip install --upgrade nose-parameterized
pip install --upgrade nose2

export PYTHONPATH=${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python:$PYTHONPATH

python -c "import numpy;print('Numpy version:',numpy.version.version)"

pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python
nose2
popd