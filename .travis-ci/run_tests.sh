pip install --upgrade nose-parameterized

export PYTHONPATH=${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python:$PYTHONPATH

pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/test
python -m unittest test_variants
popd