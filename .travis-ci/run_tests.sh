pip install --upgrade nose-parameterized

export PYTHONPATH=${BUILD_ROOT}/bin/build/Open-FVS/python:$PYTHONPATH

pushd ${BUILD_ROOT}/bin/build/Open-FVS/test
python -m unittest test_variants
popd