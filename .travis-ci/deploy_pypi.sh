#!/bin/bash
# Deploy compiled wheels and source archives to PYPI
# TWINE_USERNAME and TWINE_PASSWORD should set as global variables

# python setup.py artifacts are moved relative to the build dir.
pushd ${TRAVIS_BUILD_DIR}/bin/build/Open-FVS/python

if [ $TRAVIS_BRANCH = 'dev' ]; then
  echo "On dev branch, upload to testpypi."
  export TWINE_REPOSITORY=https://testpypi.python.org/pypi
  export TWINE_REPOSITORY_URL=https://testpypi.python.org/pypi

elif [ $TRAVIS_BRANCH = 'master' ] && [ -z ${TRAVIS_TAG+x}]; then
  echo "On master branch with tag, upload to pypi."
  export TWINE_REPOSITORY=https://pypi.python.org/pypi
  export TWINE_REPOSITORY_URL=https://pypi.python.org/pypi

else
  popd
  exit 0
fi

twine upload dist/*.gz --skip-existing

## PYPI doesn't accept binary wheels for linux
#twine upload dist/*.whl --skip-existing

popd
exit 0