#!/bin/bash
# Deploy compiled wheels and source archives to PYPI
# TWINE_USERNAME and TWINE_PASSWORD should set as global variables

# python setup.py artifacts are moved relative to the build dir.
pushd ${TRAVIS_BUILD_DIR}/artifacts

if [ $TRAVIS_BRANCH = 'dev' ]; then
  TWINE_REPOSITORY_URL=https://testpypi.python.org
elif [ $TRAVIS_BRANCH = 'master' ] && [ -z ${TRAVIS_TAG+x}]; then
  TWINE_REPOSITORY_URL=https://pypi.python.org
else
  popd
  exit
fi

twine upload *.whl --skip-existing
twine upload *.gz --skip-existing

popd