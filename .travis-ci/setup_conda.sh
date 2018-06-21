conda config --set always_yes yes --set changeps1 no
conda config --add channels conda-forge

# hash -r
# Edit the environment.yml file for the target Python version
# sed -i -E 's/(python=)(.*)/\1'$TRAVIS_PYTHON_VERSION'/' ./environment.yml
# conda env create --force -q -n pyfvs -f ${TRAVIS_BUILD_DIR}/environment.yml

# FIXME: Numpy 1.14 is buggy on conda-forge for Python 2.7
if [ "$TRAVIS_PYTHON_VERSION" = "2.7" ]; then
    export pkg="numpy=1.13 pandas cython pyodbc click setuptools wheel sphinx alabaster matplotlib pip twine"
else
    export pkg="numpy pandas cython pyodbc click setuptools wheel sphinx alabaster matplotlib pip twine"
fi

conda create --force -q -n pyfvs python=$TRAVIS_PYTHON_VERSION $pkg
source activate pyfvs
pip install pytest sphinxcontrib-napoleon

conda info -a
