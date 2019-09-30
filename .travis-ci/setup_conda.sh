conda config --set always_yes yes --set changeps1 no
conda config --add channels conda-forge
conda update -y -q --all
conda --version

# hash -r
# Edit the environment.yml file for the target Python version
# sed -i -E 's/(python=)(.*)/\1'$TRAVIS_PYTHON_VERSION'/' ./environment.yml
# conda env create --force -q -n pyfvs -f ${TRAVIS_BUILD_DIR}/environment.yml

export pkg="numpy pandas cython pyodbc click setuptools wheel sphinx alabaster matplotlib pip twine"

conda create -y -q --update-deps --clobber -n pyfvs python=$TRAVIS_PYTHON_VERSION $pkg
source activate pyfvs
pip install pytest sphinxcontrib-napoleon

conda info -a
