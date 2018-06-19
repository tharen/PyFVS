#!/bin/bash

# Use miniconda in travis-ci http://conda.pydata.org/docs/travis.html
# We do this conditionally because it saves us some downloading if the
# version is the same.
if test -e $HOME/miniconda/bin ; then
    echo "Miniconda is already installed."

else
    echo "Installing Miniconda."
    
    rm -rf $HOME/miniconda
    mkdir -p $HOME/download
    
    if [[ "$TRAVIS_PYTHON_VERSION" == "2.7" ]]; then
      wget https://repo.continuum.io/miniconda/Miniconda-latest-Linux-x86_64.sh -O $HOME/download/miniconda.sh;

    else
      wget https://repo.continuum.io/miniconda/Miniconda3-latest-Linux-x86_64.sh -O $HOME/download/miniconda.sh;
    fi
    
    chmod +x $HOME/download/miniconda.sh
    $HOME/download/miniconda.sh -b -p $HOME/miniconda
    $HOME/miniconda/bin/conda update -q conda --yes

fi

