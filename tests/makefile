
#	$Id: makefile 242 2012-05-11 14:30:23Z ncrookston.fs@gmail.com $	

# runs all the tests where the directory names start with FVS, ignoring errors.

export TestDirs = $(shell ls -d FVS*)

all : $(addsuffix .test,$(TestDirs))
	  
%.test : 
	cd $(basename $@); ${MAKE} --ignore-errors
	

