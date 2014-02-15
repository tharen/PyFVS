
# $Id: makefile 469 2012-11-15 04:39:49Z ncrookston.fs@gmail.com $

all : Rapi

Rapi : 
	@echo Test Rapi: test api functions using R
	R --vanilla --quiet < Rapi.R > Rapi.out
	diff -w Rapi.out Rapi.out.save
	@echo Test Rapi ended
	

