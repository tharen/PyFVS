"""
Demonstrate the fvs_step module by iterating through a FVS projection

"""

import os
import sys
import time
import numpy
import pylab

import pyfvspnc as fvs

print fvs.__file__

# #NOTE: Currently the FVS API expects a treelist file with the same basename
#           if the dbs extension is not being used.
kwd = r'C:\workspace\Open-FVS\google_code\branches\PyFVS\tests\pyfvs\fvspnc\pnt01.key'

num_cycles = 10  # #TODO: add this to a keyword template
year_zero = 1990  # #TODO: get this from FVS
cycle_len = 10
reps = 1

# Initialize a Numpy integer array to collect the summary records
# #FIXME: Array size parameters need to be in a Fortran module
ntrees, ncycles, nplots, maxtrees, maxspecies, maxplots, maxcycles = fvs.fvsdimsizes()
summary = numpy.zeros((reps, num_cycles + 1, 20), dtype='i')
tree_attrs = numpy.zeros((5, maxtrees))

st = time.clock()

trees = open('trees.txt', 'w')

# The current FVS API requires using command line style arguments passed to the
# setcommandline subroutine to initialize the FVS arrays
cl = '--keywordfile=%s' % (kwd,)
for rep in xrange(reps):

    # initialize the run
    i = fvs.fvssetcmdline(cl)

    # print 'FVS Returned with exit code %d' % i

    cycle_year = year_zero
    fvs.fvs_step.fvs_init(kwd)
    # populate the initial summary record
    fvs.fvssummary(summary[rep, 0], 1)

    for cycle in xrange(1, num_cycles + 1):
        tree_attrs[:] = 0.0

        rtn = fvs.fvs_step.fvs_grow()

        if rtn != 0:
            raise ValueError('FVS return with error code %d' % rtn)

        # load the rep summary array for the current cycle
        # #NOTE: copying the IOSUM array could happen after the run
        # #NOTE: IOSUM at the end of the cycle is populated with pre-growth values
        fvs.fvssummary(summary[rep, cycle], cycle + 1)

#        ntrees, ncycles, nplots, maxtrees, maxspecies, maxplots, maxcycles = fvs.fvsdimsizes()
#        fvs.fvstreeattr('dbh', 3, 'get', tree_attrs[0, :ntrees])
#        fvs.fvstreeattr('tpa', 3, 'get', tree_attrs[1, :ntrees])

        itrn = fvs.contrl.itrn
        tree_attrs[0, :] = fvs.arrays.dbh[:]
        tree_attrs[1, :] = fvs.arrays.prob[:]

        baa = sum(fvs.arrays.dbh[:] ** 2 * fvs.arrays.prob[:] * 0.005454154) / fvs.plot.grospc + .5
        print summary[rep, cycle, 10], baa
#        print summary[rep, cycle, 2], sum(tree_attrs[1, :]) / fvs.plot.grospc + .5, fvs.outcom.ontcur[6] / fvs.plot.grospc + .5

#        tpa = fvs.arrays.prob[:itrn]
#        dbh = fvs.arrays.dbh[:itrn]
#
#        print sum(tree_attrs[1, :]) - sum(tpa)
#        print sum(tree_attrs[1, :] * tree_attrs[0, :] ** 2) - sum(fvs.arrays.prob * fvs.arrays.dbh ** 2)

#        trees.write('Year %d\n' % cycle_year)
#        trees.write('DBH\n')
#        trees.write('\n'.join('%.2f' % a for a in attrs))
#        trees.write('\n')

        cycle_year += cycle_len

    fvs.fvs_step.fvs_end()

    # close all IO files
    # NOTE: this was added to fvssetcmdline in Open-FVS @ r493
    fvs.filclose()

    # collect the summary values
#    for i in xrange(1, num_cycles + 2):
        # by passing a slice of the numpy array, the f2py wrappers ensure the
        # array is modified inplace, this could be changed in the future so
        # that summaries, treelists, etc. are returned as variables.  However,
        # this would likely incur some modest and unecessary overhead.



    # Print the periodic growth for this iteration to show progress
    print '%3d CUFT %s' % (rep, ' '.join('%5d' % v for v in summary[rep, :, 3]))
    # print 'BDFT',','.join('%6d' % v for v in summary[:,5])

et = time.clock()

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / (rep + 1))

sys.exit()

# plot the cubic foot growth curves for each iteration
# mean_curve = numpy.mean(summary[:, :, 3], axis=0)
for s in summary[:]:
    pylab.plot(s[:, 0], s[:, 3])

# pylab.plot(summary[i, :, 0], mean_curve)

# pylab.boxplot(summary[:, :, 3], positions=summary[0, :, 0])
pylab.show()
