"""
Demonstrate the fvs_step module by iterating through a FVS projection

"""

import os
import sys
import time
import numpy
import pylab

import pyodbc

import pyfvspnc as fvs

dsn = ';'.join(('driver={sql server native client 10.0}'
                , 'server=fm03824\\sqlexpress'
                , 'database=state_forests_inventory_dev'
#                 , 'uid=SFUser', 'pwd=Trees123'
                , 'trusted_connection=yes'))
conn = pyodbc.connect(dsn)
cur = conn.cursor()

stand_sql = 'select * from sli.v_fvs_standinit where sli_revision_uid=35 and stand_id=30006'
stand_rows = cur.execute(stand_sql).fetchall()

trees_sql = 'select * from sli.v_fvs_standinit where sli_revision_uid=35 and stand_id=30006'
tree_rows = cur.execute(trees_sql).fetchall()

conn.close()

# #TODO: not functional below here

kwd = r'foo.key'

num_cycles = 10  # #TODO: add this to a keyword template
base_year = 2013
cycle_len = 5
boot_reps = 1

# Initialize a Numpy integer array to collect the summary records
summary = numpy.zeros((boot_reps, num_cycles + 1, 20), dtype='i')
tree_attrs = numpy.zeros((5, fvs.prgprm_mod.maxtre))

st = time.clock()

# The current FVS API requires using command line style arguments passed to the
# setcommandline subroutine to initialize the FVS arrays
cl = '--keywordfile=%s' % (kwd,)
for cnt in xrange(boot_reps):

    # initialize the run
    i = fvs.fvssetcmdline(cl)

    # print 'FVS Returned with exit code %d' % i

    cycle = 0
    cycle_year = base_year
    fvs.fvs_step.fvs_init()

    while 1:
        tree_attrs[:] = 0.0

        rtn = fvs.fvs_step.fvs_grow()

        if rtn != 0:
            raise ValueError('FVS return with error code %d' % rtn)

#        ntrees, ncycles, nplots, maxtrees, maxspecies, maxplots, maxcycles = fvs.fvsdimsizes()
        fvs.fvstreeattr('dbh', 3, 'get', tree_attrs[0])
        fvs.fvstreeattr('tpa', 3, 'get', tree_attrs[2])
#        trees.write('Year %d\n' % cycle_year)
#        trees.write('DBH\n')
#        trees.write('\n'.join('%.2f' % a for a in attrs))
#        trees.write('\n')

        cycle_year += cycle_len
        cycle += 1

        if cycle >= num_cycles + 1: break

    fvs.fvs_step.fvs_end()

    # close all IO files
    # NOTE: this was added to fvssetcmdline in Open-FVS @ r493
    fvs.filclose()

    # collect the summary values
    for i in xrange(1, num_cycles + 2):
        # by passing a slice of the numpy array, the f2py wrappers ensure the
        # array is modified inplace, this could be changed in the future so
        # that summaries, treelists, etc. are returned as variables.  However,
        # this would likely incur some modest and unecessary overhead.
        r = fvs.fvssummary(summary[cnt, i - 1], i)


    # Print the periodic growth for this iteration to show progress
    print '%3d CUFT %s' % (cnt, ' '.join('%5d' % v for v in summary[cnt, :, 3]))
    # print 'BDFT',','.join('%6d' % v for v in summary[:,5])

et = time.clock()

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / (cnt + 1))

# plot the cubic foot growth curves for each iteration
# mean_curve = numpy.mean(summary[:, :, 3], axis=0)
for s in summary[:]:
    pylab.plot(s[:, 0], s[:, 3])

# pylab.plot(summary[i, :, 0], mean_curve)

# pylab.boxplot(summary[:, :, 3], positions=summary[0, :, 0])
pylab.show()
