"""
Demonstrates calling fvs from python as well as a method to bootstrap the plot
records.
"""

import os
import sys
import shutil
import time
import numpy

try:
    import pylab
    plot = True

except:
    print 'Plotting requires the matplotlib Python package'
    plot = False

import pyfvspnc as fvs
print fvs.__file__

kwds = [os.path.join(r'C:\workspace\yield_tables\trees', f) for f in os.listdir(r'C:\workspace\yield_tables\trees') if f.endswith('.key')]
kwds.sort()
kwds = kwds[:10]
num_runs = len(kwds)

num_cycles = 20
cycle_len = 5

# Create a numpy recarray of summary variables
sumvars = ('year', 'age', 'tpa', 'tcuft', 'mcuft', 'mbdft', 'rtpa', 'rtcuft'
        , 'rmcuft', 'rmbdft', 'baa', 'ccf', 'topht', 'perlen', 'accr', 'mort'
        , 'samwt', 'cvrtype', 'sizecls', 'stkcls')

summary = numpy.zeros(
        (num_runs, num_cycles + 1)
        , dtype=zip(sumvars, ['I4'] * len(sumvars))
        )

# temporary array to hold current growth cycle summary variables
repsum = numpy.zeros(len(sumvars), dtype='I4')

trees = numpy.zeros(1, dtype=[('year', 'i4'), ('cycle', 'i4'), ('spp', 'a2'), ('tpa', 'f4')])

spp_codes = numpy.array([fvs.fvsspeciescode(i) for i in range(0, fvs.prgprm_mod.maxsp)])

st = time.clock()
run_id = 0
for k, kwd in enumerate(kwds):
        cl = '--keywordfile=%s' % (kwd,)

        # initialize the run
        i = fvs.fvs_step.fvs_init(kwd)
        summary[k, 0] = fvs.outcom_mod.iosum[:, 0]

        fvs.contrl_mod.ncyc = num_cycles + 1
        for i in xrange(2, fvs.contrl_mod.ncyc):
            fvs.contrl_mod.iy[i] = fvs.contrl_mod.iy[i - 1] + cycle_len

        # loop through growth cycles, collecting summary and tree stats
        for cycle in range(0, fvs.contrl_mod.ncyc):

            fvs.fvs_step.fvs_grow()
            summary[k, cycle] = fvs.outcom_mod.iosum[:, cycle]

        # close all IO files
        fvs.fvs_step.fvs_end()
        fvs.filclose()

        print 'Run: %-4d' % (k + 1,), ','.join('%6.0f' % v for v in summary[k, :]['mbdft'])  # (1.0 * summary[k, :]['mbdft'] / summary[k, :]['age']))

numpy.savetxt('trees.txt', fvs.tree_data.live_tpa, fmt='%.3f')
print numpy.sum(fvs.tree_data.live_tpa[1, :] * fvs.tree_data.cuft_total[1, :])

et = time.clock()
print 'Total Time: {:.1f} {:.3f} sec per run.'.format(et - st, (et - st) / (k + 1))

if plot:
    for r in xrange(summary.shape[0]):
        pylab.plot(summary[r, :]['mbdft'], color='blue', alpha=0.25)
    # pylab.boxplot(summary[:,:]['tcuft'])

    xbar_cuft = numpy.mean(summary['mbdft'], axis=0)
    pylab.plot(xbar_cuft, linewidth=3, color='green')
    pylab.show()
