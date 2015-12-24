"""
Tests collecting tree records in a Pandas dataframe
"""

import os
import sys
import shutil
import time
import threading
from datetime import datetime

from numpy.random import choice

# try:
#     import pylab
#     plot = True
#
# except:
#     print 'Plotting requires the matplotlib Python package'
#     plot = False

import pyfvspnc as fvs
import fvs_tables

def readtrees(treelist='pnt01.tre.save'):
    tl = open(treelist).readlines()
    plots = {}
    for l in tl:
        plotid = l.split()[2]
        if not plots.has_key(plotid):
            plots[plotid] = []

        plots[plotid].append(l.replace(plotid, 'xidx').rstrip())

    return plots

def resampleplots(plots):
    newplots = {}
    plotids = plots.keys()
    resample = choice(plotids, len(plotids), replace=True)
    for i in xrange(len(resample)):
        newplots[i] = plots[resample[i]]

    return newplots

def writetrees(plots, treelist='pnt01.tre'):
    with open(treelist, 'w') as tl:
        for id, trees in plots.items():
            for treerow in trees:
                treerow = treerow.replace('xidx', '%04d' % id)
                tl.write('%s\n' % treerow)

class TimeStamper(object):
    """
    Strictly increasing timestamp
    
    http://stackoverflow.com/a/157711/673590
    """
    def __init__(self):
        self.lock = threading.Lock()
        self.prev = None
        self.count = 0

    def getTimestamp(self):
        with self.lock:
            ts = str(datetime.now())
            if ts == self.prev:
                ts += '.%04d' % self.count
                self.count += 1
            else:
                self.prev = ts
                self.count = 1
        return ts

timestamper = TimeStamper()

reps = 1
kwds = ('pnt01.key',)
# kwds = [os.path.join(root,kwd) for kwd in kwds]

# num_cycles = 5
# cycle_len = 5
# year_zero = 2010

h5_path = 'trees.h5'
fvs_tables.init_fvs_tables(h5_path)

# Get a list of species codes for lookup
# (fvs_code,fia_code,plant_code,nchfvs,nchfia,nchplant,rtnCode) = fvsSpeciesCode(index)
# spp_codes = numpy.array(map(lambda x: fvs.fvsspeciescode(x)[0].strip()
#         , range(fvs.prgprm_mod.maxsp)))
spp_codes = [fvs.fvsspeciescode(x)[0].strip() for x in range(fvs.prgprm_mod.maxsp)]

st = time.clock()
rc = 0
for kwd in kwds:
    plottrees = readtrees('pnt01.tre.save')

    for x in xrange(reps):
        print 'Rep:', x
        # bootstrap the plot records
        tls = resampleplots(plottrees)
        writetrees(tls)

        run_id = fvs_tables.init_run(h5_path, x, time.time())

# # Execute FVS using the original API routines
#         cmd = '--keywordfile={}'.format(kwd)
#         fvs.fvssetcmdline(cmd)
# #         fvs.fvs()
#
#         cycle_year = year_zero
#         for cycle in range(1, num_cycles):
#             fvs.fvssetstoppointcodes(6, cycle_year)
#             fvs.fvs()
#             fvs.fvsgetstoppointcodes()
#             cycle_year += cycle_len
#
#         fvs.filclose()

        # Initialize the FVS run
        fvs.fvs_step.fvs_init(kwd)
        num_cycles = fvs.contrl_mod.ncyc

        year_zero = fvs.contrl_mod.iy[0]

        # loop through growth cycles, collecting summary and tree stats
        for cycle in range(num_cycles):
            # call the FVS grower loop
            fvs.fvs_step.fvs_grow()

        # close all IO files
        fvs.fvs_step.fvs_end()
        fvs.filclose()


        fvs_tables.store_trees(
                h5_path, fvs.tree_data, spp_codes, fvs.contrl_mod.iy
                , num_cycles=num_cycles, run_id=run_id)
        fvs_tables.store_snags(
                h5_path, fvs.snag_data, spp_codes, fvs.contrl_mod.iy
                , num_cycles=num_cycles, run_id=run_id)

        rc += 1

et = time.clock()

# replace the overwritten tree data file
shutil.copy2('pnt01.tre.save', 'pnt01.tre')

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / rc)

sys.exit()

# if plot:
#     for r in xrange(summary.shape[0]):
#         pylab.plot(summary[r, :]['tcuft'], color='blue', alpha=0.25)
#     # pylab.boxplot(summary[:,:]['tcuft'])
#
#     xbar_cuft = numpy.mean(summary['tcuft'], axis=0)
#     pylab.plot(xbar_cuft, linewidth=3, color='green')
#     pylab.show()
