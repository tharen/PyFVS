"""
Tests collecting tree records in a Pandas dataframe
"""

import os
import sys
import shutil
import time
import numpy

import pandas

try:
    import pylab
    plot = True

except:
    print 'Plotting requires the matplotlib Python package'
    plot = False

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
    resample = numpy.random.choice(plotids, len(plotids), replace=True)
    for i in xrange(len(resample)):
        newplots[i] = plots[resample[i]]

    return newplots

def writetrees(plots, treelist='pnt01.tre'):
    with open(treelist, 'w') as tl:
        for id, trees in plots.items():
            for treerow in trees:
                treerow = treerow.replace('xidx', '%04d' % id)
                tl.write('%s\n' % treerow)


reps = 1
kwds = ('pnt01.key',)  # 'pnt02.key')
# kwds = [os.path.join(root,kwd) for kwd in kwds]

num_periods = 5
period_len = 10
year_zero = 1990

trees_df = pandas.DataFrame(dtype=fvs_tables.trees_dtype)

h5 = fvs_tables.init_tree_store('trees.h5')

# Create a numpy recarray of summary variables
sumvars = ('year', 'age', 'tpa', 'tcuft', 'mcuft', 'mbdft', 'rtpa', 'rtcuft'
        , 'rmcuft', 'rmbdft', 'baa', 'ccf', 'topht', 'perlen', 'accr', 'mort'
        , 'samwt', 'cvrtype', 'sizecls', 'stkcls')

# Get a list of species codes for lookup
# (fvs_code,fia_code,plant_code,nchfvs,nchfia,nchplant,rtnCode) = fvsSpeciesCode(index)
spp_codes = numpy.array(map(lambda x: fvs.fvsspeciescode(x)[0].strip()
        , range(fvs.prgprm_mod.maxsp)))

st = time.clock()
run_id = 0
for kwd in kwds:
    plottrees = readtrees('pnt01.tre.save')

    rep_trees = []
    for x in xrange(reps):
        print 'Rep:', x
        # bootstrap the plot records
        tls = resampleplots(plottrees)
        writetrees(tls)

        # cl = numpy.array((1,),dtype='a100')
        cl = '--keywordfile=%s' % (kwd,)

        # initialize the run
        i = fvs.fvssetcmdline(cl)

        cycle_year = year_zero

        # Initialize the FVS run
        fvs.fvs_step.fvs_init()

        # loop through growth cycles, collecting summary and tree stats
        for cycle in range(1, num_periods + 1):

            # call the FVS grower loop
            fvs.fvs_step.fvs_grow()
            # print 'FVS Returned with exit code %d' % rtn

            cycle_year += period_len


        # close all IO files
        fvs.fvs_step.fvs_end()
        fvs.filclose()

        run_id += 1

        rep_trees.append(
                fvs_tables.trees_to_dataframe(
                        fvs.tree_data, spp_codes, stand_id=0, run_id=x
                        , year_zero=year_zero, period_len=period_len))
        
        d=fvs.snag_data
        snags = pandas.DataFrame({
                'snag_spp':d.snag_spp[cycle,:]
                ,'snag_dbh':d.snag_dbh[cycle,:]
                ,'soft_snags':d.snag_dense_soft[cycle,:]
                ,'hard_snags':d.snag_dense_hard[cycle,:]
                ,'total_snags':d.snag_dense_hard[cycle,:]+d.snag_dense_soft[cycle,:]
                })
        snags.to_csv('snags.csv')
        
#         df = fvs_tables.trees_to_dataframe(fvs.tree_data, spp_codes
#                 , stand_id=0, run_id=x, year_zero=year_zero, period_len=period_len)
#         trees_df = pandas.concat([trees_df, df])

#         fvs_tables.store_trees(h5, fvs.tree_data, spp_codes, stand_id=0, run_id=x)

    trees_df = pandas.concat(rep_trees)
#     trees_df = pandas.DataFrame(numpy.hstack(rep_trees))
    trees_df.to_csv('trees.txt', float_format='%.4f', index=False
            , cols=fvs_tables.trees_dtype.names)

#     hdfstore = pandas.HDFStore('trees.h5')
#     hdfstore.put('/tree_datax', trees_df, format='t')

h5.close()

#         # Collect the projected trees
#         i = numpy.sum(fvs.tree_data.spp_seq != 0)
#         tree_id = fvs.tree_data.tree_id[cycle, :i]
#         prob = fvs.tree_data.live_tpa[cycle, :i]
#         spp = spp_codes[fvs.tree_data.spp_seq[cycle, :i]]
#         dbh = fvs.tree_data.live_dbh[cycle, :i]
#         ht_total = fvs.tree_data.ht_total[cycle, :i]
#         ht_merch_cuft = fvs.tree_data.ht_merch_cuft[cycle, :i]
#         ht_merch_bdft = fvs.tree_data.ht_merch_bdft[cycle, :i]
#
#         trees = pandas.DataFrame({'tree_id':tree_id, 'prob':prob, 'species':spp
#                                   , 'dbh':dbh, 'ht_total':ht_total
#                                   , 'ht_merch_cuft':ht_merch_cuft
#                                   , 'ht_merch_bdft':ht_merch_bdft
#                                   })
#         dt = numpy.dtype([('tree_id', 'U4'), ('prob', 'F8'), ('spp', 'S2')])
#         trees = numpy.core.records.fromarrays([tree_id, prob, spp], dtype=dt)

#         trees = trees_df.from_dict({'tree_id':tree_id
#                                     , 'prob':prob
#                                     , 'year':999
#                                     }, dtype=trees_dtype)
#
#
# trees.to_csv('trees.txt')

et = time.clock()

# replace the overwritten tree data file
shutil.copy2('pnt01.tre.save', 'pnt01.tre')

# #summary_names = ('year','age','tpa_b','gross_cuft_b','merch_cuft_b','gross_bdft_b','net_bdft_b','tpa_r','gross_cuft_r','top_cuft_r','gross_merch_cuft_r','net_merch_cuft_r','gross_bdft_r','net_bdft_r','baa_a','ccf_a','topht_a','per_len','accr_cuft','mort_cuft','sam_wt','for_cvr','size_class','stock_class')
# ##summary = numpy.recarray((num_periods+1,20),dtype='i',names=summary_names)

print '%d reps; total elapsed time: %.2f, %.3f second per rep' % (reps, et - st, (et - st) / run_id)

sys.exit()

if plot:
    for r in xrange(summary.shape[0]):
        pylab.plot(summary[r, :]['tcuft'], color='blue', alpha=0.25)
    # pylab.boxplot(summary[:,:]['tcuft'])

    xbar_cuft = numpy.mean(summary['tcuft'], axis=0)
    pylab.plot(xbar_cuft, linewidth=3, color='green')
    pylab.show()
