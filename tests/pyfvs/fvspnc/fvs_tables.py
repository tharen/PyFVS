import uuid

import numpy
import tables
import pandas

trees_dtype = numpy.dtype([('run_id', long), ('stand_id', long), ('cycle', int)
        , ('year', int), ('plot_id', int), ('tree_seq', int), ('tree_id', int)
        , ('age', int), ('species', 'S2'), ('spp_seq', int), ('live_tpa', float)
        , ('cut_tpa', float), ('mort_tpa', float), ('dbh', float)
        , ('dbh_incr', float), ('ba_pctl', float)
        , ('pnt_bal', int), ('ht_total', float), ('ht_merch_cuft', float)
        , ('ht_merch_bdft', float), ('ht_incr', float), ('cr_width', float)
        , ('cr_ratio', int), ('cuft_total', float), ('cuft_net', float)
        , ('bdft_net', float), ('defect_cuft', int), ('defect_bdft', int)
        ])

# Names of tree_data module variables
# Order is important and needs to match trees_dtype
tree_data_names = ('plot_seq', 'tree_seq', 'tree_id', 'age', 'spp_seq'
        , 'live_tpa', 'cut_tpa', 'mort_tpa', 'live_dbh', 'dbh_incr', 'ba_pctl'
        , 'pnt_bal', 'ht_total', 'ht_merch_cuft', 'ht_merch_bdft', 'ht_incr'
        , 'cr_width', 'cr_ratio', 'cuft_total', 'cuft_net', 'bdft_net'
        , 'defect_cuft', 'defect_bdft')

def trees_to_recarray(
        tree_data, spp_codes, run_id=0, stand_id=0
        , year_zero=0, cycle_len=10):
    """
    Copy FVS projection tree lists to a Numpy record array. 
    """

    x = tree_data.tree_id.shape[0]
    y = tree_data.tree_id.shape[1]
    idx = numpy.array([range(y), ] * x, dtype=int)
    mask = idx < tree_data.num_recs[:, numpy.newaxis]

    l = []
    d = [numpy.zeros_like(tree_data.plot_seq, dtype=int)[mask].flatten()
            , numpy.zeros_like(tree_data.plot_seq, dtype=int)[mask].flatten()
            , numpy.zeros_like(tree_data.plot_seq, dtype=int)
            , None
            ]

    d[0][:] = run_id
    d[1][:] = stand_id
    d[2][:] = numpy.array(range(x))[:, numpy.newaxis]
    d[2] = d[2][mask].flatten()
    d[3] = d[2] * cycle_len + year_zero

    for name in tree_data_names:
        if name == 'spp_seq':
            d.append(spp_codes[getattr(tree_data, name)[mask].flatten()])
            d.append(getattr(tree_data, name)[mask].flatten())
        elif name == 'plot_seq':
            # TODO: add plot_id lookup like spp_codes
            d.append(getattr(tree_data, name)[mask].flatten())
        else:
            d.append(getattr(tree_data, name)[mask].flatten())

    df = numpy.core.records.array(d, dtype=trees_dtype)

    return df

def trees_to_dataframe(
        tree_data, spp_codes, run_id=0, stand_id=0
        , year_zero=0, cycle_len=10):
    """
    Copy FVS projection tree lists to a Pandas dataframe. 
    """

    x = tree_data.tree_id.shape[0]
    y = tree_data.tree_id.shape[1]
    idx = numpy.array([range(y), ] * x, dtype=int)
    mask = idx < tree_data.num_recs[:, numpy.newaxis]

    l = []
    d = [numpy.zeros_like(tree_data.plot_seq, dtype=int)[mask].flatten()
            , numpy.zeros_like(tree_data.plot_seq, dtype=int)[mask].flatten()
            , numpy.zeros_like(tree_data.plot_seq, dtype=int)
            , None
            ]

    d[0][:] = run_id
    d[1][:] = stand_id
    d[2][:] = numpy.array(range(x))[:, numpy.newaxis]
    d[2] = d[2][mask].flatten()
    d[3] = d[2] * cycle_len + year_zero

    for name in tree_data_names:
        if name == 'spp_seq':
            d.append(spp_codes[getattr(tree_data, name)[mask].flatten()])
            d.append(getattr(tree_data, name)[mask].flatten())
        elif name == 'plot_seq':
            d.append(getattr(tree_data, name)[mask].flatten())
        else:
            d.append(getattr(tree_data, name)[mask].flatten())

    df = pandas.DataFrame(dict(zip(trees_dtype.names, d)))

    return df

class RunMaster(tables.IsDescription):
    """
    Master table linking projections to data tables
    """
    run_id = tables.StringCol(36, pos=0)
    stand_id = tables.UInt16Col(pos=1)
    silv_regime = tables.StringCol(36, pos=2)
    num_cycles = tables.UInt16Col(pos=3)
    min_year = tables.UInt16Col(pos=4)
    max_year = tables.UInt16Col(pos=5)
    # timestamp = tables.UInt32Col(pos=2)
    timestamp = tables.Float64Col(pos=6)

class SnagData(tables.IsDescription):
    """
    Snag data array definitions
    real, dimension(maxcy1,mxsnag) :: hard_density, soft_density &
            , dbh_dead, hard_ht, soft_ht, hard_vol, soft_vol
    integer, dimension(maxcy1,mxsnag) :: spp, year_dead
    """
    run_id = tables.StringCol(36, pos=0)
    cycle = tables.UInt16Col(pos=1)
    year = tables.UInt16Col(pos=2)
    species = tables.StringCol(2, pos=3)
    dbh_dead = tables.Float16Col(pos=4)
    year_dead = tables.UInt16Col(pos=5)
    hard_density = tables.Float16Col(pos=6)
    hard_ht = tables.Float16Col(pos=7)
    hard_vol = tables.Float16Col(pos=8)
    soft_density = tables.Float16Col(pos=9)
    soft_ht = tables.Float16Col(pos=10)
    soft_vol = tables.Float16Col(pos=11)

class TreeData(tables.IsDescription):
    """
    FVS tree_data array definitions
    integer, dimension(maxcy1,maxtre) :: tree_id,plot_seq,age,spp_seq
    real, dimension(maxcy1,maxtre) :: live_tpa,cut_tpa,mort_tpa
    real, dimension(maxcy1,maxtre) :: live_dbh,dbh_incr,ba_pctl,pnt_bal
    real, dimension(maxcy1,maxtre) :: ht_total,ht_merch_cuft,ht_merch_bdft,ht_incr
    real, dimension(maxcy1,maxtre) :: cr_width,cr_ratio
    real, dimension(maxcy1,maxtre) :: cuft_total,cuft_net,bdft_net &
            ,defect_cuft,defect_bdft
    """
    run_id = tables.StringCol(36, pos=0)
    cycle = tables.UInt16Col(pos=1)
    year = tables.UInt16Col(pos=2)
    plot_id = tables.UInt8Col(pos=4)
    tree_seq = tables.UInt16Col(pos=5)
    tree_id = tables.UInt16Col(pos=6)
    age = tables.UInt8Col(pos=7)
    species = tables.StringCol(2, pos=8)
    spp_seq = tables.UInt8Col(pos=9)
    live_tpa = tables.Float32Col(pos=10)
    cut_tpa = tables.Float32Col(pos=11)
    mort_tpa = tables.Float32Col(pos=12)
    dbh = tables.Float32Col(pos=13)
    dbh_incr = tables.Float32Col(pos=14)
    ba_pctl = tables.Float32Col(pos=15)
    pnt_bal = tables.Float32Col(pos=16)
    ht_total = tables.Float32Col(pos=17)
    ht_merch_cuft = tables.Float32Col(pos=18)
    ht_merch_bdft = tables.Float32Col(pos=19)
    ht_incr = tables.Float32Col(pos=20)
    cr_width = tables.Float32Col(pos=21)
    cr_ratio = tables.Float32Col(pos=22)
    cuft_total = tables.Float32Col(pos=23)
    cuft_net = tables.Float32Col(pos=24)
    bdft_net = tables.Float32Col(pos=25)
    defect_cuft = tables.Float32Col(pos=26)
    defect_bdft = tables.Float32Col(pos=27)

def init_fvs_tables(fp, title=''):
    h5 = tables.openFile(fp, mode='w', title=title)
    f = tables.Filters(complib='blosc')
    h5.create_table('/', 'run_master', RunMaster, 'FVS Run Primary Table', filters=f)
    h5.create_table('/', 'tree_data', TreeData, 'FVS Tree Data', filters=f)
    h5.create_table('/', 'snag_data', SnagData, 'FVS Snag Data', filters=f)
    h5.flush()

    h5.root.run_master.cols.run_id.create_index()

    h5.root.tree_data.cols.run_id.create_index()
    h5.root.tree_data.cols.cycle.create_index()

    h5.root.snag_data.cols.run_id.create_index()
    h5.root.snag_data.cols.cycle.create_index()

    h5.close()

def init_run(h5_path, stand_id, timestamp):
    h5 = tables.open_file(h5_path, mode='a')
    tbl = h5.root.run_master
#     try:
#         id = max(tbl.cols.run_id) + 1  # [tbl.colindexes['run_id'][-1]] + 1
#
#     except ValueError:
#         id = 0
    id = uuid.uuid1()

    row = h5.root.run_master.row
    row['run_id'] = id
    row['stand_id'] = stand_id
    row['timestamp'] = 0
    row['timestamp'] = timestamp
    row.append()
    h5.close()
    return id

def delete_run(h5_path, run_id):
    h5 = tables.open_file(h5_path, mode='a')

    # Delete snags
    rows = h5.root.snag_data['run_id'] == run_id
    h5.root.snag_data.remove_rows(rows)

    # Delete trees
    rows = h5.root.tree_data['run_id'] == run_id
    h5.root.tree_data.remove_rows(rows)

    # Delete the run
    rows = h5.root.run_master['run_id'] == run_id
    h5.root.run_master.remove_rows(rows)

    h5.close()

    return True

def wavg(group):
    d = group['dbh_dead']
    w = group['hard_density']
    return (d * w).sum() / w.sum()

def store_snags(h5_path, snag_data, spp_codes, cycle_years
        , num_cycles=40, run_id=None):
    """
    Append FVS projection detail snag estimates to the snag_data HDF table.
    """
    h5 = tables.open_file(h5_path, mode='a')
    row = h5.root.snag_data.row

    columns = [
            'dbh_dead', 'year_dead'
            , 'hard_density', 'hard_ht', 'hard_vol'
            , 'soft_density', 'soft_ht', 'soft_vol'
            ]

    # TODO: compress the snag list using Pandas groupby and apply methods

    all_columns = ['spp_seq', ] + columns
    for i in range(num_cycles + 1):
        rc = snag_data.num_recs[i]
        snag_df = pandas.DataFrame(dict(zip(all_columns, [getattr(snag_data, col)[i, :rc] for col in all_columns])))
        snag_df['dbhgrp'] = (snag_df['dbh_dead'] / 5).astype(int) * 5
        snag_df['hard_htgrp'] = (snag_df['hard_ht'] / 10).astype(int) * 10
        snag_df['soft_htgrp'] = (snag_df['soft_ht'] / 10).astype(int) * 10
        snag_grps = snag_df.groupby(('spp_seq', 'dbhgrp', 'year_dead', 'hard_htgrp', 'soft_htgrp')
                , sort=True)

        blah = snag_grps.apply(wavg)
        blah.to_csv('foo.txt', mode='a')

        for r in xrange(rc):
            row['run_id'] = run_id
            row['cycle'] = i
            row['year'] = cycle_years[i]
            row['species'] = spp_codes[getattr(snag_data, 'spp_seq')[i, r]]
            for col in columns:
                row[col] = getattr(snag_data, col)[i, r]

            row.append()

    h5.close()

def store_trees(h5_path, tree_data, spp_codes, cycle_years
        , num_cycles=40, run_id=None):
    """
    Append a projection tree list in a tree_data HDF table
    """
    h5 = tables.open_file(h5_path, mode='a')

    # TODO: add plot_id lookup argument
    row = h5.root.tree_data.row
    for i in range(num_cycles + 1):
        rc = tree_data.num_recs[i]
        for r in xrange(rc):
            row['run_id'] = run_id
            row['cycle'] = i
            row['year'] = cycle_years[i]
            # #TODO: add plot_id lookup like spp_codes
            row['plot_id'] = tree_data.plot_seq[i, r]
            row['tree_seq'] = tree_data.tree_seq[i, r]
            row['tree_id'] = tree_data.tree_id[i, r]
            row['age'] = tree_data.age[i, r]
            row['species'] = spp_codes[tree_data.spp_seq[i, r]]
            row['spp_seq'] = tree_data.spp_seq[i, r]
            row['live_tpa'] = tree_data.live_tpa[i, r]
            row['cut_tpa'] = tree_data.cut_tpa[i, r]
            row['mort_tpa'] = tree_data.mort_tpa[i, r]
            row['dbh'] = tree_data.live_dbh[i, r]
            row['dbh_incr'] = tree_data.dbh_incr[i, r]
            row['ba_pctl'] = tree_data.ba_pctl[i, r]
            row['pnt_bal'] = tree_data.pnt_bal[i, r]
            row['ht_total'] = tree_data.ht_total[i, r]
            row['ht_merch_cuft'] = tree_data.ht_merch_cuft[i, r]
            row['ht_merch_bdft'] = tree_data.ht_merch_bdft[i, r]
            row['ht_incr'] = tree_data.ht_incr[i, r]
            row['cr_width'] = tree_data.cr_width[i, r]
            row['cr_ratio'] = tree_data.cr_ratio[i, r]
            row['cuft_total'] = tree_data.cuft_total[i, r]
            row['cuft_net'] = tree_data.cuft_net[i, r]
            row['bdft_net'] = tree_data.bdft_net[i, r]
            row['defect_cuft'] = tree_data.defect_cuft[i, r]
            row['defect_bdft'] = tree_data.defect_bdft[i, r]

            row.append()

    h5.close()
