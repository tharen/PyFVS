import numpy
import tables
import pandas

trees_dtype = numpy.dtype([('run_id', long), ('stand_id', long), ('period', int)
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
        , year_zero=0, period_len=10):
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
    d[3] = d[2] * period_len + year_zero

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
        , year_zero=0, period_len=10):
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
    d[3] = d[2] * period_len + year_zero

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
    run_id = tables.UInt32Col(pos=0)
    stand_id = tables.UInt16Col(pos=3)
    period = tables.UInt16Col(pos=1)
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

def init_tree_store(fp, title=''):
    h5 = tables.openFile(fp, mode='w', title=title)
    f = tables.Filters(complib='blosc')
    h5.create_table('/', 'tree_data', TreeData, 'FVS Tree Data', filters=f)
    h5.flush()
    h5.root.tree_data.cols.run_id.create_index()
    h5.root.tree_data.cols.period.create_index()
    h5.root.tree_data.cols.stand_id.create_index()

    return h5

def store_trees(h5, tree_data, spp_codes, stand_id=None, run_id=None):
    """
    Append a projection tree list in a tree_data HDF table
    """
    # TODO: add plot_id lookup argument
    row = h5.root.tree_data.row
    for i, rc in enumerate(tree_data.num_recs):
        for r in xrange(rc):
            row['run_id'] = run_id
            row['period'] = i
            row['stand_id'] = stand_id
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

    h5.root.tree_data.flush()

