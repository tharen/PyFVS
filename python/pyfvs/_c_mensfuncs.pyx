"""
Functions to compute mensurational summaries and statistics from tree lists.
"""

import numpy as np

import numexpr as ne
import pandas as pd

cimport numpy as np
cimport cython
from libc.math cimport sqrt

#DTYPE = np.int
#ctypedef np.int_t DTYPE_t

@cython.boundscheck(False)
def _cython_calc_tree_yield(
        np.ndarray[np.float32_t, ndim=1] tpa
        ,np.ndarray[np.float32_t, ndim=1] dbh
        ,np.ndarray[np.float32_t, ndim=1] ht
        ,np.ndarray[np.double_t, ndim=1] bdft
        ,np.ndarray[np.double_t, ndim=1] cuft
        ,np.ndarray[np.double_t, ndim=1] tot_cuft
        ,np.ndarray[np.double_t, ndim=1] pond
        ,np.ndarray[np.double_t, ndim=1] ht_wgt
        ):

    cdef unsigned int i
    cdef float tpa_sum=0.0
    cdef float baa_sum=0.0
    cdef float qmd_=0.0
    cdef float sdi_sum=0.0
    cdef float bdft_net_sum=0.0
    cdef float cuft_net_sum=0.0
    cdef float cuft_total_sum=0.0
    cdef float pond_value_sum=0.0
    cdef float ht_wgt_sum=0.0

    cdef np.ndarray[float] top = np.zeros(2,dtype=np.float32)
    cdef np.ndarray[float] vals = np.zeros(11,dtype=np.float32)

    for i in range(tpa.shape[0]):
        tpa_sum += tpa[i]
        baa_sum += tpa[i] * dbh[i] * dbh[i] * 0.005454154
        qmd_ += tpa[i] * dbh[i] * dbh[i]
        sdi_sum += tpa[i] * (dbh[i]/10.0)**1.605
        bdft_net_sum += tpa[i] * bdft[i]
        cuft_net_sum += tpa[i] * cuft[i]
        cuft_total_sum += tpa[i] * tot_cuft[i]
        pond_value_sum += tpa[i] * pond[i]
        ht_wgt_sum += ht_wgt[i]

    top[:] = _cython_calc_topht(dbh, tpa, ht, 40.0)

    if tpa_sum>0.0:
        ht_sum = ht_wgt_sum / baa_sum
        qmd_sum = sqrt(qmd_ / tpa_sum)

        #TODO: populate the existing s series
        vals[0] = tpa_sum
        vals[1] = baa_sum
        vals[2] = qmd_sum
        vals[3] = sdi_sum
        vals[4] = ht_sum
        vals[5] = top[0] #top_ht
        vals[6] = top[1] #top_dbh
        vals[7] = bdft_net_sum
        vals[8] = cuft_net_sum
        vals[9] = cuft_total_sum
        vals[10] = pond_value_sum

    return vals

@cython.boundscheck(False)
cpdef _cython_calc_topht(
        np.ndarray[np.float32_t, ndim=1] dbh
        ,np.ndarray[np.float32_t, ndim=1] tpa
        ,np.ndarray[np.float32_t, ndim=1] ht
        ,np.float n=40.0):
    """
    Return the average height and diameter of the n largest trees by diameter.
    """
    cdef int i
    cdef double delta
    cdef double cumtpa = 0.0
    cdef double cumht = 0.0
    cdef double cumd = 0.0
    cdef double topht = 0.0
    cdef double topd = 0.0
    cdef np.ndarray[np.int_t, ndim=1] idx = np.zeros(dbh.shape[0], dtype=np.int)
    cdef np.ndarray[np.double_t, ndim=1] top = np.zeros(2, dtype=np.double)

    idx[:] = np.argsort(dbh)
    for i in idx[::-1]:
        cumtpa += tpa[i]
        cumht += tpa[i] * ht[i]
        cumd += tpa[i] * dbh[i]**2.0

        if cumtpa == n:
            break

        # Finalize the search by adjusting the weight of the last tree added
        if cumtpa > n:
            delta = cumtpa - n
            cumtpa -= delta
            cumht -= delta * ht[i]
            cumd -= delta * dbh[i]**2.0
            break

    if cumtpa > 0.0:
        topht = cumht / cumtpa
        topd = (cumd / cumtpa)**0.5
    else:
        topht = 0.0
        topd = 0.0

    top[0] = topht
    top[1] = topd

    return top
