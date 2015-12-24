"""
Binary search vs. linear search to estimate site height in FVS

FVS uses a linear search to estimate the age of a site tree of equivalent height
and the potential height growth of a tree from individual species site index
curves.  The approximate age along the site curve is found when a) the search 
returns a site height within a tolerance of two feet, or b) with the next point
along the curve exceeds the current tree height.  Since the resolution of the 
search is two years, there is a potential bias if the search algorithm 
consistently returns the latter.  What's more, the linear search is a rather 
slow method for locating a value in a sorted list.  Algorithms such as binary
search are readily implemented and offer significant performance improvements, 
reduced bias, and increased precision. 
"""

import time
import pandas
from matplotlib import pylab

import pyfvspnc as fvs

spp = 16
site_idx = range(70,152,2)
height = range(20,150,2)

age_step = 2.0
search_toler = 2.0

linear_estimates = []
st = time.clock()
for site in site_idx:
    for ht in height:
        est_age = 0.0

        for i,est_age in enumerate(range(2,402,2)):
            est_ht = fvs.htcalc(site,spp,est_age)
            ht_error = est_ht-ht

            if abs(ht_error)<=search_toler or est_ht>ht:
                break

        if ht_error>100:
            print i,site,ht,est_age,est_ht
            raise
                            
        if est_age==0.0:
            est_age = 200
            est_ht = ht
            ht_error = est_ht-ht
            
        linear_steps = i+1
        linear_estimates.append({
                'method':'linear','species':spp,'site':site,'height':ht
                ,'est_ht':est_ht,'est_age':est_age,'ht_error':ht_error
                ,'steps':linear_steps
                })
                
linear_time = time.clock() - st
print '{:.4f}'.format(linear_time*1000)


#age_incr = (100,50.0,25.0,12.5,6.25,3.0625,1.53125,0.765625)#,.38)#,.19)
age_incr = [64,32,16,8,4,2]
binary_estimates = []
st = time.clock()
for site in site_idx:
    for ht in height:
    
        est_age = 128
        # Run the search to the desired resolution, no need to check the tolerance
        for i,incr in enumerate(age_incr):
            # Binary search steps are constant, but enumerate to be fair
            est_ht = fvs.htcalc(site_idx,spp,est_age)
            if est_ht<ht:
                est_age += incr
            else:
                est_age -= incr

        ht_error = est_ht-ht
        binary_steps = i+1
        binary_estimates.append({
                'method':'binary','species':spp,'site':site,'height':ht
                ,'est_ht':est_ht,'est_age':est_age,'ht_error':ht_error
                ,'steps':binary_steps
                })
        
binary_time = time.clock() - st
print '{:.4f}'.format(binary_time*1000)

foo = pandas.concat([pandas.DataFrame(linear_estimates),pandas.DataFrame(binary_estimates)])
foo_grp = foo.groupby(('method','species'))

legend = []
for k,g in foo_grp:
    print k,g[['height','est_ht','ht_error']].describe()
    pylab.hist(g['ht_error'],alpha = 0.5,bins=30)
    legend.append(k)

pylab.legend(legend)

    
    