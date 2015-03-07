import sys
sys.path.append('c:/workspace/Open-FVS/refactor/')
import refactor_include as rf

inc = {'prgprm.f77':'prgprm_mod'
        ,'arrays.f77':'arrays_mod'
        ,'plot.f77':'plot_mod'
        ,'coeffs.f77':'coeffs_mod'
        ,'esparm.f77':'esparm_mod'
        ,'escomn.f77':'escomn_mod'
        ,'pden.f77':'pden_mod'
        ,'econ.f77':'econ_mod'
        ,'htcal.f77':'htcal_mod'
        ,'contrl.f77':'contrl_mod'
        ,'rancom.f77':'rancom_mod'
        ,'screen.f77':'screen_mod'
        ,'varcom.f77':'varcom_mod'
        ,'fvsstdcm.f77':'fvsstdcm_mod'
        ,'outcom.f77':'outcom_mod'
        }

x = rf.search_depends('.','OUTCOM.F77')
rf.refactor_files(x,inc)