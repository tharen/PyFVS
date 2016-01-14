'''
Created on Jan 12, 2016

@author: THAREN
'''
import unittest

# https://pypi.python.org/pypi/nose-parameterized/
from nose_parameterized import parameterized

from pyfvs import fvs

variants = [
        ('pnc_basic', 'pnc'),
        ('wcc_basic', 'wcc')]

def nw_bg_test_kwds():
    kwds = """STDIDENT
TEST_BG   Testing a simple bare ground run
STDINFO          712    CHS131         0       200        35         6
MANAGED            0         1

INVYEAR         2010
DESIGN             0         1       999         1         0         1         1

NUMCYCLE          10

NOTREES
ESTAB           2010
PLANT           2011        DF       450        95         2         1         0
END

ECHOSUM

PROCESS
STOP

"""
    return kwds

class VariantTests(unittest.TestCase):
    @parameterized.expand(variants)
    def test_load_variant(self, name, variant):
        f = fvs.FVS(variant)

    @parameterized.expand(variants)
    def test_variant_name(self, name, variant):
        f = fvs.FVS(variant)
        self.assertEqual(f.variant, variant, 'Variant name failed')

    @parameterized.expand(variants)
    def test_fsv_step_fvs_grow(self, name, variant):
        f = fvs.FVS(variant)
        p = './bg_test.key'
        with open(p, 'w') as foo:
            foo.write(nw_bg_test_kwds())
        f.fvs_step.fvs_init(p)

        for c in range(f.contrl_mod.ncyc):
            r = f.fvs_step.fvs_grow()

        r = f.fvs_step.fvs_end()
        print('FVS Return Code: %s' % r)
        self.assertEqual(r, 0, 'FVS Return Code: %s' % r)

