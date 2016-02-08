'''
Created on Feb 2, 2016

@author: THAREN
'''

import unittest

# https://pypi.python.org/pypi/nose-parameterized/
from nose_parameterized import parameterized

from pyfvs import fvs
from pyfvs.keywords import keywords as kw

variants = [
        ('pnc_basic', 'pnc'),
        ('wcc_basic', 'wcc')]

def bg_kwds():
    kwds = kw.KeywordSet(top_level=True)
    kwds += kw.STDIDENT('TEST_BG', 'Testing a simple bare ground run')
    kwds += kw.STDINFO(712, 'CHS131', 0, 200, 35, 6)
    kwds += kw.MANAGED(0, True)
    kwds += kw.INVYEAR(2010)
    kwds += kw.DESIGN(0, 1, 999, 1, 0, 1, 1)
    kwds += kw.NUMCYCLE(10)
    kwds += kw.NOTREES()

    estab = kw.ESTAB(2010)
    estab += kw.PLANT(2011, 'DF', 450, 95, 2, 1, 0)
    kwds += estab

    kwds += kw.ECHOSUM()

    return kwds

fvs_variant = 'PN'

class TreesTest(unittest.TestCase):

    def test_fsv_trees(self):
        f = fvs.FVS(fvs_variant)

        p = './bg_test.key'
        kwds = bg_kwds()
        kwds.write(p)

        f.init_projection(p)

        for c in range(f.num_cycles):
            r = f.grow_projection()

        r = f.end_projection()
        print('FVS Return Code: %s' % r)
        self.assertEqual(r, 0, 'FVS Return Code: %s' % r)


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
#     kwds = bg_kwds()
#     print(kwds)
