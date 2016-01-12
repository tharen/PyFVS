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

class VariantTests(unittest.TestCase):
    @parameterized.expand(variants)
    def test_load_variant(self, name, variant):
        f = fvs.FVS(variant)

    @parameterized.expand(variants)
    def test_variant_name(self, name, variant):
        f = fvs.FVS(variant)
        self.assertEqual(f.variant, variant, 'Variant name failed')
