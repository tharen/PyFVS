import unittest

from pyfvs import fvs

variants = ['pnc', 'wcc']

class ParametrizedTestCase(unittest.TestCase):
    """ TestCase classes that want to be parametrized should
        inherit from this class.
        
        Adapted from:
        http://eli.thegreenplace.net/2011/08/02/python-unit-testing-parametrized-test-cases
    """
    def __init__(self, methodName='runTest', variant=None):
        super(ParametrizedTestCase, self).__init__(methodName)
        self.variant = variant

    @staticmethod
    def parametrize(testcase_klass, variant=None):
        """ Create a suite containing all tests taken from the given
            subclass, passing them the parameter 'param'.
        """
        testloader = unittest.TestLoader()
        testnames = testloader.getTestCaseNames(testcase_klass)
        suite = unittest.TestSuite()
        for name in testnames:
            suite.addTest(testcase_klass(name, variant=variant))
        return suite

class PNCTest(unittest.TestCase):
    variant = 'pnc'

    def setUp(self):
        pass

    def test_import_variants(self):
        print(self.variant)
        f = fvs.FVS(self.variant)

    def test_equal(self):
        self.assertTrue(1 == 0)

    def test_float_equal(self):
        self.assertAlmostEqual(100.001, 100.0, 2)

def test_variants():
    suite = unittest.TestSuite()
    suite.addTest(ParametrizedTestCase.parametrize(PyFVSTest, 'pnc'))
    suite.addTest(ParametrizedTestCase.parametrize(PyFVSTest, 'wcc'))
    return suite

if __name__ == '__main__':
    unittest.main()
