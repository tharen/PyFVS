from distutils.core import setup, Command
import os, sys
import glob

all_variants = {}
for sl in glob.glob('bin/*_sourceList.txt'):
    v = sl[7:9].lower()
    all_variants[v] = {'src_list': sl}

class BuildFVSVariants(Command):
    description = 'Build requested FVS variant extension modules.'
    user_options = [
        ('variants=','v','List of FVS variants to compile, seperated by ";"')
        ]
    
    def initialize_options(self):
        self.cwd = None
        self.variants = None

    def finalize_options(self):
        self.cwd = os.getcwd()
        self.variants = [v.lower() for v in self.variants.split(';')]
        self.var_dicts = {}
        for v in self.variants:
            v = v.lower()
            self.var_dicts[v] = all_variants[v]

    def run(self):
        assert os.getcwd()==self.cwd, 'Must be in package root: %s' % self.cwd
        print('Compile FVS variant extension modules: {}'.format(
                str(self.variants),))
        
                
setup(
    cmdclass = {
        'build_vars': BuildFVSVariants
        }
    )

