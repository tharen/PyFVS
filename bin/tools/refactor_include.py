"""
Refactor Fortran procedures, replacing include statements with module use statements

NOTES:
    1) EQUIVALENCE statements cause conflicts with module data arrays.
        eg. C:\workspace\Open-FVS\refactor\src\base\src\htgstp.f:26.25:
              EQUIVALENCE (WK6(2),HT1),(WK6(3),HT2),(WK6(4),PRB),
              Error: EQUIVALENCE attribute conflicts with USE ASSOCIATED attribute in 'wk6' at (1)

    2) Some "common" include variable names are reused, split includes into 
        separate modules, or hack up a 'use ... : only ...,... routine
        
"""

import os
import glob
import shutil
import re

include_modules = {
        'arrays.f77':'arrays'
        , 'calcom.f77':'calcom'
        , 'calden.f77':'calden'
        , 'cicom.f77':'cicom'
        , 'climate.f77':'climate'
        , 'coeffs.f77':'coeffs'
        , 'contrl.f77':'contrl'
        , 'cvcom.f77':'cvcom'
        , 'cwdcom.f77':'cwdcom'
        , 'dbstk.f77':'dbstk'
        , 'ecncom.f77':'ecncom'
        , 'ecncomsaves.f77':'ecncomsaves'
        , 'econ.f77':'econ'
        , 'emcom.f77':'emcom'
        , 'escom2.f77':'escom2'
        , 'escomn.f77':'escomn'
        , 'eshap.f77':'eshap'
        , 'eshap2.f77':'eshap2'
        , 'eshoot.f77':'eshoot'
        , 'esrncm.f77':'esrncm'
        , 'estcor.f77':'estcor'
        , 'estree.f77':'estree'
        , 'eswsbw.f77':'eswsbw'
        , 'fvsstdcm.f77':'fvsstdcm'
        , 'ggcom.f77':'ggcom'
        , 'glblcntl.f77':'glblcntl'
        , 'htcal.f77':'htcal'
        , 'hvdncm.f77':'hvdncm'
        , 'includesvn.f77':'includesvn'
        , 'keycom.f77':'keycom'
        , 'kotcom.f77':'kotcom'
        , 'metric.f77':'metric'
        , 'multcm.f77':'multcm'
        , 'opcom.f77':'opcom'
        , 'outcom.f77':'outcom'
        , 'pden.f77':'pden'
        , 'plot.f77':'plot'
        , 'ppcisn.f77':'ppcisn'
        , 'ppcmad.f77':'ppcmad'
        , 'ppcntl.f77':'ppcntl'
        , 'ppdncm.f77':'ppdncm'
        , 'ppeprm.f77':'ppeprm'
        , 'ppexcm.f77':'ppexcm'
        , 'ppgpcm.f77':'ppgpcm'
        , 'pphvcm.f77':'pphvcm'
        , 'ppllcm.f77':'ppllcm'
        , 'ppmdcm.f77':'ppmdcm'
        , 'ppspla.f77':'ppspla'
        , 'ppspnb.f77':'ppspnb'
        , 'ppsprd.f77':'ppsprd'
        , 'rancom.f77':'rancom'
        , 'screen.f77':'screen'
        , 'sncom.f77':'sncom'
        , 'sstgmc.f77':'sstgmc'
        , 'stdstk.f77':'stdstk'
        , 'sumtab.f77':'sumtab'
        , 'svdata.f77':'svdata'
        , 'svdead.f77':'svdead'
        , 'svrcom.f77':'svrcom'
        , 'twigcom.f77':'twigcom'
        , 'varcom.f77':'varcom'
        , 'volstd.f77':'volstd'
        , 'workcm.f77':'workcm'

        # fire\base\common
        , 'fmcom.f77':'fmcom'
        , 'fmfcom.f77':'fmfcom'
        , 'fmopcm.f77':'fmopcm'
        , 'fmparm.f77':'fmparm'
        , 'fmprop.f77':'fmprop'
        , 'fmsngl.f77':'fmsngl'
        , 'fmsvcm.f77':'fmsvcm'

        # pn\common
        , 'esparm.f77':'esparm'
        , 'prgprm.f77':'prgprm'
        }

def main(src, dest=None, inplace=False):
    """
    Walk a directory tree, mirroring files and refactoring along the way.
    """
    src = os.path.abspath(src)
    dest = os.path.abspath(dest)
    for root, dirs, files in os.walk(src):
        
        if not inplace:
            # Mirror and refactor
            if root.endswith('api'):
                for fn in files:
                    src_fn = os.path.join(root, fn)
                    dest_fn = os.path.join(root.replace(src, dest), fn)
                    print 'Copy: {}'.format(src_fn)
                    shutil.copy2(src_fn, dest_fn)
            else:
                for fn in files:
                    src_fn = os.path.join(root, fn)
                    dest_fn = os.path.join(root.replace(src, dest), fn)
                    if fn.endswith('.f'):
                        print 'Refactor: {}'.format(src_fn)
                        refactor(src_fn, include_modules, dest_fn)

                    else:
                        print 'Copy: {}'.format(src_fn)
                        shutil.copy2(src_fn, dest_fn)

            for dir in dirs:
                try:
                    os.makedirs(os.path.join(root.replace(src, dest), dir))
                except WindowsError:
                    pass
                except:
                    raise
        
        else:
            # Backup and refactor
            for fn in files:
                if fn.endswith('.f'):
                    src_fn = os.path.join(root, fn)
                    print 'Refactor: {}'.format(src_fn)
                    refactor(src_fn, include_modules)
            
def refactor(src, include_modules, dest=None):
    """
    Refactor a fortran (F77) module by replacing INCLUDE statements with USE.
    
    Args
    ----
    :param src: Fixed format fortran module to refactor.
    :param include_modules: Dict of include file names to refactor {'file.f77':'file_mod',...}
    :param dest: (optional) Refactored output file name.
    """
    insrc = open(src).readlines()

    drop_lines = []
    header = []
    footer = []
    routines = []
    in_routine = False
    in_signature = False
    routine_name = None
    empty_tally = 0
    max_empty = 1

    include_pat = re.compile('^include\s+[\'\"](.*\.f77)[\'\"]')
    sig_pat = re.compile('^((logical|real|int)\s)?(subroutine|function|block data|program)\s+(.+)\s*\(?.*')
#     cont_pat = re.compile('\-\s*.*\)')
    module_pat = re.compile('^use\s+(.*)')

    # Scan the lines, separating each line based on content
    for i, line in enumerate(insrc):
        line = line.rstrip()
        _line = line.strip().lower()
        
        # If the line as a function, subroutine, etc. set the appropriate flag
        #  and initialize a routine dict object.
        m = re.search(sig_pat, _line)
        if m:
            in_routine = True
            in_header = False
            in_signature = True
            routine_name = m.groups()[3]
            routine_type = m.groups()[2]
            print '\t{} {}'.format(routine_type, routine_name)
            routine = {'name':routine_name
                       , 'type':routine_type
                       , 'signature':[line, ]
                       , 'modules':set()
                       , 'body':[]
                       }
            routines.append(routine)
            continue
        
        # Check for line continuations in signature blocks
        if in_signature:
            if len(_line) and _line[0] in ('-', '&', '>', '1'):
                routines[-1]['signature'].append(line)
                continue
            else:
                in_signature = False

        # Convert includes to module uses
        m = re.search(include_pat, _line)
        if m:
            # If this include file is to be refactored, set up the module line
            if include_modules.has_key(m.groups()[0]):
                module = include_modules[m.groups()[0]]
                routines[-1]['modules'].add(module)
                continue
        
        # Capture any module currently in use
        # FIXME: some use statements may be on multiple lines
        m = re.search(module_pat, _line)
        if m:
            routines[-1]['modules'].add(m.groups()[0])
            continue
        
        # skip implicit none, it will be forced in the output file
        if 'implicit none' in _line:
            continue
        
        # Anything else must be body text or headers
        if in_routine:
            routines[-1]['body'].append(line)
        else:
            header.append(line)

#     print routines

    if dest is None:
        # Refactoring inplace
        back = '{}.bak'.format(src)
        print 'Backup {} -> {}'.format(src,back)
        shutil.copy2(src, back)
        dest = src
        
    with open(dest, 'w') as out:
        for line in header:
            out.write(line + '\n')

        for routine in routines:
            for line in routine['signature']:
                out.write(line + '\n')

            for module in routine['modules']:
                out.write('      use {}\n'.format(module))

            out.write('      implicit none\n')

            for line in routine['body']:
                # skip consecutive empty lines
                if line.lower() in ('c', '!', 'commons', '!commons'):
                    empty_tally += 1
                    if empty_tally > max_empty:
                        continue
                else:
                    empty_tally = 0

                out.write(line + '\n')

def refactor_files(fl,includes):
    """
    Refactor a list of source files.
    
    Args
    ----
    :param fl: List of file paths.
    :param includes: Dict of include file names and corresponding modules.
    """
    for f in fl:
        refactor(f,includes)
        
def search_depends(r,include):
    """
    Return a list of files with include dependencies.
    """
    dl = glob.glob(r)
    fl = []
    for d in dl:
        r,p=os.path.split(d)
        if p.lower() in ('.settings','.git','.hg','bin','changenotes','tests'):
            continue
            
        for root,dirs,files in os.walk(d):
            for f in files:
                n,e = os.path.splitext(f)
                if not e in ('.f',):
                    continue
                if include in open(os.path.join(root,f)).read():
                    fl.append(os.path.join(root,f))
    return fl
    
if __name__ == '__main__':
    main(r'C:\workspace\Open-FVS\bitbucket\src'
         , r'C:\workspace\Open-FVS\refactor\src')
