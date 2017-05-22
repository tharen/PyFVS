

import os
import subprocess
import shutil

import refactor_include as ri
import refactor_f90 as r9

left_root = 'C:/workspace/pyfvs_trunk_temp'
right_root = 'C:/workspace/pyfvs_svn'

file_map = 'C:/workspace/forest_modeling/pyfvs/resources/file_map.txt'

for l in open(file_map).readlines():
    if l.strip().startswith('#') or l.strip()=='':
        continue
    
    print(l)
    
    left = os.path.join(left_root,l.split('->')[0].strip())
    
    # Apply automatic refactorings
    left_tmp = os.path.join(left_root,l.split('->')[0].strip()) + '.tmp'
    r = ri.refactor(left, ri.include_modules, left_tmp)
    if not r:
        shutil.copy(left, left_tmp)
        
    r9.main(left_tmp, left_tmp)
    
    right = os.path.join(right_root,l.split('->')[1].strip())
    
    args = [r"C:\Program Files (x86)\WinMerge\WinMergeU.exe",'/e','/u','/maximize',left_tmp,right]
    r = subprocess.check_call(args)
    
    os.remove(left_tmp)
    