# Refactor Fortran 77 fixed format to Fortran 90+ free format

import os
import re

def refactor_comments(lines):
    for i in range(len(lines)):
        line = lines[i]
        if line[0].lower() in ('c','*'):
            l = list(line)
            l[0] = '!'
            line = ''.join(l)
            lines[i] = line
    
    return lines

def refactor_line_continuation(lines):
    last_code_line = 0
    for i in range(len(lines)):
        line = lines[i]
        
        if len(line)>5 and line[5] in ('&','>','+'):
            l = list(line)
            l[5] = ' '
            lines[i] = ''.join(l)
            lines[last_code_line] = lines[last_code_line].rstrip() + ' &\n'
        
        # TODO: Variable declarations and common blocks can be continued
        #       with a number in the first 5 columns.
            
        if (len(line.strip())>0
                and not line.strip()[0]=='!' 
                and not line[0] in ('c','C','*')
                ):
            last_code_line = i
    
    return lines
    
refactor_funcs = [
    refactor_comments
    , refactor_line_continuation
    ]
    
def main(fin, fout):
    print('Refactor: {} -> {}'.format(fin, fout))
    lines = open(fin).readlines()
    
    for f in refactor_funcs:
        lines = f(lines)
    
    with open(fout, 'w') as out:
        out.writelines(lines)

def do_all(root):
    file_map = [
        ('wc/src/cratet.f','api/variants/wc/cratet.f90')
        ]

    for fin, fout in file_map:
        fin = os.path.join(root, fin)
        fout = os.path.join(root, fout)
        main(fin, fout)
        
if __name__=='__main__':
    
    # fin = 'c:/workspace/pyfvs_svn/wc/src/cratet.f'
    # fout = 'c:/workspace/pyfvs_svn/api/variants/wc/cratet.f90'
    # main(fin, fout)
    
    do_all('c:/workspace/pyfvs_svn')
