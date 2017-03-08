"""
Rebuild libpython MinGW import library
"""

import os
import sys
import subprocess

if not sys.platform=='win32':
    print('Not a Windows platform. Skipping libpython generation.')
    sys.exit()
    
ver = ''.join(str(v) for v in sys.version_info[:2])
pfx = sys.exec_prefix

# Find the python DLL
o = subprocess.check_output('where python{}.dll'.format(ver))
try:
    dll = o.decode().split('\r\n')[0]
except:
    print('Error locating Python DLL. Make sure it is available in the search path.')
    sys.exit(1)
    
print('Build libpython MinGW import library')
print('Python version: {}'.format(sys.version_info[:2]))
print('Python DLL: {}'.format(dll))
print('')

pwd = os.path.abspath(os.curdir)
os.chdir(os.path.join(pfx,'libs'))

cmd = ['gendef',dll] #os.path.join(pfx,dll)]
print('CMD: ' + ' '.join(cmd))
subprocess.call(cmd)

cmd = ['dlltool','--dllname',dll,'--def','python{}.def'.format(ver)
    ,'--output-lib','libpython{}.a'.format(ver)]

print('CMD: ' + ' '.join(cmd))
subprocess.call(cmd)
    
os.chdir(pwd)