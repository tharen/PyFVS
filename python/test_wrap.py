import ctypes

fvs = ctypes.cdll.LoadLibrary('libfvspnc.so')
rtn = ctypes.c_int
kwd = ctypes.c_char_p(b'/home/ubuntu/PyFVS_ref/python/test/foo.key')
print('foo')
fvs.run_fvs(kwd)
print('bar')
