import ctypes

fvs = ctypes.cdll.LoadLibrary('libfvspnc.so')
rtn = ctypes.c_int
kwd = ctypes.c_char_p(b'/home/ubuntu/PyFVS/python/test/foo.key')
fvs.run_fvs(kwd)

#fvs.cfv
