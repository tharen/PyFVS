import ctypes

fvs = ctypes.cdll.LoadLibrary('libfvspnc.so')
v = ctypes.c_char_p(b'Foo')
fvs.version(v)
print(v.value)
