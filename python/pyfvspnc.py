import ctypes

fvs = ctypes.cdll.LoadLibrary('libfvspnc.so')
v = ctypes.c_char_p(b'Foo')
fvs.version(v)
print(v.value)
print(fvs.get_maxtre())
print(fvs.get_maxplt())
print(fvs.get_maxsp())
print(fvs.get_maxcyc())

