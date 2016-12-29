
cdef extern:
    void fvs_version "version" (char* ver)

def py_version():
    cdef char* v

    _v = b'\0*7'
    v = _v
    fvs_version(v)
    return v

