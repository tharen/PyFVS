
cdef extern:
    void fvs_version "version" (char* ver)

def version():
    cdef char* v = ""
    fvs_version(v)
    return v

