from numpy.f2py import f2py2e

def gen_pyf(ext_pyf, ext_name, sources):
    """Generate the F2PY wrapper .pyf module."""
    
    sources = open('./pyext_source.txt').read()
    sources = sources.split(';')

    cmd_args = [
        '-h', ext_pyf
        ,'-m', ext_name
        ,'--overwrite-signature'
        ]
    cmd_args.extend(sources)
    f2py2e.run_main(cmd_args)
