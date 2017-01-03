import pyfvspnc as f

def test_version():
    v = f.api_version()
    print(v)

def test_wrappers():
    assert f.get_maxtre()==3000
    print(f.get_maxtre())

def test_run():
    kwd = b'test/pn_test.key'
    r = f.run_fvs(kwd)
    assert r==0

def test_commons():
    kwd = b'test/pn_test.key'
    r = f.run_fvs(kwd)

    #print(f.get_arrays())

if __name__=='__main__':
    test_version()
    test_wrappers()
    test_run()
    print('Done')


