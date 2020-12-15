SVN Mirror workflow
====

PyFVS tracks the [Open-FVS](https://sourceforge.net/p/open-fvs/code/HEAD/tree/) 
SVN repository on SourceForge. Updating the mirrors of trunk and PyFVS branches
requires special care.

### Mirror Branches

Mirror branches must remain pure, e.g. only SVN commits. If commits are to be 
pushed back to SVN the history must be linear.

 - mirror-svn-trunk
 - svn-pyfvs-mirror

### Mod branches

Tweaks to the SVN mirror may be necessary for continuous integration, staging,
etc.  Any modifications that need to be in the GitHub repository, but are not
relevant for SourceForge should be kept in separate branches.

 - svn-trunk-ci
 - svn-pyfvs-ci

Other svn-ish branches are (or will be) obsolete and could be deleted in the future.

---

# Initialize the mirror branches

The following are the steps used to create the SVN mirror.

    cd c:\workspace
    git clone https://github.com/tharen/PyFVS.git pyfvs_svn
    cd pyfvs_svn

### Add the SVN remote

    git svn init --prefix=open-fvs/ --trunk=https://svn.code.sf.net/p/open-fvs/code/trunk

.git/config should now include the following:

    [svn-remote "svn"]
        url = https://svn.code.sf.net/p/open-fvs/code
        fetch = trunk:refs/remotes/open-fvs/trunk

  **NOTE:** Additional fetch statements can be added to pull branches, etc.

    fetch = branches/{PyFVS,FMSCrelease}:refs/remotes/open-fvs/*
    
### Fetch the necessary history

    git svn fetch svn -r1055:HEAD
    
  **NOTE:** PyFVS was created with [r1055](https://sourceforge.net/p/open-fvs/code/1055/)

### Create the mirror branch

    git checkout -b svn-trunk-mirror
    # Make the current working directory consistent with trunk
    git reset --hard remotes/open-fvs/trunk
    git svn rebase
    https://sourceforge.net/p/open-fvs/code/1055/

### Update Refs

If `git svn fetch` or other `git svn` commands return something like the following,

    Unable to determine upstream SVN information from working tree history

You likely need to explicitly associate the remote references.
    
    git update-ref refs/remotes/open-fvs/trunk refs/remotes/origin/svn-trunk-mirror

  **NOTE:** `git svn init --prefix=open-fvs ...` is supposed to do this, but may
  not in all cases.

# Update the Mirror

When the SVN repo is updated the changes should be pulled and pushed to GitHub.

    git checkout svn-trunk-mirror
    git reset --hard remotes/open-fvs/trunk
    git svn rebase
    git push origin svn-trunk-mirror
