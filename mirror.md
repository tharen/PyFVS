# Update the SVN mirror branches

# svn-trunk-mirror must remain identical to "trunk" in SVN
git checkout -b svn-trunk-mirror origin/svn-trunk-mirror

# Pull any outstanding SVN commits into the mirror branch
git svn rebase

# Push the new SVN commits to origin (e.g. GitHub)
git push origin svn-trunk-mirror

# Continuous integration branches have minimal modifications to enable
#   building and testing on automated CI systems, e.g. travis, AppVeyor

# Rebase the trunk CI build scripts
git checkout svn-trunk-ci
git rebase svn-trunk-mirror

# Verify the build
# Push to GitHub
git push origin svn-trunk-mirror
