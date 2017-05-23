"""
Strip Git information from the _version.py file.

Called as a pre-commit hook to avoid committing irrelevant Git metadata.
"""

import sys
import re

version_path = 'python/pyfvs/_version.py'

content = open(version_path).read()

content = re.sub(
        r'(__git_describe__) = \'(.*)\'(.*)'
        , r"\1 = ''"
        , content) 

content = re.sub(
        r'(__git_commit__) = \'(.*)\'(.*)'
        , r"\1 = ''"
        , content) 

with open(version_path, 'w') as _:
    _.write(content)
