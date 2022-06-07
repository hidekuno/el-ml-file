Mailing List thread viewer by Emacs lisp program
=================

## Overview
- This is a personally created tool.
- I forgot about Emacs lisp programing, so I implemented this program.
- I can not publish it because it contains sensitive data about Mailing List messages.

### Requirement
- emacs(>=26.3) is installed.
- python(>=3.8.10) is installed.

### install
```
cd ${HOME}
git clone https://github.com/hidekuno/el-ml-file
sudo cp el-ml-file/treeml.py /usr/local/bin/.
```

### create index
```
cd ${HOME}
tar -xvfz <ml-archive-name>.tar.gz .
cd <ml-archive-name>
~/el-ml-file/create_idx.sh
```

### run in Emacs
```
M-x load-file ~/el-ml-file/ml-file.el
M-x ml-file
```
