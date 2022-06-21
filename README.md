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

### Get some data and create a file for each article.

### create index
```
cd ${HOME}
cd ${ml-archive-name}
~/el-ml-file/create_idx.sh
```

### run in Emacs
```
M-x load-file ~/el-ml-file/ml-file.el
M-x ml-file
```
<img src="https://user-images.githubusercontent.com/22115777/172289182-ace767ad-73dc-4e58-ab96-d2c2cfbfe189.png" width=60% height=60%>
