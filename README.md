Mailing List thread viewer by Emacs lisp program
=================

## Overview
- This is a personally created tool.
- I forgot about Emacs lisp programing, so I implemented this program.
- I can not publish it because it contains sensitive data about Mailing List messages.

### Requirement
- emacs(>=26.3) is installed.
- python(>=3.8.10) is installed.
- python-dateutil(>=2.7.3) is installed.
- pytz(>= 2019.3) is installed.
- GNU Awk(>=5.0.1) is installed.
- GNU sed(>=4.7) is installed.

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
<img src="https://user-images.githubusercontent.com/22115777/199161643-56797a9c-9d34-4539-aa20-d3d61e477f12.png" width=60% height=60%>

### byte compile
```
cd ~/el-ml-file
make
```

### run in Emacs(byte code)
```
M-x load-file ~/el-ml-file/ml-file.elc
M-x ml-file
```
