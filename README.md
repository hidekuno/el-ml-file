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
```

### Get some data and create a file for each article.
ex.)
  https://lists.ubuntu.com/archives/ubuntu-jp.mbox/ubuntu-jp.mbox

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
autoconf
./configure
make
```

### run in Emacs(byte code)
```
M-x load-file ~/el-ml-file/ml-file.elc
M-x ml-file
```

### ml-file Quick Start Guide

`ml-file` is a program for comfortably browsing locally stored mailing list archives within Emacs.

#### 1. How to Start

1.  Run `M-x ml-file` in Emacs.
2.  The minibuffer will display `Mailing List Name: `. Enter the name of the mailing list (directory name) you wish to browse and press `Enter`.
3.  A buffer named `ml-file` will open, displaying email threads in a tree format.

#### 2. Basic Operations

##### Thread List Screen (`ml-file` buffer)

This is the main screen where email threads (groups of related messages) are displayed in a tree structure.

| Key | Action |
| :--- | :--- |
| `f` or `Enter` | Open the email at the cursor line. |
| `g` | Perform a word search across the entire mailing list archive. |
| `b` | Switch to the search results screen after searching with `g`. |
| `Space` | Scroll down. |
| `q` | Exit `ml-file`. |

##### Email Display Screen

This screen displays individual emails selected from the thread list screen.

| Key | Action |
| :--- | :--- |
| `q` or `Enter` | Close the email display screen and return to the previous screen. |
| `g` | Jump to a related email by selecting a `Message-ID` or similar text as a region and pressing `g`. |
| `Space` | Scroll down. |

##### Search Results Screen (`ml-grep` buffer)

This screen displays the results of a search initiated by pressing `g` in the thread list screen.

| Key | Action |
| :--- | :--- |
| `f` or `Enter` | Open the email corresponding to the search result at the cursor line and jump to that location. |
| `g` | Show the location of the corresponding email within the thread list screen. |
| `Space` | Scroll down. |
| `q` | Close the search results screen. |

