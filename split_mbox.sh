#!/bin/bash

function error_stop() {
  echo "ERROR: ${BASH_LINENO[0]}行目:" $1 >&2
  exit 1
}

[ $# -ne 1 ] && error_stop "Usage: split_mbox.sh file"

FILE=$1
[ ! -f $FILE ] && error_stop "No such file: $FILE"

cat $FILE| awk -v cnt=1 '(/^From /){filename=sprintf("%05d",cnt++)}(!/^From /){print $0 >filename}'

TMPFILE=`mktemp`
for F in `ls [0-9]*`
do
  $HOME/el-ml-file/decodeml.py -f $F > $TMPFILE
  mv $TMPFILE $F
done
