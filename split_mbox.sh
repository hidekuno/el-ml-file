#!/bin/sh 

if [ $# -ne 1 ]; then
  echo "Usage: split_mbox.sh file" >&2
  exit 1
fi
FILE=$1
if [ ! -f $FILE ]; then
  echo "No such file: $FILE" >&2
  exit 1
fi

cat $FILE| awk -v cnt=1 '(/^From /){filename=sprintf("%05d",cnt++)}(!/^From /){print $0 >filename}'

TMPFILE=`mktemp`
for F in `ls [0-9]*`
do
  $HOME/el-ml-file/decodeml.py -f $F > $TMPFILE
  mv $TMPFILE $F
done
