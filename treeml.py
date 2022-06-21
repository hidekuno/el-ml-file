#!/usr/bin/python
#
# 1) create index file before run this program
#    cd ${WHERE}
#    ${INSTALLDIR}/create_idx.sh
#
# hidekuno@gmail.com
#
import sys
import os
from dateutil import parser
from pytz import timezone
import argparse

mails = {} # set of class Message
cache = {} # set of class Tree

class Message(object):
    def __init__(self,filename):
        self.filename = filename
        self.date = None
        self.subject = ""
        self.messageId = None
        self.references = None

class Tree(object):
    def __init__(self, item, parent = None):
        self.item = item
        self.parent = parent
        self.children = []

    def add(self, child):
        self.children.append(child)

    def printName(self):
        return self.item.filename  + ' ' + self.item.subject

def walk(tree, indent=0):
    print("  " * indent + tree.printName())

    for rec in tree.children:
        walk(rec, indent+1)

def set_ref(key,ht,filename):
    if key in ht:
        if not mails[filename].references:
            mails[filename].references = ht[key].filename

def adjust_jst(d):
    return str(parser.parse(d,tzinfos={"JDT": 9 * 3600,
                                       "EST": -5 * 3600,
                                       "UT": 0 * 3600,})
               .astimezone(timezone('Asia/Tokyo')))

def makeMessages():
    dates = {}
    messages = {}

    fd = open('idx1','r')
    for line in fd:
        line = line.rstrip()
        rec = line.split(':')
        if not rec[0] in mails:
            mails[rec[0]] = Message(rec[0])

        ml = mails[rec[0]]
        if rec[1].lower() == "date":
            ml.date = adjust_jst(line.split(":Date: ")[1])
            dates[ml.date] = ml
        if rec[1].lower() == "message-id":
            ml.messageId = " ".join(line.split(" ")[1:])
            messages[ml.messageId] = ml
        if rec[1].lower() == "subject":
            ml.subject = line.split(":Subject: ")[1]
    fd.close()
    return dates,messages

def makeRef(dates,messages):
    fd = open('idx2','r')
    for line in fd:
        line = line.rstrip()
        rec = line.split(':')
        filename = rec[0]

        if rec[1].lower() == "in-reply-to":
            irt = line.split(' ')[1:]

            if irt[0][0] == '<' and irt[-1][-1] == '>':
                set_ref(" ".join(irt[0:]),messages,filename)

            elif irt[0] == 'Your' and irt[1] == 'message':
                set_ref(adjust_jst(" ".join(irt[3:]).replace('.', '').replace('"','')), dates, filename)

            elif 5 < len(irt) and irt[2] == 'message' and irt[3] == 'of':
                set_ref(adjust_jst(" ".join(irt[4:]).replace('.', '').replace('"','')), dates, filename)

        if rec[1].lower() == "references":
            ref = line.split(' ')[1:]

            if len(ref) < 1 or ref[0] == 'Your' or ',' in line:
                pass
            elif ref[-1][0] == '<':
                set_ref(ref[-1],messages,filename)
            elif ref[-1][-1] == '>':
                set_ref(" ".join(ref[-2:]),messages,filename)
    fd.close()

# create tree recursive
def create_tree(rec,top):
    if not rec.references in cache:
        if rec.references:
            create_tree(mails[rec.references],top)

    if not rec.filename in cache:
        if rec.references:
            parent = cache[rec.references]
            cache[rec.filename] = Tree(rec, parent)
            parent.add(cache[rec.filename])
        else:
            cache[rec.filename] = Tree(rec, top)
            top.add(cache[rec.filename])

if __name__ == "__main__":

    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument('ml', type=str)
    args = arg_parser.parse_args(sys.argv[1:])

    os.chdir(os.path.join(os.environ['HOME'], args.ml))

    dates, messages = makeMessages()
    makeRef(dates,messages)
    top = Tree(Message(args.ml))

    for k in mails.keys():
        create_tree(mails[k],top)
    walk(top)
