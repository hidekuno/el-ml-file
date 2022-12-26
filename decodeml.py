#!/usr/bin/env python
import sys,email
from email.header import decode_header
from email.utils import parsedate_to_datetime
import argparse

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument('-f', dest='filename', type=str, required=True)
args = arg_parser.parse_args(sys.argv[1:])

with open(args.filename, 'rb') as fd:
    msg = email.message_from_bytes(fd.read())

    for k in msg.keys():
        header = ''
        for tup in decode_header(str(msg[k])):
            if type(tup[0]) is bytes:
                charset = tup[1]
                if charset:
                    header += tup[0].decode(tup[1])
                else:
                    header += tup[0].decode()
            elif type(tup[0]) is str:
                header += tup[0]
        print(k + ':', header.replace("\n",' ') if k == "References" else header)

    print('')
    for part in msg.walk():
        if part.get_content_maintype() == "multipart":
            continue
        if not part.get_filename():
            charset = part.get_content_charset()
            if charset:
                print(part.get_payload(decode=True).decode(charset, errors="replace"))
            else:
                print(part.get_payload(decode=True).decode())
