#!/bin/sh

OS=`uname`
case $OS in
    Darwin)
	if [ -x /usr/local/bin/gsed ]; then
	    SED=/usr/local/bin/gsed
	else
	    exit 1
	fi
	;;
    Linux)
	SED=/usr/bin/sed
	;;
    *)
	exit 1
esac

egrep -r -m 3 -i "^(Subject|Message-Id|Date):" . |tr -d '\015'|sort > idx1
egrep -r -m 2 -i "^(In-Reply-to|References):" .  |tr -d '\015'|sort > idx2

# maillinkg list was corrected To make the ml-file work
$SED -i -e 's/Date:\(.*\)jst/Date:\1JST/' \
        -e 's/+900/+0900/' \
        -e 's/+0900(JST)/+0900 (JST)/' \
        -e 's/          U\*/ U*/' idx1

$SED -i -e 's/In-Reply-To:\(.*\)jst/In-Reply-To:\1JST/' \
        -e 's/References: .*message of.*</References: </' \
        -e 's/<Your.*</</' -e 's/> Your.*$//' -e 's/></> </g' \
        -e 's/In-Reply-To: .*<\(.*\)>.*$/In-Reply-To: <\1>/i' \
        -e 's/          U\*/ U*/' \
        -e '/In-Reply-To: $/d' idx2
