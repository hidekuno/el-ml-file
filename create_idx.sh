#!/bin/sh
egrep -r -m 3 -i "^(Subject|Message-Id|Date):" . |tr -d '\015'|sort > idx1
egrep -r -m 2 -i "^(In-Reply-to|References):" .  |tr -d '\015'|sort > idx2

sed -i -e 's/Date:\(.*\)jst/Date:\1JST/' -e 's/+900/+0900/' idx1
sed -i -e 's/In-Reply-To:\(.*\)jst/In-Reply-To:\1JST/' idx2
sed -i -e 's/References: .*message of.*</References: </' idx2
sed -i -e 's/<Your.*</</' -e 's/> Your.*$//' -e 's/></> </g' idx2
