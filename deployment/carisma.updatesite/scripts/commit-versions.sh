#!/bin/bash
if [ ! -d ../../carisma.core ]; then
echo "Fehler: Es wird erwartet, dass dieses Skript in seinem Standard-Ordner liegt (carisma.updatesite/scripts) und von dort ausgefuehrt wird!"
exit 1;
fi
cd ../..

for a in `ls -1 | sort`; do
if [ -d $a ]; then
echo $a
cd $a
if [ -f ./META-INF/MANIFEST.MF ]; then
newversion=`grep Bundle-Version ./META-INF/MANIFEST.MF | sed -e 's/\r//g' | sed -e 's/\n//g'`
newversionnr=`echo $newversion | sed -e 's/Bundle-Version: //g' | sed -e 's/.qualifier//g'`

svn ci -m "increased bundle version to $newversionnr"
fi
cd ..
fi
done
