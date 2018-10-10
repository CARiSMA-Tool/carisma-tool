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
if [ -f ./pom.xml ]; then
xsltproc -o ./pom.tmp ../carisma.updatesite/scripts/pom-updater.xslt ./pom.xml
rm ./pom.xml
mv ./pom.tmp ./pom.xml

oldversion=`grep Bundle-Version ./META-INF/MANIFEST.MF | sed -e 's/\r//g' | sed -e 's/\n//g'`
oldversionnr=`echo $oldversion | sed -e 's/Bundle-Version: //g' | sed -e 's/.qualifier//g'`

sed -i -e "s/XXXVERSIONXXX/$oldversionnr-SNAPSHOT/g" ./pom.xml
#echo "sed -i -e 's/$pomversionnr-SNAPSHOT/$oldversionnr-SNAPSHOT/g' ./pom.xml"
echo "increased pom.xml updated to version $oldversionnr"
fi
fi
cd ..
fi
done
echo "Please run './commit-versions.sh' to transfer the changes to the SVN repo..."
