#!/bin/bash
if [ ! -d ../../carisma.core ]; then
echo "Fehler: Es wird erwartet, dass dieses Skript in seinem Standard-Ordner liegt (carisma.updatesite/scripts) und von dort ausgefuehrt wird!"
exit 1;
fi
cd ../..

if [ -f ./carisma.updatesite/scripts/report.txt ]; then
cp ./carisma.updatesite/scripts/report.txt ./carisma.updatesite/scripts/report.txt.bak
rm ./carisma.updatesite/scripts/report.txt
fi
for a in `ls -1 | sort`; do
if [ -d $a ]; then
echo $a
cd $a
if [ -f ./META-INF/MANIFEST.MF ]; then
oldversion=`grep Bundle-Version ./META-INF/MANIFEST.MF | sed -e 's/\r//g' | sed -e 's/\n//g'`
oldversionnr=`echo $oldversion | sed -e 's/Bundle-Version: //g' | sed -e 's/.qualifier//g'`
# echo "$oldversion"
# echo "$oldversionnr"

oldmajor=`echo $oldversionnr | awk -F. '{print $1}'`
oldminor=`echo $oldversionnr | awk -F. '{print $2}'`
oldserv=`echo $oldversionnr | awk -F. '{print $3}'`
# echo "$oldmajor.$oldminor.$oldserv"

declare -i newmajor
declare -i newminor
declare -i newserv

if [ -f ./major ]; then
newmajor=$oldmajor+1
newminor=0
newserv=0
rm ./major
else if [ -f ./minor ]; then
newmajor=$oldmajor
newminor=$oldminor+1
newserv=0
rm ./minor
else if [ -f ./changes ]; then
newmajor=$oldmajor
newminor=$oldminor
newserv=$oldserv+1
else
newmajor=$oldmajor
newminor=$oldminor
newserv=$oldserv
fi
fi
fi

if [ -f ./changes ]; then
rm ./changes
fi

newversionnr=`echo $newmajor.$newminor.$newserv`
newversion=`echo Bundle-Version: $newversionnr.qualifier`

if [[ $oldversion != $newversion ]]; then
sed -i -e "s/$oldversion/$newversion/g" ./META-INF/MANIFEST.MF
echo "increased bundle version to $newversionnr"

echo -n -e "$a: \t" >> ../carisma.updatesite/scripts/report.txt
echo "$oldversionnr -> $newversionnr" >> ../carisma.updatesite/scripts/report.txt
fi

fi
cd ..
fi
done
echo "Please run './update-poms.sh' to update the pom.xml files accordingly..."
