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
svn up
svnall=`svn log -l 1 . | head -n 2 | tail -n 1 | awk '{print $1}'`
svnmanifest=`svn log -l 1 ./META-INF/MANIFEST.MF | head -n 2 | tail -n 1 | awk '{print $1}'`

echo "$svnall = $svnmanifest ?"
if [[ $svnall == $svnmanifest ]]; then
echo "no changes"
else
echo "changed"
svn log -r HEAD:$svnmanifest > ./changes
if [ `cat ./changes | grep '#increase-minor'` ]; then 
touch ./minor
echo "forces new minor version"
fi
fi

fi
cd ..
fi
done