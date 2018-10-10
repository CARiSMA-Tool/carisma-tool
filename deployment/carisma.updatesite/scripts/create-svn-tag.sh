#!/bin/bash
if [[ "$1" == "" ]]; then
echo "Syntax: $0 [YYYY-MM-DD_name]"
echo "!!!Achtung: keine Leerzeichen im Releasenamen erlaubt!!!"
exit 1
fi
echo "svn cp https://ls14svn.cs.tu-dortmund.de/carisma/trunk https://ls14svn.cs.tu-dortmund.de/carisma/tags/Release_$1"
#svn cp https://ls14svn.cs.tu-dortmund.de/carisma/trunk https://ls14svn.cs.tu-dortmund.de/carisma/tags/Release_$1
