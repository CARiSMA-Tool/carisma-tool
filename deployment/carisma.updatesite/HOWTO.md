# Vorgehen zum Erstellen/Aktualisieren der Update-Site

1. `category.xml` in Eclipse öffnen und Categories / Features anpassen.
2. Ein git Tag mit dem Namensschema 'build-YYYYMMDD' setzen. YYYYMMDD entspricht dem aktuellen Datum. Es wird davon ausgegangen, dass es maximal ein Release pro Tag gibt. Es kann auch eine Uhrzeit angefügt werden, relevant ist nur, dass das tag mit dem String `build-` beginnt.
3. Das git tag ins GitHub Remote Repository pushen. 

Eine GitHub Action baut daraufhin das release (und damit alle relevanten Dateien) und veröffentlicht den Inhalt der Update-Site auf GitHub Pages und erstellt im Anschluss noch ein entsprechendes _GitHub Release_ mit einem ZIP-Archiv der Update-Site.
