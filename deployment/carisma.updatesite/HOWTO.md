# Vorgehen zum Erstellen/Aktualisieren der Update-Site

1. `category.xml` in Eclipse öffnen und Categories / Features anpassen.
2. Ein git Tag mit dem Namensschema 'build-YYYYMMDD' setzen. YYYYMMDD entspricht dem aktuellen Datum. Es wird davon ausgegangen, dass es maximal ein Release pro Tag gibt.
3. das git tag ins GitHub Remote Repository pushen, eine GitHub Action baut daraufhin das release und veröffentlicht es auf GitHub Pages.
4. Nach einem erfolgreichen, lokalen Aufruf von `./mvnw clean verify` muss aktuell noch manuell ein ZIP-File `carisma-updatesite.zip` mit dem Inhalt aus `deployment/carisma.updatesite/target/repository/` erstellt werden.
5. Via https://github.com/CARiSMA-Tool/carisma-tool/release/new wird ein entsprechendes neues _GitHub Release_ erstellt
