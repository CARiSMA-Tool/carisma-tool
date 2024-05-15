# Vorgehen zum Erstellen/Aktualisieren der Update-Site

1. `category.xml` in Eclipse öffnen und Categories / Features hinzufuegen / ändern / löschen
2. `index.html` (verändert sich nicht), `artifacts.jar`, `content.jar`, `plugins/*`, `features/*` aus `target/repository/` auf der Update Site verfügbar machen (via SVN).
   Dabei die Ordner `features` und `plugins` nicht löschen/neu hinzufügen, sondern nur (leeren und) neue `.jar` hinzufügen.
3. Ein git Tag setzen mit dem Schema 'build-YYYYMMDDhhmm'. Der Wert YYYYMMDDhhmm sollte dem Wert von `target/repository/features/feature.carisma.core_x.y.z.YYYYMMDDhhmm.jar` entsprechen
4. Ein ZIP-File `carisma-updatesite.zip` erstellen mit den folgenden Dateien bzw. Verzeichnissen im Hauptverzeichnis: `artifacts.jar`, `content.jar`, `plugins/*`, `features/*`.
5.Via https://github.com/CARiSMA-Tool/carisma-tool/release/new ein neues Release erstellen
