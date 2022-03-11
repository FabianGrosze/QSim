## tag 14.08
Teile des 1D-Codes wurden neu formatiert um die Lesbarkeit zu verbessern. 
Die Berechnungen der Güte bleiben unverändert.
    
Die meisten Änderungen beziehen sich auf `qsim1d.f90`:
* Sämtliche Einrückungen im Code angepasst.
* Soweit der Code verständlich war, wurden goto-Anweisungen durch Schleifen und Verzweigungen ersetzt.
* Es gab eine if-else-Konstruktion um den gesamten Code, so dass mehrere tausend Zeilen in der else-Schleife lagen. Code wurde so umgeschrieben, dass diese Verzweigung in dieser Form nicht mehr nötig ist.
* ModellA.txt, ModellG.txt, AParam.txt werden jetzt innerhalb eigener Subroutinen eingelesen.
* Fehlermeldungen: Bisher werden Fehler umständlich mit goto-Anweisungen und Fehlernummern, die auf eine externe Datei mit vorformulierten Fehlermeldungen verweist, gehandhabt. Es gibt jezt die Subroutine `call fehlerAusgeben(text)`, welcher der Text zur Fehlermeldung direkt übergeben wird. Die Routine gibt den Text auf der Konsole sowie in `file1.err` (Schnittstelle für Gerris) aus und beendet anschließend QSim mit Exit-Code 1.

Änderungen in anderen Files sind lediglich neue Formatierungen ohne funktionale Veränderung

## TAG 14.06

including extended and restructured temperature module `temperw_kern.f90`
* evaporation (different formulas) added - in Gerris, use "Verdunstungsberechnung nach Sweers (1976)" für Vergleichbarkeit mit bisherigen Versionen
* including subroutine into QSim3D


## tag 14.04
 
Dokumentationsportal in eigenem Verzeichnis (27aug20):
 
Die Dokumentation umfasst ergänzende Materialien wie PDF's, Bilder, Beispieldateien u.a., 
die nicht von Doxygen aus dem Quellcode generiert werden.
 
Diese sind nicht im Versionierungssystem Git enthalten, sondern finden sich in einem .taz Archiv,
das von der Download-Seite des Dokumentationsportals heruntergeladen werden kann.
 
Vor einer Neuerstellung des Dokumentationsportals mittels "make" im doku Verzeichnis muss dieses Archiv (qsim_doku.taz)
im doku Verzeichnis entpackt werden.
 
Werden bei Arbeiten am Dokumentationsportal Ergänzungen oder Änderungen in den ergänzenden Materialien vorgenommen,
ist es nötig, diese vorab mittels "make save" in ein .taz Archiv einzubinden, das dann bei einer 
Neuerstellung des Dokumentationsportals mittels "make" im doku Verzeichnis wieder ins Dokumentationsportal hochgeladen wird.
 
 
 
## tag 14.02
Neu in 14.02 (20aug20):
 
- Mehr als 40000 Zeitpunkte in einer Randbedingungs-Zeitreihe möglich.
- Initialisierung nicht belegter Sedimentflüsse bei abgeschaltetem Sedflux-Modul.
- Initialisierung Schwermetallkonzentrationen bei Rechnungen ohne Schwermetalle.
- Fehler durch nicht initialisierten Strang wird abgefangen.
