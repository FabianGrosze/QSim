TAG> 14.04


Dokumentationsportal in eigenem Verzeichnis (27aug20):

Die Dokumentation umfasst erg�nzende Materialien wie PDF's, Bilder, Beispieldateien u.a., 
die nicht von Doxygen aus dem Quellcode generiert werden.

Diese sind nicht im Versionierungssystem Git enthalten, sondern finden sich in einem .taz Archiv,
das von der Download-Seite des Dokumentationsportals heruntergeladen werden kann.

Vor einer Neuerstellung des Dokumentationsportals mittels "make" im doku Verzeichnis muss dieses Archiv (qsim_doku.taz)
im doku Verzeichnis entpackt werden.

Werden bei Arbeiten am Dokumentationsportal Erg�nzungen oder �nderungen in den erg�nzenden Materialien vorgenommen,
ist es n�tig, diese vorab mittels "make save" in ein .taz Archiv einzubinden, das dann bei einer 
Neuerstellung des Dokumentationsportals mittels "make" im doku Verzeichnis wieder ins Dokumentationsportal hochgeladen wird.



TAG> 14.02
Neu in 14.02 (20aug20):

- Mehr als 40000 Zeitpunkte in einer Randbedingungs-Zeitreihe m�glich.
- Initialisierung nicht belegter Sedimentfl�sse bei abgeschaltetem Sedflux-Modul.
- Initialisierung Schwermetallkonzentrationen bei Rechnungen ohne Schwermetalle.
- Fehler durch nicht initialisierten Strang wird abgefangen.
