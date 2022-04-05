
## 14.10

adding additional heavy metal concentrations.

restructured modules: Schwermetall, erosion, Schwebstoff and Sedimentbelastung

enable input of erosion parameters

## 14.07.01
compiled at 16.mae.2022 


## TAG 14.06

including extended and restructured temperature module `temperw_kern.f90`
* evaporation (different formulas) added - in Gerris, use "Verdunstungsberechnung nach Sweers (1976)" für Vergleichbarkeit mit bisherigen Versionen
* including subroutine into QSim3D


## tag 14.04

Restrukturierung und Dokumentation pH-Modul

 
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
