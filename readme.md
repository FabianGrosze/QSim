## 14.09.07
Bugfix: correct mechanism for interpolation in `subroutine wettles_wetter`


## 14.09.06

* Bugfix: correct if-condition in reading from `ModellA.txt` in QSim 1D.

## 14.09.05

* Bugfix: correct arguments in call ph for groyne-fields

## 14.09.04
Refactoring of Code: module for pH

## 14.09.03
* Bugfix: remove output of heavy metals in ErgebT.txt to avoid issues with Gerris.

## 14.09.02
Bugfix:
* correct format for output files
* correct dimensions of `tausc` in alagae

## 14.09.01
* apply parts of style guide rules to sourcecode. No changes in calculations.

## 14.09.00
* enabling cross sections in 3D (schnitt.txt)
* additional heavy metals 

## 14.08.00
Stable release comprising evaporation options in temperature module (see 14.06) and reading of UnTRIM SPM (see 14.07.02)


## 14.07.02
added functionality for reading SPM from UnTRIM hydrodynamics in QSim3D (see qims3d/module_suspendedMatter.f90)

## 14.10

* adding additional heavy metal concentrations.
* restructured modules: Schwermetall, erosion, Schwebstoff and Sedimentbelastung
* enable input of erosion parameters

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
 
* Mehr als 40000 Zeitpunkte in einer Randbedingungs-Zeitreihe möglich.
* Initialisierung nicht belegter Sedimentflüsse bei abgeschaltetem Sedflux-Modul.
* Initialisierung Schwermetallkonzentrationen bei Rechnungen ohne Schwermetalle.
* Fehler durch nicht initialisierten Strang wird abgefangen.
