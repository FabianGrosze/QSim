# QSim 14

## 14.09
### 14.09.29
* New module for reading of salinity from UnTRIM hydrodynamics in QSim3D

### 14.09.28
* Create module and corefunction for corophium (note this module is currently turned off)

### 14.09.27
* fixed initialisation of photoinhibition of cyanobacteria in groyne field

### 14.09.26
* bug fixing and partial refactoring (incl. commenting) of Dreissena routine

### 14.09.25
* Replace semi-implicit timesteping in oxygen with explicit timesteping.

### 14.09.24
* Create modules for macrophytes and hnf

### 14.09.23
* Correct calculations in Nitrogen

### 14.09.22
Bugfixes
* Correct groyne fields
* Correct temperature conversions to absolute zero
* Minimize compiler warnings

### 14.09.21
* bug fixes and minor refactoring in `nitrogen.f90`, `nitrifiers.f90`, `oxygen.f90` and `sedimentation.f90` to prevent division by 0, log(0) and uninitialised use of variables

### 14.09.20
* New module for coliform bacteria

### 14.09.19

* Add `implicit none` to all files
* Fix some compiler warnings in QSim1D

### 14.09.18

* Refactored `konsum.f90` (incl. `implicit none`)
* Fixed bugs introduced in 14.09.16 (`konsum.f90`, `funkstar.f90`)
* updated makefiles for compilation on HPC:
   * corrected 1D makefile
   * added 'debug' compilation option for 1D and 3D, together with 'clean_debug' and 'clean_release' rules
   * implemented automatic detection of QSim version number from `version-string.f90` and naming of executables
* added compilation script 'make_qsim_log.sh' to allow logging of compiler messages during compilation of 1D and 3D

* Tested successfully for Elbe River model in both 'Debug' and 'Release' mode

### 14.09.17
* Turn off 'Sediment-Kenngrößen' in GUI Gerris

### 14.09.16
First version compiled and tested using 'Release' (= default) and 'Debug' mode (1D) and yielding identical results

Tests run:
* tracer simulation with simple model
* coliform simulation with simple model
* water quality simulation with simple model without groins
* water quality simulation with simple model with groins

Bugs fixed:
* bug fix in 3D hydrodynamics time selection
* several uninitialised variables
* several initalisation errors for groin-related variables
* fixed treatment of cloud typ
* bug fix in interpolation of boundary conditions - see metabolism/set_cloud_reflectance.f90

### 14.09.15
Fixed some bugs that remained from previous versions and entirely removed (unused) `aparamles.f90` from code base.

### 14.09.14
Improvement of Errormessaging in QSim1D. `subroutine qerror` is now available to both QSim1D and QSim3D. 
Thisway errormessages can be sent from any part of the program. The former mechanism of using errorcodes (`ifehl`) and defining
messages in file `fehlermeldungen.txt` becomes obsolete and is removed entirely from QSim.

### 14.09.13
* QSim3D: initialise tracers with 0; write actual mean age to ganglinien files
* QSim1D: Remove xml-definitions for deactivated modules

### 14.09.12
Code Refactoring
* Modules and Corefunctions for nitrogen
* Correct Issues in ussage of module aparam

### 14.09.11
Code Refactoring - Calculations were not altered.
* Modules and Corefunctions for oxygen and silicate
* New Module `module_metabolism` to cpmbine all metabolism-modules

### 14.09.10

* Modules and Corefunctions for organic carbon and phosphate. Calculations
are not change, only code was refactored
* The following modules are turned off as decided in QSim-Runde:
  * Sedimentflux
  * HNF
  * Corophium
  * Macrophytes
  * Benthic Algae

### 14.09.09

* Bugfix (QSim3D): Correct error in initializing variable schwi

### 14.09.08

* Bugfix: vtk-output now includes all variables
* Bugfix: correct zone-numbering in qsim3d

### 14.09.07
* Bugfix: correct mechanism for interpolation in `subroutine wettles_wetter`


### 14.09.06

* Bugfix: correct if-condition in reading from `ModellA.txt` in QSim 1D.

### 14.09.05

* Bugfix: correct arguments in call ph for groyne-fields


### 14.09.04
* Refactoring of Code: module for pH

### 14.09.03
* Bugfix: remove output of heavy metals in ErgebT.txt to avoid issues with Gerris.

### 14.09.02
Bugfix:
* correct format for output files
* correct dimensions of `tausc` in alagae

### 14.09.01
* apply parts of style guide rules to sourcecode. No changes in calculations.

## 14.09.00
* enabling cross sections in 3D (schnitt.txt)
* additional heavy metals 

### 14.08.00
Stable release comprising evaporation options in temperature module (see 14.06) and reading of UnTRIM SPM (see 14.07.02)


### 14.07.02
added functionality for reading SPM from UnTRIM hydrodynamics in QSim3D (see qims3d/module_suspendedMatter.f90)

### 14.10

* adding additional heavy metal concentrations.
* restructured modules: Schwermetall, erosion, Schwebstoff and Sedimentbelastung
* enable input of erosion parameters

### 14.07.01
compiled at 16.mae.2022 


## 14.06
including extended and restructured temperature module `temperw_kern.f90`
* evaporation (different formulas) added - in Gerris, use "Verdunstungsberechnung nach Sweers (1976)" für Vergleichbarkeit mit bisherigen Versionen
* including subroutine into QSim3D


## 14.04
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

 
## 14.02
Neu in 14.02 (20aug20):
 
* Mehr als 40000 Zeitpunkte in einer Randbedingungs-Zeitreihe möglich.
* Initialisierung nicht belegter Sedimentflüsse bei abgeschaltetem Sedflux-Modul.
* Initialisierung Schwermetallkonzentrationen bei Rechnungen ohne Schwermetalle.
* Fehler durch nicht initialisierten Strang wird abgefangen.
