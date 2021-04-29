Rotatorien - Umsetzung {#lnk_rotatorien_num}
===============================

## Herkunft ##
UNTERPROGRAMM ZUR BERECHNUNG DES ZOOPLANKTONEINFLUSSES\n            
AUF DEN STOFF-UND SAUERSTOFFHAUSHALT EINES FLIEGEWAESSERS \n        
AUTOR :      VOLKER KIRCHESCH    \n                                 
entnommen aus Version qsim13.301_28mae18\n 


## Schnittstellenbeschreibung ##
Die Rotatorien-Biomasse wird in der Subroutine konsum() berechnet, welche
die folgenden Abhängigkeiten hat:

subroutine konsum() \ref vkigr, \ref tempw, \ref vo2, \ref tflie  &\n
  &, \ref ezind, \ref zooind, \ref abszo, \ref ir, \ref flag,elen,ior,anze,qeinl,vabfl &\n
  &,jiein, \ref fopire, \ref grote, \ref dzres1, \ref dzres2, \ref zresge  &\n
  &, \ref irmaxe, \ref zexki, \ref zexgr, \ref zexbl                       &\n
  &, \ref aki, \ref agr, \ref abl, \ref iwied, \ref rmuas, \ref iras, \ref tgzoo, \ref bac, \ref zbac    &\n
  &, \ref rakr, \ref rbar, \ref chnf, \ref zhnf, \ref ilbuhn, \ref zakie
  , \ref zagre, \ref zable, \ref hnfza, \ref algzok     &\n
  &, \ref algzog, \ref algzob, \ref akiz, \ref agrz, \ref ablz
  , \ref algzkz, \ref algzgz, \ref algzbz, \ref nkzs, \ref monats    &\n
  &, \ref itags, \ref uhrz, \ref mstr, \ref azstrs, \ref kontroll, \ref iglob)
 
## IT-Realisierung ##
Die QSim Subroutine konsum() wird in QSim-3D von der Hüllroutine konsum_huelle() aufgerufen (siehe dazu: \ref hüllen). 

## Aufruf-Parameter:(Stand QSim 13.30)##
| Variablen-Name \n QSim-1D | Daten-Feld \n QSim-3D | Beschreibung | Einheit |
| ------------------------- | --------------------- | ------------ | ------- |
| vkigr	| \ref tiefengemittelte_planktische_variable 19 | Anteil Kieselalgen | - |
| TEMPW	| \ref tiefengemittelte_planktische_variable 1 | Wasser-Temperatur | °C |
| VO2		| \ref tiefengemittelte_planktische_variable 2 | Sauerstoff | mg/l |
| TFLIE	|  real(deltatt)/86400  | Zeitschritt | TFLIE in d; deltat in s  |
| | | | |
| ezind	| - | keine Einleitungen in QSim-3D | - |
| ZOOIND	| \ref tiefengemittelte_planktische_variable 50 | Individuendichte | 1/l |
| abszo	| \ref tiefengemittelte_übergabe_variable 6 | Absterberate | 1/d |
| ir		| \ref tiefengemittelte_übergabe_variable 42 | Ingestionsrate | mg/(l*h) |
| flag		| 0 | keine Einleitungen | - |
| elen		| 1 | Elementlänge (nicht verwendet) | - |
| ior		| 1 | Laufindex | - |
| anze		| 1 | Anzahl der Profile im aktuellen Strang | - |
| qeinl	| 0 | keine Einleitung | - |
| vabfl	| 0 | keine Einleitung | - |
| | | | |
| jiein	| 0 | keine Punkt-Einleitungen | - |
| FopIRe	| \ref globaleParameter | Halbsättigungskonstante für Futteraufnahme | mg/l |
| GRote	| \ref globaleParameter | Gewicht einer Rotatorie | µg |
| dzres1	| \ref tiefengemittelte_übergabe_variable 27 | Grund-Respiration Konsumenten |  |
| dzres2	| \ref tiefengemittelte_übergabe_variable 28 | Fraßabhängige Respirationsrate |  |
| zresge	| \ref globaleParameter | Grundrespiration Rotatorien | 1/d |
| | | | |
| irmaxe	| \ref globaleParameter | max. Gewichtsspez. Algenaufnahmerate |  |
| zexki	| \ref tiefengemittelte_übergabe_variable 16 | Ausscheidungen infolge Konsums von Kieselalgen |  |
| zexgr	| \ref tiefengemittelte_übergabe_variable 17 | Ausscheidungen infolge Konsums von Grünalgen |  |
| zexbl	| \ref tiefengemittelte_übergabe_variable 18 | Ausscheidungen infolge Konsums von Blaualgen  |  |
| | | | |
| aki		| \ref tiefengemittelte_planktische_variable 8 | Biomasse der Kieselalgen | mg/l |
| agr		| \ref tiefengemittelte_planktische_variable 9 | Biomasse der Grünalgen | mg/l |
| abl		| \ref tiefengemittelte_planktische_variable 10 | Biomasse der Blaualgen | mg/l |
| iwied	| 0 | unbenutzte Variable | - |
| rmuas	| \ref tiefengemittelte_übergabe_variable 76 | Ausgabe: Nettowachstumsrate | 1/d |
| iras		| \ref tiefengemittelte_übergabe_variable 79 | Ausgabe: Ingestionsrate | 1/d |
| | | | |
| rakr		| \ref tiefengemittelte_übergabe_variable 77 | Ausgabe: fraßabhängige Respiration | 1/d |
| rbar		| \ref tiefengemittelte_übergabe_variable 78 | Ausgabe: GRund?-Respiration? | 1/d |
| CHNF		| \ref tiefengemittelte_planktische_variable 48 | C-Masse der heterotrophen Nanoflagelaten | mg C / l |
| zHNF		| \ref tiefengemittelte_übergabe_variable 74 | Aufnahmerate der HNF | 1/d |
| ilbuhn	| 0 | keine Buhnen | - |
| zakie	| \ref globaleParameter | Filtrierbarkeit Kieselalgen | - |
| zagre	| \ref globaleParameter | Filtrierbarkeit Grünalgen | - |
| zable	| \ref globaleParameter | Filtrierbarkeit Blaualgen | - |
| HNFza	| \ref tiefengemittelte_übergabe_variable 75 | Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. |  |
| algzok	| \ref tiefengemittelte_übergabe_variable 53 | kiesel-Algen-Konsum | mg/l |
| | | | |
| algzog	| \ref tiefengemittelte_übergabe_variable 72 | grün-Algen-Konsum | mg/l |
| algzob	| \ref tiefengemittelte_übergabe_variable 73 | blau-Algen-Konsum | mg/l |
| akiz		| \ref tiefenaufgelöste_planktische_variable 8 | Biomasse kiesel-Algen | mg/l |
| agrz		| \ref tiefenaufgelöste_planktische_variable 9 | Biomasse grün-Algen | mg/l |
| ablz		| \ref tiefenaufgelöste_planktische_variable 10 | Biomasse blau-Algen | mg/l |
| algzkz	| \ref tiefenaufgelöste_übergabe_variable 26 | kiesel-Algen-Konsum | mg/l |
| algzgz	| \ref tiefenaufgelöste_übergabe_variable 27 | grün-Algen-Konsum | mg/l |
| algzbz	| \ref tiefenaufgelöste_übergabe_variable 28 | blau-Algen-Konsum | mg/l |
| nkzs		| noch 1 | Anzahl Tiefenschichten |  |
| monats	| module ::modell monat | monat |  |
| | | | |
| itags	| module ::modell tag | tag |  |
| mstr		| 1 | Strangzähler |  |

Quelle konsum_huelle.f95; zurück zu: \ref lnk_ueberblick

 
aus Datei: rotatorien-numerik.md; 

Code in Datei konsum.f90
