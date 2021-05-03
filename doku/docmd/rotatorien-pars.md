Rotatorien - Formelzeichen/Variablennamen {#lnk_rotatorien_pars}
=========================================

## Liste der Formelzeichen und Variablennamen des Rotatorien-Bausteins:(Stand *QSim 13.30*) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablenname \n Quellcode | Herkunft | 
| ------ | --------| -------| --------| ------- | ------- | 
|  | Anteil Kieselalgen | - | - | [vkigr](\ref tiefengemittelte_planktische_variable 19) | e | 

<!-- to do: untenstehende Tabelle in obige Struktur umschreiben -->

| Variablen-Name \n QSim-1D | Daten-Feld \n QSim-3D | Beschreibung | Einheit |
|----------------|------------|--------------|---------|
| vkigr	| \ref tiefengemittelte_planktische_variable 19 | Anteil Kieselalgen | - |
| TEMPW	| \ref tiefengemittelte_planktische_variable 1  | Wasser-Temperatur  | °C |
| VO2	| \ref tiefengemittelte_planktische_variable 2  | Sauerstoff | mg/l |
| TFLIE	| real(dt)/86400  | Zeitschritt | TFLIE in d; dt in s |
|----------------|------------|--------------|---------|
| ezind	 | - | keine Einleitungen in QSim-3D          | - |
| ZOOIND | \ref tiefengemittelte_planktische_variable 50 | Individuendichte | 1/l |
| abszo	 | \ref tiefengemittelte_übergabe_variable 6     | Absterberate     | 1/d |
| ir	 | \ref tiefengemittelte_übergabe_variable 42    | Ingestionsrate   | mg/(l*h) </|
| flag	 | 0 | keine Einleitungen                     | - |
| elen	 | 1 | Elementlänge (nicht verwendet)         | - |
| ior	 | 1 | Laufindex                              | - |
| anze	 | 1 | Anzahl der Profile im aktuellen Strang | - |
| qeinl	 | 0 | keine Einleitung                       | - |
| vabfl	 | 0 | keine Einleitung                       | - |
|----------------|------------|--------------|---------|
| jiein	 | 0 | keine Punkt-Einleitungen | - |
| FopIRe | \ref globaleParameter | Halbsättigungskonstante für Futteraufnahme | mg/l |
| GRote	 | \ref globaleParameter | Gewicht einer Rotatorie | µg |
| dzres1 | \ref tiefengemittelte_übergabe_variable 27 | Grund-Respiration Konsumenten |  |
| dzres2 | \ref tiefengemittelte_übergabe_variable 28 | Fraßabhängige Respirationsrate |  |
| zresge | \ref globaleParameter | Grundrespiration Rotatorien | 1/d |
|----------------|------------|--------------|---------|
| irmaxe | \ref globaleParameter | max. Gewichtsspez. Algenaufnahmerate |  |
| zexki	 | \ref tiefengemittelte_übergabe_variable 16 | Ausscheidungen infolge Konsums von Kieselalgen |  |
| zexgr	 | \ref tiefengemittelte_übergabe_variable 17 | Ausscheidungen infolge Konsums von Grünalgen |  |
| zexbl	 | \ref tiefengemittelte_übergabe_variable 18 | Ausscheidungen infolge Konsums von Blaualgen  |  |
|----------------|------------|--------------|---------|
| aki	 | \ref tiefengemittelte_planktische_variable 8  | Biomasse der Kieselalgen | mg/l |
| agr	 | \ref tiefengemittelte_planktische_variable 9  | Biomasse der Grünalgen | mg/l |
| abl	 | \ref tiefengemittelte_planktische_variable 10 | Biomasse der Blaualgen | mg/l |
| iwied	 | 0 | unbenutzte Variable | - |
| rmuas	 | \ref tiefengemittelte_übergabe_variable 76 | Ausgabe: Nettowachstumsrate | 1/d |
| iras	 | \ref tiefengemittelte_übergabe_variable 79 | Ausgabe: Ingestionsrate     | 1/d |
|----------------|------------|--------------|---------|
| rakr	 | \ref tiefengemittelte_übergabe_variable 77 | Ausgabe: fraßabhängige Respiration | 1/d |
| rbar	 | \ref tiefengemittelte_übergabe_variable 78 | Ausgabe: GRund?-Respiration? | 1/d |
| CHNF	 | \ref tiefengemittelte_planktische_variable 48 | C-Masse der heterotrophen Nanoflagelaten | mg C / l |
| zHNF	 | \ref tiefengemittelte_übergabe_variable 74 | Aufnahmerate der HNF | 1/d |
| ilbuhn | 0 | keine Buhnen | - |
| zakie	 | \ref globaleParameter | Filtrierbarkeit Kieselalgen | - |
| zagre	 | \ref globaleParameter | Filtrierbarkeit Grünalgen   | - |
| zable	 | \ref globaleParameter | Filtrierbarkeit Blaualgen   | - |
| HNFza	 | \ref tiefengemittelte_übergabe_variable 75 | Ausgabe: HNFza(ior) = (zHNF(ior)/CHNF(ior))*24. |  |
| algzok | \ref tiefengemittelte_übergabe_variable 53 | kiesel-Algen-Konsum | mg/l |
|----------------|------------|--------------|---------|
| algzog | \ref tiefengemittelte_übergabe_variable 72 | grün-Algen-Konsum | mg/l |
| algzob | \ref tiefengemittelte_übergabe_variable 73 | blau-Algen-Konsum | mg/l |
| akiz	 | \ref tiefenaufgelöste_planktische_variable 8 | Biomasse kiesel-Algen | mg/l |
| agrz	 | \ref tiefenaufgelöste_planktische_variable 9 | Biomasse grün-Algen | mg/l |
| ablz	 | \ref tiefenaufgelöste_planktische_variable 10 | Biomasse blau-Algen | mg/l |
| algzkz | \ref tiefenaufgelöste_übergabe_variable 26 | kiesel-Algen-Konsum | mg/l |
| algzgz | \ref tiefenaufgelöste_übergabe_variable 27 | grün-Algen-Konsum | mg/l |
| algzbz | \ref tiefenaufgelöste_übergabe_variable 28 | blau-Algen-Konsum | mg/l |
| nkzs	 | noch 1 | Anzahl Tiefenschichten    |  |
| monats | module ::modell monat | monat      |  |
|----------------|------------|--------------|---------|
| itags	 | module ::modell tag | tag          |  |
| mstr	 | 1                   | Strangzähler |  |

Herkunft:
+ x - Bilanzvariable QSim
+ b - berechnet, Zwischengröße / Übergabevariable
+ e - Eingabe
+ v - Vorgabe im Quellcode gesetzt

aus Datei: rotatorien-pars.md;

Code in Datei konsum.f90 

