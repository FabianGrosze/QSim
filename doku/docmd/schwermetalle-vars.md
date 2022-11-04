Schwermetalle - Formelzeichen/Variablennamen {#lnk_schwermet_vars}
========================================

## Liste der Formelzeichen und Variablennamen, BausteinA:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | Quelle |
|----------------|------------|--------------|---------|---------|---------|---------|
| \f$ c_1 \f$ | 1. Koeffizient für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | c1X | e |  |
| \f$ e_1 \f$ | 1. Exponent für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | e1X | e |  |
| \f$ c_2 \f$ | 2. Koeffizient für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | c2X | e |  |
| \f$ e_2 \f$ | 2. Exponent für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | e2X | e |  |
| \f$ c_3 \f$ | 3. Koeffizient für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | c3X | e |  |
| \f$ e_3 \f$ | 3. Exponent für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | e3X | e |  |
| \f$ c_4 \f$ | 4. Koeffizient für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | c4X | e |  |
| \f$ e_4 \f$ | 4. Exponent für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | e4X | e |  |
| \f$ c_5 \f$ | 5. Koeffizient für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | c5X | e |  |
| \f$ e_5 \f$ | 5. Exponent für Berechnung des Verteilungskoeffizient nach ATV-Modell | - | - | e5X | e |  |
| \f$ K_D \f$ | Verteilungskoeffizient | \f$ \Lg \f$ | ... | VTkoeff_X | b | ATV-Modell; I |
| \f$ K_D \f$ | Verteilungskoeffizient | \f$ \Lg \f$ | ... | VTkoeffDe_X | b | Deltares; I |
| \f$ K*D \f$ | Verteilungskoeffizient nach der Zeitschrittweite \f$ \Delta t \f$  | \f$ \Lg \f$ | ... | VTKoeffX_neu | b | II |
| Δt*86400	| Zeitschrittweite (nicht verwendet) | d | Minuten bis 1 h | tflie | e | ... |
| \f$ SM_{ges} \f$	| Gesamt-Konzentration des Schwermetalls X (im Wasser gelöst und an Feststoff adsorbiert) | \f$ \ugL \f$ | ... | hgsX | b | II |
| \f$ SM_{gel} \f$ | Konzentration des gelös-ten Schwermetalls X in Wasser | \f$ \ugL \f$ | ... | hglX | b | II |
| \f$ C_{SS} \f$ | Schwebstoffkonzentration | \f$ \mgL \f$ | ... | SSAlg | b | III |
| \f$ SS_{sed} \cdot \Delta t \f$ | sedimentierte Schweb-stoffe ohne Algen in einem Zeitschritt | \f$ \mgL \f$ | ... | sedSS | b | III |
| \f$ A_{sed,ki} \cdot \Delta t  \f$ | sedimentierte Kieselalgen in einem Zeitschritt | \f$ \mgL \f$ | ... | sedalk | b | IV |
| \f$ A_{sed,gr} \cdot \Delta t  \f$ | sedimentierte Grünalgen in einem Zeitschritt | \f$ \mgL \f$ | ... | sedalg | b | IV |
| \f$ A_{sed,bl} \cdot \Delta t  \f$ | sedimentierte Cyanobakterien in einem Zeitschritt | \f$ \mgL \f$ | ... | sedalb | b | IV |
| \f$ SM_E \cdot \Delta t \f$ | Konzentrationsänderung der Gesamt-Schwermetalle im Wasserkörper durch Erosion in einem Zeitschritt | \f$ \ugL \f$ | ... | JX | b | V |
| \f$ Sed_{SM_p} \f$ | Schwermetallbelastung des Sediments | \f$ \mgkg \f$ | ... | XSed | b | V |
| M	| Erosionsbeiwert | \f$\kgmqsinv\f$ | \f$ 7,5 \cdot 10^{-4(1)} \f$ | M | v | ... |
| E*Δt	| Erodierte Sedimentmenge in einem Zeitschritt | \f$ \kgmqsinv \f$ | ... | SSeros | b | V |
| \f$ \tau_0 \f$ | Sohlschubspannung | \f$ \kgminvsq \f$ | ... | taus | b | V |
| \f$ \tau_{0,krit} \f$	| kritische Erosionsschubspannung, tiefenabhängig | \f$ \kgminvsq \f$ | \f$ 1,25 \f$ | tausc | v/b | v: \cite Kern_1997 |
| n	| empirischer Exponent | - | 3,2 | n | v | \cite Kern_1997 |
| v	| Fließgeschwindigkeit | \f$ \ms \f$ | ... | vmitt | b | VI |
| \f$ D_x \f$ | longitudinaler Dispersionskoeffizient | \f$ \mqs \f$ | ... | DL | b | VII |
| H	| Gewässertiefe | m | ... | \ref tiefe | b | VI |
| m	| Anzahl der Zeitschritte bis zum Erosionsbeginn | - | ... | anzZeit | b | V |

\n \n
 
X: Pb, Cd, Cr, Fe, Cu, Mn, Ni, Hg, U, Zn, As 

Herkunft: \n
+ x - Bilanzvariable QSim  \n
+ b - berechnet, Zwischengröße/Übergabevariable  \n
+ e - Eingabe  \n
+ v - Vorgabe im Quellcode gesetzt  \n

Berechnet in: \n
I	Modul verteilungskoeff() \n
II	Modul schwermetalle() \n
III	Modul schweb() \n
IV	Module algaeski(), algaesgr(), algaesbl() \n
V 	Modul erosion() \n
VI	Hydraulikprogramm HYDRAX \n
VII	Programm qsim() \n

\n\n

Textquelle: schwermetalle-vars.md; 
Codesource: Schwermetalle.f90 und Schwermetalle_kern.f90 ; 
zurück: \ref lnk_schwermetalle
