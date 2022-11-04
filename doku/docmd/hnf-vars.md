Heterotrophe Nanoflagellaten (HNF) - Formelzeichen/Variablennamen {#lnk_hnf_vars}
=================================================================

## Liste der Formelzeichen und Variablennamen, BausteinA:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | Referenz |
|---------------|------------|--------------|---------|---------|---------|----|
| BAC | Bakterienbiomasse | µg C * l-1 |  | BAC | b | |
| excrHNF | Exkretionsrate der HNF | d-1 |  | exHNF | b | |
| EXCRHNF | Exkretionsfaktor der HNF | - | 0.5 | HNFex |  | Baretta-Bekker et al. 1998 |
| fHNF(T) | Temperaturabhängiger Faktor der HNF | - | - | ftemp | b | Baretta-Bekker et al. 1998 |
| GrazBac,HNF | Grazingrate der Bakterien durch HNF | mgC*L-1*d-1 | - | HNFBAC | b | |
| GrazHNF,DR | Grazingrate der HNF durch Dreissena | mgC*L-1*d-1 | - | drHNF | b |  |
| GrazHNF,Rot | Grazingrate der HNF durch Rotatorien | mgC*l-1*d-1 | - | zHNF | b |  |
| HNF | HNF Biomasse | µg C * l-1 | Messwert | CHNF |  |  |
| Ks,Bac | Halbsättigungskonstante der HNF für Bakterienbiomasse | µg C * l-1 | 14,3 | BACkse |  | abgeleitet von Jürgens 1992 |
| morHNF | altersspezifische Mortalität an HNF | d-1 | 0,1 | morHNF |  | Baretta-Bekker et al. 1998 |
| µHNF | Netto HNF Wachstumsrate | d-1 |  | mueHNF |  |  |
| Q10,HNF | Temperaturkoeffizient für HNF | - | 2.0 | q10 |  | Baretta-Bekker et al. 1998 |
| respHNF | Respirationsrate der HNF | d-1 |  | respHNF | | |
| respHNF,G at 20 °C | Grundrespirationsrate der HNF | d-1 | 0,05 | rGHNF |  | abgeleitet von Baretta-Bekker et al. 1998 |
| T | Wassertemperatur | °C | Messwert | TempW |  |  |
| upHNF,max | Maximale Aufnahmerate der HNF | d-1 | 1,61 | upmHNF |  | abgeleitet von Jürgens 1992 |
| YHNF | Ertragskoeffizient der HNF [-] | - | 0,33 | HNFass |  | Straile 1997 |


Herkunft:
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: hnf-vars.md; Codesource: hnf.f90; zurück: \ref lnk_hnf
