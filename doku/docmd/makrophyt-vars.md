Makrophyten - Formelzeichen/Variablennamen {#lnk_makrophyt_vars}
========================================

\warning Momentan ist das Modul ausgeschaltet, das heißt, Makrophyten werden 
nicht simuliert. Es handelt sich um eine alte Dokumentation.

\n\n

## Liste der Formelzeichen und Variablennamen, BausteinA:(Stand QSim xx.yy)

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft |
|------|------------|---------|-----|-------|---|
|      |            |         |  2  | mcona | v |
|      |            |         | 0,5 | mconb | v |
|      |            |         | 2   | pcona | v |
|      |            |         | 1   | pconb | v |
|      | Minimale Aufwuchsalgendichte | g/m² | 0,01 | phytmi | v |
|      | Maximale Aufwuchsalgendichte | g/m² | 0,2  | phytma | v |
|      | Optimale Lichtintensiät für Makrophyten      | | 60 | miopt | v |
|      | Kompensationslichtintensität für Makrophyten | | 20 | GRSTR | v |
| \f$ Start_{Tag,Mak} \f$ | Makrophyten Start-Tag (StartTag) | ... | ... | itstart | e |
| \f$ Start_{Mon,Mak} \f$ | Makrophyten Start-Monat (StartMonat) | ... | ... | mstart (?) | e |
| \f$ Max_{Tag,Mak} \f$	 | Makrophyten Max.-Tag (MaxTag) | ... | ... | itmax | e |
| \f$ Max_{Mon,Mak} \f$  | Makrophyten Max.-Monat (MaxMonat)  | ... | ... | mmax (?) | e |
| \f$ Ende_{Tag,Mak} \f$ | Makrophyten Ende-Tag (EndTag)  | ... | ... | itend | e |
| \f$ Ende_{Mon,Mak} \f$ | Makrophyten Ende-Monat (EndMonat) | ... | ... | mend (?) | e |
| \f$ A_{Mak,max} \f$	 | Makrophyten max. Dichte (Sommer)  | g/m² | ... | PflMax | e/b |
| \f$ A_{Mak,min} \f$	 | Makrophyten min. Dichte (Winter)  | g/m² | ... | Pflmin | e/b |


Herkunft:
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: makrophyt-vars.md; Codesource: mphyt.f90; zurück: \ref lnk_makrophyt
