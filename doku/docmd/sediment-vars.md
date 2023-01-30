Sediment - Formelzeichen/Variablennamen {#lnk_sediment_vars}
========================================

## Liste der Formelzeichen und Variablennamen, BausteinA:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | Referenz |
|----------------|------------|--------------|---------|---------|---------|---|
| DNH4 | Diffusionskoeffizient für Ammonium im Sediment | m2*s-1 |  |  |  |  | 
| DNO3 | Diffusionskoeffizient für Nitrat im Sediment | m2*s-1 |  |  |  |  | 
| DO2 | Diffusionskoeffizient für Sauerstoff im Sediment | m2*s-1 |  |  |  |  | 
| DPO4 | Diffusionskoeffizient für Phosphor im Sediment | m2*s-1 |  |  |  |  | 
| θmisch | Temperaturkoeffizient für ωmisch | - |  |  |  |  | 
| H1 | Dicke der aeroben Schicht | m |  |  |  |  | 
| H1* | Schichtdicke der alt abgelagerten partikulären organischen Kohlenstoffverbindungen | m | Formel [82] |  |  |  | 
| H2 | Dicke der anaeroben 2. Schicht im Sediment | cm | 10 |  |  |  | 
| Hsed,orgC | Schichtdicke der frisch abgelagerten Kohlenstoffverbindungen | m |  |  |  |  | 
| i | Laufvariable der Zeitschritte | - |  |  |  |  | 
| JPOC_neu | mittlerer Sedimentationsfluss an partikulärem organischen Kohlenstoff während der Simulation | gC*m-2*d-1 |  |  |  |  | 
| JPON_neu | mittlerer Sedimentationsfluss an partikulärem organischem Stickstoff während der Simulation | gC*m-2*d-1 |  |  |  |  | 
| JPOP_neu | mittlerer Sedimentationsfluss an partikulärem organischem Phosphor während der Simulation | gC*m-2*d-1 |  |  |  |  | 
| KL0,1,NH4 | Massentransferkoeffizient für Ammonium | m*d-1 |  |  |  |  | 
| KL0,1,NO3 | Massentransferkoeffizient für Nitrat | m*d-1 |  |  |  |  | 
| KL0,1,O2 | Massentransferkoeffizient für Sauerstoff | m*d-1 |  |  |  |  | 
| KL0,1,PO4 | Massentransferkoeffizient für Phosphor | m*d-1 |  |  |  |  | 
| n | Anzahl der Zeitschritte | - |  |  |  |  | 
| O2(0) | Sauerstoffkonzentration an der Sedimentoberfläche | mg*L-1 |  |  |  |  | 
| O2(1) | Sauerstoffkonzentration am Boden der obersten Sedimentschicht | mg*L-1 |  |  |  |  | 
| ϕ | Sedimentporosität | dm3*dm-3 |  |  |  |  | 
| POC1,R | Referenzkonzentration an leicht abbaubaren partikulären organischen Kohlenstoffverbindungen | g C*kg-1 |  |  |  |  | 
| POC1(2) | Gehalt an leicht abbaubaren partikulären organischen Kohlenstoffverbindungen in der 2. Schicht | g*m-3 |  |  |  |  | 
| POCDet,sed | pro Zeitschritt sedimentierte Menge an Detritus-Kohlenstoff | gC*m-2*d-1 |  |  |  |  | 
| POCPhyto,sed | pro Zeitschritt sedimentierter Phytoplankton-Kohlenstoff | gC*m-2*d-1 |  |  |  |  | 
| PONDet,sed | pro Zeitschritt sedimentierte Menge an Detritus-Stickstoff | gC*m-2*d-1 |  |  |  |  | 
| PONPhyto,sed | pro Zeitschritt sedimentierte Menge an Phytoplankton-Stickstoff | gC*m-2*d-1 |  |  |  |  | 
| POPDet,sed | pro Zeitschritt sedimentierte Menge an Detritus-Phosphor | gC*m-2*d-1 |  |  |  |  | 
| POPPhyto,sed | pro Zeitschritt sedimentierte Menge an Phytoplankton-Phosphor | gC*m-2*d-1 |  |  |  |  | 
| Re | Reynoldszahl | - | Berechnet |  |  |  | 
| ρS | Dichte des Sediments | kg*m-3 |  |  |  |  | 
| Sc | Schmidt-Zahl | - |  |  |  |  | 
| Δt | Berechnungszeitschrittweite | d |  |  |  |  | 
| Tiefe | Höhe der Wassersäule über dem Sediment am Berechnungspunkt | m |  |  |  |  | 
| TP | Periode des Geschwindigkeitspulses | s |  |  |  |  | 
| U | Amplitude des sinusförmigen Pulses der Turbulenz ins Sediment | m*s-1 |  |  |  |  | 
| vt | Eddy-Diffusionskoeffizient | m2*s-1 |  |  |  |  | 
| W | Eindringtiefe des sinusförmigen Pulses der Turbulenz ins Sediment | m*s-1 |  |  |  |  | 
| ωmisch,0 | Vermischungsgeschwindigkeit für partikuläres organisches Material durch Bioturbation ohne Berücksichtigung der organischen Belastung und des Sauerstoffgehalts | m*d-1 |  |  |  |  | 


Herkunft:
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: sediment-vars.md; Codesource: SedFlux.f90, sedflux_huelle.f95; 
zurück: \ref lnk_sediment
