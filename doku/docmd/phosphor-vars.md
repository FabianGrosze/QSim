Phosphor - Formelzeichen/Variablennamen {#lnk_phosphor_vars} 
========================================

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
| \f$ P_{ges} \f$  | Gesamt-Phosphorgehalt im Gewässer | \f$ mg P \cdot L^{-1} \f$ | - | gesP | x |
| \f$ C_{org, sed} \f$ | Gesamtmasse Kohlenstoff, die je Zeitschritt sedimentiert | \f$ mg C \cdot L^{-1} \cdot t^{-1}\f$ |  -  |  orgCsd  |  b?  |
| \f$ Y_{C:P} \f$ | P/C Verhältnis von Phosphor zu Kohlenstoff in organischem Material | \f$ mg P \cdot mg C^{-1} \f$ |  -  |  pl0  |  b?  |
| \f$ A_{sed, i} \f$ | Sedimentierte Menge der Algengruppe *i* | \f$ mgBio \cdot L^{-1} \cdot t^{-1}\f$ |  -  |  sedalk, sedalb, sedalg  |  ...  |
| \f$ Q_{P,A_i} \f$ | Phosphoranteil der Algenbiomasse der Algengruppe *i*  | \f$ mgP \cdot mgBio^{-1} \f$ |  ...  |  Q_PK, Q_PB, Q_PG  |  ...  |
| \f$ SRP \f$ | Konzentration gelöster reaktiver Phosphat | \f$ mg P \cdot L^{-1} \f$ |  -  |  gelP  |  x  |
| \f$ r_{P, bac} \f$ | Phosphat-P-Freisetzung beim Abbau org. Kohlenstoffverbidungen | \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ |  ...  |  doP bzw. bsbctP |  b  |
| \f$ a_{Alg} \f$ | Netto-Ausscheidung durch Algen | \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ |  ...  |  mw_agrP, mw_akiP, mw_ablP  |  ...  |
| \f$ a_{Rot} \f$ | Ausscheidung der Rotatorien |  \f$ mg P \cdot L^{-1} \cdot t^{-1} \f$ |  ...  |  gelPzo  |  b  |
| \f$ u_{P, i} \f$ | P-Aufnahme der Algengruppe *i* | \f$ mg P \cdot mg Bio^{-1} \f$ |  ...  |  up_PGz, up_PKz, up_PBz  |  ...  |
| \f$ \mu_{A_i} \f$ | Brutto-Zuwachs der Algenbiomasse der Algengruppe *i* | \f$ mg Bio \cdot L^{-1} \cdot t^{-1} \f$ |  ...  |  agrbrz, akibrz, ablbrz  |  ...  |
| \f$ A_{resp, A_i} \f$ | Respirierte Algenbiomasse der Algengruppe *i* | \f$ mg Bio \cdot L^{-1} \cdot t^{-1} \f$ |  ...  |  algagz, algakz, algabz |  ...  |
| \f$ r_{z, grund} \f$ | Grundrespiration des Zooplanktons | \f$ mg Bio \cdot L^{-1} \cdot t^{-1}\f$ |  ...  |  dzres1  |  ...  |
| \f$ r_{z, A_i} \f$ | Fraßabhängige Respirationsrate des Zooplanktons | \f$ mg Bio \cdot L^{-1} \cdot t^{-1}\f$ |  ...  |  dzres2  |  ...  |
| \f$ \theta_{A_i} \f$ | Anteil der Algenklasse *i* an der Gesamtalgenmenge | - |  ...  |  hconKi, hcongr, hconbl  |  ...  |
| \f$ Q_{P,A_i} \f$ | Phosphoranteil der Algenbiomasse der Algengruppe *i* | \f$ mgP \cdot mgBio^{-1} \f$ |  ...  |  Q_PK, Q_PG, Q_PB  |  ...  |



Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Text source: phosphor-vars.md; Code sources: module_phosphate.f90, phosphate.f90
and phosphate_wrapper_3d.f95; go back to: \ref lnk_phosphor
