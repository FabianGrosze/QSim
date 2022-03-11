Silikat - Formelzeichen/Variablennamen {#lnk_silikat_vars} 
========================================

## Liste der Formelzeichen und Variablennamen, Silikat:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
| *Si*  | Si-Konzentration im Gewässer | mg Si/L | - | si | b|
| \f$ Si_{ein} \f$  | Änderung der Si-Konzentration durch Einletung | mg Si/L | - | ... | b|
| \f$ Si_{Sed} \f$  | rückgelöstes Silikat aus dem Sediment | g Si/m3 | - | dsised | b |
| \f$ Si_{up}\f$  | Si-Aufnahme durch pelagische und benthische Kieselalgen | mg Si/d | - | akisi | b |
| \f$ Si_L \f$ | Silikat-Konzentration im Einleiter | \f$mg Si \cdot L^{-1}\f$ | - | sil | b |
| \f$ Q_L \f$  | Einleitmenge der linienförmigen Zuflüsse | \f$m^3 \cdot m^{-1}\f$ | - | qeinll | e |
| \f$ Q_{i-1} \f$  | Abfluss im Gewässer am vorangegangenen Knoten | m^3/s | - | hcq = vabfl(ior-m) | b |
| \f$ Q_{ein} \f$  | Wassermenge des Zuflusses | m^3/s | - | hcqe | e |
| \f$ Si_{ein} \f$ | Si-Konzentration im Zufluss | mg Si/L | - | hcsie | e |
| \f$ r_{Sed,Si} \f$ | Silizium-Flux aus dem Sediment | g Si/m2/d | - | hjsi | b |
| \f$ tflie \f$  | Zeitschrittweite | d ?? | ... | tflie | ...|
| *h*  | mittlere Wassertiefe | m | - | tiefe | b |
| \f$ u_{Si} \f$  | Si-Aufnahmerate der Kieselalgen | mgSi/(mgBio*d) | ... | up_siz | ...|
| \f$ \mu_{diat} \f$ | Brutto-Zuwachs Kiesel-Algen-Biomasse | mgBio/l je Zeitschritt | ... | akibrz | ...|
| \f$ r_{diat} \f$ | Respirierte Algenbiomasse der Kieselalgen | mgBio/l je Zeitschritt | ... | algakz | ...|
| \f$ \mu_{bent} \f$ | Wachstum benthischer Kieselalgen | mg Bio/l je Zeitschritt | ... | albewk | ...|
| \f$ q_{Si, max} \f$ |  max. Si-Gehalt der Kieselalgenzelle | mg Si/mgBio | ... | qmx_sk | ...|

Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: silikat-vars.md; Codesource: silikat.f90; zurück: \ref lnk_silikat
