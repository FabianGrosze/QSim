Benthische Algen - Formelzeichen/Variablennamen {#lnk_albenth_vars} 
========================================

## Liste der Formelzeichen und Variablennamen, Benthische Algen:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
| ... | ... | ... | 0.398*exp(0.0465*Tempw(ior)) | fTChlC | ... |
| ... | Chla:C (benth.) Kieselalgen | ... | 21.5 | ChlCK | ... |
| ... | Chla:C (benth.) Grünalgen | ... | 21.5 | ChlCG | ... |
| ... | alt? | ... | ... | Cmatki | ... |
| ... | alt? | ... | ... | Cmatgr | ... |
| ... | Kieselalgenbiomasse repiriert? | ... | ... | Alberg | ... |
| ... | Grünalgenbiomasse repiriert? | ... | ... | Alberk | ... |
| ... | Wachstum Grünalgenbiomasse in mg/l? | ... | ... | Albewg | ... |
| ... | Wachstum Kieselalgenbiomasse in mg/l? | ... | ... | Albewk | ... |
| ... | Licht | ... | ... | OBFLI | ... |
| ... | absoluter Zuwachs benthischer Grünalgen (mit Resp) | ... | ... | Abemg2 | ... |
| ... | absoluter Zuwachs benthischer Kieselalgen (mit Resp) | ... | ... | Abekm2 | ... |
| ... | ... | ... | ... | Abegr | ... |
| ... | ... | ... | ... | Abeki | ... |
| ... | C-Anteil Kieselalgen | ... | 0.48 | Caki | ... |
| ... | C-Anteil Grünalgen | ... | 0.48 | Cagr | ... |
| ... | Grünalgenwachstumsrate | ... | ... | aggrow | ... |
| ... | Kieselalgenwachstumsrate nicht bei Resp | ... | ... | akgrow | ... |
| ... | Grünalgen nach Zeitschritt (mit Lichtfaktor) | ... | ... | abegrt | ... |
| ... | Kieselalgen nach Zeitschritt (mit Lichtfaktor) | ... | ... | abekit | ... |
| ... | Zuwachs Grünalgen (ohne Resp) | ... | ... | albewg(ior) | ... |
| ... | Zuwachs Kieselalgen (ohne Resp) | ... | ... | albewk(ior) | ... |
| ... | ... | ... | 1.7 | b1 | ... |
| ... | ... | ... | 0.187 | b2 | ... |
| ... | wachstumsabhängige Respiration | ... | 0.2 | frespL | ... |
| ... | Grundrespiration | ... | 0.085 | respG | ... |
| ... | Respiration Grünalgen | ... | respG + aggrow * frespL | abgre | ... |
| ... | Respiration Kieselalgen | ... | respG + akgrow * frespL | abkre | ... |
| ... | Respirationsverlust Grünalgen | ... | abegr*(1.-(exp(-abgre*tflie))) | alberg(ior) | ... |
| ... | Respirationsverlust Kieselalgen | ... | abeki*(1.-(exp(-abkre*tflie))) | alberk(ior) | ... |
| ... | Temperaturabh. Respiration Grünalgen | ... | alberg(ior)/(1.+exp(b1-b2*tempw(ior))) | alberg(ior) | ... |
| ... | Temperaturabh. Respiration Kieselalgen | ... | alberk(ior)/(1.+exp(b1-b2*tempw(ior))) | alberk(ior) | ... |
| ... | Globalstrahlung, wird in Licht umgerechnet | ... | ... | SCHWI | ... |
| ... | Zeitintervall | ... | ... | TFLIE | ... |
| ... | Wassertemperatur | ... | ... | TEMPW | ... |
| ... | Wassertiefe | ... | ... | TIEFE | ... |
| ... | mittlere Fließgeschwindigkeit | ... | ... | VMITT | ... |
| ... | Nitrat | ... | ... | VNO3 | ... |
| ... | Ammonium | ... | ... | VNH4 | ... |
| ... | gelöstes P | ... | ... | gelp | ... |
| ... | Abschnittslänge, *nicht verwendet* | ... | ... | elen | ... |
| ... | Fläche? | ... | ... | flae | ... |
| ... | Laufindex | ... | ... | ior | ... |
| ... | Laufindex | ... | ... | anze | ... |
| ... | max. Wachstum Grünalgen | ... | 2,03 | aggmax | ... |
| ... | ks N Grünalgen | ... | 0,028 | agksn | ... |
| ... | ks P Grünalgen | ... | 0,03 | agksp | ... |
| ... | Silizium | ... | ... | si | ... |
| ... | ks N Kieselalgen | ... | 0,02 | akksn | ... |
| ... | ks P Kieselalgen | ... | 0,03 | akksp | ... |
| ... | ks Si Kieselalgen | ... | 0,11 | akkssi | ... |
| ... | max. Wachstum Grünalgen | ... | 2.1 | akgmax | ... |
| ... | Zuwachs Kieselalgen (Grün) | mg/l | ... | Albewk(g) | b |
| ... | Respirationsverlust Kieselalgen (Grün) | mg/l | ... | Alberk(g) | b |
| ... | Alte Grünalgenbiomasse -> berechnete Grünalgenbiomasse | g/m²? | ... | abegm2 | ... |
| ... | Alte Kieselalgenbiomasse -> berechnete Kieselalgenbiomasse | g/m²? | ... | abekm2 | ... |
| ... | ... | ... | ... | vabfl | ... |
| ... | ... | ... | = albewg s.o. | cmatgr | ... |
| ... | ... | ... | = albewk s.o. | cmatki | ... |
| ... | Kieselalgen-Chlorophyll | ... | ... | akchl | ... |
| ... | Grünalgen-Chlorophyll | ... | ... | agchl | ... |
| ... | Extinktionskoeffizient | ... | ... | extk | ... |
| ... | Falls 0 keine Berechnung benth. Algen | ... | ... | ilang | ... |
| ... | Licht am Boden | ... | ... | algip1 | ... |
| ... | 1% Licht = Kompensationstiefe | ... | ... | Igrenz | ... |
| ... | Schichtdicke produktiver Schicht (Wassersäule) | ... | ... | KxI | ... |
| ... | Faktor für Lichtattenuation | ... | ... | Kx | ... |
| ... | (roPeri: Schichtdicke des Periphytons) | ... | ... | zPeriI | ... |
| ... |  benth. Algen ges. (= abegr + abeki), Dicke des Periphytonrasens | ... | ... | zPeri | ... |
| ... | Sättigungslichtstärke (Kiesel) aus Modell AQUATOX | ... | 147 | saetbk | ... |
| ... | Sättigungslichtstärke (Grün) aus Modell AQUATOX | ... | 176 | saetbg | ... |
| ... | Dichte des Periphytons | g/m3 | 1030000 | roPeri | (s. Uhlmann) |
| ... | C:Biomasse Grünalgen | ... | 0.48 | Cagr | ... |
| ... | C:Biomasse Kieselalgen | ... | 0.48 | Caki | ... |
| ... | ... | ... | 0.61519 | LNQ | ... |
| ... | Sättigungslichtstärke benth. Grünalgen | mueE/(m2*s) | 176 | Saetbg | s. Modell AQUATOX |
| ... | Sättigungslichtstärke benth. Kieselalgen | mueE/(m2*s) | 147 | saetbk | ... |
| ... | ... | ... | ... | ... | ... |

Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\note Das für planktische Kiesel- und Grünalgen in der Parameterliste 
eingegebene C:Chl Verhältnis sowie deren maximale Wachstumsrate bei 20° und, 
die ks-Werte für N und P werden auch für benthische Algen verwendet. Die übrigen 
physiologischen Parameter werden für benthische Algen derzeit im Code 
festgesetzt, auf die jeweiligen Belegungen wird in der Dokumentation jeweils 
hingewiesen.


\n\n

Textquelle: albenth-vars.md; Codesource: albenth.f90; zurück: \ref lnk_albenth





