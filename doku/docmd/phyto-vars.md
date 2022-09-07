Phytoplankton - Formelzeichen/Variablennamen {#lnk_phyto_vars} 
========================================

## Liste der Formelzeichen und Variablennamen, Benthische Algen:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
| ...  | ... | ... | ... | ... | ... |



| Symbol | Einheit | Wert | Kurzbeschreibung | Codebez. | Variablentyp |
| ------ | ------- | ---- | ---------------- | -------- | ------------ |
| Algenbiomasse und Chlorophyll-a ||||||
| \f$A_{ki}\f$  | mg L\f$^{-1}\f$ | -- | Biomasse der Kieselalgen | \ref aki | x |
| \f$A_{bl}\f$  | mg L\f$^{-1}\f$ | -- | Biomasse der Blaualgen | \ref abl | x |
| \f$A_{gr}\f$  | mg L\f$^{-1}\f$ | -- | Biomasse der Grünalgen | \ref agr | x |
| \f$\CBio{ki}\f$ | \f$\gXg{C}\f$ | 0.48 | C:Biomasse-Verhältnis der Kieselalgen | \ref Caki | v |
| \f$\CBio{bl}\f$ | \f$\gXg{C}\f$ | 0.48 | C:Biomasse-Verhältnis der Blaualgen | \ref Cabl | v |
| \f$\CBio{gr}\f$ | \f$\gXg{C}\f$ | 0.48 | C:Biomasse-Verhältnis der Grünalgen | \ref Cagr | v |
| Chl-a  | \f$\ugChlL\f$ | -- | Gesamtkonzentration des Chl-a | \ref chla | Ausgabevariable, Randwert|
| \f$\Chl\f$ | \f$\ugChlL\f$ | -- | Konzentration des Chl-a der Algenklasse *i* | \ref chlaki, \ref chlabl, \ref chlagr | Ausgabevariable|
| \f$\aChl{ki}\f$ | - | --  | Anteil der Kieselalgen am Gesamt-Chl-a | \ref vkigr | Ausgabevariable, Randwert|
| \f$\aChl{bl}\f$ | - | --  | Anteil der Blaualgen am Gesamt-Chl-a | \ref antbl | Ausgabevariable, Randwert|
| \f$\CChl\f$  | \f$\gCmgChl\f$ | -- | C:Chl-a-Verhältnis der Algenklasse *i* | \ref CChlaz | interne Variable|
| \f$\CChld\f$ | \f$\gCmgChl\f$ | -- | C:Chl-a-Verhältnis dunkeladaptierter Algen der Klasse *i* | \ref akchl, \ref abchl, \ ref agchl, \mbtt{CChl\_Stern} | e (für 20 °C), interne Variable|
| \f$\aCChl{ki}\f$ | °C\f$^{-1}\f$ | -0.059 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Kieselalgen | \ref a1ki | hart codierte Variable|
| \f$\aCChl{bl}\f$ | °C\f$^{-1}\f$ | -0.062 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Blaualgen | \ref a1bl  | hart codierte Variable|
| \f$\aCChl{gr}\f$ | °C\f$^{-1}\f$ | -0.032 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Grünalgen | \ref a1gr  | hart codierte Variable|
| Lichtabhängiges Wachstum ||||||
| \f$\mu_{A,ki}^{max}\f$ | d\f$^{-1}\f$ | 1.6 | Maximale Brutto-Wachstumsrate der Kieselalgen bei \Tref | \ref akgmax | e|
| \f$\mu_{A,bl}^{max}\f$ | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Blaualgen bei \Tref | \ref abgmax | e|
| \f$\mu_{A,gr}^{max}\f$ | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Grünalgen bei \Tre | \ref aggmax | e|
| \f$\mu_{A,i}\f$ | d\f$^{-1}\f$  | --  | Brutto-Wachstumsrate der Algenklasse *i* | \ref akgrow, \ref abgrow, \ref aggrow | interne Variable|
| \f$P_{A,i}^{max}\f$ | d\f$^{-1}\f$  | -- | Maximale lichtabhängige Photosyntheserate der Algenklasse *i* (ohne Lichthemmung) | \ref PCmax | interne Variable|
| \f$P_{A,i}\f$ | d\f$^{-1}\f$  | -- | Mittlerer lichtabhängige Photosyntheserate der Algenklasse *i* (ohne Lichthemmung) im Interval $[z, z+\dz]$ | \ref PCmit | interne Variable|
| \f$$S_0$\f$  | \f$\calms\f$  | -- | Kurzwellige Netto-Globalstrahlung an der Gewässeroberfläche | \ref schwi | Ausgabevariable|
| \f$I_0\f$  | \f$\uEms\f$ | -- | Photosynthetisch aktive Einstrahlung (PAR) an der Gewässeroberfläche | \ref I0 | interne Variable|
| \f$\Izm\f$ | \f$\uEms\f$ | -- | Mittlere PAR im Tiefeninterval \f$[z, z+\dz]\f$ | \ref Ic | interne Variable|
| *H* | m | -- | Gesamtwassertiefe | \ref TIEFE | Ausgabevariable|
| *z* | m | -- | Aktuelle Tiefe innerhalb der Wassersäule  | \ref z  | interne Variable|
| \f$\dz\f$ | m | -- | Vertikale Verdriftungstrecke aufgrund von Vermischung | \ref dz  | interne Variable|
| \f$\Dzm\f$ | \f$\mqs\f$ | -- | tiefengemittelte turbulente Diffusivität | \ref xmuet  | interne Variable|
| \f$\tau_R\f$ | s  | 100 | Relaxationszeit der Algen | \ref tauad | hart codierte Variable|
| \f$\ust\f$ | \ms | --  | Schubspannungsgeschwindigkeit  | \ref ust | interne Variable|
| \f$\kappa\f$ | - | 0.4 | von-K\'arm\'an-Konstante  | -- | hart codierter Wert|
| \f$\epsilon\f$  | \f$\minv\f$ | --  | Lichtextinktionskoeffizient | \ref extk  | Ausgabevariable|
| \f$\epsm{SS}\f$ | \Lmmg | --  | Mittlerer Extinktionskoeffizient von mineralischem Schwebstoff und Zooplankton | \ref asmit | interne Variable|
| \f$\epsm{Chl-a}\f$| \LmugChl | --  | Mittlerer Extinktionskoeffizient von Chl-a| \ref acmit | interne Variable|
| \f$\epsm{W}\f$ | \f$\minv\f$ | --  | Mittlerer Extinktionskoeffizient von Wasser | \ref awmit | interne Variable|
| \f$\epsm{H}\f$ | \f$\minv\f$ | --  | Mittlerer Extinktionskoeffizient von Huminstoffen  | \ref ahmit | interne Variable|
| \f$\eps{SS}\f$ | \Lmmg | --  | Wellenlängenspezifische Extinktionskoeffizienten von mineralischem Schwebstoff und Zooplankton| \ref as | Eingelesen aus e_extnct.dat|
| \f$\epsc\f$ | \f$\LmugChl\f$ | --  | Wellenlängenspezifische Extinktionskoeffizienten von Chl-a der Algenklasse *i* | \ref ack}, \ref acb, \ref acg | Eingelesen aus e_extnct.dat|
| \f$\eps{W}\f$  | \f$\minv\f$ | --  | Wellenlängenspezifische Extinktionskoeffizienten von Wasser | \ref aw | Eingelesen aus e_extnct.dat|
| \f$\eps{H}\f$  | \f$\minv\f$ | 0.75, --| Wellenlängenspezifische Extinktionskoeffizienten von Huminstoffen | \ref alamda, \ref ah | Eingabeparamter (für \f$\lambda_0\f$), interne Variable|
| \f$\lambda_0\f$ | nm| 440 | Referenzwellenlänge für Berechnung von \f$\epsm{H}\f$ | \f$\ref lamda0\f$| hart codierte Variable|
| \f$k_\lambda\f$ | \f$n\minv\f$| 0.016 | Koeffizient des Exponenten für Berechnung von \f$\epsm{H}\f$| \f$\ref Slamda\f$| hart codierte Variable|
| Lichtadaption ||||||
| \f$\Ik\f$ | \f$\uEms\f$ | --  | Sättigungsintensität des Wachstums der Algenklasse *i*| \ref Ikk, \ref Ikb, \ref Ikg, \ref Saettk, \ref Saettb, \ref Saettg | e (für 20\,°C), interne Variable|
| \f$\ak{ki}\f$  | - | 0.837 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{ki}\f$ | -- | hart codierter Wert|
| \f$\ak{bl}\f$  | - | 0.525 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{bl}\f$ | -- | hart codierter Wert|
| \f$\ak{gr}\f$  | - | 0.183 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{gr}\f$ | -- | hart codierter Wert|
| \f$\bk{ki}\f$  | °C\f$^{-1}\f$ | 0.0089  | Koeffizient des Exponenten der Temperaturabhängigkeit von \f$\Ik{ki}\f$ | -- | hart codierter Wert|
| \f$\bk{bl}\f$  | °C\f$^{-1}\f$ | 0.0322  | Koeffizient des Exponenten der Temperaturabhängigkeit von \f$\Ik{bl}\f$ | -- | hart codierter Wert|
| \f$\bk{gr}\f$  | °C\f$^{-1}\f$ | 0.0848  | Koeffizient des Exponenten der Temperaturabhängigkeit von \Ik{gr} | -- | hart codierter Wert|
| \f$\rChlC\f$ | \f$\mgChlgCd\f$  | --  | Verhältnis von Chl-a-Synthese zu C-Aufnahme der Photosynthese der Algenklasse *i* | \f$\mbtt{roh\_Chlz} \f$  | interne Variable|
| \f$\alphaCChl\f$  | \f$\gCmqsmgChluE\f$ | --  | Chl-a-spezifischer Anfangsanstieg des lichtabhängigen Wachstums der Algenklasse *i* | \f$\mbtt{alpha\_chl}\f$ | interne Variable|
| \f$\CA\f$ | \f$\text{mg\,#1\,L$^{-1}$}{\text{C}}\f$| -- | Konzentration des in der Algenklasse *i* gebundenen C | -- | --|
| \f$\mu_{A,i}^\text{Chl-a}\f$  | \f$\ugChlLd\f$ | --  | Chl-a-Produktionsrate der Algenklasse *i* | -- | --|
| Lichthemmung ||||||
| \f$\theta_{\text{D1},i}\f$  | - | 0--1| Relative Konzentration des D1-Proteins der Algenklasse *i* | 1 - \ref svhemk, 1 - \ref svhemb, 1 - \ref svhemb | Ausgabevariable|
| \f$\sigma_{PSII,0}\f$ | \f$\mquE\f$ | 1.5 | Absorptionsfläche des Photosystems II dunkeladaptierter Algen | \ref PSII0 | hart codierte Variable|
| \f$\sigma_{PSII,i}\f$ | \f$\mquE\f$ | --  | Absorptionsfläche des Photosystems II  | \ref PSII  | interne Variable|
| \f$\kappa_{PSII}\f$ | - | 0.22| Exponent der Abhängigkeit von \sigma_{PSII,#1}{i} vom C:Chl-a-Verhältnis| --  | hart codierter Wert|
| \f$k_d\f$ | - | \f$1.04\cdot10^{-8}\f$ | Schädigungskonstante des D1-Proteins | \ref Kd0 | hart codierte Variable|
| \f$$k_r$\f$ | \f$\sinv \f$ | \f$4.5\cdot10^{-5}\f$  | Reparaturrate des D1-Proteins  | \ref Tau0 | hart codierte Variable|
| Nährstofflimitierung ||||||
| \f$f_{N\ddot{a}hr,i}\f$ | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse *i* | \ref F5 | interne Variable|
| \f$f_{\text{#1},i}{$X$}\f$ | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse *i* für Nährstoff $X$ (N, P, Si; Si nur für Kieselalgen) | \ref F51, \ref F52, \ref F53  | interne Variable|
| \f$\Q \f$ | \f$\gXg{$X$}\f$  | --  | Zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P) | \mbtt{Q\_NK}, \mbtt{Q\_PK}, \mbtt{Q\_NB}, \mbtt{Q\_PB}, \mbtt{Q\_NG}, \mbtt{Q\_PG} | Ausgabevariable|
| \f$\Qmax{X}\f$ | \f$\gXg{$X$}\f$  | --  | Maximale zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P, Si)  | \mbtt{Qmx\_NK}, \mbtt{Qmx\_PK}, \mbtt{Qmx\_SK}, \mbtt{Qmx\_NB}, \mbtt{Qmx\_PB}, \mbtt{Qmx\_NG}, \mbtt{Qmx\_PG} | e|
| \f$\Qmin\f$ | \f$\gXg{$X$}\f$  | --  | Maximale zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P, Si)  | \mbtt{Qmn\_NK}, \mbtt{Qmn\_PK}, \mbtt{Qmn\_SK}, \mbtt{Qmn\_NB}, \mbtt{Qmn\_PB}, \mbtt{Qmn\_NG}, \mbtt{Qmn\_PG} | e|
| \f$\khs{N}{i}\f$  | \f$\text{mg\,#1\,L$^{-1}$}{N}\f$ | 0.007 | N-Halbsättigungskonstante der Algenklasse *i* | \ref akksN, \ref abksN, \ref agksN | e|
| \f$\khs{P}{i}\f$  | \text{mg\,#1\,L$^{-1}$}{P} | 0.023 | P-Halbsättigungskonstante der Algenklasse *i* | \f$\ref akksP, \ref abksP, \ref agksP\f$ | e|
| \f$\khs{Si}{ki}\f$ | \text{mg\,#1\,L$^{-1}$}{Si}  | 0.08| Si-Halbsättigungskonstante der Kieselalgen| \ref akksSi| e|
| Temperaturabhängigkeit des Wachstums ||||||
| \f$f_{T,i}^\mu\f$ | - | 0--1| Temperaturfaktor des Wachstums der Algenklasse *i* | \ref FTA | interne Variable|
| *T* | °C | --  | Wassertemperatur | \ref tempw | Ausgabevariable, Randwert|
| \f$\Tref\f$ | °C | 20  | Referenztemperatur | \ref Te0 | hart codierte Variable|
| \f$\Topt{ki}\f$ | °C | 20.3| Temperatur des optimalen Wachstums der Kieselalgen | \ref ToptK | e|
| \f$\Topt{bl}\f$ | °C | 23.7/ 31.8 | Temperatur des optimalen Wachstums der Blaualgen (faden-/koloniebildend)  | \ref ToptB | e|
| \f$\Topt{gr}\f$ | °C | 30.2| Temperatur des optimalen Wachstums der Grünalgen | \ref ToptG | e|
| \f$\kTmu{ki}\f$ | °C\f$^{-1}\f$ | 0.0065  | Temperaturkoeffizient des Wachstums der Kieselalgen | \ref kTemp\_Ki  | e|
| \f$\kTmu{bl}\f$ | °C\f$^{-1}\f$ | 0.0069/ 0.0081  | Temperaturkoeffizient des Wachstums der Blaualgen (faden-/koloniebildend) | \ref kTemp\_Bl  | e| 
| \f$\kTmu{gr}\f$ | °C\f$^{-1}\f$ | 0.0041  | Temperaturkoeffizient des Wachstums der Grünalgen  | \f$\mbtt{kTemp\_Gr}\f$  | e|
| Respiration ||||||
| \f$\resp\f$ | d\f$^{-1}\f$ | --  | Respirationsrate der Algenklasse *i* | \ref akres, \ref abres, \ref agres, \ref akraus, \ref abraus, \ref abraus | interne Variable, Ausgabevariable|
| \f$\respd{ki}\f$  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Kieselalgen bei \Tref | \ref akremi| e|
| \f$\respd{bl}\f$  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Blaualgen bei \Tref | \ref abremi| e|
| \f$\respd{gr}\f$  | d\f$^{-1}\f$ | 0.11| Grundrespirationsrate dunkeladaptierter Grünalgen bei \Tref | \ref agremi| e|
| \f$\kTresp{ki}\f$ | °C\f$^{-1}\f$ | 0.07| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Kieselalgen | \ref kTresp| hart codierte Variable|
| \f$\kTresp{bl}\f$ | °C\f$^{-1}\f$ | 0.09| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Blaualgen | \ref kTresp| hart codierte Variable|
| \f$\kTresp{bl}\f$ | °C\f$^{-1}\f$ | 0.058 | Koeffizient der Temperaturabhängigkeit der Grundrespiration der Grünalgen | \ref kTresp| hart codierte Variable|
| \f$\respg{ki}\f$  | - | 0.2 | Respirationskosten des Wachstums der Kieselalgenklasse| \ref frmuke}, \ref frespg  | e|
| \f$\respg{bl}\f$  | - | 0.2 | Respirationskosten des Wachstums der Blaualgenklasse  | \ref frmube}, \ref frespg  | e|
| \f$\respg{gr}\f$  | - | 0.067 | Respirationskosten des Wachstums der Grünalgenklasse  | \ref frmuge}, \ref frespg  | e|
| Mortalität ||||||
| \f$\mor\f$ | d\f$^{-1}\f$ | --  | Gesamtmortalitätsrate der Algenklasse *i* | \ref akmor,  \ref abmor,  \ref agmor | interne Variable|
| \f$\morX{0}\f$ | d\f$^{-1}\f$ | 0.02| Grundmortalitätsrate der Algenklasse *i*  | \ref akmomi,  \ref abmomi,  \ref agmomi | hart codierte Variable|
| \f$\morX{\text{NP}}\f$| d\f$^{-1}\f$  | 0.8 | Maximale nährstoffbedingte Mortalitätsrate der Algenklasse *i*| \ref akmoma,  \ref abmoma,  \ref agmoma | hart codierte Variable|
| \f$\fmor\f$ | - | --  | Skalierungsfaktor der nährstoffbedingten Mortalitätsrate der Algenklasse *i* | -- | --|
| \f$\fmorX{0}\f$ | d\f$^{-1}\f$ | 0.05| Minimaler Skalierungskoeffizient der nährstoffbedingten Mortalitätsrate der Algenklasse *i* | \ref fmor0 | hart codierte Variable|
| Sedimentation ||||||
| \f$\fsed{ki}, \fsed{gr}\f$ | - | 0.5 | Sedimentierbarer Anteil der Kiesel- bzw. Grünalgen  | \ref askie, \ref asgre | e|
| \f$\fsed{bl}\f$ | - | 0 | Sedimentierbarer Anteil der Blaualgen | \ref asble| e|
| \f$\Xsed{a}\f$ | - | 0--1| Sedimentierter Anteil der Algenklasse *i*  | \ref Oc | interne Variable|
| \f$\Xsed{\alpha}\f$| -| --  | Empirischer Koeffizient der Berechnung der Sedimentation in Abhängigkeit der Sinkgeschwindigkeit der Algenklasse *i* | \ref ased | interne Variable|
| \f$\Xsed{\beta}\f$| - | 2.7 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | \ref bsed | hart codierte Variable|
| \f$\Xsed{\gamma}\f$| -| 0.5 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | \ref prop | hart codierte Variable|
| \f$\Xsed{q}\f$ | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | \ref qsgr | interne Variable|
| \f$\qsed\f$ | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | \ref qssed | interne Variable|
| \f$\w{i}\f$ | \f$\ms\f$ | --  | Mittlere Sinkgeschwindigkeit der Algenklasse *i* in Abhängigkeit vom mittleren Zellvolumen  | \ref wsAlg | interne Variable|
| \f$\w{0}\f$ | \f$\ms\f$ | --  | Sinkgeschwindigkeit der Algenklasse *i* in Abhängigkeit von der Schubspannnungsgeschwindigkeit | \ref wsgr | interne Variable|
| \f$\wEff\f$ | \f$\ms\f$ | --  | Effektive Sinkgeschwindigkeit der Algenklasse *i* | \ref ws | interne Variable|
| \f$\V{ki}\f$ | \f$\umc\f$  | 1400| Mittleres Zellvolumen der Kieselalgen | \ref ZellV | hart codierte Variable|
| \f$\V{bl}\f$ | \f$\umc\f$  | 1000| Mittleres Zellvolumen der Blaualgen| \ref ZellV | hart codierte Variable|
| \f$\V{gr}\f$ | \f$\umc\f$  | 300 | Mittleres Zellvolumen der Grünalgen| \ref ZellV | hart codierte Variable|




Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: phyto-vars.md; Codesource: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück: \ref lnk_phytoplankton
