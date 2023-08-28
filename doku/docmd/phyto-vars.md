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
| \f$\CChl\f$  | \f$\gCmgChl\f$ | -- | C:Chl-a-Verhältnis der Algenklasse *i* | *CChlaz* | interne Variable|
| \f$\CChld\f$ | \f$\gCmgChl\f$ | -- | C:Chl-a-Verhältnis dunkeladaptierter Algen der Klasse *i* | \ref akchl, \ref abchl, \ref agchl, \`CChl_Stern` | e (für 20 °C), interne Variable|
| \f$\aCChl{ki}\f$ | °C\f$^{-1}\f$ | -0.059 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Kieselalgen | *a1ki* | hart codierte Variable|
| \f$\aCChl{bl}\f$ | °C\f$^{-1}\f$ | -0.062 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Blaualgen | *a1bl*  | hart codierte Variable|
| \f$\aCChl{gr}\f$ | °C\f$^{-1}\f$ | -0.032 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Grünalgen | *a1gr*  | hart codierte Variable|
| Lichtabhängiges Wachstum ||||||
| \f$\mu_{A,ki}^{max}\f$ | d\f$^{-1}\f$ | 1.6 | Maximale Brutto-Wachstumsrate der Kieselalgen bei T_{ref} | \ref akgmax | e|
| \f$\mu_{A,bl}^{max}\f$ | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Blaualgen bei T_{ref} | \ref abgmax | e|
| \f$\mu_{A,gr}^{max}\f$ | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Grünalgen bei T_{ref} | \ref aggmax | e|
| \f$\mu_{A,i}\f$ | d\f$^{-1}\f$  | --  | Brutto-Wachstumsrate der Algenklasse *i* | *akgrow*, *abgrow*, *aggrow* | interne Variable|
| \f$P_{A,i}^{max}\f$ | d\f$^{-1}\f$  | -- | Maximale lichtabhängige Photosyntheserate der Algenklasse *i* (ohne Lichthemmung) | *PCmax* | interne Variable|
| \f$P_{A,i}\f$ | d\f$^{-1}\f$  | -- | Mittlerer lichtabhängige Photosyntheserate der Algenklasse *i* (ohne Lichthemmung) im Interval \f$[z, z+\dz]\f$ | *PCmit* | interne Variable|
| \f$$S_0$\f$  | \f$\calms\f$  | -- | Kurzwellige Netto-Globalstrahlung an der Gewässeroberfläche | \ref schwi | Ausgabevariable|
| \f$I_0\f$  | \f$\uEms\f$ | -- | Photosynthetisch aktive Einstrahlung (PAR) an der Gewässeroberfläche | *I0* | interne Variable|
| \f$\Izm\f$ | \f$\uEms\f$ | -- | Mittlere PAR im Tiefeninterval \f$[z, z+\dz]\f$ | *ic* | interne Variable|
| *H* | m | -- | Gesamtwassertiefe | \ref tiefe | Ausgabevariable|
| *z* | m | -- | Aktuelle Tiefe innerhalb der Wassersäule  | *z*  | interne Variable|
| \f$\dz\f$ | m | -- | Vertikale Verdriftungstrecke aufgrund von Vermischung | *dz*  | interne Variable|
| \f$\Dzm\f$ | \f$\mqs\f$ | -- | tiefengemittelte turbulente Diffusivität | *xmuet*  | interne Variable|
| \f$\tau_R\f$ | s  | 100 | Relaxationszeit der Algen | *tauad* | hart codierte Variable|
| \f$\ust\f$ | \f$ \ms \f$ | --  | Schubspannungsgeschwindigkeit  | *ust* | interne Variable|
| \f$\kappa\f$ | - | 0.4 | von-K\'arm\'an-Konstante  | -- | hart codierter Wert|
| \f$\epsilon\f$  | \f$\minv\f$ | --  | Lichtextinktionskoeffizient | \ref extk  | Ausgabevariable|
| \f$\epsm{SS}\f$ | \f$ \Lmmg \f$  | --  | Mittlerer Extinktionskoeffizient von mineralischem Schwebstoff und Zooplankton | *asmit* | interne Variable|
| \f$\epsm{Chl-a}\f$| \f$ \LmugChl \f$ | --  | Mittlerer Extinktionskoeffizient von Chl-a| *acmit* | interne Variable|
| \f$\epsm{W}\f$ | \f$\minv\f$ | --  | Mittlerer Extinktionskoeffizient von Wasser | *awmit* | interne Variable|
| \f$\epsm{H}\f$ | \f$\minv\f$ | --  | Mittlerer Extinktionskoeffizient von Huminstoffen  | *ahmit* | interne Variable|
| \f$\eps{SS}\f$ | \f$ \Lmmg \f$ | --  | Wellenlängenspezifische Extinktionskoeffizienten von mineralischem Schwebstoff und Zooplankton| \ref as | Eingelesen aus e_extnct.dat|
| \f$\epsc\f$ | \f$\LmugChl\f$ | --  | Wellenlängenspezifische Extinktionskoeffizienten von Chl-a der Algenklasse *i* | \ref ack}, \ref acb, \ref acg | Eingelesen aus e_extnct.dat|
| \f$\eps{W}\f$  | \f$\minv\f$ | --  | Wellenlängenspezifische Extinktionskoeffizienten von Wasser | \ref aw | Eingelesen aus e_extnct.dat|
| \f$\eps{H}\f$  | \f$\minv\f$ | 0.75, --| Wellenlängenspezifische Extinktionskoeffizienten von Huminstoffen | \ref alamda, \ref ah | Eingabeparamter (für \f$\lambda_0\f$), interne Variable|
| \f$\lambda_0\f$ | nm| 440 | Referenzwellenlänge für Berechnung von \f$\epsm{H}\f$ | *lamda0* | hart codierte Variable|
| \f$k_\lambda\f$ | \f$n\minv\f$| 0.016 | Koeffizient des Exponenten für Berechnung von \f$\epsm{H}\f$| *Slamda* | hart codierte Variable|
| Lichtadaption ||||||
| \f$\Ik\f$ | \f$\uEms\f$ | --  | Sättigungsintensität des Wachstums der Algenklasse *i*| *ikk*, *Ikb*, *Ikg*, \ref saettk, \ref saettb, \ref saettg | e (für 20\,°C), interne Variable|
| \f$\ak{ki}\f$  | - | 0.837 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{ki}\f$ | -- | hart codierter Wert|
| \f$\ak{bl}\f$  | - | 0.525 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{bl}\f$ | -- | hart codierter Wert|
| \f$\ak{gr}\f$  | - | 0.183 | Koeffizient der Temperaturabhängigkeit von \f$\Ik{gr}\f$ | -- | hart codierter Wert|
| \f$\bk{ki}\f$  | °C\f$^{-1}\f$ | 0.0089  | Koeffizient des Exponenten der Temperaturabhängigkeit von \f$\Ik{ki}\f$ | -- | hart codierter Wert|
| \f$\bk{bl}\f$  | °C\f$^{-1}\f$ | 0.0322  | Koeffizient des Exponenten der Temperaturabhängigkeit von \f$\Ik{bl}\f$ | -- | hart codierter Wert|
| \f$\bk{gr}\f$  | °C\f$^{-1}\f$ | 0.0848  | Koeffizient des Exponenten der Temperaturabhängigkeit von \f$ \Ik{gr} \f$ | -- | hart codierter Wert|
| \f$\rChlC\f$ | \f$\mgChlgCd\f$  | --  | Verhältnis von Chl-a-Synthese zu C-Aufnahme der Photosynthese der Algenklasse *i* | `roh_Chlz`  | interne Variable |
| \f$\alphaCChl\f$  | \f$\gCmqsmgChluE\f$ | --  | Chl-a-spezifischer Anfangsanstieg des lichtabhängigen Wachstums der Algenklasse *i* | `alpha_chl` | interne Variable |
| \f$\CA\f$ | \f$\text{mg\,#1\,L$^{-1}$}{\text{C}}\f$| -- | Konzentration des in der Algenklasse *i* gebundenen C | -- | --|
| \f$\mu_{A,i}^\text{Chl-a}\f$  | \f$\ugChlLd\f$ | --  | Chl-a-Produktionsrate der Algenklasse *i* | -- | --|
| Lichthemmung ||||||
| \f$\theta_{\text{D1},i}\f$  | - | 0--1| Relative Konzentration des D1-Proteins der Algenklasse *i* | 1 - \ref svhemk, 1 - \ref svhemb, 1 - \ref svhemb | Ausgabevariable|
| \f$\sigma_{PSII,0}\f$ | \f$\mquE\f$ | 1.5 | Absorptionsfläche des Photosystems II dunkeladaptierter Algen | *PSII0* | hart codierte Variable|
| \f$\sigma_{PSII,i}\f$ | \f$\mquE\f$ | --  | Absorptionsfläche des Photosystems II  | *PSII*  | interne Variable|
| \f$\kappa_{PSII}\f$ | - | 0.22| Exponent der Abhängigkeit von \f$ \sigma_{PSII,}{i} \f$ vom C:Chl-a-Verhältnis| --  | hart codierter Wert|
| \f$k_d\f$ | - | \f$1.04\cdot10^{-8}\f$ | Schädigungskonstante des D1-Proteins | *Kd0* | hart codierte Variable|
| \f$$k_r$\f$ | \f$\sinv \f$ | \f$4.5\cdot10^{-5}\f$  | Reparaturrate des D1-Proteins  | *Tau0* | hart codierte Variable|
| Nährstofflimitierung ||||||
| \f$f_{N\ddot{a}hr,i}\f$ | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse *i* | *F5* | interne Variable|
| \f$f_{\text{#1},i}{$X$}\f$ | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse *i* für Nährstoff $X$ (N, P, Si; Si nur für Kieselalgen) | *F51*, *F52*, *F53*  | interne Variable|
| \f$\Q \f$ | \f$\gXg{$X$}\f$  | --  | Zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P) | \ref q_nk, \ref q_pk, \ref q_nb, \ref q_pb, \ref q_ng, \ref q_pg | Ausgabevariable|
| \f$\Qmax{X}\f$ | \f$\gXg{$X$}\f$  | --  | Maximale zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P, Si)  | \ref qmx_nk, \ref qmx_pk, \ref qmx_sk, \ref qmx_nb, \ref qmx_pb, \ref qmx_ng, \ref qmx_pg | e|
| \f$\Qmin\f$ | \f$\gXg{$X$}\f$  | --  | Maximale zellinterne Nährstoffquota der Algenklasse *i* für Nährstoff $X$ (N, P, Si)  | \ref qmn_nk, \ref qmn_pk, \ref qmn_sk, \ref qmn_nb, \ref qmn_pb, \ref qmn_ng, \ref qmn_pg | e|
| \f$\khs{N}{i}\f$  | \f$\text{mg\,#1\,L$^{-1}$}{N}\f$ | 0.007 | N-Halbsättigungskonstante der Algenklasse *i* | \ref akksn, \ref abksn, \ref agksn | e|
| \f$\khs{P}{i}\f$  | \f$ \text{mg\,#1\,L$^{-1}$}{P} \f$ | 0.023 | P-Halbsättigungskonstante der Algenklasse *i* | \ref akksp, \ref abksp, \ref agksp | e|
| \f$\khs{Si}{ki}\f$ | \f$ \text{mg\,#1\,L$^{-1}$}{Si} \f$ | 0.08| Si-Halbsättigungskonstante der Kieselalgen| \ref akkssi| e|
| Temperaturabhängigkeit des Wachstums ||||||
| \f$f_{T,i}^\mu\f$ | - | 0--1| Temperaturfaktor des Wachstums der Algenklasse *i* | *FTA* | interne Variable|
| \f$ T \f$ | °C | --  | Wassertemperatur | \ref tempw | Ausgabevariable, Randwert|
| \f$T_{ref}\f$ | °C | 20  | Referenztemperatur | *Te0* | hart codierte Variable|
| \f$\Topt{ki}\f$ | °C | 20.3| Temperatur des optimalen Wachstums der Kieselalgen | \ref toptk | e|
| \f$\Topt{bl}\f$ | °C | 23.7/ 31.8 | Temperatur des optimalen Wachstums der Blaualgen (faden-/koloniebildend)  | *ToptB* | e|
| \f$\Topt{gr}\f$ | °C | 30.2| Temperatur des optimalen Wachstums der Grünalgen | \ref toptg | e|
| \f$\kTmu{ki}\f$ | °C\f$^{-1}\f$ | 0.0065  | Temperaturkoeffizient des Wachstums der Kieselalgen | *kTemp_Ki*  | e|
| \f$\kTmu{bl}\f$ | °C\f$^{-1}\f$ | 0.0069/ 0.0081  | Temperaturkoeffizient des Wachstums der Blaualgen (faden-/koloniebildend) | *ktemp_bl*  | e| 
| \f$\kTmu{gr}\f$ | °C\f$^{-1}\f$ | 0.0041  | Temperaturkoeffizient des Wachstums der Grünalgen  | *ktemp_gr*  | e|
| Respiration ||||||
| \f$\resp\f$ | d\f$^{-1}\f$ | --  | Respirationsrate der Algenklasse *i* | *akres*, *abres*, *agres*, \ref akraus, *abraus*, *agraus* | interne Variable, Ausgabevariable|
| \f$\respd{ki}\f$  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Kieselalgen bei T_{ref} | \ref akremi| e|
| \f$\respd{bl}\f$  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Blaualgen bei T_{ref} | \ref abremi| e|
| \f$\respd{gr}\f$  | d\f$^{-1}\f$ | 0.11| Grundrespirationsrate dunkeladaptierter Grünalgen bei T_{ref} | \ref agremi| e|
| \f$\kTresp{ki}\f$ | °C\f$^{-1}\f$ | 0.07| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Kieselalgen | *kTresp* | hart codierte Variable|
| \f$\kTresp{bl}\f$ | °C\f$^{-1}\f$ | 0.09| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Blaualgen | *kTresp* | hart codierte Variable|
| \f$\kTresp{bl}\f$ | °C\f$^{-1}\f$ | 0.058 | Koeffizient der Temperaturabhängigkeit der Grundrespiration der Grünalgen | *kTresp* | hart codierte Variable|
| \f$\respg{ki}\f$  | - | 0.2 | Respirationskosten des Wachstums der Kieselalgenklasse| \ref frmuke, *frespg*  | e|
| \f$\respg{bl}\f$  | - | 0.2 | Respirationskosten des Wachstums der Blaualgenklasse  | \ref frmube, *frespg*  | e|
| \f$\respg{gr}\f$  | - | 0.067 | Respirationskosten des Wachstums der Grünalgenklasse  | \ref frmuge, *frespg*  | e|
| Mortalität ||||||
| \f$\mor\f$ | d\f$^{-1}\f$ | --  | Gesamtmortalitätsrate der Algenklasse *i* | *akmor*,  *abmor*,  *agmor* | interne Variable|
| \f$\morX{0}\f$ | d\f$^{-1}\f$ | 0.02| Grundmortalitätsrate der Algenklasse *i*  | *akmomi*,  *abmomi*,  *agmomi* | hart codierte Variable|
| \f$\morX{\text{NP}}\f$| d\f$^{-1}\f$  | 0.8 | Maximale nährstoffbedingte Mortalitätsrate der Algenklasse *i*| *akmoma*,  *abmoma*,  *agmoma* | hart codierte Variable|
| \f$\fmor\f$ | - | --  | Skalierungsfaktor der nährstoffbedingten Mortalitätsrate der Algenklasse *i* | -- | --|
| \f$\fmorX{0}\f$ | d\f$^{-1}\f$ | 0.05| Minimaler Skalierungskoeffizient der nährstoffbedingten Mortalitätsrate der Algenklasse *i* | *fmor0* | hart codierte Variable|
| Sedimentation ||||||
| \f$\fsed{ki}, \fsed{gr}\f$ | - | 0.5 | Sedimentierbarer Anteil der Kiesel- bzw. Grünalgen  | \ref askie, \ref asgre | e|
| \f$\fsed{bl}\f$ | - | 0 | Sedimentierbarer Anteil der Blaualgen | \ref asble| e|
| \f$\Xsed{a}\f$ | - | 0--1| Sedimentierter Anteil der Algenklasse *i*  | *Oc* | interne Variable|
| \f$\Xsed{\alpha}\f$| -| --  | Empirischer Koeffizient der Berechnung der Sedimentation in Abhängigkeit der Sinkgeschwindigkeit der Algenklasse *i* | *ased* | interne Variable|
| \f$\Xsed{\beta}\f$| - | 2.7 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | *bsed* | hart codierte Variable|
| \f$\Xsed{\gamma}\f$| -| 0.5 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | *prop* | hart codierte Variable|
| \f$\Xsed{q}\f$ | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | *qsgr* | interne Variable|
| \f$\qsed\f$ | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse *i* | *qssed* | interne Variable|
| \f$\w{i}\f$ | \f$\ms\f$ | --  | Mittlere Sinkgeschwindigkeit der Algenklasse *i* in Abhängigkeit vom mittleren Zellvolumen  | *wsAlg* | interne Variable|
| \f$\w{0}\f$ | \f$\ms\f$ | --  | Sinkgeschwindigkeit der Algenklasse *i* in Abhängigkeit von der Schubspannnungsgeschwindigkeit | *wsgr* | interne Variable|
| \f$\wEff\f$ | \f$\ms\f$ | --  | Effektive Sinkgeschwindigkeit der Algenklasse *i* | *ws* | interne Variable|
| \f$\V{ki}\f$ | \f$\umc\f$  | 1400| Mittleres Zellvolumen der Kieselalgen | *ZellV* | hart codierte Variable|
| \f$\V{bl}\f$ | \f$\umc\f$  | 1000| Mittleres Zellvolumen der Blaualgen| *ZellV* | hart codierte Variable|
| \f$\V{gr}\f$ | \f$\umc\f$  | 300 | Mittleres Zellvolumen der Grünalgen| *ZellV* | hart codierte Variable|




Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: phyto-vars.md; Codesource: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück: \ref lnk_phytoplankton
