Phytoplankton - Formelzeichen/Variablennamen {#lnk_phyto_vars} 
========================================

## Liste der Formelzeichen und Variablennamen, Benthische Algen:(Stand QSim xx.yy) ##

| Formelzeichen/ \n Variablen-Name | Bedeutung | Einheit | Wert | Variablennamen \n Quellcode | Herkunft | 
|----------------|------------|--------------|---------|---------|---------|
| ...  | ... | ... | ... | ... | ... |


<!-- debugging 

| Symbol | Einheit | Wert | Kurzbeschreibung | Codebez. | Variablentyp |
| ------ | ------- | ---- | ---------------- | -------- | ------------ |
| Algenbiomasse und Chlorophyll-a |
| $A_{ki}$  | mg L\f$^{-1}\f$ | -- | Biomasse der Kieselalgen | \mbtt{aki} | x |
| $A_{bl}$  | mg L\f$^{-1}\f$ | -- | Biomasse der Blaualgen | \mbtt{abl} | x |
| $A_{gr}$  | mg L\f$^{-1}\f$ | -- | Biomasse der Grünalgen | \mbtt{agr} | x |
| \CBio{ki} | \gXg{C} | 0.48 | C:Biomasse-Verhältnis der Kieselalgen | \mbtt{Caki} | v |
| \CBio{bl} | \gXg{C} | 0.48 | C:Biomasse-Verhältnis der Blaualgen | \mbtt{Cabl} | v |
| \CBio{gr} | \gXg{C} | 0.48 | C:Biomasse-Verhältnis der Grünalgen | \mbtt{Cagr} | v |
| Chl-a  | \ugChlL | -- | Gesamtkonzentration des Chl-a | \mbtt{chla} | Ausgabevariable, Randwert|
| \Chl | \ugChlL | -- | Konzentration des Chl-a der Algenklasse $i$ | \mbtt{chlaki}, \mbtt{chlabl}, \mbtt{chlagr} | Ausgabevariable|
| \aChl{ki} | - | --  | Anteil der Kieselalgen am Gesamt-Chl-a | \mbtt{vkigr} | Ausgabevariable, Randwert|
| \aChl{bl} | - | --  | Anteil der Blaualgen am Gesamt-Chl-a | \mbtt{antbl} | Ausgabevariable, Randwert|
| \CChl  | \gCmgChl | -- | C:Chl-a-Verhältnis der Algenklasse $i$ | \mbtt{CChlaz} | interne Variable|
| \CChld | \gCmgChl | -- | C:Chl-a-Verhältnis dunkeladaptierter Algen der Klasse $i$ | \mbtt{akchl}, \mbtt{abchl}, \mbtt{agchl}, \mbtt{CChl\_Stern} | e (für 20\,°C), interne Variable|
| \aCChl{ki} | \$^\circ\$C\$^{-1}\$ | -0.059 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Kieselalgen | \mbtt{a1ki} | hart codierte Variable|
| \aCChl{bl} | \$^\circ\$C\$^{-1}\$ | -0.062 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Blaualgen | \mbtt{a1bl}  | hart codierte Variable|
| \aCChl{gr} | \$^\circ\$C\$^{-1}\$ | -0.032 | Koeffizient der Temperaturabhängigkeit des C:Chl-a-Verhältnisses der Grünalgen | \mbtt{a1gr}  | hart codierte Variable|
| \mu_{A,#1}^{max}{ki} | d\f$^{-1}\f$ | 1.6 | Maximale Brutto-Wachstumsrate der Kieselalgen bei \Tref | \mbtt{akgmax} | e|
| \mu_{A,#1}^{max}{bl} | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Blaualgen bei \Tref | \mbtt{abgmax} | e|
| \mu_{A,#1}^{max}{gr} | d\f$^{-1}\f$ | 1.2 | Maximale Brutto-Wachstumsrate der Grünalgen bei \Tre | \mbtt{aggmax} | e|
| \mu_{A,i} | d\f$^{-1}\f$  | --  | Brutto-Wachstumsrate der Algenklasse $i$ | \mbtt{akgrow}, \mbtt{abgrow}, \mbtt{aggrow} | interne Variable|
| P_{A,i}^{max} | d\f$^{-1}\f$  | -- | Maximale lichtabhängige Photosyntheserate der Algenklasse $i$ (ohne Lichthemmung) | \mbtt{PCmax} | interne Variable|
| P_{A,i} | d\f$^{-1}\f$  | -- | Mittlerer lichtabhängige Photosyntheserate der Algenklasse $i$ (ohne Lichthemmung) im Interval $[z, z+\dz]$ | \mbtt{PCmit} | interne Variable|
| $S_0$  | \calms  | -- | Kurzwellige Netto-Globalstrahlung an der Gewässeroberfläche | \mbtt{schwi} | Ausgabevariable|
| $I_0$  | \uEms | -- | Photosynthetisch aktive Einstrahlung (PAR) an der Gewässeroberfläche | \mbtt{I0} | interne Variable|
| \Izm | \uEms | -- | Mittlere PAR im Tiefeninterval $[z, z+\dz]$ | \mbtt{Ic} | interne Variable|
| $H$ | m | -- | Gesamtwassertiefe | \mbtt{TIEFE} | Ausgabevariable|
| $z$ | m | -- | Aktuelle Tiefe innerhalb der Wassersäule  | \mbtt{z}  | interne Variable|
| \dz | m | -- | Vertikale Verdriftungstrecke aufgrund von Vermischung | \mbtt{dz}  | interne Variable|
| \Dzm | \mqs | -- | tiefengemittelte turbulente Diffusivität | \mbtt{xmuet}  | interne Variable|
| $\tau_R$ | s  | 100 | Relaxationszeit der Algen | \mbtt{tauad} | hart codierte Variable|
| \ust | \ms | --  | Schubspannungsgeschwindigkeit  | \mbtt{ust} | interne Variable|
| $\kappa$ | - | 0.4 | von-K\'arm\'an-Konstante  | -- | hart codierter Wert|
| $\epsilon$  | \minv | --  | Lichtextinktionskoeffizient | \mbtt{extk}  | Ausgabevariable|
| \epsm{SS} | \Lmmg | --  | Mittlerer Extinktionskoeffizient von mineralischem Schwebstoff und Zooplankton | \mbtt{asmit} | interne Variable|
| \epsm{Chl-a}| \LmugChl | --  | Mittlerer Extinktionskoeffizient von Chl-a| \mbtt{acmit} | interne Variable|
| \epsm{W} | \minv | --  | Mittlerer Extinktionskoeffizient von Wasser | \ref awmit | interne Variable|
| \epsm{H} | \minv | --  | Mittlerer Extinktionskoeffizient von Huminstoffen  | \mbtt{ahmit} | interne Variable|
| \eps{SS} | \Lmmg | --  | Wellenlängenspezifische Extinktionskoeffizienten von mineralischem Schwebstoff und Zooplankton| \mbtt{as} | Eingelesen aus \mbtt{e\_extnct.dat}|
| \epsc | \LmugChl | --  | Wellenlängenspezifische Extinktionskoeffizienten von Chl-a der Algenklasse $i$ | \mbtt{ack}, \mbtt{acb}, \mbtt{acg} | Eingelesen aus \mbtt{e\_extnct.dat}|
| \eps{W}  | \minv | --  | Wellenlängenspezifische Extinktionskoeffizienten von Wasser | \mbtt{aw} | Eingelesen aus \mbtt{e\_extnct.dat}|
| \eps{H}  | \minv | 0.75, --| Wellenlängenspezifische Extinktionskoeffizienten von Huminstoffen | \mbtt{alamda}, \mbtt{ah} | Eingabeparamter (für $\lambda_0$), interne Variable|
| $\lambda_0$ | nm| 440 | Referenzwellenlänge für Berechnung von \epsm{H} | \mbtt{lamda0}| hart codierte Variable|
| $k_\lambda$ | n\minv| 0.016 | Koeffizient des Exponenten für Berechnung von \epsm{H}| \mbtt{Slamda}| hart codierte Variable|
| \Ik | \uEms | --  | Sättigungsintensität des Wachstums der Algenklasse $i$| \mbtt{Ikk}, \mbtt{Ikb}, \mbtt{Ikg}, \mbtt{Saettk}, \mbtt{Saettb}, \mbtt{Saettg}| e (für 20\,°C), interne Variable|
| \ak{ki}  | - | 0.837 | Koeffizient der Temperaturabhängigkeit von \Ik{ki} | -- | hart codierter Wert|
| \ak{bl}  | - | 0.525 | Koeffizient der Temperaturabhängigkeit von \Ik{bl} | -- | hart codierter Wert|
| \ak{gr}  | - | 0.183 | Koeffizient der Temperaturabhängigkeit von \Ik{gr} | -- | hart codierter Wert|
| \bk{ki}  | \$^\circ\$C\$^{-1}\$ | 0.0089  | Koeffizient des Exponenten der Temperaturabhängigkeit von \Ik{ki} | -- | hart codierter Wert|
| \bk{bl}  | \$^\circ\$C\$^{-1}\$ | 0.0322  | Koeffizient des Exponenten der Temperaturabhängigkeit von \Ik{bl} | -- | hart codierter Wert|
| \bk{gr}  | \$^\circ\$C\$^{-1}\$ | 0.0848  | Koeffizient des Exponenten der Temperaturabhängigkeit von \Ik{gr} | -- | hart codierter Wert|
| \rChlC | \mgChlgCd  | --  | Verhältnis von Chl-a-Synthese zu C-Aufnahme der Photosynthese der Algenklasse $i$ | \mbtt{roh\_Chlz}  | interne Variable|
| \alphaCChl  | \gCmqsmgChluE | --  | Chl-a-spezifischer Anfangsanstieg des lichtabhängigen Wachstums der Algenklasse $i$ | \mbtt{alpha\_chl} | interne Variable|
| \CA | \text{mg\,#1\,L$^{-1}$}{\text{C}}| -- | Konzentration des in der Algenklasse $i$ gebundenen C | -- | --|
| \mu_{A,i}^\text{Chl-a}  | \ugChlLd | --  | Chl-a-Produktionsrate der Algenklasse $i$ | -- | --|
| \theta_{\text{D1},i}  | - | 0--1| Relative Konzentration des D1-Proteins der Algenklasse $i$ | \mbtt{1 - svhemk}, \mbtt{1 - svhemb}, \mbtt{1 - svhemb} | Ausgabevariable|
| \sigma_{PSII,#1}{0} | \mquE | 1.5 | Absorptionsfläche des Photosystems II dunkeladaptierter Algen | \mbtt{PSII0} | hart codierte Variable|
| \sigma_{PSII,#1}{i} | \mquE | --  | Absorptionsfläche des Photosystems II  | \mbtt{PSII}  | interne Variable|
| \kappa_{PSII} | - | 0.22| Exponent der Abhängigkeit von \sigma_{PSII,#1}{i} vom C:Chl-a-Verhältnis| --  | hart codierter Wert|
| $k_d$ | - | $1.04\cdot10^{-8}$ | Schädigungskonstante des D1-Proteins | \mbtt{Kd0} | hart codierte Variable|
| $k_r$ | \sinv | $4.5\cdot10^{-5}$  | Reparaturrate des D1-Proteins  | \mbtt{Tau0}  | hart codierte Variable|
| f_{N\ddot{a}hr,i} | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse $i$ | \mbtt{F5} | interne Variable|
| f_{\text{#1},i}{$X$} | - | 0--1| Nährstofflimitierungsfaktor des Wachstums der Algenklasse $i$ für Nährstoff $X$ (N, P, Si; Si nur für Kieselalgen) | \mbtt{F51}, \mbtt{F52}, \mbtt{F53}  | interne Variable|
| \Q  | \gXg{$X$}  | --  | Zellinterne Nährstoffquota der Algenklasse $i$ für Nährstoff $X$ (N, P) | \mbtt{Q\_NK}, \mbtt{Q\_PK}, \mbtt{Q\_NB}, \mbtt{Q\_PB}, \mbtt{Q\_NG}, \mbtt{Q\_PG} | Ausgabevariable|
| \Qmax{X} | \gXg{$X$}  | --  | Maximale zellinterne Nährstoffquota der Algenklasse $i$ für Nährstoff $X$ (N, P, Si)  | \mbtt{Qmx\_NK}, \mbtt{Qmx\_PK}, \mbtt{Qmx\_SK}, \mbtt{Qmx\_NB}, \mbtt{Qmx\_PB}, \mbtt{Qmx\_NG}, \mbtt{Qmx\_PG} | e|
| \Qmin | \gXg{$X$}  | --  | Maximale zellinterne Nährstoffquota der Algenklasse $i$ für Nährstoff $X$ (N, P, Si)  | \mbtt{Qmn\_NK}, \mbtt{Qmn\_PK}, \mbtt{Qmn\_SK}, \mbtt{Qmn\_NB}, \mbtt{Qmn\_PB}, \mbtt{Qmn\_NG}, \mbtt{Qmn\_PG} | e|
| \khs{N}{i}  | \text{mg\,#1\,L$^{-1}$}{N} | 0.007 | N-Halbsättigungskonstante der Algenklasse $i$ | \mbtt{akksN}, \mbtt{abksN}, \mbtt{agksN} | e|
| \khs{P}{i}  | \text{mg\,#1\,L$^{-1}$}{P} | 0.023 | P-Halbsättigungskonstante der Algenklasse $i$ | \mbtt{akksP}, \mbtt{abksP}, \mbtt{agksP} | e|
| \khs{Si}{ki}| \text{mg\,#1\,L$^{-1}$}{Si}  | 0.08| Si-Halbsättigungskonstante der Kieselalgen| \mbtt{akksSi}| e|
| f_{T,i}^\mu | - | 0--1| Temperaturfaktor des Wachstums der Algenklasse $i$ | \mbtt{FTA} | interne Variable|
| $T$ | °C | --  | Wassertemperatur | \mbtt{tempw} | Ausgabevariable, Randwert|
| \Tref | °C | 20  | Referenztemperatur | \mbtt{Te0} | hart codierte Variable|
| \Topt{ki} | °C | 20.3| Temperatur des optimalen Wachstums der Kieselalgen | \mbtt{ToptK} | e|
| \Topt{bl} | °C | 23.7/ 31.8 | Temperatur des optimalen Wachstums der Blaualgen (faden-/koloniebildend)  | \mbtt{ToptB} | e|
| \Topt{gr} | °C | 30.2| Temperatur des optimalen Wachstums der Grünalgen | \mbtt{ToptG} | e|
| \kTmu{ki} | \$^\circ\$C\$^{-1}\$ | 0.0065  | Temperaturkoeffizient des Wachstums der Kieselalgen | \mbtt{kTemp\_Ki}  | e|
| \kTmu{bl} | \$^\circ\$C\$^{-1}\$ | 0.0069/ 0.0081  | Temperaturkoeffizient des Wachstums der Blaualgen (faden-/koloniebildend) | \mbtt{kTemp\_Bl}  | e| 
| \kTmu{gr} | \$^\circ\$C\$^{-1}\$ | 0.0041  | Temperaturkoeffizient des Wachstums der Grünalgen  | \mbtt{kTemp\_Gr}  | e|
| \resp | d\f$^{-1}\f$ | --  | Respirationsrate der Algenklasse $i$ | \mbtt{akres}, \mbtt{abres}, \mbtt{agres}, \mbtt{akraus}, \mbtt{abraus}, \mbtt{abraus} | interne Variable, Ausgabevariable|
| \respd{ki}  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Kieselalgen bei \Tref | \mbtt{akremi}| e|
| \respd{bl}  | d\f$^{-1}\f$ | 0.085 | Grundrespirationsrate dunkeladaptierter Blaualgen bei \Tref | \mbtt{abremi}| e|
| \respd{gr}  | d\f$^{-1}\f$ | 0.11| Grundrespirationsrate dunkeladaptierter Grünalgen bei \Tref | \mbtt{agremi}| e|
| \kTresp{ki} | \$^\circ\$C\$^{-1}\$ | 0.07| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Kieselalgen | \mbtt{kTresp}| hart codierte Variable|
| \kTresp{bl} | \$^\circ\$C\$^{-1}\$ | 0.09| Koeffizient der Temperaturabhängigkeit der Grundrespiration der Blaualgen | \mbtt{kTresp}| hart codierte Variable|
| \kTresp{bl} | \$^\circ\$C\$^{-1}\$ | 0.058 | Koeffizient der Temperaturabhängigkeit der Grundrespiration der Grünalgen | \mbtt{kTresp}| hart codierte Variable|
| \respg{ki}  | - | 0.2 | Respirationskosten des Wachstums der Kieselalgenklasse| \mbtt{frmuke}, \mbtt{frespg}  | e|
| \respg{bl}  | - | 0.2 | Respirationskosten des Wachstums der Blaualgenklasse  | \mbtt{frmube}, \mbtt{frespg}  | e|
| \respg{gr}  | - | 0.067 | Respirationskosten des Wachstums der Grünalgenklasse  | \mbtt{frmuge}, \mbtt{frespg}  | e|
| \mor| d\f$^{-1}\f$ | --  | Gesamtmortalitätsrate der Algenklasse $i$ | \mbtt{akmor},  \mbtt{abmor},  \mbtt{agmor} | interne Variable|
| \morX{0} | d\f$^{-1}\f$ | 0.02| Grundmortalitätsrate der Algenklasse $i$  | \mbtt{akmomi},  \mbtt{abmomi},  \mbtt{agmomi} | hart codierte Variable|
| \morX{\text{NP}}| d\f$^{-1}\f$  | 0.8 | Maximale nährstoffbedingte Mortalitätsrate der Algenklasse $i$| \mbtt{akmoma},  \mbtt{abmoma},  \mbtt{agmoma} | hart codierte Variable|
| \fmor | - | --  | Skalierungsfaktor der nährstoffbedingten Mortalitätsrate der Algenklasse $i$ | -- | --|
| \fmorX{0} | d\f$^{-1}\f$ | 0.05| Minimaler Skalierungskoeffizient der nährstoffbedingten Mortalitätsrate der Algenklasse $i$ | \mbtt{fmor0} | hart codierte Variable|
| \fsed{ki}, \fsed{gr} | - | 0.5 | Sedimentierbarer Anteil der Kiesel- bzw. Grünalgen  | \mbtt{askie}, \mbtt{asgre} | e|
| \fsed{bl} | - | 0 | Sedimentierbarer Anteil der Blaualgen | \mbtt{asble} | e|
| \Xsed{a} | - | 0--1| Sedimentierter Anteil der Algenklasse $i$  | \mbtt{Oc} | interne Variable|
| \Xsed{\alpha}| -| --  | Empirischer Koeffizient der Berechnung der Sedimentation in Abhängigkeit der Sinkgeschwindigkeit der Algenklasse $i$ | \mbtt{ased} | interne Variable|
| \Xsed{\beta}| - | 2.7 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse $i$ | \mbtt{bsed} | hart codierte Variable|
| \Xsed{\gamma}| -| 0.5 | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse $i$ | \mbtt{prop} | hart codierte Variable|
| \Xsed{q} | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse $i$ | \mbtt{qsgr} | interne Variable|
| \qsed | - | --  | Empirischer Koeffizient der Berechnung der Sedimentation der Algenklasse $i$ | \mbtt{qssed} | interne Variable|
| \w{i} | \ms | --  | Mittlere Sinkgeschwindigkeit der Algenklasse $i$ in Abhängigkeit vom mittleren Zellvolumen  | \mbtt{wsAlg} | interne Variable|
| \w{0} | \ms | --  | Sinkgeschwindigkeit der Algenklasse $i$ in Abhängigkeit von der Schubspannnungsgeschwindigkeit | \mbtt{wsgr} | interne Variable|
| \wEff | \ms | --  | Effektive Sinkgeschwindigkeit der Algenklasse $i$ | \mbtt{ws} | interne Variable|
| \V{ki} | \umc  | 1400| Mittleres Zellvolumen der Kieselalgen | \mbtt{ZellV} | hart codierte Variable|
| \V{bl} | \umc  | 1000| Mittleres Zellvolumen der Blaualgen| \mbtt{ZellV} | hart codierte Variable|
| \V{gr} | \umc  | 300 | Mittleres Zellvolumen der Grünalgen| \mbtt{ZellV} | hart codierte Variable|

end debugging -->


Herkunft: 
+ x - Bilanzvariable QSim 
+ b - berechnet, Zwischengröße/Übergabevariable 
+ e - Eingabe 
+ v - Vorgabe im Quellcode gesetzt 

\n\n

Textquelle: phyto-vars.md; Codesource: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück: \ref lnk_phytoplankton
