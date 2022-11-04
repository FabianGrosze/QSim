Phytoplankton - Prozesse {#lnk_phyto_prozesse}
===================== 
Die einzelnen Teilprozesse, die im Phytoplanktonbaustein berücksichtigt werden 
sind:
* [Wachstum](\ref lnk_phy_wachstum) in Abhängigkeit von
  - [Licht](\ref lnk_phy_licht)
  - [Nährstoffen](\ref lnk_phy_Naehrstoffe)
  - [Temperatur](\ref lnk_phy_Temperatur)
* [Respiration](\ref lnk_phy_Respiration)
* [Mortalität](\ref lnk_phy_Mortalitaet)
* [Sedimentation](\ref lnk_phy_Sedimentation)
* Grazing durch
  - [Rotatorien](\ref lnk_phy_Rotatorien)
  - _[Dreissena spp.](\ref lnk_phy_Dreissena)_
  - _[Corophium](\ref lnk_phy_Corophium)_
  - [Heterotrophe Nanoflagellaten](\ref lnk_phy_HNF)

  
  
# Grundgleichung und Übersicht {#lnk_phy_Grundgleichung}

Die Veränderung der Biomasse der Algengruppe *i* (\f$A_i\f$) wird in QSim über 
folgende Differentialgleichung beschrieben:
\f{equation}{
 \frac{dA_i}{dt} = \left( \mu_{A,i} - r_{A,i}^{resp} - r_{A,i}^{mor} \right) 
 \cdot A_i - {SED}_{A,i} - {GRA}_{A,i}^{ROT} - 
 {GRA}_{A,i}^{DR} - {GRA}_{A,i}^{COR} - 
 {GRA}_{A,i}^{HNF} + {ADV} + {MIX} 
  \label{eq:dAdt}
\f}

Die einzelnen Terme beschreiben dabei die folgenden Prozesse, welche in den 
entsprechenden Unterkapiteln dieser Dokumentation genauer beschrieben werden; 
ausgenommen Advektion (ADV) und Vermischung (MIX):

  - \f$ P_{A,i} \f$:     Wachstum [\f$ d^-1 \f$], abhängig von: \n
    * Licht (s. Kapitel \ref lnk_phy_licht)
    * Nährstoffen (s. Kapitel \ref lnk_phy_Naehrstoffe)
    * Temperatur (s. Kapitel \ref lnk_phy_Temperatur)
  - Respiration (\f$r_{A,i}^{resp} \cdot A_i\f$; s. Kapitel \ref lnk_phy_Respiration)
  - Mortalität (\f$r_{A,i}^{resp} \cdot A_i\f$; s. Kapitel \ref lnk_phy_Mortalitaet)
  - Sedimentation (SED\f$_{A,i}\f$; s. Kapitel \ref lnk_phy_Sedimentation)
  - Grazing durch
	* Rotatorien (GRA\f$_{A,i}^{ROT}\f$; s. Kapitel \ref lnk_phy_Rotatorien)
	* _Dreissena spp._ (GRA\f$_{A,i}^{DR}\f$; s. Kapitel \ref lnk_phy_Dreissena)
	* *Corophium* (GRA\f$_{A,i}^{COR}\f$; s. Kapitel \ref lnk_phy_Corophium)
	* Heterotrophe Nanoflagellaten (GRA\f$_{A,i}^{HNF}\f$; s. Kapitel \ref lnk_phy_HNF)

Die Vorbelegung der Algenbiomasse sowie die Nährstoff:Biomasse- und 
Kohlenstoff:Chlorophyll-a-Verhältnisse zu Beginn einer Simulation werden im 
Kapitel \ref phy_vorbelegung beschrieben.

<!-- Kapitel 3 -->

# Wachstum {#lnk_phy_wachstum}

Die Brutto-Primärproduktion wird in QSim über eine lichtabhängige 
Photosyntheserate beschrieben, welche zusätzlich durch Lichthemmung, die 
Verfügbarkeit von Nährstoffen sowie die Umgebungstemperatur beeinflusst wird:

\f{equation}{
 {BPP}_{A,i} = \mu_{A,i} \cdot A_i = \left( P_{A,i} \cdot f_{LH,i} \cdot 
 f_{N\ddot{a}hr,i} \cdot f_{T,i}^\mu \right) \cdot A_i 
\f}
<!--  \label{Eq:Growth} -->

\f$ BPP_{A,i} \f$:  Brutto-Primärproduktion [\f$ \mgLd \f$] \n
\f$ \mu_{A,i} \f$:  Brutto-Wachstumsrate [\f$\dinv\f$] \n
\f$ P_{A,i}   \f$:  lichtabhängige Brutto-Photosyntheserate (ohne Lichthemmung) [\f$\dinv\f$] \n 
\f$ f_{LH,i}  \f$:  Lichthemmungsfakor [-] (s. \ref lnk_phy_Lichthemmung) \n 
\f$ f_{N\ddot{a}hr,i}\f$: Nährstofflimitierungsfaktor [-] (s. \ref lnk_phy_NutLim) \n 
\f$ f_{T,i}^\mu \f$: Temperaturabhängiger Wachstumsfaktor [-] (s. \ref lnk_phy_Temperatur) \n


## Lichtabhängige Photosyntheserate und Unterwasserlicht {#lnk_phy_licht}

*Intro-Text für lichtabhängige Photosynthese* \n

In QSim wird die lichtabhängige Photosyntheserate (ohne Lichthemmung) wie folgt 
berechnet:

\f{equation}{
 P_{A,i} = P_{A,i}^{max} \cdot \left(1 - e^{-\frac{\Izm}{\Ik} \cdot 
 \frac{\CChld()}{\CChl}} \right),     {[\dinv]} 
 \label{eq:pcp}
\f}

mit der maximalen lichtanhängigen Photosyntheserate \f$P_{A,i}^{max}\f$ 
(ohne Lichthemmung; in d\f$^{-1}\f$) bei der optimalen Temperatur 
\f$T_{opt, i}\f$ (in °C):

\f{equation}{
 P_{A,i}^{max} = \frac{\mu_{A,i}^{max}{i} \cdot e^{\kTmu{i} \cdot 
 \left( \Tref - \Topt{i} \right)^2} + \respd{i}(\Topt{i}) }{1 - \respg{i}}.   
 \label{Eq:Pcpmax}
\f}

\f$ P_{A,i}^{max} \f$: maximale Photosyntheserate [\f$ \dinv \f$] \n
... \n

Hier beschreiben \f$\mu_{A,i}^{max}{i}\f$ die maximale Brutto-Wachstumsrate 
(in \f$\dinv\f$) bei der Referenztemperatur \f$ \Tref = 20\f$ °C, \f$\kTmu{i}\f$ 
den Temperaturkoeffizienten des Wachstums (nach James, 2010 \cite James2010; 
s. Kapitel \ref lnk_phy_Temperatur), \f$\respd{i}\f$
die Respirationsrate dunkeladaptierter Algen (in \f$\dinv\f$; s. 
Kapitel \ref lnk_phy_Respiration) und \f$\respg{i}\f$ die Respirationskosten des 
Wachstums (dimensionslos; s. Kapitel \ref lnk_phy_Respiration). Die maximale 
Wachstumsrate \f$\mu_{A,i}^{max}{i}\f$ ist ein Eingabeparamter in QSim, dessen 
Standardwerte wie folgt sind:

* \f$ \mu_{A,ki}^{max} = 1.6 \: \f$ für Kieselalgen
* \f$ \mu_{A,gr}^{max} = 1.2 \: \f$ für Grünalgen
* \f$ \mu_{A,bl}^{max} = 1.2 \: \f$ für Blaualgen

In Gleichung \f$\eqref{eq:pcp}\f$ beschreibt \f$ \Izm \f$ die mittlere Lichtintensität 
(in \f$\uEms\f$), die das Phytoplankton infolge von vertikaler Durchmischung 
erfährt. Die Berechnungen der Sättigungsintensität \f$\Ik{i}\f$ sowie des 
aktuellen C:Chl-a-Verhältnisses \f$ \CChl \f$ und dessen dunkeladaptierter Algen 
\f$\CChld i\f$ (beide in \f$\gCmgChl\f$) sind in Kapitel 
\ref lnk_phy_Lichtadaption detailiert beschrieben.

Die mittlere Lichtintensität, die eine Algenzelle erfährt, berechnet sich in 
QSim in Abhängigkeit von ihrer Position und ihrer vertikalen Verdriftung 
innerhalb der Wassersäule:
\f{equation}{
  \Izm = \frac{I_0}{\dz} \int_{z + \dz}^z e^{-\epsilon z} dz. 
      {[\uEms]} 
  \label{Eq:Iz}
\f}

Die Tiefe _z_ (in m) entspricht hier dem oberen Ende der Verdriftungstrecke 
\f$\dz\f$ (in m). Die an der Gewässeroberfläche verfügbare photosynthetisch aktive 
Strahlung \f$I_0\f$ (PAR; in \f$\uEms\f$) wird in QSim aus der einfallenden 
Netto-Globalstrahlung \f$S_0\f$ (in \f$\calms\f$) wie folgt berechnet:
\f{equation}{
  I_0 = 5.846 \cdot 4.2 \cdot S_0.     {[\uEms]}
\f}

Die Netto-Globalstrahlung \f$S_0\f$ entspricht dabei dem nicht reflektierten Anteil 
der kurzwelligen Gesamteinstrahlung, welcher vom Einfallswinkel und somit von 
Tages- und Jahreszeit abhängt. \f$S_0\f$ wird in QSim in strahlg.f90 
berechnet. Der Faktor 4.2 bewirkt die Umrechnung von \f$\calms\f$ in \f$Wm\f$, während 
der Faktor 5.846 für die Umrechnung von \f$\Wm\f$ in \f$\uEms\f$ ausgehend von einem 
Wellenlängenbereich der PAR von 400 nm bis 700 nm verwendet wird.

\note Die Herkunft des Faktors 5.846 ist unklar. 
Thimijan, 1983 \cite Thimijan1983 geben einen Wert von 4.57 an. Der exakte Umrechnungsfaktor 
von cal in J ist zudem 4.184.

Die Verdriftungsstrecke \f$\dz\f$ wird in QSim mit dem Ansatz von 
Falkowski, 1981 \cite Falkowski1981 berechnet: 
<!-- was \citet {Falkowski1981} berechnet -->

\f{equation}{
  \dz = \sqrt{2 \cdot \tau_R \cdot \Dzm},     {[m]}
\f}

mit der Relaxationszeit der Algen \f$\tau_R\f$, welche in QSim als 100 s 
angenommen wird. \f$\Dzm\f$ beschreibt die tiefengemittelte turbulente Diffusivität 
(in \f$\mqs\f$), welche für ein vertikal durchmischtes Gewässer abgeschätzt werden 
kann Vieiral, 1993 \cite Vieira1993:
<!-- war: \citep{Vieira1993} -->
\f{equation}{
  \Dzm = \frac{1}{6} \cdot \ust \cdot \kappa \cdot H     {[\mqs]}
\f}

Hier beschreiben \f$\ust\f$ die Schubspannungsgeschwindigkeit (in \f$\ms\f$),
\f$ \kappa = 0.4 \f$ die dimensionlose von-Kármán-Konstante und 
*H* die Wassertiefe (in m). 


Der Lichtextinktionskoeffizient \f$ \epsilon \f$ (in \f$\minv\f$) in 
Glg. \f$\eqref{Eq:Iz}\f$ berechnet sich in QSim als:
\f{equation}{
  \epsilon = \epsm{SS} \cdot ({SS} + {ZOO}) + \epsm{Chl-a} \cdot {Chl-a} + \epsm{W} + \epsm{H},     {[\minv]} 
  \label{Eq:ext}
\f}

mit dem mittleren Extinktionskoeffizienten für Schwebstoff \f$\epsm{SS}\f$ 
(in \f$\,\Lmmg\f$), den Schwebstoff- und Zooplanktonkonzentrationen SS bzw. ZOO 
(in mg L\f$^{-1}\f$), dem mittleren Extinktionskoeffizienten für Chlorophyll-a 
\f$\epsm{Chl-a}\f$ (in \f$\LmugChl\f$), der Chlorophyll-a-Konzentration Chl-a 
(in \f$\ugChlL\f$), sowie den mittleren Extinktionskoeffizienten von Wasser 
\f$\eps{W}\f$ und Huminstoffen \f$\eps{H}\f$ (beide in \f$\minv\f$). 
\f$\eps{Chl-a}\f$ berechnet sich dabei als gewichtetes Mittel der 
Extinktionskoeffizienten der drei Algenklassen:
\f{equation}{
  \epsm{Chl-a} = \frac{\sum_{i=1}^3 \epsc{i} \cdot \Chl}{\sum_{i=1}^3 \Chl}.     {[\minv]} \label{Eq:ext_chla}
\f}

Die verschiedenen Extinktionskoeffizienten (s. Glgn. \f$\eqref{Eq:ext}\f$ und 
\f$\eqref{Eq:ext_chla}\f$) entsprechen dabei (mit Ausnahme von \f$\eps{H}\f$) jeweils 
den mittleren Extinktionskoeffizienten im Wellenlängenbereich von 400 nm bis 
700 nm. Diese werden in QSim aus den wellenlängenspezifischen Werten (in 
10-nm-Schritten), welche aus der Datei e_extnct.dat eingelesen werden, 
berechnet. Für die Huminstoffe berechnet QSim die wellenlängenabhängigen 
\f$\eps{H}(\lambda)\f$ als:
\f{equation}{
  \eps{H}(\lambda) = \eps{H}(\lambda_0) \cdot e^{-k_\lambda \cdot 
  \left( \lambda - \lambda_0 \right)},
\f}

welche anschließend ebenfalls über den Wellenlängenbereich von 400 nm bis 
700 nm gemittelt werden. Hier bezeichnet \f$\eps{H}(\lambda_0)\f$ den 
Extinktionskoeffizienten der Huminstoffe bei \f$\lambda_0 = 440\f$ nm 
(in \f$\minv\f$), 
welcher ein Eingabeparameter ist (Standardwert: 0.75). Der Koeffizient 
\f$k_\lambda = 0.016 \: n\minv\f$ ist in QSim hart codiert. 
\note Die Herkunft dieser Berechnung ist unklar. Weiterhin verwendet QSim 
faktisch den Wellenlängenbereich von 395 bis 705 nm, da es 31 Werte in 
10-nm-Schritten beginnend bei 400 und endend bei 700 einliest.


### Einfluss von Lichtadaption {#lnk_phy_Lichtadaption}

*Text zu Lichtadaption*

Die Lichtadaption der Algen wird in QSim über die Veränderung des 
Chlorophyll-a:C-Verhältnis nach Geider, 1997 \cite Geider1997 beschrieben. Genauer erfolgt 
dies über die Veränderung der Chlorophyll-a-Synthese relativ zur C-Aufnahme 
durch Photosynthese. Die Chlorophyll-a-Synthese ist in QSim wie folgt 
beschrieben:
\f{equation}{
  \frac{d\Chl}{dt} = \rChlC \cdot \CBio{i} \cdot \mu_{A,i} \cdot A_i     {[\ugChlLd]} \label{Eq:dChla_dt},
\f}

mit dem aktuellen Verhältnis von Chlorophyll-a- zu C-Synthese \f$\rChlC\f$ (in 
\f$\mgChlgCd\f$), welches in Anlehnung an Geider, 1997 \cite Geider1997 berechnet wird:
\f{equation}{
  \rChlC =  \frac{1}{\CChld(T)} \cdot \frac{\CChl \cdot 
  P_{A,i}}{\alphaCChl \cdot \Izm}.     {[\mgChlgCd]} 
  \label{Eq:ChlaC_Synthese}
\f}
mit dem chlorophyll-a-spezifischen Anfangsanstieg der lichtabhängigen 
Wachstumsfunktion \f$\alphaCChl\f$ (in \f$\gCmqsmgChluE \f$), welcher in QSim 
wie folgt berechnet wird:
\f{equation}{
  \alphaCChl = \frac{P_{A,i}^{max} \cdot f_{T,i}^\mu \cdot \CChld(T)}{\Ik(T)},     
  {[\gCmqsmgChluE]}
\f}
mit der Sättigungsintensität \f$ \Ik(T) \f$ (in \f$ \uEms \f$) bei Temperatur 
\f$ T \f$, welche über eine empirische Gleichung berechnet wird:
\f{equation}{
  \Ik(T) =  \ak{i} \cdot \Ik(\Tref) \cdot e^{\bk{i} \cdot T},     
  {[\uEms]} \label{Eq:I_k}
\f}
mit der Sättigungsintensität \f$\Ik(\Tref)\f$ bei \f$\Tref = 20\f$ °C, welche ein 
Eingabeparameter ist (in \f$\uEms\f$), sowie den Koeffizienten \f$\ak{i}\f$ 
(dimensionslos) und \f$\bk{i}\f$ (in \f$ \degCinv \f$), welche in QSim die 
folgenden Werte annehmen:

* \f$\ak{ki} = 0.837, \bk{ki} = 0.0089\f$ für Kieselalgen
* \f$\ak{gr} = 0.183, \bk{gr} = 0.0848\f$ für Grünalgen
* \f$\ak{bl} = 0.525, \bk{bl} = 0.0322\f$ für Blaualgen

\note Die Koeffizienten in Glg. \f$\eqref{Eq:I_k}\f$ sind in QSim 
in den jeweiligen Algenroutinen hart codiert und teilweise durch 
`if`-Konstruktion mit `real`-Vergleichen gesetzt. Es wird davon 
ausgegangen, dass diese `real`-Vergleich nie greifen und somit die im Code 
gesetzten Standardwerte verwendet werden. Die Herkunft der verwendeten Werte 
ist unklar.

Die C-Synthese in QSim berechnet sich analog zu Glg. \f$\eqref{Eq:dChla_dt}\f$ als:
\f{equation}{
  \frac{d\CA}{dt} = \CBio{i} \cdot \mu_{A,i} \cdot A_i,     
  {[\mgCLd]} \label{Eq:dC_dt}
\f}
mit der C-Konzentration der Algenklasse *i* \f$\CA\f$ (in mg C L\f$^{-1}\f$). 
Über einen Berechnungszeitschritt \f$\Delta t\f$
ergeben sich dann aus Glg. \f$\eqref{Eq:dChla_dt}\f$ und Glg. \f$\eqref{Eq:dC_dt}\f$:
\f{equation}{
  \Chl(t+\Delta t) = \Chl(t) \cdot e^{\CChld(T) \cdot \rChlC \cdot \mu_{A,i} 
  \cdot \Delta t},     {[\ugChlL]} \label{Eq:Chla_neu}
\f}
\f{equation}{  
  \CA(t+\Delta t)  = \CA(t)  \cdot e^{\mu_{A,i} \cdot \Delta t}.     
  {[mg C L^{-1}]}. 
  \label{Eq:C_neu}
\f}
Unter Verwendung der Chlorophyll-a-Syntheserate \f$\mu_{A,i}^{Chl-a} = 
\CChld(T) \cdot \rChlC \cdot \mu_{A,i}\f$ (in d\f$^{-1}\f$) und durch Division 
von Glg. \f$\eqref{Eq:C_neu}\f$ durch Glg. \f$\eqref{Eq:Chla_neu}\f$ ergibt sich schließlich:
\f{equation}{
 \frac{\CA(t+\Delta t)}{\Chl(t+\Delta t)} = \frac{\CA(t)}{\Chl(t)} \cdot 
 e^{\mu_{A,i} - \mu_{A,i}^{Chl-a}},\nonumber
\f}
\f{equation}{  
 \CChl(t+\Delta t) = \CChl(t) \cdot e^{\mu_{A,i} - \mu_{A,i}^{Chl-a}}.    
 {[\gCmgChl]}.
\f}


### Einfluss von Lichthemmung  {#lnk_phy_Lichthemmung}

_Intro-Text zu Lichthemmung_ \n

Die Lichthemmung (oder Photoinhibition) des Algenwachstums wird in QSim über 
die relative Konzentration des D1-Proteins, \f$f_{LH,i} = \theta_{{D1},i}\f$, 
beschrieben. Die zeitliche Änderung von \f$\theta_{{D1},i}\f$ wird dabei 
entsprechend des Ansatzes von Han, 2000 \cite Han2000 berechnet:
\f{equation}{
  \frac{d\theta_{{D1},i}}{dt} = -k_{d} \cdot \Izm \cdot 
  \sigma_{PSII,i} \cdot \theta_{{D1},i} + k_r \cdot \left( 1 - 
  \theta_{{D1},i} \right).     
  {[dimensionslos]} \label{Eq:D1}
\f}
mit der Schädigungskonstante \f$k_d=1.04\cdot10^{-8}\f$ (dimensionslos) und der 
Reparaturrate \f$k_r = 4.5\cdot10^{-5}\f$ (in s\f$^{-1}\f$). 
\note Die genannten Werte entsprechen den in QSim verwendeten Werten. 
Han, 2000 \cite Han2000 nennen einen Bereich von \f$ 5.6 \cdot 10^{-5} \leq k_r 
\leq 2.2 \cdot 10^{-4}\f$, welcher vom Wert in QSim unterschritten wird.

\f$I\f$ beschreibt das aktuelle PAR (in \f$\uEms\f$) und \f$\sigma_{PSII,i}\f$ 
beschreibt die Absorptionsfläche der 
Algen (in \f$\mquE\f$) in Abhängigkeit vom aktuellen C:Chl-a-Verhältnis:
\f{equation}{
  \sigma_{PSII,i} = \sigma_{PSII,0} \cdot \left( \frac{\CChld()}{\CChl} 
  \right)^{\kappa_{PSII}}.     {[\mquE]}
\f}
Hier bezeichnet \f$\sigma_{PSII,0} = 1.5\f$ \f$\mquE\f$ die Absorptionsfläche 
dunkeladaptierter Algen und \f$\kappa_{PSII}\f$ ist ein empirischer Exponent, 
welcher in QSim auf 0.22 gesetzt ist. Der Bruchterm kann als Schließungsgrad 
des Photosystems II interpretiert werden. 
\note Die Abhängigkeit von \f$ \sigma_{PSII,i}\f$ vom C:Chl-a-Verhältnis ist 
in QSim zwar codiert, aber nicht korrekt angewendet, weshalb immer gilt: 
\f$\sigma_{PSII,i} = \sigma_{PSII,0} = 1.5\f$. Der Wert 
\f$\kappa_{PSII}=0.22\f$ stammt laut lichthemmung.f90 von einem Fit zu 
Daten von Anning, 2000 \cite Anning2000, genaue Herkunft ist aber unklar.

Die Differentialgleichung \f$\eqref{Eq:D1}\f$ wird in QSim mit einem 
Runge-Kutta-Verfahren 4. Ordnung gelöst. 
\note Es ist unklar, warum RK4 notwendig ist; zudem ist dieses falsch 
implementiert, s. lichthemmung.f90 .


## Nährstoffabhängigkeit des Wachstums {#lnk_phy_Naehrstoffe}

_Text zu Nährstoffabhängigkeit_ \n


### Nährstofflimitierung {#lnk_phy_NutLim}

Die Limitierung des Algenwachstums wird über einen Nährstofflimitierungsfaktor 
beschrieben:

\f{equation}{
  f_{Nähr,i} = min( f_{N}, f_{P,i}, f_{Si,i}),  {[dimensionslos]} 
  \label{Eq:f_nut}
\f}
wobei die verschiedenen Faktoren (\f$f_{N,i}\f$ etc.) die Limitierung 
aufgrund von Stickstoff (N), Phosphor (P) und Silikat (Si) beschreiben. 
Letzterer ist nur für Kieselalgen von Bedeutung. Die Berechnung dieser 
Limitierungsfaktoren erfolgt in Abhängigkeit der gewählten Eingabeparameter für 
die minimalen (\f$\Qmin\f$) und maximalem zellinternen 
Nährstoff:Biomasse-Verhältnisse der Algen (\f$\Qmax{X}\f$):
\f{equation}{
  f_{X,i} =
 \begin{cases}
  % case 1: Qmax > 1.25 * Qmin
  \frac{\Q - \Qmin}{\Qmax{X} - \Qmin} &  {wenn} \;\Qmax{X} > 1.25 \cdot \Qmin,\\
  % case 2: otherwise
  \frac{X}{\khs{$X$}{i} + X}   &   {sonst.}
 \end{cases}     
 {[dimensionslos]} \label{Eq:NutLimX}
\f}
\f$\Qmin\f$ und \f$\Qmax{X}\f$ sind jeweils Eingabeparameter (in \f$\gXg{$X$}\f$), 
wobei *X* für den jeweiligen Nährstoff steht (N, P oder Si). Der beschriebene 
erste Fall 
entspricht dem Modell nach Geider, 1998 \cite Geider1998. Im beschriebenen zweiten Fall 
wird die Nährstofflimitierung in Form eines Michaelis-Menten-Ansatzes auf Basis 
der Nährstoffkonzentrationen im Wasser und einer Halbsättigungskonstante 
\f$\khs{$X$}{i}\f$ in mg L\f$^{-1}\f$ beschrieben, welche ebenfalls ein 
Eingabeparameter ist. 
\note QSim wendet für die Berechnung 
des Nährstofflimitierungsfaktors Glg. \f$\eqref{Eq:NutLimX}\f$ für N, P und Si an. 
Hingegen wird für Si-Aufnahme kein zellinterner Speicher angewendet. Dies 
scheint inkonsistent und sollte geprüft werden.


### Zellinterne Nährstoffspeicher für N und P {#lnk_phy_NutStore}
QSim berechnet die zeitliche Änderung der zellinternen 
Nährstoff:Biomasse-Verhältnisse der Algen nach [Droop 1973]
(\ref lnk_ext_literatur) :

\f{equation}{
  \frac{d\Q}{dt} = -\mu_{A,i} \cdot \Q + \UP,
\f}

wobei \f$\UP \f$ die Veränderung des zellinternen Nährstoff:Biomasse-Verhältnisses 
des Nährstoffs *X* (N oder P) aufgrund der Nährstoffaufnahme beschreibt. Die 
Berechnung von \f$\UP\f$ ist analog zur Nährstofflimitierung (s. 
Glg. \f$\eqref{Eq:f_nut}\f$) in QSim abhängig von den gesetzten Werten für 
\f$\Qmax{X}\f$ und \f$\Qmin\f$:
\f{equation}{
  \UP =
\begin{cases}
% case 1: Qmax > 1.25*Qmin
\UPmax \cdot \frac{1 - \frac{\Q}{\Qmax{X}}}{1 - \frac{\Q}{\Qmax{X}} + 0.01} \cdot \frac{X}{\khs{$X$}{i} + X}   &   {wenn}   \;\Qmax{X} > 1.25 \cdot \Qmin,\\
% case 2: otherwise
\Qmax{X} \cdot \left( \mu_{A,i} - \resp \right)\cdot \frac{A_i}{A_i^*}   &   {sonst.}
\end{cases}     {[\gXgd{$X$}]} \label{Eq:up_nut}
\f}

Der erste Fall folgt dem Ansatz von Flynn, 2001 \cite Flynn2001. Hier beschreibt \f$\UPmax\f$
die maximale Änderungsrate des Nährstoff:Biomasse-Verhältnisses. 
\note Herkunft des Werts 0.01 im Nenner unklar. 
Flynn, 2001 \cite Flynn2001 verwendet andere Werte mit Unterschieden zwischen den 
verschiedenen Nährstoffen. 

Der zweite Fall impliziert ein Nährstoff:Biomasse-Verhältnis 
\f$\Q = \Qmax{X} = const.\f$ Hier beschreibt \f$ \resp \f$
die Respirationsrate (in d \f$^{-1}\f$); \f$ A_i\f$ und \f$ A_i^* \f$ 
repräsentieren die Algenbiomassen am Anfang und am Ende eines 
Berechnungszeitschritts aufgrund des Netto-Wachstums. 
\note Diese Berechnung rechnet in QSim 
fälschlicherweise zweimal den Biomasseverlust aufgrund der Respiration ein.

Die Rate \f$\UPmax\f$ berechnet sich unter Berücksichtigung der 
Temperaturabhängigkeit nach James, 2010 \cite James2010 als:
\f{equation}{
  \UPmax = \Qmax{X} \cdot f_{T,i}^\mu \cdot \mu_{A,i}^{max} \cdot 
  e^{\kTmu{i} \cdot\left( \Tref - \Topt{i} \right)^2}     {[\gXgd{X}]}
\f}
Die Berechnung des Temperaturfaktors \f$f_{T,i}^\mu\f$ auf Basis der Parameter 
\f$\kTmu{i}\f$ und \f$\Topt{i}\f$ (beides Eingabeparameter) sind in Kapitel 
\ref lnk_phy_Temperatur beschrieben.

Für Silizium verwendet QSim keinen zellinternen Speicher, weshalb die 
Veränderung des Silizium:Biomasse-Verhältnisses in Abhängigkeit von der 
Umgebungskonzentration des Silikats (in {mg L\f$^{-1}\f$ Si) 
analog zum zweiten Fall in Glg. \f$\eqref{Eq:up_nut}\f$ berechnet wird:
\f{equation}{
  {UP}_{{Si},i} = \Qmax{{Si}} \cdot \frac{{Si}}{\khs{Si}{i} + {Si}} \cdot \left( \mu_{A,i} - \resp \right)\cdot \frac{A_i}{A_i^*}      {[\gXgd{Si}]} \label{Eq:up_Si}
\f} 


## Temperaturabhängigkeit des Wachstums {#lnk_phy_Temperatur}

Die Temperaturabhängigkeit des Wachstums wird in QSim über einen Faktor 

\f$f_{T,i}^\mu\f$ nach James, 2010 \cite James2010 beschrieben:
\f{equation}{
  f_{T,i}^\mu = e^{-\kTmu{i} \cdot\left( T - \Topt{i} \right)^2}.     {[dimensionslos]} \label{Eq:fT_growth}
\f}
Der Koeffizient \f$\kTmu{i}\f$ (in °C\f$^{-2}\f$) und die optimale Temperatur 
\f$\Topt{i}\f$ (in °C) sind Eingabeparameter in QSim, deren Standardwerte durch den 
Fit von Glg. \f$\eqref{Eq:fT_growth}\f$ an verschiedene Datensätze ermittelt wurden. 
Diese Standardwerte sind wie folgt:

* \f$\kTmu{ki} = 0.0065, \Topt{ki} = 20.3\f$ für Kieselalgen
* \f$\kTmu{gr} = 0.0041, \Topt{gr} = 30.2\f$ für Grünalgen
* \f$\kTmu{bl} = 0.0069, \Topt{bl} = 23.7\f$ für fadenbildende Blaualgen
* \f$\kTmu{bl} = 0.0081, \Topt{bl} = 31.8\f$ für koloniebildende Blaualgen


<!-- Kapitel 4 -->

# Respiration {#lnk_phy_Respiration}
Introtext zur Respiration\n

Die Gesamtrespiration der Algenklasse *i* errechnet sich in QSim als:
\f{equation}{
 \RESP = \resp \cdot A_i,     
\f}

\f$ \RESP \f$: Gesamtrespiration der Algenklasse *i* [mg L\f$^{-1}\f$d\f$^{-1}\f$]

mit der Gesamtrespirationsrate \f$\resp\f$ (in d\f$^{-1}\f$) und der Algenbiomasse 
\f$A_i\f$ (in mg L\f$^{-1}\f$d\f$^{-1}\f$). Die Gesamtrespirationsrate ergibt 
sich aus der Grundrespirationsrate dunkeladaptierter Algen (\f$ \respd() \f$) und 
der wachstumsbedingten Respiration (\f$\respg{i} \cdot \mu_{A,i}\f$):
\f{equation}{
 \resp = \respd(T) + \respg{i} \cdot \mu_{A,i}. \label{Eq:r_resp}     {[\dinv]}
\f}
\note Diese Gleichung passt nicht zu der Berechnung des 
Wachstums, wenn man die Gleichungen ineinander einsetzt.

Die Grundrespiration dunkeladaptierter Algen \f$ \respd() \f$ in Abhängigkeit der 
Temperatur *T* ist wie folgt beschrieben:
\f{equation}{
 \respd(T) = \respd( \Tref ) \cdot e^{\kTresp{i} \cdot \left( T - \Tref \right)}.     {[\dinv]}
\f}
Hier repräsentiert \f$\respd(\Tref)\f$ die Grundrespirationsrate dunkeladaptierter 
Algen bei \f$\Tref = 20\f$ °C, welche in QSim ein Eingabeparameter (in 
d\f$^{-1}\f$) ist. Der Koeffizient der Temperaturabhängigkeit, \f$\kTresp{}\f$, nimmt 
in QSim für die verschiedenen Algenklassen die folgenden Werte an (in \f$ 
\degCinv \f$):

* \f$\kTresp{ki} = 0.070\f$ für Kieselalgen
* \f$\kTresp{gr} = 0.058\f$ für Grünalgen
* \f$\kTresp{bl} = 0.090\f$ für Blaualgen

\note Diese Werte sind in den Algenroutinen hart codiert und 
ihre Herkunft ist nicht geprüft.

Im wachstumsbedingten Term in Glg. \f$\eqref{Eq:r_resp}\f$ beschreibt der Faktor 
\f$\respg{i}\f$ beschreibt die anteilige Respiration infolge von Wachstum und ist in 
QSim ein Eingabeparameter mit einem Standardwert von 0.2 (dimensionslos).


<!-- Kapitel 5 --> 

# Mortalität {#lnk_phy_Mortalitaet}

*Introtext zur Mortalität* \n

Die Gesamtmortalität (ohne Grazing) der Algenklasse *i* errechnet sich in 
QSim als:
\f{equation}{
 \MOR = \mor \cdot A_i,     
\f}

\f$ \MOR \f$: Gesamtmortalität [mg L\f$^{-1}\f$d\f$^{-1}\f$]

mit der Gesamtmortalitätsrate \f$\mor\f$ (in d\f$^{-1}\f$), welche sich aus der 
Grundmortalitätsrate der Algenklasse (\f$\morX{0}\f$) und einer nährstoffabhängigen 
Mortalität (\f$\morX{{NP}} \cdot \fmorX{{NP}}\f$) berechnet:
\f{equation}{
 \mor = \morX{0} + \morX{{NP}} \cdot \fmor.     {[\dinv]} \label{Eq:r_mort}
\f}
Die Grundmortalität ist in QSim für alle Algenklassen auf \f$\morX{0} = 
0.02\f$ \f$\dinv\f$ gesetzt. Die nährstoffabhängige Mortalität in 
Glg. \f$\eqref{Eq:r_mort}\f$ berechnet sich für alle Algenklassen über eine maximale 
Rate \f$ \morX{{NP}} = 0.8 \dinv \f$ und einen Nährstofffaktor (unter 
Vernachlässigung des Einflusses von Si):
\f{equation}{
 \fmor = 1 - \left( \frac{min\left( \fmorX{0}, f_{N,i}, f_{P,i} \right)}
 { \fmorX{0} } \right)^8      
 {[dimensionslos]}\label{Eq:f_morn}
\f}

Der minimale Skalierungsfaktor \f$\fmorX{0}\f$ nimmt in QSim für alle Algenklassen 
den Wert 0.05 an.

\note \f$\morX{0} = 0.02, \morX{{NP}} = 0.8\f$ und 
\f$\fmorX{0} = 0.05\f$ hart codiert und Herkunft unbekannt. Der Exponent 8 in 
Glg. \f$\eqref{Eq:f_morn}\f$ ist ebenfalls hart codiert und seine Herkunft unbekannt. 
Sollte Si-Einfluss besser auch berücksichtigt werden?

In QSim wird zudem die aktuelle Gesamtmortalitätsrate mit dem entsprechenden 
Wert des vorangegangenen Zeitschritts verglichen:
\f{equation}{
 \mor(t) = max\left( \mor(t), \mor(t-\Delta t) \right).     {[\dinv]}
\f}
\note Diese Berechnung soll bewirken, dass sich eine erhöhte 
Gesamtmortalitätsrate durch Zugabe von Nährstoffen nicht wieder verringert. In 
dieser Form, unterbindet diese Gleichung jedoch eine Zunahme der 
Gesamtmortalitätsrate, wenn \f$min\left(f_{N,i}, f_{P,i} \right) < \fmorX{0}\f$ 
und weiter abnimmt (s. Glg. \f$\eqref{Eq:f_morn}\f$).


<!-- Kapitel 6 -->

# Sedimentation von lebenden Algen {#lnk_phy_Sedimentation}
<!-- hier könnte auf die Sedimentations-Unterseite verlinkt werden 
(lnk_sedimentation), die aber gerade noch nicht überprüft ist -->

Introtext zur Sedimentation. 
\note Die gesamte Berechnung ist sehr kryptisch und ihr Herkunft unbekannt. 
Die Beschreibungen der Variablen sind so weit möglich aus dem Code in 
sedimentation.f90 zusammengereimt. 
und teilweise kreativ benannt.)

In QSim können Algen absinken und am Boden sedimentieren. Die Berechnung des 
Sedimentationsfluxes findet dabei über eine empirische Formel statt:
\f{equation}{
 \SED = \left( 1 - \Xsed{q} \right) \cdot \fsed{i} \cdot \Xsed{a}  \cdot A_i  
 \label{Eq:sed}
\f}

\f$ \SED \f$: Sedimentation [mg L\f$^{-1}\f$]

Hier beschreibt \f$\fsed{i}\f$ den sedimentierbaren Anteil der Algenklasse *i*
(dimensionslos; 0--1), welcher in QSim ein Eingabeparameter ist. \f$\Xsed{a}\f$ 
bezeichnet den sedimentierten Anteil (dimensionslos; 0--1) und \f$\Xsed{q}\f$ 
beschreibt einen empirisch berechneten Koeffizienten (dimensionslos; \f$0 < 
\Xsed{q} < 1\f$). Der sedimentierte Anteil \f$\Xsed{a}\f$ wird wie folgt 
berechnet:
\f{equation}{
 \Xsed{a} = 1 - e^{-\frac{\Xsed{\gamma} \cdot \wEff \cdot \Delta t}{H}}.     
 {[dimensionslos]} \label{Eq:a_sed}
\f}
Der dimensionslose Koeffizient \f$\Xsed{\gamma}\f$ hat in QSim den Wert 
\f$\Xsed{\gamma} = 0.5\f$. Die effektive Sinkgeschwindigkeit \f$\wEff\f$ 
(in \f$ms\f$) wird ebenfalls über eine empirische Gleichung berechnet:
\f{equation}{
 \wEff = 10^{\frac{1}{\Xsed{\beta}} \cdot \left( ln\left(\Xsed{\alpha}\right) - ln \left(  \frac{1}{\qsed} -1 \right)\right)} \cdot min\left( 1, e^{-604.2 \cdot\ust} \right),     {[\ms]} \label{eq:w_eff}
\f}

mit dem Koeffizienten \f$\Xsed{\beta} = 2.7\f$ (dimensionslos). Der Koeffizient 
\f$\Xsed{\alpha}\f$ berechnet sich in Abhängigkeit von der mittleren 
Sinkgeschwindigkeit \f$\w{i}\f$ der Algenklasse *i*:
\f{equation}{
 \Xsed{\alpha} = e^{\Xsed{\beta} \cdot log(\w{i})},    {[dimensionslos]}
\f}
wobei \f$\w{i}\f$ über eine empirische Funktion in Abhängigkeit des mittleren 
Zellvolumnes \f$\V{i}\f$ der Algenklasse *i* (in \f$\umc\f$) berechnet wird:
\f{equation}{
 \w{i} = 10^{2.0155\cdot log(\V{i}) - 11.512}.     {[\ms]}
\f}
Die mittleren Zellvolumina sind in QSim wie folgt 
\note (im jeweiligen Baustein hart codiert):

* \f$\V{ki} = 1400 \:\umc\f$ für Kieselalgen
* \f$\V{gr} =  300 \:\umc\f$ für Grünalgen
* \f$\V{bl} = 1000 \:\umc\f$ für Blaualgen

Der Koeffizient \f$\qsed\f$ in Glg. \f$\eqref{eq:w_eff}\f$ wird wie folgt 
berechnet:
\f{equation}{
 \qsed = \frac{\Xsed{q} + 1}{2},    {[dimensionslos]}
\f}
mit:
\f{equation}{
 \Xsed{q} = \frac{1}{1 + e^{\Xsed{\beta} \cdot log\left( \frac{\w{i}}{\w{0}} 
 \right)}}.    
 {[dimensionslos]}
\f}
Hier beschreibt \f$\w{0}\f$ eine empirische Sinkgeschwindigkeit in Abhängigkeit 
von der Schubspannungsgeschwindigkeit \f$\ust\f$:
\f{equation}{
 \w{0} = 0.14 {\ust}^2 + 0.0054 \ust + 0.00000125.     {[\ms]}
\f}


<!-- Kapitel 7 --> 

# Grazing {#lnk_phy_Grazing}

*Allgemeiner Grazing Text*

## Rotatorien {#lnk_phy_Rotatorien}

*Text zu Grazing durch Rotatorien... *

## *Dreissena spp.* {#lnk_phy_Dreissena}

*Text zu Grazing durch *Dreissena spp.* ... *

## *Corophium* {#lnk_phy_Corophium}

*... Text zu Grazing durch *Corophium* *

## Heterotrophe Nanogflagellaten {#lnk_phy_HNF}

*... Text zu Grazing durch heterotrophe Nanoflagellaten*
 
 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: phyto-prozess.md; Codesource: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück: \ref lnk_phytoplankton