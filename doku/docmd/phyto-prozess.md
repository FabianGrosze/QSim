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
  - [*Dreissena spp.*](\ref lnk_phy_Dreissena)
  - [*Corophium*](\ref lnk_phy_Corophium)
  - [Heterotrophe Nanoflagellaten](\ref lnk_phy_HNF)
  
<!-- Test ob es funktioniert
\f$ \ccSum{k=0}{d}{text} \f$ <br>

und dies \f$ \E^2 \f$  -->
  
# Grundgleichung und Übersicht {#lnk_phy_Grundgleichung}
Die Veränderung der Biomasse der Algengruppe \f$i\f$ (\f$A_i\f$) wird in QSim über 
folgende Differentialgleichung beschrieben:
\f[
 \frac{dA_i}{dt} = \left( \mu_{A,i} - r_{A,i}^{resp} - r_{A,i}^{mor} \right) 
 \cdot A_i - {SED}_{A,i} - {GRA}_{A,i}^{ROT} - 
 {GRA}_{A,i}^{DR} - {GRA}_{A,i}^{COR} - 
 {GRA}_{A,i}^{HNF} + {ADV} + {MIX}. \label{Eq:dAdt}
\f]

Die einzelnen Terme beschreiben dabei die folgenden Prozesse, welche in den 
entsprechenden Unterkapiteln dieser Dokumentation genauer beschrieben werden; 
ausgenommen Advektion (ADV) und Vermischung (MIX):
\f$ P_{A,i} \f$:     Wachstum [\f$ d^-1 \f$], abhängig von \n
  - Licht (s. Kapitel \ref lnk_phy_licht)
  - Nährstoffen (s. Kapitel \ref lnk_phy_Naehrstoffe)
  - Temperatur (s. Kapitel \ref lnk_phy_Temperatur)
  - Respiration (\f$r_{A,i}^{resp} \cdot A_i\f$; s. Kapitel \ref lnk_phy_Respiration)
  - Mortalität (\f$r_{A,i}^{resp} \cdot A_i\f$; s. Kapitel \ref lnk_phy_Mortalitaet)
  - Sedimentation (SED\f$_{A,i}\f$; s. Kapitel \ref lnk_phy_Sedimentation)
  - Grazing durch
	* Rotatorien (GRA\f$_{A,i}^{ROT}\f$; s. Kapitel \ref lnk_phy_Rotatorien)
	* it{Dreissena spp.} (GRA\f$_{A,i}^{DR}\f$; s. Kapitel \ref lnk_phy_Dreissena)
	* it{Corophium} (GRA\f$_{A,i}^{COR}\f$; s. Kapitel \ref lnk_phy_Corophium)
	* Heterotrophe Nanoflagellaten (GRA\f$_{A,i}^{HNF}\f$; s. Kapitel \ref lnk_phy_HNF)

Die Vorbelegung der Algenbiomasse sowie die Nährstoff:Biomasse- und 
Kohlenstoff:Chlorophyll-a-Verhältnisse zu Beginn einer Simulation werden im 
Kapitel \ref phy_vorbelegung beschrieben.

<!-- Kapitel 3 -->

# Wachstum {#lnk_phy_wachstum}

Die Brutto-Primärproduktion wird in QSim über eine lichtabhängige 
Photosyntheserate beschrieben, welche zusätzlich durch Lichthemmung, die 
Verfügbarkeit von Nährstoffen sowie die Umgebungstemperatur beeinflusst wird:

\f[
 {BPP}_{A,i} = \mu_{A,i} \cdot A_i = \left( P_{A,i} \cdot f_{LH,i} \cdot 
 f_{N\ddot{a}hr,i} \cdot f_{T,i}^\mu \right) \cdot A_i 
\f]
<!--  \label{Eq:Growth} -->
Hier sind 

<!-- debugging 
\f$ BPP_{A,i} \f$:  Brutto-Primärproduktion [\f$ mg L^{-1} d^{-1} \f$] \n
\f$ \mu_{A,i} \f$:  Brutto-Wachstumsrate [d\f$^{-1}\f$] \n
\f$ P_{A,i}   \f$:  lichtabhängige Brutto-Photosyntheserate (ohne Lichthemmung) [d\f$^{-1}\f$] \n 
\f$ f_{LH,i}  \f$:  Lichthemmungsfakor [-] (s. \ref lnk_phy_Lichthemmung) \n 
\f$ f_{N\ddot{a}hr,i}\f$: Nährstofflimitierungsfaktor [-] (s. \ref lnk_phy_NutLim) \n 
\f$ f_{T,i}^\mu \f$: Temperaturabhängiger Wachstumsfaktor [-] (s. \ref lnk_phy_Temperatur) \n

debugging eng -->

## Lichtabhängige Photosyntheserate und Unterwasserlicht {#lnk_phy_licht}

*Intro-Text für lichtabhängige Photosynthese* \n

<!-- debugging
In QSim wird die lichtabhängige Photosyntheserate (ohne Lichthemmung) wie folgt 
berechnet:

\f[
 P_{A,i} = P_{A,i}^{max} \cdot \left(1 - e^{-\frac{\Izm}{\Ik} \cdot \frac{\CChld}{\CChl}} 
 \right), \quad \quad {[\dinv]} \label{Eq:Pcp}
\f]

mit der maximalen lichtanhängigen Photosyntheserate P_{A,i}^{max}\ (ohne Lichthemmung; 
in d\f$^{-1}\f$) bei der optimalen Temperatur \Topt{i} (in °C):

\f[
 P_{A,i}^{max} = \frac{\mu_{A,#1}^{max}{i} \cdot e^{\kTmu{i} \cdot \left( \Tref - \Topt{i} 
 \right)^2} + \respd{i}(\Topt{i}) }{1 - \respg{i}}. \quad \quad {[\dinv]} 
 \label{Eq:Pcpmax}
\f]

Hier beschreiben \mu_{A,#1}^{max}{i} die maximale Brutto-Wachstumsrate (in d\f$^{-1}\f$) bei der 
Referenztemperatur $\Tref = 20$\,°C, \kTmu{i} den Temperaturkoeffizienten des 
Wachstums (nach {James2010}; s. Kapitel~\ref lnk_phy_Temperatur), \respd{i} 
die Respirationsrate dunkeladaptierter Algen (in d\f$^{-1}\f$; s. 
Kapitel~\ref lnk_phy_Respiration) und \respg{i} die Respirationskosten des Wachstums 
(dimensionslos; s. Kapitel~\ref lnk_phy_Respiration). Die maximale Wachstumsrate 
\mu_{A,#1}^{max}{i} ist ein Eingabeparamter in QSim, dessen Standardwerte wie folgt sind:

* $\mu_{A,#1}^{max}{ki} = 1.6$ für Kieselalgen
* $\mu_{A,#1}^{max}{gr} = 1.2$ für Grünalgen
* $\mu_{A,#1}^{max}{bl} = 1.2$ für Blaualgen

In Glg.~\eqref{Eq:Pcp} beschreibt \Izm\ die mittlere Lichtintensität (in \uEms), 
die das Phytoplankton infolge von vertikaler Durchmischung erfährt. Die 
Berechnungen der Sättigungsintensität \Ik{i} sowie des aktuellen 
C:Chl-a-Verhältnisses \CChl\ und dessen dunkeladaptierter Algen \CChld{i} (beide 
in \gCmgChl) sind in Kapitel~\ref lnk_phy_Lichtadaption detailiert 
beschrieben.\par

Die mittlere Lichtintensität, die eine Algenzelle erfährt, berechnet sich in 
QSim in Abhängigkeit von ihrer Position und ihrer vertikalen Verdriftung 
innerhalb der Wassersäule:
\f[
  \Izm = \frac{I_0}{\dz} \int_{z + \dz}^z e^{-\epsilon z} dz. \quad \quad {[\uEms]} \label{Eq:Iz}
\f]
Die Tiefe $z$ (in m) entspricht hier dem oberen Ende der Verdriftungstrecke 
\dz\ (in m). Die an der Gewässeroberfläche verfügbare photosynthetisch aktive 
Strahlung $I_0$ (PAR; in \uEms) wird in QSim aus der einfallenden 
Netto-Globalstrahlung $S_0$ (in \calms) wie folgt berechnet:
\f[
  I_0 = 5.846 \cdot 4.2 \cdot S_0. \quad \quad {[\uEms]}
\f]
Die Netto-Globalstrahlung $S_0$ entspricht dabei dem nicht reflektierten Anteil 
der kurzwelligen Gesamteinstrahlung, welcher vom Einfallswinkel und somit von 
Tages- und Jahreszeit abhängt. $S_0$ wird in QSim in \mbtt{strahlg.f90} 
berechnet. Der Faktor 4.2 bewirkt die Umrechnung von \calms\ in \Wm, während 
der Faktor 5.846 für die Umrechnung von \Wm\ in \uEms\ ausgehend von einem 
Wellenlängenbereich der PAR von 400\,nm bis 700\,nm verwendet wird. 
color{red}{Hinweis: Die Herkunft des Faktors 5.846 ist unklar. 
\ref Thimijan1983 geben einen Wert von 4.57 an. Der exakte Umrechnungsfaktor 
von cal in J ist zudem 4.184.}

Die Verdriftungsstrecke \dz\ wird in QSim mit dem Ansatz von 
\citet{Falkowski1981} berechnet:
\f[
  \dz = \sqrt{2 \cdot \tau_R \cdot \Dzm}, \quad \quad {[m]}
\f]
mit der Relaxationszeit der Algen $\tau_R$, welche in QSim als 100\,s 
angenommen wird. \Dzm\ beschreibt die tiefengemittelte turbulente Diffusivität 
(in \mqs), welche für ein vertikal durchmischtes Gewässer abgeschätzt werden 
kann \citep{Vieira1993}:
\f[
  \Dzm = \frac{1}{6} \cdot \ust \cdot \kappa \cdot H. \quad \quad {[\mqs]}
\f]
Hier beschreiben \ust\ die Schubspannungsgeschwindigkeit (in \ms), $\kappa = 
0.4$ die dimensionlose von-K\'arm\'an-Konstante und $H$ die Wassertiefe (in m). \n

Der Lichtextinktionskoeffizient $\epsilon$ (in \minv) in Glg.~\eqref{Eq:Iz} 
berechnet sich in QSim als:
\f[
  \epsilon = \epsm{SS} \cdot ({SS} + {ZOO}) + \epsm{Chl-a} \cdot {Chl-a} + \epsm{W} + \epsm{H}, \quad \quad {[\minv]} \label{Eq:ext}
\f]
mit dem mittleren Extinktionskoeffizienten für Schwebstoff $\epsm{SS}$ 
(in\Lmmg), den Schwebstoff- und Zooplanktonkonzentrationen SS bzw. ZOO (in mg 
L\f$^{-1}\f$), dem mittleren Extinktionskoeffizienten für Chlorophyll-a 
$\epsm{Chl-a}$ (in \LmugChl), der Chlorophyll-a-Konzentration Chl-a (in 
\ugChlL), sowie den mittleren Extinktionskoeffizienten von Wasser $\eps{W}$ und 
Huminstoffen $\eps{H}$ (beide in \minv). $\eps{Chl-a}$ berechnet sich dabei als 
gewichtetes Mittel der Extinktionskoeffizienten der drei Algenklassen:
\f[
  \epsm{Chl-a} = \frac{\sum_{i=1}^3 \epsc{i} \cdot \Chl}{\sum_{i=1}^3 \Chl}. \quad \quad {[\minv]} \label{Eq:ext_chla}
\f]
Die verschiedenen Extinktionskoeffizienten (s. Glgn.~\eqref{Eq:ext} und 
\eqref{Eq:ext_chla}) entsprechen dabei (mit Ausnahme von \eps{H}) jeweils den 
mittleren Extinktionskoeffizienten im Wellenlängenbereich von 400\,nm bis 
700\,nm. Diese werden in QSim aus den wellenlängenspezifischen Werten (in 
10-nm-Schritten), welche aus der Datei \mbtt{e\_extnct.dat} eingelesen werden, 
berechnet. Für die Huminstoffe berechnet QSim die wellenlängenabhängigen 
$\eps{H}(\lambda)$ als:
\f[
  \eps{H}(\lambda) = \eps{H}(\lambda_0) \cdot e^{-k_\lambda \cdot \left( \lambda - \lambda_0 \right)},
\f]
welche anschließend ebenfalls über den Wellenlängenbereich von 400\,nm bis 
700\,nm gemittelt werden. Hier bezeichnet $\eps{H}(\lambda_0)$ den 
Extinktionskoeffizienten der Huminstoffe bei $\lambda_0 = 440$\,nm (in \minv), 
welcher ein Eingabeparameter ist (Standardwert: 0.75). Der Koeffizient 
$k_\lambda = 0.016$\,n\minv\ ist in QSim hart codiert. color{red}{Hinweis: 
Die Herkunft dieser Berechnung ist unklar. Weiterhin verwendet QSim faktisch 
den Wellenlängenbereich von 395 bis 705\,nm, da es 31 Werte in 10-nm-Schritten 
beginnend bei 400 und endend bei 700 einliest.}

end debuggin -->

### Einfluss von Lichtadaption {#lnk_phy_Lichtadaption}

Text zu Lichtadaption\n

<!-- debugging
Die Lichtadaption der Algen wird in QSim über die Veränderung des 
Chlorophyll-a:C-Verhältnis nach \citet{Geider1997} beschrieben. Genauer erfolgt 
dies über die Veränderung der Chlorophyll-a-Synthese relativ zur C-Aufnahme 
durch Photosynthese. Die Chlorophyll-a-Synthese ist in QSim wie folgt 
beschrieben:
\f[
  \frac{d\Chl}{dt} = \rChlC \cdot {C:Bio}_{A,i} \cdot \mu_{A,i} \cdot A_i \quad \quad {[\ugChlLd]} \label{Eq:dChla_dt},
\f]

mit dem aktuellen Verhältnis von Chlorophyll-a- zu C-Synthese \rChlC\ (in 
\mgChlgCd), welches in Anlehnung an \citet{Geider1997} berechnet wird:
\f[
  \rChlC =  \frac{1}{\CChld(T)} \cdot \frac{\CChl \cdot P_{A,i}}{\alphaCChl \cdot \Izm}. \quad \quad {[\mgChlgCd]} \label{Eq:ChlaC_Synthese}
\f]
mit dem chlorophyll-a-spezifischen Anfangsanstieg der lichtabhängigen 
Wachstumsfunktion \alphaCChl\ (in \gCmqsmgChluE), welcher in QSim wie folgt 
berechnet wird:
\f[
  \alphaCChl = \frac{P_{A,i}^{max} \cdot f_{T,i}^\mu \cdot \CChld(T)}{\Ik(T)}, \quad \quad {[\gCmqsmgChluE]}
\f]
mit der Sättigungsintensität $\Ik(T)$ (in \uEms) bei Temperatur $T$, welche 
über eine empirische Gleichung berechnet wird:
\f[
  \Ik(T) =  \ak{i} \cdot \Ik(\Tref) \cdot e^{\bk{i} \cdot T}, \quad \quad {[\uEms]} \label{Eq:I_k}
\f]
mit der Sättigungsintensität $\Ik(\Tref)$ bei $\Tref = 20$\,°C, welche ein 
Eingabeparameter ist (in \uEms), sowie den Koeffizienten \ak{i} (dimensionslos) 
und \bk{i} (in \f$ circ$C$^{-1} \f$), welche in QSim die folgenden Werte 
annehmen:

* $\ak{ki} = 0.837$, $\bk{ki} = 0.0089$ für Kieselalgen
* $\ak{gr} = 0.183$, $\bk{gr} = 0.0848$ für Grünalgen
* $\ak{bl} = 0.525$, $\bk{bl} = 0.0322$ für Blaualgen

color{red}{Hinweis: Die Koeffizienten in Glg.~\eqref{Eq:I_k} sind in QSim 
in den jeweiligen Algenroutinen hart codiert und teilweise durch 
\mbtt{if}-Konstruktion mit \mbtt{real}-Vergleichen gesetzt. Es wird davon 
ausgegangen, dass diese \mbtt{real}-Vergleich nie greifen und somit die im Code 
gesetzten Standardwerte verwendet werden. Die Herkunft der verwendeten Werte 
ist unklar.}\par

Die C-Synthese in QSim berechnet sich analog zu Glg.~\eqref{Eq:dChla_dt} als:
\f[
  \frac{d\CA}{dt} = \CBio{i} \cdot \mu_{A,i} \cdot A_i, \quad \quad {[\mgCLd]} \label{Eq:dC_dt}
\f]
mit der C-Konzentration der Algenklasse \f$i\f$ \CA (in 
{mg\,#1\,L$^{-1}$}{C}). Über einen Berechnungszeitschritt $\Delta t$ 
ergeben sich dann aus Glg.~\eqref{Eq:dChla_dt} und Glg.~\eqref{Eq:dC_dt}:
\begin{align}
\Chl(t+\Delta t) &= \Chl(t) \cdot e^{\CChld(T) \cdot \rChlC \cdot \mu_{A,i} \cdot \Delta t}, \quad \quad {[\ugChlL]} \label{Eq:Chla_neu}\\
\CA(t+\Delta t)  &= \CA(t)  \cdot e^{\mu_{A,i} \cdot \Delta t}. \quad \quad {[\{mg\,#1\,L$^{-1}$}{C}]}. \label{Eq:C_neu}
\end{align}
Unter Verwendung der Chlorophyll-a-Syntheserate $\mu_{A,i}^{Chl-a} = 
\CChld(T) \cdot \rChlC \cdot \mu_{A,i}$ (in d\f$^{-1}\f$) und durch Division 
von Glg.~\eqref{Eq:C_neu} durch Glg.~\eqref{Eq:Chla_neu} ergibt sich schließlich:
\begin{align}
\frac{\CA(t+\Delta t)}{\Chl(t+\Delta t)} &= \frac{\CA(t)}{\Chl(t)} \cdot e^{\mu_{A,i} - \mu_{A,i}^{Chl-a}},\nonumber\\
\CChl(t+\Delta t) &= \CChl(t) \cdot e^{\mu_{A,i} - \mu_{A,i}^{Chl-a}}. \quad \quad {[\gCmgChl]}.
\end{align}

end debugging -->

### Einfluss von Lichthemmung  {#lnk_phy_Lichthemmung}
Intro-Text zu Lichthemmung\n

<!-- debugging
Die Lichthemmung (oder Photoinhibition) des Algenwachstums wird in QSim über 
die relative Konzentration des D1-Proteins, $f_{LH,i} = \theta_{{D1},i}$, 
beschrieben. Die zeitliche Änderung von \theta_{{D1},i}\ wird dabei 
entsprechend des Ansatzes von \citet{Han2000} berechnet:
\f[
  \frac{d\theta_{{D1},i}}{dt} = -k_{d} \cdot \Izm \cdot \sigma_{PSII,#1}{i} \cdot \theta_{{D1},i} + k_r \cdot \left( 1 - \theta_{{D1},i} \right). \quad \quad {[dimensionslos]} \label{Eq:D1}
\f]
mit der Schädigungskonstante $k_d=1.04\cdot10^{-8}$ (dimensionslos) und der 
Reparaturrate $k_r = 4.5\cdot10^{-5}$ (in s$^{-1}$). color{red}{Die 
genannten Werte entsprechen den in QSim verwendeten Werten. \citet{Han2000} 
nennen einen Bereich von $5.6\cdot10^{-5} \leq k_r \leq 2.2\cdot10^{-4}$, 
welcher vom Wert in QSim unterschritten wird.} \f$I\f$ beschreibt das aktuelle 
PAR (in \uEms) und \sigma_{PSII,#1}{i} beschreibt die Absorptionsfläche der 
Algen (in \mquE) in Abhängigkeit vom aktuellen C:Chl-a-Verhältnis:
\f[
  \sigma_{PSII,#1}{i} = \sigma_{PSII,#1}{0} \cdot \left( \frac{\CChld}{\CChl} \right)^\kappa_{PSII}. \quad \quad {[\mquE]}
\f]
Hier bezeichnet $\sigma_{PSII,#1}{0} = 1.5$\,\mquE\ die Absorptionsfläche 
dunkeladaptierter Algen und \kappa_{PSII}\ ist ein empirischer Exponent, 
welcher in QSim auf 0.22 gesetzt ist. Der Bruchterm kann als Schließungsgrad 
des Photosystems II interpretiert werden. color{red}{Hinweis: Die 
Abhängigkeit von \sigma_{PSII,#1}{i} vom C:Chl-a-Verhältnis ist in QSim zwar 
codiert, aber nicht korrekt angewendet, weshalb immer gilt: 
$\sigma_{PSII,#1}{i} = \sigma_{PSII,#1}{0} = 1.5$. Der Wert 
$\kappa_{PSII}=0.22$ stammt laut \mbtt{lichthemmung.f90} von einem Fit zu Daten 
von \citet{Anning2000}, genaue Herkunft ist aber unklar.}\par

Die Differentialgleichung~\eqref{Eq:D1} wird in QSim mit einem 
Runge-Kutta-Verfahren 4. Ordnung gelöst. color{red}{Hinweis: Es ist 
unklar, warum RK4 notwendig ist; zudem ist dieses falsch implementiert, s. 
\mbtt{lichthemmung.f90}.}

end debugging --> 

## Nährstoffabhängigkeit des Wachstums {#lnk_phy_Naehrstoffe}
Text zu Nährstoffabhängigkeit\n


### Nährstofflimitierung {#lnk_phy_NutLim}
Die Limitierung des Algenwachstums wird über einen Nährstofflimitierungsfaktor 
beschrieben:

<!-- debugging
\f[
  f_{N\ddot{a}hr,i} = min\left( f_{{#1},i}{N}, f_{{#1},i}{P}, f_{{#1},i}{Si} \right), \quad \quad {[dimensionslos]} \label{Eq:f_nut}
\f]
wobei die verschiedenen Faktoren (f_{{#1},i}{N} etc.) die Limitierung 
aufgrund von Stickstoff (N), Phosphor (P) und Silikat (Si) beschreiben. 
Letzterer ist nur für Kieselalgen von Bedeutung. Die Berechnung dieser 
Limitierungsfaktoren erfolgt in Abhängigkeit der gewählten Eingabeparameter für 
die minimalen (\Qmin) und maximalem zellinternen 
Nährstoff:Biomasse-Verhältnisse der Algen (\Qmax{X}):
\f[
  f_{{#1},i}{$X$} =
\begin{cases}
% case 1: Qmax > 1.25*Qmin
\frac{\Q - \Qmin}{\Qmax{X} - \Qmin} \quad & \quad {wenn} \quad \Qmax{X} > 1.25 \cdot \Qmin,\\
% case 2: otherwise
\frac{X}{\khs{$X$}{i} + X} \quad & \quad {sonst.}
\end{cases} \quad \quad {[dimensionslos]} \label{Eq:NutLimX}
\f]
\Qmin\ und \Qmax{X} sind jeweils Eingabeparameter (in \gXg{$X$}), wobei $X$ für 
den jeweiligen Nährstoff steht (N, P oder Si). Der beschriebene erste Fall 
entspricht dem Modell nach \citet{Geider1998}. Im beschriebenen zweiten Fall 
wird die Nährstofflimitierung in Form eines Michaelis-Menten-Ansatzes auf Basis 
der Nährstoffkonzentrationen im Wasser und einer Halbsättigungskonstante 
\khs{$X$}{i} in {mg\,#1\,L$^{-1}$}{$X$}) beschrieben, welche ebenfalls ein 
Eingabeparameter ist. color{red}{Hinweis: QSim wendet für die Berechnung 
des Nährstofflimitierungsfaktors Glg.~\eqref{Eq:NutLimX} für N, P und Si an. 
Hingegen wird für Si-Aufnahme kein zellinterner Speicher angewendet. Dies 
scheint inkonsistent und sollte geprüft werden.}

end debugging -->

### Zellinterne Nährstoffspeicher für N und P {#lnk_phy_NutStore}
QSim berechnet die zeitliche Änderung der zellinternen 
Nährstoff:Biomasse-Verhältnisse der Algen nach [Droop 1973](\ref lnk_ext_literatur) :

<!-- debugging

\f[
  \frac{d\Q}{dt} = -\mu_{A,i} \cdot \Q + \UP,
\f]
wobei \UP\ die Veränderung des zellinternen Nährstoff:Biomasse-Verhältnisses 
des Nährstoffs $X$ (N oder P) aufgrund der Nährstoffaufnahme beschreibt. Die 
Berechnung von \UP\ ist analog zur Nährstofflimitierung (s. 
Glg.~\eqref{Eq:f_nut}) in QSim abhängig von den gesetzten Werten für \Qmax{X} 
und \Qmin:
\f[
  \UP =
\begin{cases}
% case 1: Qmax > 1.25*Qmin
\UPmax \cdot \frac{1 - \frac{\Q}{\Qmax{X}}}{1 - \frac{\Q}{\Qmax{X}} + 0.01} \cdot \frac{X}{\khs{$X$}{i} + X} \quad & \quad {wenn} \quad \Qmax{X} > 1.25 \cdot \Qmin,\\
% case 2: otherwise
\Qmax{X} \cdot \left( \mu_{A,i} - \resp \right)\cdot \frac{A_i}{A_i^*} \quad & \quad {sonst.}
\end{cases} \quad \quad {[\gXgd{$X$}]} \label{Eq:up_nut}
\f]
Der erste Fall folgt dem Ansatz von \citet{Flynn2001}. Hier beschreibt \UPmax\ 
die maximale Änderungsrate des Nährstoff:Biomasse-Verhältnisses. 
color{red}{Hinweis: Herkunft des Werts 0.01 im Nenner unklar. 
\citet{Flynn2001} verwendet andere Werte mit Unterschieden zwischen den 
verschiedenen Nährstoffen.} Der zweite Fall impliziert ein 
Nährstoff:Biomasse-Verhältnis $\Q\ = \Qmax{X} = const.$ Hier beschreibt \resp\ 
die Respirationsrate (in d\f$^{-1}\f$); $A_i$ und $A_i^*$ repärsentieren die 
Algenbiomassen am Anfang und am Ende eines Berechnungszeitschritts aufgrund des 
Netto-Wachstums. color{red}{Hinweis: Diese Berechnung rechnet in QSim 
fälschlicherweise zweimal den Biomasseverlust aufgrund der Respiration ein.} 
Die Rate \UPmax\ berechnet sich unter Berücksichtigung der 
Temperaturabhängigkeit nach \citet{James2010} als:
\f[
  \UPmax = \Qmax{X} \cdot f_{T,i}^\mu \cdot \mu_{A,#1}^{max}{i} \cdot e^{\kTmu{i} \cdot\left( \Tref - \Topt{i} \right)^2}. \quad \quad {[\gXgd{X}]}
\f]
Die Berechnung des Temperaturfaktors f_{T,i}^\mu\ auf Basis der Parameter 
\kTmu{i} und \Topt{i} (beides Eingabeparameter) sind in Kapitel~\ref 
lnk_phy_Temperatur beschrieben.\par

Für Silizium verwendet QSim keinen zellinternen Speicher, weshalb die 
Veränderung des Silizium:Biomasse-Verhältnisses in Abhängigkeit von der 
Umgebungskonzentration des Silikats (in {mg\,#1\,L$^{-1}$}{{Si}}) 
analog zum zweiten Fall in Glg.~\eqref{Eq:up_nut} berechnet wird:
\f[
  {UP}_{{Si},i} = \Qmax{{Si}} \cdot \frac{{Si}}{\khs{Si}{i} + {Si}} \cdot \left( \mu_{A,i} - \resp \right)\cdot \frac{A_i}{A_i^*}  \quad \quad {[\gXgd{{Si}}]} \label{Eq:up_Si}
\f] 

end debugging -->

## Temperaturabhängigkeit des Wachstums {#lnk_phy_Temperatur}
Die Temperaturabhängigkeit des Wachstums wird in QSim über einen Faktor 

<!-- debugging

$f_{T,i}^\mu$ nach \citet{James2010} beschrieben:
\f[
  f_{T,i}^\mu = e^{-\kTmu{i} \cdot\left( T - \Topt{i} \right)^2}. \quad \quad {[dimensionslos]} \label{Eq:fT_growth}
\f]
Der Koeffizient \kTmu{i} (in \f$^\circ$C$^{-2}\f$) und die optimale Temperatur 
\Topt{i} (in °C) sind Eingabeparameter in QSim, deren Standardwerte durch den 
Fit von Glg.~\eqref{Eq:fT_growth} an verschiedene Datensätze ermittelt wurden. 
Diese Standardwerte sind wie folgt:

* $\kTmu{ki} = 0.0065$, $\Topt{ki} = 20.3$ für Kieselalgen
* $\kTmu{gr} = 0.0041$, $\Topt{gr} = 30.2$ für Grünalgen
* $\kTmu{bl} = 0.0069$, $\Topt{bl} = 23.7$ für fadenbildende Blaualgen
* $\kTmu{bl} = 0.0081$, $\Topt{bl} = 31.8$ für koloniebildende Blaualgen

-->

<!-- Kapitel 4 -->

# Respiration {#lnk_phy_Respiration}
Introtext zur Respiration\n
<!-- Debugging 
Die Gesamtrespiration der Algenklasse \f$i\f$ errechnet sich in QSim als:
\f[
 \RESP = \resp \cdot A_i, \quad \quad 
\f]

\f$ \RESP \f$: Gesamtrespiration der Algenklasse *i* [mg L\f$^{-1}\f$d\f$^{-1}\f$]

mit der Gesamtrespirationsrate \resp\ (in d\f$^{-1}\f$) und der Algenbiomasse 
\f$A_i\f$ (in mg L\f$^{-1}\f$d\f$^{-1}\f$). Die Gesamtrespirationsrate ergibt 
sich aus der Grundrespirationsrate dunkeladaptierter Algen (\respd) und der 
wachstumsbedingten Respiration ($\\respg{i} \cdot \mu_{A,i}$):
\f[
 \resp = \respd(T) + \respg{i} \cdot \mu_{A,i}. \label{Eq:r_resp} \quad \quad {[d$^{-1}$]}
\f]
color{red}{Hinweis: Diese Gleichung passt nicht zu der Berechnung des 
Wachstums, wenn man die Gleichungen ineinander einsetzt.}\par

Die Grundrespiration dunkeladaptierter Algen \respd\ in Abhängigkeit der 
Temperatur $T$ ist wie folgt beschrieben:
\f[
 \respd(T) = \respd( \Tref ) \cdot e^{\kTresp{i} \cdot \left( T - \Tref \right)}. \quad \quad {[\dinv]}
\f]
Hier repräsentiert $\respd(\Tref)$ die Grundrespirationsrate dunkeladaptierter 
Algen bei $\Tref = 20$\,°C, welche in QSim ein Eingabeparameter (in 
d\f$^{-1}\f$) ist. Der Koeffizient der Temperaturabhängigkeit, \kTresp, nimmt 
in QSim für die verschiedenen Algenklassen die folgenden Werte an (in \f$ 
circ$C$^{-1} \f$):

* $\kTresp{ki} = 0.070$ für Kieselalgen
* $\kTresp{gr} = 0.058$ für Grünalgen
* $\kTresp{bl} = 0.090$ für Blaualgen

color{red}{Hinweis: Diese Werte sind in den Algenroutinen hart codiert und 
ihre Herkunft ist nicht geprüft.}\par

Im wachstumsbedingten Term in Glg.~\eqref{Eq:r_resp} beschreibt der Faktor 
\respg{i} beschreibt die anteilige Respiration infolge von Wachstum und ist in 
QSim ein Eingabeparameter mit einem Standardwert von 0.2 (dimensionslos).

debugging -->

<!-- Kapitel 5 --> 

# Mortalität {#lnk_phy_Mortalitaet}
*Introtext zur Mortalität* \n
<!-- debugging
Die Gesamtmortalität (ohne Grazing) der Algenklasse \f$i\f$ errechnet sich in 
QSim als:
\f[
 \MOR = \mor \cdot A_i, \quad \quad 
\f]

\f$ \MOR \f$: Gesamtmortalität [mg L\f$^{-1}\f$d\f$^{-1}\f$]

mit der Gesamtmortalitätsrate \mor\ (in d\f$^{-1}\f$), welche sich aus der 
Grundmortalitätsrate der Algenklasse (\morX{0}) und einer nährstoffabhängigen 
Mortalität ($\morX{{NP}} \cdot \fmorX{{NP}}$) berechnet:
\f[
 \mor = \morX{0} + \morX{{NP}} \cdot \fmor. \quad \quad {[\dinv]} \label{Eq:r_mort}
\f]
Die Grundmortalität ist in QSim für alle Algenklassen auf $\morX{0} = 
0.02$\,\dinv\ gesetzt. Die nährstoffabhängige Mortalität in 
Glg.~\eqref{Eq:r_mort} berechnet sich für alle Algenklassen über eine maximale 
Rate $\morX{{NP}} = 0.8$\,\dinv\ und einen Nährstofffaktor (unter 
Vernachlässigung des Einflusses von Si):
\f[
 \fmor = 1 - \left( \frac{min\left( \fmorX{0}, f_{{#1},i}{N}, f_{{#1},i}{P} \right)}{ \fmorX{0} } \right)^8  \quad \quad {[dimensionslos]}\label{Eq:f_morn}
\f]

Der minimale Skalierungsfaktor \fmorX{0} nimmt in QSim für alle Algenklassen 
den Wert 0.05 an.\par

color{red}{Hinweis: $\morX{0} = 0.02$, $\morX{{NP}} = 0.8$ und 
$\fmorX{0} = 0.05$ hart codiert und Herkunft unbekannt. Der Exponent 8 in 
Glg.~\eqref{Eq:f_morn} ist ebenfalls hart codiert und seine Herkunft unbekannt. 
Sollte Si-Einfluss besser auch berücksichtigt werden?}\par

In QSim wird zudem die aktuelle Gesamtmortalitätsrate mit dem entsprechenden 
Wert des vorangegangenen Zeitschritts verglichen:
\f[
 \mor(t) = max\left( \mor(t), \mor(t-\Delta t) \right). \quad \quad {[\dinv]}
\f]
color{red}{Hinweis: Diese Berechnung soll bewirken, dass sich eine erhöhte 
Gesamtmortalitätsrate durch Zugabe von Nährstoffen nicht wieder verringert. In 
dieser Form, unterbindet diese Gleichung jedoch eine Zunahme der 
Gesamtmortalitätsrate, wenn $min\left(f_{{#1},i}{N}, f_{{#1},i}{P} 
\right) < \fmorX{0}$ und weiter abnimmt (s. Glg.~\eqref{Eq:f_morn}).}

debugging end --> 

<!-- Kapitel 6 -->


# Sedimentation von lebenden Algen {#lnk_phy_Sedimentation}
Introtext zur Sedimentation. color{red}{(Hinweis: Die gesamte Berechnung 
ist sehr kryptisch und ihr Herkunft unbekannt. Die Beschreibungen der Variablen 
sind so weit möglich aus dem Code in `sedimentation.f90` zusammengereimt 
und teilweise kreativ benannt.)}\par
<!-- .. was \mbtt{sedimentation.f90} -->
<!-- debugging

In QSim können Algen absinken und am Boden sedimentieren. Die Berechnung des 
Sedimentationsfluxes findet dabei über eine empirische Formel statt:
\f[
 \SED = \left( 1 - \Xsed{q} \right) \cdot \fsed{i} \cdot \Xsed{a}  \cdot A_i  \label{Eq:sed}
\f]

\f$ \SED \f$: Sedimentation [mg L\f$^{-1}\f$]

Hier beschreibt \fsed{i} den sedimentierbaren Anteil der Algenklasse \f$i\f$ 
(dimensionslos; 0--1), welcher in QSim ein Eingabeparameter ist. \Xsed{a} 
bezeichnet den sedimentierten Anteil (dimensionslos; 0--1) und \Xsed{q} 
beschreibt einen empirisch berechneten Koeffizienten (dimensionslos; $0 < 
\Xsed{q} < 1$). Der sedimentierte Anteil \Xsed{a} wird wie folgt berechnet:
\f[
 \Xsed{a} = 1 - e^{-\frac{\Xsed{\gamma} \cdot \wEff \cdot \Delta t}{H}}. \quad \quad {[dimensionslos]} \label{Eq:a_sed}
\f]
Der dimensionslose Koeffizient \Xsed{\gamma} hat in QSim den Wert 
$\Xsed{\gamma} = 0.5$. Die effektive Sinkgeschwindigkeit \wEff\ (in \ms) wird 
ebenfalls über eine empirische Gleichung berechnet:
\f[
 \wEff = 10^{\frac{1}{\Xsed{\beta}} \cdot \left( ln\left(\Xsed{\alpha}\right) - ln \left(  \frac{1}{\qsed} -1 \right)\right)} \cdot min\left( 1, e^{-604.2 \cdot\ust} \right), \quad \quad {[\ms]} \label{Eq:w_eff}
\f]
mit dem Koeffizienten $\Xsed{\beta} = 2.7$ (dimensionslos). Der Koeffizient 
\Xsed{\alpha} berechnet sich in Abhängigkeit von der mittleren 
Sinkgeschwindigkeit \w{i}\ der Algenklasse $i$:
\f[
 \Xsed{\alpha} = e^{\Xsed{\beta} \cdot log(\w{i})}, \quad\quad {[dimensionslos]}
\f]
wobei \w{i} über eine empirische Funktion in Abhängigkeit des mittleren 
Zellvolumnes \V{i} der Algenklasse \f$i\f$ (in \umc) berechnet wird:
\f[
 \w{i} = 10^{2.0155\cdot log(\V{i}) - 11.512}. \quad \quad {[\ms]}
\f]
Die mittleren Zellvolumina sind in QSim wie folgt color{red}{(im 
jeweiligen Baustein hart codiert)}:

* $\V{ki} = 1400$\,\umc\ für Kieselalgen
* $\V{gr} =  300$\,\umc\ für Grünalgen
* $\V{bl} = 1000$\,\umc\ für Blaualgen

Der Koeffizient \qsed\ in Glg.~\eqref{Eq:w_eff} wird wie folgt berechnet:
\f[
 \qsed = \frac{\Xsed{q} + 1}{2}, \quad\quad {[dimensionslos]}
\f]
mit:
\f[
 \Xsed{q} = \frac{1}{1 + e^{\Xsed{\beta} \cdot log\left( \frac{\w{i}}{\w{0}} \right)}}. \quad\quad {[dimensionslos]}
\f]
Hier beschreibt \w{0} eine empirische Sinkgeschwindigkeit in Abhängigkeit von 
der Schubspannungsgeschwindigkeit \ust:
\f[
 \w{0} = 0.14 {\ust}^2 + 0.0054 \ust + 0.00000125. \quad \quad {[\ms]}
\f]

debugging end -->

<!-- Kapitel 7 --> 

# Grazing {#lnk_phy_Grazing}
*Allgemeiner Grazing Text*

## Rotatorien {#lnk_phy_Rotatorien}
*... Text zu Grazing durch Rotatorien*

## it{Dreissena spp.} {#lnk_phy_Dreissena}
*... Text zu Grazing durch *Dreissena spp.* *

## it{Corophium} {#lnk_phy_Corophium}
*... Text zu Grazing durch *Corophium* *

## Heterotrophe Nanogflagellaten {#lnk_phy_HNF}
*... Text zu Grazing durch heterotrophe Nanoflagellaten*
 
 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: phyto-prozess.md; Codesource: albenth.f90, algaesbl.f90,
algaesgr.f90; zurück: \ref lnk_phytoplankton