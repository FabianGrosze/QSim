pH - Prozesse {#lnk_ph_prozess}
===========================

Im Einzelnen werden folgende Prozesse berücksichtigt:

* \ref CO2Bilanz
   - \ref lnk_co2_austausch über die Wasseroberfläche
   - CO2-Quellen: Abbau organischer Kohlenstoffverbindungen, 
      Atmung Konsumenten (\ref co2lief)
   - CO2-Senken: Algenwachstum, Makrophytenwachstum (\ref co2verb)
* \ref Nitribit
* \ref dca , Fällung/Rücklösung
* pH-Wert Änderung (\ref pHiter)

# Gesamter, inorganischer, gelöster Kohlenstoff (DIC) {#DIC}
Die Summe des im Wasser gelösten inorganischen Kohlenstoffs ergibt sich aus 
der Differenz von Säurekapazität und Basenkapazität.
\f[ DIC = (m - p)/1000 \f]

(*[Erklärung der Formelzeichen](\ref lnk_ph_pars)*)


# Protonenkonzentration {#Proton}

Dazu werden zuerst die Protonen- und die Hydroxidkonzentration aus dem pH-Wert 
bestimmt:\n
\f[ H = 10^{(hk-pH)} \f] 
\f[ OH = 10^{(pH+hk-pk_w)} \f]
mit der temperaturabhängigen Dissoziationskonstante des Wassers: 

\f$ pk_w = (4471,33/T)+0,017053*T-6,085 \f$

und der Leitfähigkeitskorrektur:

\f$ hk =\frac{0,5*\sqrt{lf*0,000017}}{1+1,4*\sqrt{lf*0,000017}} \f$ \n


# Kohlensäureformen  {#Aufteilung}

Der DIC, (\ref DIC) wird nun abhängig von Temperatur, Leitfähigkeit und pH-Wert 
auf die drei Fraktionen, Kohlendioxid, Hydrogencarbonat und Carbonat aufgeteilt.

Die Säurekonstante, welche das Verhältnis von Kohlendioxid zu Hydrogencarbonat 
bestimmt, ist von der Temperatur und Leitfähigkeit abhängig. Für sie wird 
folgender Ansatz verwendet:

\f$ k_1 = 10^{ \{(\sqrt{lf*0,000017}/(1+1,4*\sqrt{lf*0,000017})) - 
 ((17052,0/T)+215,21*log10(T)-0,12675*T-545,56) \} } \f$

 Die Säurekonstante, welche das Verhältnis von Hydrogencarbonat zu Carbonat 
bestimmt, ist von der Temperatur und Leitfähigkeit abhängig. Für sie wird 
folgender Ansatz verwendet: 

\f$ k_2 =  10^{((2*\sqrt{lf*0,000017})/(1 + 1,4 * \sqrt{lf * 0,000017})) - 
  ((2902,39/T) + 0,02379 * T - 6,498)} \f$ \n

Damit berechnen sich dann die Kohlensäurefraktionen wie folgt:
\f[ CO_2  = DIC * \left(\frac{H^2}      {H^2 + k_1*H + k_1*k_2}\right) \f]
\f[ HCO_3 = DIC * \left(\frac{k_1*H}    {H^2 + k_1*H + k_1*k_2}\right) \f]
\f[ CO_3  = DIC * \left(\frac{k_1*k_2}  {H^2 + k_1*H + k_1*k_2}\right) \f]


# CO2-Bilanz {#CO2Bilanz}

## CO2-Gasaustauch {#lnk_co2_austausch}
Die Berechnung des Austauschs von gasförmigem CO2 über die Wasseroberfläche 
beginnt mit dem Anatz für die temperaturabhängige Sättigungskonzentration von 
CO2 im Wasser nach folgender Formel:                                                                        
\f[ {CO_2}^{eq}= -0,00000907*(T-273,16)^3 + 0,0009662*(T-273,16)^2 - 
  0,04657*(T-273,16) + 1.27  \f]
Der Wert wird in der Einheit mg/l berechnet. Änderungen der CO2-Konzentration 
in der Luft können damit nicht erfasst werden. 

Die Berechnung des Belüftungsbeiwertes \f$ b_{bei} \f$ erfolgt mit der 
Subroutine Belueftung_K2(), welche auch schon beim Austausch von Sauerstoff 
benutzt wude (\ref lnk_o2_oberflaechenaustausch). 
Die Unterschiede in der Löslichkeit von Sauerstoff und Kohlendioxyd in Wasser 
werden dabei vernachlässigt.
Die Subroutine Belueftung_K2() ermittelt in Abhängigkiet von 
Wassertiefe, Fließgeschwindigkeit, Reibungsbeiwert, Temperatur und 
Windgeschwindigkeit den Belüftungsbeiwert \f$ b_{bei} \f$ in 1/d.
Die Zu- und Abnahme von im Wasser gelöstem Kohlendioxid im jeweiligen 
Zeitschritt setzt sich dann folgendermaßen zusammen:
\f[ (\Delta CO_2)_{luft} =  ({CO_2}^{eq}/(44*1000) - CO_2) * 
  (1 - e^{(-b_{bei} \Delta t \sqrt{32/44})})  \f]

## Biologischer Kohlenstoffumsatz {#Cbio}

### CO2-Lieferung {#co2lief}

Die folgenden Prozesse geben Kohlendioxid ins Wasser ab. 
Das Formelzeichen \f$ (\Delta CO_2)_{lief}\f$ steht für die Summe dieser CO2 
liefernden Prozesse in mol/l im jeweiligen Zeitschritt.

* Der bakterielle Abbau organischer Substanzen ( \ref lnk_orgC ) erzeugt CO2.
  Dazu wird der von Bakterien mineralisierter Kohlenstoffgehalt je Zeitschritt 
  in der Subroutine orgc() berechnet und mittels der Variable \ref bsbct 
  übergeben.
  \n\n
* planktische Kiesel, Grün- und Blau- \ref lnk_phytoplankton geben in lichtlosen 
  Zeiten CO2 ab. 
  Dazu wird der Biomasseverlust pro Zeitschritt, der in den Subroutinen 
  algaeski(), algaesgr() und algaesbl() berechnet wurde, 
  mittels der Variablen  \ref dalgak,  \ref dalgag und  \ref dalgab übergeben.
  Die Multiplikation mit den Kohlenstoffgehalten der Algen  \ref Caki,  
  \ref Cagr und  \ref Cabl ergeben dann die CO2 Zunahme im Wasser.
  \n\n
* \ref lnk_albenth (Kiesel- und Grünalgen) geben in lichtlosen Zeiten CO2 ab.
  Dazu wird der Biomasseverlust pro Zeitschritt, der in der Subroutine albenth() 
  berechnet wurde, mittels der Variablen \ref alberg und \ref alberk übergeben.
  Die Multiplikation mit den Kohlenstoffgehalten der Algen \ref Caki und 
  \ref Cagr ergeben dann die CO2 Zunahme im Wasser.
  \n\n
* \ref lnk_makrophyt geben in lichtlosen Zeiten CO2 ab.
  Dazu wird der Sauerstoffverbrauch pro Zeitschritt, der in der Subroutine 
  mphyt() berechnet wurde, mittels der Variable \ref po2r übergeben und in 
  CO2-Konzentration umgerechnet.
  \n\n
* Bei der Atmung geben \ref lnk_dreissena Kohlendioxid ab.
  Dazu wird deren Biomasseverlust pro Zeitschritt, der in der Subroutine 
  dreissen() berechnet wurde, mittels der Variable \ref resdr übergeben.
  Die Multiplikation mit dem Kohlenstoffgehalten *cdr* ergibt dann die 
  CO2-Zunahme im Wasser.
  \n\n <!-- #mf: cdr war \ref cdr, ist aber nirgends definiert. Ist ein anderer
  Parameter gemeint? -->
* Bei der Atmung geben Konsumenten (\ref lnk_rotatorien) Kohlendioxid ab.
  Dazu wird der Biomasseverlust pro Zeitschritt, der in der Subroutine konsum() 
  berechnet wurde, mittels der Variablen \ref dzres1 (Grundrespiration) 
  \ref dzres2 (fraßabhängige Respiration) übergeben.
  Die Multiplikation mit den Kohlenstoffgehalt *CRot* ergibt dann die 
  CO2-Zunahme im Wasser.

### CO2-Verbrauch {#co2verb}

Die folgenden Prozesse gehören zur Primärproduktion und entnehmen Kohlendioxid 
aus dem Wasser. 
Das Formelzeichen \f$ (\Delta CO_2)_{verb} \f$ steht für die Summe dieser CO2 
verbrauchenden Prozesse in mol/l im jeweiligen Zeitschritt.

* planktische Kiesel, Grün- und Blau- \ref lnk_phytoplankton entnehmen das 
  zum Wachstum benötigte CO2 aus dem im Wasser.
  Dazu wird der Zuwach an Biomasse pro Zeitschritt, der in den Subroutinen 
  algaeski(), algaesgr() und algaesbl() berechnet wurde, 
  mittels der Variablen  \ref dalgki,  \ref dalggr und  \ref dalgbl übergeben.
  Die Multiplikation mit den Kohlenstoffgehalten der Algen \ref Caki, \ref Cagr 
  und \ref Cabl ergeben dann die CO2-Abnahme im Wasser.
  \n\n
* \ref lnk_albenth (Kiesel- und Grünalgen) entnehmen das zum Wachstum benötigte 
  CO2 aus dem im Wasser. Dazu wird der Zuwach an Biomasse pro Zeitschritt, der 
  in der Subroutine albenth() berechnet wurde,
  mittels der Variablen \ref albewg und \ref albewk übergeben.
  Die Multiplikation mit den Kohlenstoffgehalten der Algen  \ref Caki und 
  \ref Cagr ergeben dann die CO2-Abnahme im Wasser.
  \n\n
* \ref lnk_makrophyt entnehmen das zum Wachstum benötigte CO2 aus dem im Wasser.
  Dazu wird deren Sauerstoffproduktion pro Zeitschritt, der in der Subroutine 
  mphyt() berechnet wurde, mittels der Variable \ref po2p übergeben und in 
  CO2-Konzentration umgerechnet.

Falls die Primärproduktion den Kohlendioxid-Pool im jeweiligen Zeitschritt 
ausschöpfen würde, wird angenommen, dass der restliche Kohlenstoff aus dem 
Hydrogencarbonat-Pool \f$(\Delta HCO_3)_{bio}\f$ entnommen wird.
Numerisch werden negative Konzentrationen für Kohlendioxid und Hydrogencarbonat 
nicht zugelassen (geklippt), wofür die Verletzung der Massenerhaltung in Kauf 
genommen werden muss.

## Summe
\f[ (\Delta CO_2)_{bio} = (\Delta CO_2)_{lief} - (\Delta CO_2)_{verb} \f]

zurück zu \ref lnk_ph_prozess

# Nitrifikation {#Nitribit}
Die Nitrifikation von Ammonium \f$NH_4^+ \f$ zu Nitrat \f$NO_3^-\f$ erzeugt 
je zwei Protonen:
\f[ \Delta H_N = \frac{2*\Delta N}{14 g/mol * 1000 mg/g} \f]

zurück zu \ref lnk_ph_prozess

# Calcium {#Calcium}

 ## Einfluss von Alter und Schwebstoffghalt {#Alter}

In der pH-Subroutine wird eine Altersvariabel hochgezählt.
\f[stind'=stind + \Delta t * (d/1440 min)\f]
(mit dem ' Apostoph sei hier der Wert im folgenden Zeitschritt bezeichnet)
Mit diesem Berechnungsverfahren läßt sich das Alter bestimmen, wenn der Zufluss 
über einen einzigen Rand erfolgt.

Bei zunehmenden Alter können nur immer kleinere Abweichungen vom \ref loesgleich 
eine Änderung des Calciumgehaltes hervorrufen .
Bei großen Schwebstoffgehalten \f$ ssalg\f$ wird der Calciumgehalt auch nicht 
mehr geändert.
Eine Änderung des Calciumgehalts wird nur dann durchgeführt, wenn:
\f[ \frac{15770}{ssalg} * e^{(-0,0496*\mu)}  > stind \f]

mit: \f$ \mu = \frac{(Ca/40080)*CO_3}{k_{ca}} \f$ 
, zu \f$k_{ca}\f$ siehe \ref loesgleich.

Nur dann, wenn die obige Bedingung erfüllt ist, wird in der Subroutine ph() 
die äußere Iterationsschleife aktiviert.
Wenn nicht, bleibt der Calciumgehalt gleich und nur die innere Iteration, 
\ref pHiter, ist aktiv.	  

zurück zu \ref lnk_ph_prozess

## geschachtelte Iterationen 
Die nichtlinearen Zusammenhänge zwischen dem Calciumgehalt, pH-Wert und den 
Kolensäureformen machen eine iterative Lösung erforderlich.

Dazu wird in einer äusseren Iteration zunächst eine Schätzung des Calciumgehalts 
und der Kohlensäureformen ermittelt. 
Auf dieser Basis wird dann in einer inneren Schleife die \ref pHiter durchgeführt. 
Nach dem Rücksprung in die äussere Schleife wird dann ein verbesserter 
Calciumgehalt bestimmt, der die Berechnung eines erneut verbesserten 
pH-Wertes ermöglicht ... usw.

<hr>

Schleifenanfang äussere Iteration

<hr>

## Lösungsgleichgewicht  {#loesgleich}
Die iterative Bestimmung des Calciumgehalts wird abgebrochen, wenn das 
Löslichkeitsprodukt
 \f[ \mu = \frac{Ca/40080 * CO_3}{k_{ca}} = 1 \f] 
von Calcium und Carbonat näherungsweise (\f$ \mu <10 \f$) erfüllt ist.
(auch in Sigg,Stumm 2016 wird dieser Zusammenhang so verkürzt dargestellt).
Dabei wird die Löslichkeitskonstante gemäß der empirischen Formel:
\f[
 k_{ca} = 9,41 * 10^{-9} * (10^{\frac{0,016492 * \sqrt{lf}}{(1 + 0,016080 * 
 \sqrt{lf})}}) * (1 - 0,026780 * (T - 273,16) + 0,0002933 * (T - 273,16)^2 -
 0,000001211 * (T - 273,16)^3 )
\f]
in Abhängigkeit von Leitfähigkeit und Temperatur bestimmt (Vergleich mit 
Dickson 2007 steht aus).

## Änderung Calcium-Gehalt {#dca} 
Die Änderung des Calcium-Gehalts wird entweder als:

\f[ \Delta Ca = 0,001627248 * ssalgs * e^{(0.0362* \mu)} \f]

oder als:

\f[ \Delta Ca = 40080 * \frac{(\mu-10.)*k_{ca}}{CO3}   \f]

berechnet. Der jeweils kleinere Wert wird benutzt. 
In der letztere Formel kann \f$ \Delta Ca \f$ auch negativ werden, was eine 
Calcitfällung bewirkt.

Zur Bestimmung von \f$ \mu \f$ 
und 
\f$ k_{ca} \f$ siehe oben, Abschnitt \ref loesgleich.

Damit berechnen sich dann der Calciumgehalt und die Leitfähigkeit im folgenden 
Zeitschritt zu:
	 \f[ Ca' = Ca + \Delta Ca  \f]
	 \f[ lf' = lf + \Delta Ca* 2,2\frac{\mu S/cm}{mg/l} \f]
(mit dem ' Apostoph sei hier der Wert im folgenden Zeitschritt bezeichnet)

## Änderung Kohlensäureformen und Leitfähigkeit {#dco3}
Bei der Calcium-Carbonat Fällung wird Hydrogencarbonat freigesetzt und Carbonat 
entnommen.
Die Rücklösung von Calcium hat den gegenteiligen Effekt. Es finden folgende 
Formeln Anwendung:

\f[ (\Delta HCO_3)_{Ca} = (0,56848 * pH - 5,482184) \Delta Ca / (1000mg/g * 61,02g/mol) \f]
\f[ (\Delta CO_3)_{Ca} = (3,91005 - 0,56205*pH) \Delta Ca  / (1000mg/g * 60,009g/mol) \f]

<hr>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Schleifenanfang innere Iteration

<hr>

## iterative pH-Wert Neubestimmung {#pHiter}

Dazu wird zunächst der m-Wert (und der dazugehörende p-Wert) im folgenden 
Zeitschritt berechnet: 
(mit dem ' Apostoph sei hier der Wert im folgenden Zeitschritt bezeichnet 
\f$ m' = m(t+ \Delta t)\f$)

\f$ \Delta CO_2 = (\Delta CO_2)_{bio} + (\Delta CO_2)_{luft}\f$ \n
\f$ \Delta HCO_3 = (\Delta HCO_3)_{Ca} + (\Delta HCO_3)_{bio} \f$ \n
\f$ \Delta CO_3 = (\Delta CO_3)_{Ca} \f$ \n

\f[ m' = (HCO_3+\Delta HCO_3) + 2*(CO_3+\Delta CO_3) + OH - (H + \Delta H_N) \f]
\f[ p' = (CO_3+\Delta CO_3)) - (CO_2+\Delta CO_2) - (H + \Delta H_N) + OH \f]
Zudem wird der geänderte Gesamt-Kohlenstoffgehalt zusammengezählt: 
\f[ DIC' = (CO_2+\Delta CO_2) + (HCO_3+\Delta HCO_3) + (CO_3+\Delta CO_3) \f]

Die Protonenkonzentration H wird nun solange iterativ verändert, bis das 
Ladungsgleichgewicht:
 \f[ 0 = m'/1000 - HCO_3(DIC',H) - 2*CO_3(DIC',H) + H - OH(H) \f]
näherungsweise erfüllt ist. Dabei wird für die Aufteilung der \ref Aufteilung 
wieder der oben angegebene Zusammenhang verwendet.
 
Abgebrochen wird die Iteration, wenn die Änderung des pH-Werts, 
\f$ pH' = hk - log_{10}(H') \f$,
kleiner als 0,001 ist, oder 50 Iterationsschritte gemacht wurden.

<hr>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; Schleifenende innere Iteration

<hr>

Schleifenende äussere Iteration

<hr>

# Aufruf der Subroutine / Übergabevariablen

  subroutine ph_kern( \ref mw, \ref pw, \ref ca, \ref lf, \ref tempw, \ref vph, \ref vco2                 &\n
                    , \ref tflie, \ref rau, \ref vmitt, \ref tiefe, \ref rhyd, \ref flae              &\n
                    , \ref wge, \ref wlage, \ref hws, \ref iphy                   &\n
					, \ref bsbct, \ref resdr, \ref dzres1, \ref dzres2            &\n
					, \ref dalgki, \ref dalggr, \ref dalgbl, \ref dalgak, \ref dalgag, \ref dalgab   &\n
					, \ref Caki, \ref Cagr, \ref Cabl                             &\n
					, \ref alberg, \ref alberk, \ref albewg, \ref albewk          &\n
					, \ref susn, \ref po2p, \ref po2r, \ref ssalg, \ref stind     &\n
                    , \ref kontroll, \ref iglob )
			
!!!!! in der Kernroutine ist allen Übergabe-Variablen ein "s" im Namen angehängt, \n
!!!!! um zu kennzeichen, dass es sich um lokale Variablen in dieser Subroutine handelt \n
!!!!! und nicht um die globalen Felder des Hauptprogramms. \n

zurück zu \ref pHiter

# Transport
Im Transport-Teil von QSim werden die Konzentrationen: 
Ca, m-Wert, p-Wert und die Protonenkonzentration (nach der Formel aus Abschnitt 
\ref Proton berechnet)
von der subroutine Transport() berechnet.
In gepufferten Lösungen ist die Protonenkonzentration allerdings keine 
konservative Variable. 

# p-Wert
Am Zuflussrand werden nur Ca, pH, und m-Wert als Randbedingungen vorgegeben.
Der p-Wert muss daher an den Rändern berechnet werden. 
Dies geschieht auf der Basis des m-Wertes und des pH-Wertes. 
An jedem Zuflussrand wird daher der p-Wert mit der Subroutine p-wert() bestimmt.

Die nichtlinearen Zusammenhänge erfordern auch hier eine iterative Berechnung.
In zwei geschachtelten Iterationen wird der p-Wert solange variiert, bis das 
Ladungsgleichgewicht erfüllt ist.
Dabei finden die im Abschnitt \ref pHiter angegebenen Formeln Anwendung.

## Rand und Anfangsbedingungen
Ergänzung des P-Wertes im Zufluss mit pwert(); 
Siehe dazu auch \ref lnk_randbedingungen_ergaenzen .


# QSim-Veröffentlichungen, die den pH-Baustein beschreiben und/oder anwenden:

- ...


## Literaturliste pH-Baustein

Charlton, S. R. (2020). phreeqc. https://www.usgs.gov/software/phreeqc-version-3.

Coughlan, C. H., Nicolas ; Stips, Adolf (2013). Modelling the carbonate system 
to adequately quantify ocean acidification. doi:10.2788/57998

Dickson, A. G., Sabine, C.L. and Christian, J.R. (Eds.) (2007). Guide to Best 
Practices for Ocean CO2 Measurements. PICES Special Publication 3, 191 pp. 

DIN 38404-10:2012-12, Deutsche Einheitsverfahren zur Wasser-, Abwasser- und 
Schlammuntersuchung - Physikalische und physikalisch-chemische Stoffkenngrößen 
(Gruppe C) - Teil 10: Berechnung der Calcitsättigung eines Wassers (C 10) 

Gattuso, J.-P. e. a. (2020). seacarb. https://CRAN.R-project.org/package=seacarb. 

Hales, B., Strutton, P. G., Saraceno, M., Letelier, R., Takahashi, T., 
Feely, R., . . . Chavez, F. (2012). Satellite-based prediction of pCO2 in 
coastal waters of the eastern North Pacific. Progress in Oceanography, 103, 
1-15. doi:10.1016/j.pocean.2012.03.001

Lajaunie-Salla, K., Diaz, F., Wimart-Rousseau, C., Wagener, T., Lefèvre, D., 
Yohia, C., . . . Pinazo, C. (2020). Implementation  and  assessment  of  a  
carbonate  system  model 1(Eco3M-CarbOxv1.1)   in   a   highly-dynamic   
Mediterranean 2coastal site (Bay of Marseille, France). Geoscientific Model 
Development. doi:doi.org/10.5194/gmd-2020-41

Lendt, R. (2000). Reaktionen des oberflächennahen marinen Karbonatsystems im 
nordwestlichen Arabischen Meer auf den Südwest-Monsun. Staats- und 
Universitätsbibliothek Hamburg, Hamburg. Retrieved from http://ediss.sub.uni-hamburg.de/volltexte/2000/243 

Li, S., Lu, X. X., & Bush, R. T. (2013). CO2 partial pressure and CO2 emission 
in the Lower Mekong River. Journal of Hydrology, 504, 40-56. 
doi:dx.doi.org/10.1016/j.jhydrol.2013.09.024

Middelburg, J. J. (2019). Marine Carbon Biogeochemistry.

Middelburg, J. J., Soetaert, K., & Hagens, M. (2020). Ocean Alkalinity, 
Buffering and Biogeochemical Processes. Rev Geophys, 58(3), e2019RG000681. 
doi:10.1029/2019RG000681

Munhoven, G. (2013). Mathematics of the total alkalinity–pH equation – pathway 
to robust and universal solution algorithms: the SolveSAPHE package v1.0.1. 
Geoscientific Model Development, 6(4), 1367-1388. doi:10.5194/gmd-6-1367-2013

Orr, J. C., Epitalon, J.-M., Dickson, A. G., & Gattuso, J.-P. (2018). Routine 
uncertainty propagation for the marine carbon dioxide system. Marine Chemistry,
207, 84-107. doi:10.1016/j.marchem.2018.10.006

Orr, J. C., & Epitalon, J. M. (2015). Improved routines to model the ocean 
carbonate system: mocsy 2.0. Geoscientific Model Development, 8(3), 485-499.
doi:10.5194/gmd-8-485-2015

Orr, J. C., Epitalon, J. M., & Gattuso, J. P. (2015). Comparison of ten 
packages that compute ocean carbonate chemistry. Biogeosciences, 12(5), 
1483-1510. doi:10.5194/bg-12-1483-2015

Schwoerbel, J., & Brendelberger, H. (2013). Einführung in die Limnologie 
(10. Auflage ed.). Berlin Heidelberg: Springer Spektrum.

Sigg, L., & Stumm, W. (2016). Aquatische Chemie: Einführung in die Chemie 
natürlicher Gewässer (6. Auflage ed.). Zürich: vdf Hochschulverlag.

WEISS, R. F. (1974). CARBON DIOXIDE IN WATER AND SEAWATER: THE SOLUBILITY OF 
A NON-IDEAL GAS. Marine Chemistry, 2 (1974) 203--215. 



Textquelle: ph-prozess.md ; Codesources: ph.f90 ph_kern.f90 phstart.f90 ; 
zurück: \ref lnk_ph
