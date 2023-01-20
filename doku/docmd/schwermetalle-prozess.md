Schwermetalle - Prozesse {#lnk_schwermet_prozesse}
=====================

## Teilprozesse

Im Baustein A werden folgende Teilprozesse berücksichtigt:
- Adsorption
- Sedimentation
- Erosion

# Adsorption {#lnk_schwermet_adsorpt}
Um die Adsorption der Schwermetalle an Feststoffen quantitativ zu beschreiben, 
wird hier das Konzept des Verteilungskoeffizienten \f$ K_D \f$ benutzt.

Dabei beschreibt der Verteilungskoeffizient das Konzentrationsverhältnis des 
jeweiligen Schwermetalls zwischen Feststoff und Lösung für den Fall, dass der 
Stoffaustausch einen dynamischen Gleichgewichtszustand erreicht hat. Hier wird 
angenommen, dass das dynamische Gleichgewicht verzögerungsfrei erreicht wird:

\f{equation}{ 
  K_D = \frac{SM_p}{SM_{gel} \cdot \frac{C_{SS}}{1000}}
  \label{eq:schwer_K_D}
\f} 

mit

\f$ K_D \f$:  Verteilungskoeffizient  [\f$ \Lg \f$] \n
\f$ SM_p \f$: Konzentration des an Feststoff adsorbierten (partikulär 
              gebundenen) Schwermetalls   [\f$ \ugL \f$] \n
\f$ SM_{gel} \f$:  Konzentration des gelösten Schwermetalls in Wasser [\f$ \ugL \f$] \n
\f$ C_{SS} \f$:  Schwebstoffkonzentration   [\f$ \mgL \f$] \n

\n

Hier und in allen folgenden Gleichungen steht \f$ SM \f$ und \f$ K_D \f$ für 
jeweils eines der 11 Schwermetalle.

Aus der Gesamt-Konzentration des Schwermetalls ergeben sich aus 
Gleichung \f$\eqref{eq:schwer_K_D}\f$ für die gelösten und partikulären 
Schwermetallanteile in der Wasserphase als Funktion des 
Verteilungskoeffizienten:

\f{equation}{ 
  SM_{gel} = \frac{SM_{ges}}{1 + K_D \cdot \frac{C_{SS}}{1000}} 
  \label{eq:schwer_SM_gel}
\f}

\f{equation}{ 
  SM_p = SM_{ges} - \frac{SM_{ges}}{1 + K_D \cdot \frac{C_{ss}}{1000}} 
\f}

mit

\f$ SM_{gel} \f$: Konzentration des gelösten Schwermetalls in Wasser [\f$ \ugL \f$] \n
\f$ SM_{ges} \f$: Gesamt-Konzentration des Schwermetalls (im Wasser gelöst und 
                  an Feststoff adsorbiert) [\f$ \ugL \f$] \n
\f$ SM_p \f$:     Konzentration des an Feststoff adsorbierten (partikulär 
                  gebundenen) Schwer-metalls [\f$ \ugL \f$] \n
\f$ K_D \f$:	  Verteilungskoeffizient [\f$ \Lg \f$] \n
\f$ C_{SS} \f$:	  Schwebstoffkonzentration [\f$ \mgL \f$] \n

\n
Ist bei Simulationen mit QSim an den Rändern (Startprofile, Einleitungen) sowohl 
die Gesamt-Konzentration des entsprechenden Schwermetalls als auch die 
Konzentration der gelösten Fraktion bekannt, so wird der Verteilungskoeffizient 
zu Beginn der Simulation aus Gleichung \f$\eqref{eq:schwer_SM_gel}\f$ berechnet:

\f{equation}{
  K_D = \frac{\frac{SM_{ges}}{SM_{gel}} - 1}{\frac{C_{SS}}{1000}}
  \; für \; SM_{ges} >SM_{gel}
\f}


Fehlt der Messwert für die gelöste Konzentration (Fall 1) oder die Gesamt-Konzentration (Fall 2), 
wird der Verteilungskoeffizient je nach gewähltem Berechnungsverfahren nach Tabelle 3 eingesetzt 
(Verfahren mit konstantem KD-Wert) oder nach Formel (9) ermittelt (Verfahren nach \cite ATV_2002) 
und die fehlende Konzentration berechnet:

Fall 1: \f$ SM_{ges} \f$ angegeben, \f$ SM_{gel} \f$  nicht angegeben

\f{equation}{
  SM_{gel} = \frac{SM_{ges}}{(1 + K_D \cdot \frac{C_{SS}}{1000}}
\f}

Fall 2 : \f$ SM_{ges} \f$ nicht angegeben, \f$ SM_{gel} \f$ angegeben

\f{equation}{
  SM_{ges} = SM_{gel} \cdot \left(1 + K_D \cdot \frac{C_{SS}}{1000} \right)
\f}

Wenn der Wert für die Gesamt-Konzentration kleiner oder gleich dem Wert für die 
gelöste Konzentration ist (Fall 3), erhält die gelöste Konzentration den Wert der 
Gesamt-Konzentration.
 
Fall 3 : \f$SM_{ges}\f$  und \f$SM_{gel}\f$ angegeben, aber \f$SM_{ges}\f$ < \f$SM_{gel}\f$

\f{equation}{
  SM_{gel}= SM_{ges}
\f}


In den Fällen, wo  \f$SM_{ges}\f$ und \f$SM_{gel}\f$ aus Messungen vorgegeben 
werden können, errechnet sich der \f$K_D\f$-Wert aus:

\f{equation}{
  K_D = \frac{\frac{SM_{ges}}{SM_{gel}} - 1}{\frac{C_{SS}}{1000}} 
  \; für \; SM_{ges} \geq SM_{gel}
  \label{eq:schwer_K_D_mess}
\f}


Für den Fall \f$SM_{gel} = SM_{ges} \f$ ergibt sich aus \f$\eqref{eq:schwer_K_D_mess}\f$ 
ein \f$K_D\f$-Wert von 0 mit dem dann weitergerechnet wird.

\n\n

# Der Verteilungskoeffizient KD

Im ATV-Modell \cite ATV_2002 ist der Verteilungskoeffizient für die 
Schwermetalle Zn, Cu, Cd und Ni eine Funktion vom pH-Wert und der 
Schwebstoffkonzentration und wird mit folgender empirischen, 
dimensionsbehafteten Formel bestimmt:

\f{equation}{
  K_D = \frac{c_1}{C_{SS}^{e_1}} + \frac{c_2}{C_{SS}^{e_2}} \cdot 
   \left(\frac{pH}{9} \right)^\left(\frac{c_3}{C_{SS}^{e_3}} \right) + 
   \left(\frac{c_4}{C_{SS}^{e_4}} + c_5 \right) \cdot 
   \left[\left(\frac{pH-4}{5} \right)^{e_5} - \left(\frac{pH - 4}{5} \right)^{e_5 - 1} 
   \right]
   \label{eq:schwer_K_D_atv}
\f}

mit:

\f$ K_D \f$:    K_D-Wert nach dem ATV-Modell [\f$ \Lg \f$] \n
\f$ pH \f$:     pH-Wert [-] \n
\f$ C_{SS} \f$: Schwebstoffkonzentration [\f$ \mgL \f$] \n

Die für den ATV-Modellbaustein verwendeten Konstanten \f$c_1\f$ bis \f$c_5\f$, 
sowie die Exponenten \f$e_1\f$ bis \f$e_5\f$ sind für die 11 Schwermetalle 
in Tabelle 2 zusammengestellt. Neben den originalen Werten nach 
(ATV 2002, \cite ATV_2002) 
wurden dort für die Modellinstanz Elbe (Schmilka bis Geesthacht) neue 
Zahlenwerte nach folgendem Ansatz ermittelt:

Grundlage sind die Schwermetall-Messergebnisse an den Elbe-Messstellen 
Wittenberg (oberhalb Saale-Zufluss) und Schnackenburg (unterhalb Saale-Zufluss), 
die im Zeitraum 2014 bis 2018 als Stichproben im Rahmen des Routine-Monitorings
(Intervallmessungen) von den Ländern Sachsen-Anhalt und Niedersachsen erhoben 
wurden (nach FIS Datenportal der FGG Elbe, Stand 12.2.2021). Für jedes 
Schwermetall wurde die mittlere Verteilung der gebundenen und gelösten Fraktion 
bzw. daraus berechnet der partikulären und gelösten Fraktion an den beiden 
Messstellen errechnet. In diese Berechnungen gingen nur Messergebnisse oberhalb 
der Bestimmungsgrenze ein. Ferner durfte die Konzentration der gelösten Fraktion 
aus Gründen der Plausibilität höchstens so hoch ausgewiesen sein wie die 
Konzentration der Gesamtfraktion. Die ermittelten Verteilungsverhältnisse an den 
beiden Messstellen der Elbe sind in Tabelle 1 wiedergegeben.

Nicht alle Elemente wurden im gesamten zugrunde gelegten 5-Jahreszeitraum 
gemessen und in einzelnen Jahren wurde teilweise nur die Gesamtfraktion 
bestimmt. Da außerdem die Konzentration der gelösten Fraktion von Pb, Cr, Fe 
und Hg in der Regel unter der Bestimmungsgrenze lag, ist das in Tabelle 1 für 
diese Schwermetalle ausgewiesene Verteilungsverhältnis aufgrund zu weniger 
Wertepaare nicht belastbar. Bei Uran liegen zwar auch nur sehr wenige Wertepaare 
vor, die Verhältnisse an anderen Probestellen der Binnenelbe bestätigen aber 
das fast ausschließlich gelöste Auftreten dieses Elements. 

Zusätzlich wurden die mittlere Konzentration des Schwebstoffs (gemessen als 
abfiltrierbare Stoffe) und der mittlere pH-Wert der Elbe bei Wittenberg und 
Schnackenburg im Zeitraum 2014 bis 2018 berechnet (Ergebnis: 17 mg/l 
Schwebstoff, pH 8,2). 


Da nach dem ATV-Modell \cite ATV_2002 zu Formel \f$\eqref{eq:schwer_K_D_atv}\f$ 
nur für Cd, Cu, Ni und Zn 
Konstanten und Exponenten entwickelt wurden (Tabelle 2), war bereits vor 
Ermittlung der Verteilungsverhältnisse in der Elbe eine Zuordnung der weiteren 
Schwermetalle zu diesen vier Elementen nach vermuteter Ähnlichkeit des 
Verteilungskoeffizienten erfolgt, so dass folgende Gruppen entstanden: 
Cd, Pb, Cr, Hg / Cu, As / Ni, U / Zn, Mn, Fe. Cadmium wurde später jedoch in die 
Gruppe von Zn, Mn und Fe gestellt (Tabelle 2).

Zur Berechnung des KD-Werts nach Formel \f$\eqref{eq:schwer_K_D_atv}\f$ wurden 
die Konstanten und Exponenten nach ATV-Modell \cite ATV_2002 gerundet. Die 
Konstanten c1, c2 und c4 (bei der Zn-Gruppe auch c5) wurden daraufhin im etwa 
gleichen Verhältnis so vergrößert oder verkleinert, dass das in Tabelle 1 
dargestellte empirische Verteilungsverhältnis durch den errechneten KD-Wert nach 
Formel \f$\eqref{eq:schwer_K_D_atv}\f$ bei durchschnittlichen Elbe-Verhältnissen 
(17 mg/l Schwebstoff und pH 8,2) erreicht wird.

Tabelle 1:	Durchschnittliches Verhältnis gesamte/gelöste Fraktion bzw. 
partikuläre/gelöste Fraktion von Schwermetallen der Elbe an den Messstellen 
Wittenberg und Schnackenburg im Zeitraum 2014 bis 2018

| Element | Anzahl Wertepaare gesamt / gelöst | Relation gesamt / gelöst | Relation partikulär / gelöst | Bemerkung zur Datenlage | 
|----|-----|------|------|----------------------------------| 
| Pb | 3   | 2,1  | 1,1  | nicht belastbar                  |
| Cd | 79  | 3,4  | 2,4  | 	                                |
| Cr | 3   | 1,5  | 0,5  | nur Wittenberg; nicht belastbar  |
| Fe | 3   | 9,3  | 8,3  | nur Wittenberg; nicht belastbar  |
| Cu | 82  | 1,5  | 0,5  | 	                                |
| Mn | 29  | 7,2  | 6,2  | nur Wittenberg                   |
| Ni | 111 | 1,3  | 0,3  | 	                                |
| Hg | 9   | 2,6  | 1,6  | nur Schnackenb.; nicht belastbar |
| U  | 9   | 1,03 | 0,03 | nur Schnackenb.                  |
| Zn | 38  | 2,7  | 1,7  | 	                                |
| As | 110 | 1,4  | 0,4  | 	                                |


Tabelle 2: Für die QSim-Elbe-Modellinstanz neu definierte Konstanten (*c*) und 
Exponenten (*e*) für den Berechnungsansatz nach ATV-Modell \cite ATV_2002 im 
Vergleich zu den originalen Werten

|  | Pb, Cr, Hg\f$^{*1}\f$  | Ni    | U   | Cu    | As   | Cd\f$^{*2}\f$ | Zn    | Mn, Fe |
|--|------------------|-------|-----|-------|------|---------|-------|---------|
| \f$c_{1 neu}\f$  | 	6100  |	27	  | 3	| 7	    | 5    | 190     | 130   | 480   |
| \f$c_{1 orig}\f$ | 		  | 21	  |	    | 45	|      | 49,090  | 144   |       | 
| \f$e_{1 neu}\f$  | 	1,6	  | 0,5	  | 0,5 |	0,5	| 0,5  | 1       | 1     | 1     | 
| \f$e_{1 orig}\f$ | 		  | 0,548 |		| 0,496	|      | 1,586   | 1,038 |       |
| \f$c_{2 neu}\f$  | 	1600  |	2170  |	285	| 385	| 300  | 23000   | 16200 | 59350 | 
| \f$c_{2 orig}\f$ | 		  | 1666  |		| 2541	|      | 12556   | 17769 |       |
| \f$e_{2 neu}\f$  | 	0,6	  | 0,9	  | 0,9	| 0,8	| 0,8  | 0,7     | 0,7   | 0,7   | 
| \f$e_{2 orig}\f$ | 		  | 0,872 |		| 0,807	|      | 0,641   | 0,673 |       |
| \f$c_{3 neu}\f$  | 	17	  | 63	  | 63	| 13	| 13   | 42      | 42    | 42    | 
| \f$c_{3 orig}\f$ | 		  | 63	  |	    | 13	|      | 17      | 42	 |       | 
| \f$e_{3 neu}\f$  | -0,02  |	0,2	  | 0,2	| 0,2	| 0,2  | 0,06    | 0,06  | 0,06  | 
| \f$e_{3 orig}\f$ | 		  | 0,205 |		| 0,172	|      | -0,023  | 0,056 |       |
| \f$c_{4 neu}\f$  | -31000 |	-380  |	-50 | -250	| -195 | 0,40    | 0,27  | 1     | 
| \f$c_{4 orig}\f$ | 		  | -294  |		| -1660 |     | -251,483 | 0,308 |       |
| \f$e_{4 neu}\f$  | 	1,8	  | 0,8	  | 0,8	| 0,5	| 0,5  | -1      | -1    | -1    | 
| \f$e_{4 orig}\f$ | 		  | 0,810 |		| 0,459	|      | 1,835   | -1    |       | 	
| \f$c_{5 neu}\f$  | 	0	  | 0	  | 0	| 0	    | 0    | -130    | -90   | -337  | 
| \f$c_{5 orig}\f$ | 	      | 0	  |	    | 0	    |	   | 0	     | -101  |       | 	
| \f$e_{5 neu}\f$  | 	3,8	  | 4,2	  | 4,2	| 3,2	| 3,2  | 3,5     | 3,5   | 3,5   | 
| \f$e_{5 orig}\f$ | 		  | 4,2	  |	    | 3,2	|      | 3,8     | 3,5	 |       | 
*1: Formel hilfsweise nach der ursprünglichen Cd-Formel konzipiert 
*2: Die Cd-Formel wurde in Anlehnung an die Zn-Formel neu konzipiert

\n

Liegen bei der Berechnung nach dem ATV-Modell für die Einleitungen 
(Randbedingungen) keine Schwebstoff- und/oder keine pH-Werte vor, so werden für 
diese Ränder die Werte von den Ortspunkten oberhalb der Einleitungen übernommen.

Modellierte Nebengewässer (Strang) benötigen wie jeder Strang am Modellrand 
zwingend einen pH-Wert und eine Schwebstoffkonzentration.

Im Gegensatz zum Berechnungsverfahren nach dem ATV-Modell kann auch von einem 
konstanten Wert für den Verteilungskoeffizient ausgegangen werden, also 
unabhängig vom pH-Wert und der Schwebstoffkonzentration. 

Beim QSim-Schwermetallmodul kann ein entsprechender Berechnungsansatz mit 
Verteilungskoeffizienten gemäß DELTARES \cite Deltares_2017 eingestellt werden, 
wobei fehlende Koeffizienten für Eisen, Mangan und Uran nach \cite Tomczak_2019 
ergänzt wurden (Tabelle 3). 

Alternativ können die Verteilungskoeffizienten auch nach der durchschnittlichen 
Verteilung in der Elbe im Zeitraum 2014 bis 2018 (siehe oben) gewählt werden. 
Die sich daraus unter Verwendung von Formel \f$\eqref{eq:schwer_K_D}\f$ 
(mit 17 mg/l Schwebstoff) ergebenden KD-Werte sind ebenfalls in Tabelle 3 
aufgenommen.

Tabelle 3:	Verteilungskoeffizienten nach \cite Deltares_2017 
ergänzt durch \cite Tomczak_2019 sowie nach Elbe-Verhältnissen im Zeitraum 
2014 bis 2018 

| Schwermetall | KD-Wert DELTARES(1) | KD-Wert Elbe-Monitoring | 
|--------------|---------------------|-------------------------|
| Arsen       | 282 | 22 | 
| Blei        | 640 | vorläufig wie Cadmium: 138 | 
| Cadmium     |	130 | 138 | 
| Chrom       |	290 | vorläufig wie Cadmium: 138 | 
| Kupfer      |	50  | 28  | 
| Nickel      |	9	| 16  | 
| Quecksilber |	170 | vorläufig wie Cadmium: 138 | 
| Zink        |	110 | 101 | 
| 	          | KD-Wert TOMCZAK ET AL. (2019)(2) |  | 
| Eisen       | 246 | vorläufig wie Mangan: 363 | 
| Mangan      |	165 | 363 | 
| Uran        | 12  | 2   | 
(1)Dimension nach DELTARES \cite Deltares_2017 m3/kg, das entspricht l/g
(2)die Werte wurden von l/kg auf l/g umgerechnet

\n\n

# Verringerung der Gesamt-Schwermetallkonzentration durch Sedimentation {#lnk_schwermet_sedimentation}

Unter Sedimentation versteht man das Absinken von Feststoffen aus der 
Wassersäule bei Unterschreitung einer kritischen Schubspannungsgeschwindigkeit. 
Dabei verringert sich die an Schwebstoffen adsorbierte 
Schwermetallkonzentration, und damit die Gesamt-Konzentration:

\f{equation}{
  SM_{sed} = (SM_{ges} - SM_{gel}) \cdot 
  \left( \frac{SS_{sed} + A_{sed,ki} + A_{sed,gr} + A_{sed,bl}}{C_{SS}} \right)
\f}

mit: 

\f$ SM_{sed} \f$:	Sedimentation der an Feststoffe adsorbierten Schwermetalle pro 
                Sekunde (s) [\f$ \ugL s^{-1} \f$] \n
\f$ SS_{sed} \f$: sedimentierte Schwebstoffe ohne Algen pro s [\f$ \mgL s^{-1}\f$] \n
\f$ A_{sed,ki}, A_{sed,gr}, A_{sed,bl} \f$: sedimentierte Kiesel-, Grün- und 
                Blaualgen pro s [\f$ \mgL s^{-1}\f$] \n
\f$ C_{SS} \f$: Schwebstoffkonzentration [\f$ \mgL \f$] \n

\n

# Erhöhung der Gesamt-Schwermetallkonzentration durch Erosion {#lnk_schwermet_erosion}

Unter Erosion wird hier das Herauslösen von Feststoffpartikeln aus der 
Flusssohle und der Sohle in Buhnenfeldern infolge der auf sie wirkenden 
Strömungskräfte verstanden.

Maßgebend für den Massenstrom bei Erosion ist der Schubspannungsüberschuss, 
welcher aus der Differenz zwischen der an der Sohle wirksamen und der für den 
Transportbeginn erforderlichen (kritischen) Sohlschubspannung resultiert.

Zur Beschreibung der Massenerosion konsolidierter Sedimente mit kohäsiven 
Eigenschaften hat sich die folgende Erosionsformel von 
Kern (1997) \cite Kern_1997 bewährt:

\f{equation}{
  E = M \cdot \left(\frac{\tau_0 - \tau_{0,krit}}{\tau_{0,krit}} \right)^n, 
  \; für \; \tau_0 > \tau_{0,krit} \\
  und \\
  E = 0, \; für \; \tau_0 \leq \tau_{0,krit}
  \label{eq:schwer_E}
\f}

mit

\f$ E \f$:	            Erosionsrate [\f$ \kgmqsinv \f$] \n
\f$ M \f$:	            Erosionsbeiwert [\f$ \kgmqsinv \f$] \n
\f$ \tau_0 \f$:	        Sohlschubspannung [\f$ \kgminvsq \f$] \n
\f$ \tau_{0,krit} \f$:	kritische Erosionsschubspannung [\f$ \kgminvsq \f$] \n
\f$ n \f$:	            empirischer Exponent [-] \n

Die Schwermetallfreisetzung ist proportional zur Erosionsrate E und zur 
Schadstoffbelastung im Sediment:

\f{equation}{
  SM_E = \frac{E \cdot Sed_{SM_p}}{H}
\f}

mit

\f$ SM_E \f$:  Konzentrationsänderung des Schwermetalls (Gesamt-Konzentration) 
               im Wasserkörper durch Erosion pro Sekunde [\f$ \ugL \, s^{-1} \f$] \n
\f$ Sed_{SM_p} \f$: Schwermetallbelastung des Sediments [\f$ \mgkg \f$] \n
\f$ H \f$:     Gewässertiefe [\f$m\f$] \n

\n

In der Flusssohle ist der Volumenanteil der Festphase innerhalb eines 
betrachteten Einheitsvolumens üblicherweise um drei bis fünf Größenordnungen 
größer als im Wasserkörper. Dies bedeutet für die Verteilung von sorptiven 
Schwermetallen im Flussbett, dass die im Interstitial gelösten Schadstoffanteile 
gegenüber den an die Sedimentmatrix gebundenen Anteilen äußerst klein sind.

Im Modell wird deshalb nur die Erhöhung der Gesamt-Schwermetallkonzentration 
durch die Erosion partikulär gebundener Schwermetalle berücksichtigt.

Die Schwermetallbelastung des Sediments wird in erster Näherung vereinfacht aus 
der mittleren partikulären Schwermetallkonzentration bezogen auf die mittlere 
Schwebstoffkonzentration über den Simulationszeitraum bis zum Erosionsbeginn 
ermittelt:

\f{equation}{
  Sed_{SM_p} = \frac{\sum_{i=1}^{i=m}\frac{SM_{ges}-SM_{gel}}{\frac{C_{SS}}{1000}}}{m}
\f}

mit:

\f$ Sed_{SM_p} \f$: Schwermetallbelastung des Sediments [\f$ \mgkg \f$] \n
\f$ SM_{ges} \f$: 	Gesamt-Konzentration des Schwermetalls [\f$ \ugL \f$] \n
\f$ SM_{gel} \f$: 	Konzentration des gelösten Schwermetalls in Wasser [\f$ \ugL \f$] \n
\f$ C_{SS} \f$: 	Schwebstoffkonzentration [\f$ \mgL \f$] \n
\f$ m \f$: Anzahl der Zeitschritte bis zum Erosionsbeginn (Erreichen von 
           \f$ \tau_{0,krit} \f$ gemäß Gleichung \f$ \eqref{eq:schwer_E} \f$) \n
		   
\n\n 


# Bilanzgleichung

Die Bilanzgleichung wird für die Gesamt-Schwermetallkonzentration aufgestellt:

\f{equation}{
  \frac{\partial SM_{ges}}{\partial t} = - SM_{sed} + SM_E
\f}

und diskretisiert als:

\f{equation}{
  SM_{ges}(t + \Delta t) = SM_{ges}(t) + \Delta t \cdot (SM_E - SM_{sed})
\f}

\f{equation}{
  SM_{ges}(t + \Delta t) = SM_{ges} (t)+ SM_E \cdot \Delta t - 
    (SM_{ges} - SM_{gel}) \cdot \left(\frac{SS_{sed} \cdot \Delta t + 
     A_{sed,ki} \cdot \Delta t + A_{sed,gr} \cdot \Delta t + 
     A_{sed,bl} \cdot \Delta t}{C_{SS}} \right)
\f}
   
Die gelöste Schwermetallkonzentration wird mithilfe des Verteilungskoeffizienten 
ermittelt:

\f{equation}{
  SM_{gel} = \frac{SM_{ges}(t + \Delta t)}{1 + K_{D,t} \cdot \frac{C_{SS}}{1000}}
  \label{eq:schwer_smgel}
\f}

Beide Konzentrationen, Gesamt- und gelöste Schwermetallkonzentration werden 
transportiert. Der Stofftransport (Advektion und Diffusion) wird nicht in dem 
hier beschriebenen Baustein berechnet, sondern erfolgt im Berechnungsablauf 
von QSim in einem nachfolgenden, gesonderten Stofftransport-Modul.

Der Verteilungskoeffizient für die voranstehende Gleichung berechnet sich nach 
folgender Vorschrift. Diese ermöglicht es, Zuflüsse ins Modell, bei denen 
die gelöste und die Gesamt-Konzentration gemessen wurden, mit dem sich aus 
diesen Messungen ergebenden Verteilungskoeffizienten sowohl „durchfließen“ zu 
lassen (Variante 2), als auch die Entwicklung des „zugeflossenen“ Verteilungskoeffizienten 
analog zu der Entwicklung des Koeffizienten aus der ATV-Formel (9) zu ändern (Variante 1). 
Außerdem ist die Mischung von Wasser aus verschiedenen Zuflüssen dadurch 
möglich.

Variante 1: 

\f{equation}{
  K_D(t + \Delta t) = \left( \frac{\frac{SM_{ges}(t)}{SM_{gel}(t)} - 1}
    {\frac{C_{SS}(t)}{1000}} \right) \cdot \frac{(K^*_D(t + \Delta t)}{K^*_D(t)}
\f}

\f$ K_D \f$: Verteilungskoeffizient für Gl. \f$ \eqref{eq:schwer_smgel} \f$ [\f$ \Lg \f$] \n
\f$ K*D \f$: Verteilungskoeffizienten aus Gl. \f$ \eqref{eq:schwer_K_D_atv} \f$ [\f$ \Lg \f$] \n

Variante 2: 

Bei der Berechnung mit konstantem Verteilungskoeffizient (Tabelle 3) ist der Quotient 
\f$ \frac{K^*_D(t + \Delta t)}{K^*_D(t)} \f$ immer 1.





QSim-Veröffentlichungen, die den BausteinA beschreiben und/oder anwenden:

- [U2 et al., 2002](./pdf/U2_et_al_2002rhein.pdf)
- ...

\n\n

Textquelle: schwermetalle-prozess.md; 
Codesource: Schwermetalle.f90 und Schwermetalle_kern.f90 ; 
zurück: \ref lnk_schwermetalle