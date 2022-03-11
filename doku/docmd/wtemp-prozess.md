Wassertemperatur - Prozesse {#lnk_wtemp_prozesse} 
=====================

# Bilanz {#TempAenderung} #
Die Grundlage aller verbreiteten Methoden zur Berechnung der Erwärmung eines 
Fließgewässers ist die vereinfachte Wärmehaushaltsgleichung. 
Auf deren Grundlage wird die Wärmehaushaltsbilanz des Gewässers erstellt, 
mit der die Temperaturänderung des Gewässers pro Zeiteinheit quantifiziert werden kann. 
Die vereinfachte Wärmehaushaltsgleichung lautet:

\f[ \frac{\partial T_w}{\partial t} = \frac{q_S - q_V - q_K + q_{US} + q_U - q_E }{c_w \cdot h \cdot \rho_w}   \f]

\f$\partial T_w \f$: &ensp; Wassertemperatur [°C] \n
\f$t \f$: &ensp; Zeit [h] \n
\f$q_S \f$: &ensp; Wärmestromdichte aus Strahlung [\f$ kJ/(h \cdot m²) \f$]  \n
\f$q_V \f$: &ensp; Wärmestromdichte aus Verdunstung (latente Wärme) [\f$  kJ/(h \cdot m²) \f$] \n
\f$q_K \f$: &ensp; Wärmestromdichte aus Konvektion [\f$  kJ/(h \cdot m²) \f$] \n
\f$q_{US} \f$: &ensp; Wärmestromdichte aus dem Sediment [\f$ kJ/(h \cdot m²) \f$]  \n
\f$q_U \f$: &ensp; Wärmestromdichte in das Sediment [\f$ kJ/(h \cdot m²) \f$] \n
\f$q_E \f$: &ensp; Wärmestromdichte aus direkter Einleitung [\f$ kJ/(h \cdot m²) \f$] \n
\f$c_w \f$: &ensp; spez. Wärmekapazität von Wasser = 4,1868 103 [\f$ kJ/(kg \cdot K) \f$] \n
<!-- #tbw: c_w laut Lexikon Cheime 4,1851 bei 20 °C -->
\f$h \f$: &ensp; mittlere Wassertiefe [m] \n
\f$\rho_w \f$: &ensp; Dichte des Wassers = 1.000 [kg/m3] \n

Alle weiteren evtl. beeinflussenden Komponenten, wie z.B. die Wärmeeinleitung aus der Schifffahrt 
oder aus chemischen bzw. biologischen Prozessen, sind quantitativ meist nur von untergeordneter
Bedeutung. Ein unter Umständen größerer, aber meist schwer zu quantifizierender Einfluss kann der
Zustrom von Grundwasser sein. Diese Einflüsse werden in QSim indirekt über die Kalibrierung des 
Modells berücksichtigt oder können, im Fall des Grundwassers, über einen Einleiter angenähert 
werden.

Im folgenden wird auf die einzelnen Teilprozesse der Wärmehaushaltsgleichung eingegangen.

Die Wärmestromdichte aus der Strahlung \f$q_S\f$ wir im Kapitel [Strahlung] (\ref lnk_strahlung) 
beschrieben.
<!-- #mf: schauen, ob das Wort Strahlung nun direkt mit dem Link hinterlegt ist" -->

# Verdunstung {#lnk_verdunstung}#

Verdunstung entsteht beim Druckausgleich des Dampfdrucks zwischen Wasseroberfläche 
und der darüber liegenden Luftschicht. 
Ist der Dampfdruck an der Wasseroberfläche größer als der der Luft, verdunstet Wasser aus 
dem Gewässer. Da für diesen Vorgang abhängig von der Wassertemperatur mehr oder weniger 
Verdampfungswärme benötigt wird, kühlt sich der Wasserkörper ab. 
In Abhängigkeit von den Dampfdruckverhältnissen kann der Verdunstungsterm in der 
Wärmehaushaltsbilanz auch negative Werte annehmen. In einem solchen Fall kondensiert Wasser 
aus der Luft ins Gewässer. Die bei diesen Phasenübergängen erster Ordnung aufgenommene 
oder abgegebene Energiemenge wird auch als latente (lateinisch: „verborgen“) Wärme bezeichnet.

Für die Berechnung der latenten Wärme gibt es in QSim verschiedene Berechnungsverfahren,
die über einen Schalter \f$ Schaltern einfügen \f$ in den Parametereinstellungen 
ausgewählt werden können. 

Die drei Berechnungsverfahren nach WMO, Sweers und Rimsha-Donchenko basieren auf dem 
Ansatz nach Dalton (1803). Alle drei berücksichtigen die Windgeschwindigkeit und variieren 
lediglich in der Parametrisierung der Gleichung:

\f[ E = (a + b  \cdot  \nu_{wind} )  \cdot  (p_S - p_D )  \cdot  \frac{p_{L,Ort}}{p_{L,Meer}}  \cdot  1/24000 \f]    
<!-- #tbw: bei der Formel müssen die Einheiten noch überprüft werden -->

\f$ E \f$: &ensp; Verdunstungsrate [\f$m  \cdot  h^{-1}\f$] \n
<!-- schauen, ob E und q_V das gleiche sind oder nicht (wenn ja, dann E durch q_V ersetzen -->
\f$a \f$:  &ensp; empirische Konstanten (s.u.) [-]
<!-- #mf: bzw. stand in Tabelle am Ende der Kurzdoku:
a [m  \cdot  s-1  \cdot  hPa-1]: Koeffizient für den windunabhängigen Teil der Verdunstung; Wert = 0,13; Ref = WMO 1966 
b [m  \cdot  s-1  \cdot  hPa-1]: Koeffizient für den überlagernden windbedingten Anteil; Wert = 0,0936; Ref = WMO 1966 -->

\f$ \nu_{wind} \f$: &ensp;	Windgeschwindigkeit in 2 m über der Wasseroberfläche [\f$ m s^{-1} \f$] \n
\f$p_S \f$: &ensp;	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [mbar] \n
\f$p_D \f$: &ensp;	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen 
           Thermometer [mbar] \n
\f$p_{L,Ort} \f$: &ensp;	Luftdruck bezogen auf Ortshöhe [mbar] \n
<!-- #tbw: Wird der Luftdruck irgendwo eingegeben und berücksichtigt? Wenn nicht, weglassen. -->
<!-- #mf: ich meine auch, dass hier ein anderer Faktor berecnet wird (ATkor = exp(-9.81 \cdot hWS(mstr,ior)/(287.*(templ(ior)+273.16)))  im Code) --> 
\f$p_{L,Meer} \f$: &ensp;	Luftdruck bezogen auf Meereshöhe [mbar] \n

E errechnet sich nach obiger Gleichung in mm/d. Die Umrechnung in m/h erfolgt durch Multiplikation 
mit 1/24000.
<!-- #mf: wo kommen die mm her? -->
<!-- #mf: müsste es nicht 1/86400 sein? -->

In QSim verwendete Werte für *a* und *b* (Referenz: Poß (1983)) 
| a	| b	| Literatur | 
| 0.13	| 0.0936 | WMO (1966) | 
| 0.153	| 0.063	| Sweers (1976) | 
| 0.211	| 0.103	| Rimsha-Donchenko (1957) aus Poß (1983) |


In einem vierten und fünften Verfahren erfolgt die Berechnung der Verdunstung 
ohne die Berücksichtigung des Winds.

Nach Priestley & Taylor (1972):

\f[ E = b1  \cdot  \frac{p_S - p_D}{p_S}  \cdot  \frac{\Delta}{\Delta + \gamma}  \cdot  \frac{abs(R_n)}{\lambda} \f]
<!-- #mf: nach kompilieren schauen, ob delta und gamma korrekt sind -->

Nach Hargreaves, Delclaux et al. (2007):

\f[ E = a3  \cdot  \frac{p_S - p_D}{p_S}  \cdot  (T_{L, Tr} + b3)  \cdot  \frac{abs(R_n)}{\lambda} \f]
<!-- #mf: T_{L, Tr} in °C: Einheit passt noch nicht -->

\f$ \Delta \f$: &ensp; Steigung der Sättigungsdampfdruck-Kurve [\f$ mbar  \cdot  °C^{-1}\f$] \n
\f$ \lambda \f$: &ensp; Verdampfungswärme von Wasser [\f$ KJ  \cdot  m^{-3} \f$]	  \n
\f$ \gamma \f$: &ensp; Psychrometer-Konstante [\f$ mbar  \cdot  °C^{-1}\f$] \n
* \f$ \gamma =\frac{cp_{air} \cdot p}{C_{v,t} \cdot \nu} \f$
* \f$ cp_{air}\f$:	spezifische Wärmekapazität von Luft: 1,005 \f$ [KJ \cdot kg^{-1} \cdot K-1] \f$
* *p*: atmosphärischer Druck \f$ [mbar] \f$ 
* \f$ \nu \f$: Molmassenverhältnis von Wasser und Luft [-]
* \f$ C_{v, t} \f$: Verdampfungswärme von Wasser \f$[KJ \cdot kg^{-1}] \f$
<!-- #mf hier auch schauen, ob eingerückte bullet list ohne bullet möglich -->
b1, a3, b3: empirische Konstanten (2,805; 0,04; 27,375) [-]  \n
\f$ R_n \f$: Nettostrahlung \f$[kJ \cdot m^{-2} \cdot h^{-1}]\f$
* \f$ R_n = q_s + q_{Us} - q_U \f$ 
* \f$ q_s \f$: Wärmestromdichte aus Strahlung \f$ [kJ \cdot m^{-2} \cdot h^{-1}] \f$ 
* \f$ q_{Us} \f$: Wärmestromdichte aus der vom Sediment reflektierten Strahlung 
				\f$ [kJ \cdot m^{-2} \cdot h^{-1}] \f$ 
\f$ q_U \f$: Wärmestromdichte aus Temperaturdifferenz zwischen Sediment und
	Wasser \f$[kJ \cdot m^{-2} \cdot h^{-1}]\f$  \n
\f$ T_{L, Tr} \f$: Lufttemperatur [°C] \n

Die Einführung des Quotienten  \f$\frac{p_S  -p_D }{p_S} \f$ in die Originalformeln führte 
beim Vergleich zwischen Modellergebnissen und Messungen zu deutlich besseren Ergebnissen.

Die Konstanten *b1, a3, b3* sind Kalibrierungsparameter.


Wird der Wind in einer höheren Lage gemessen als die Wasserspiegellage, wird die 
Windgeschwindigkeit durch Multiplikation mit dem Faktor \f$fkWind\f$ auf die Höhe
der Wasserspiegellage korrigiert.
<!-- #mf: fkWind noch einen schönen Namen geben -->
<!-- #mf: Achtung: h_{WS} ist von mir selbst als Name vergeben, taucht aber bestimmt anderswo 
auch auf, sollte abgestimmt werden -->

\f[ fkWind = \left(\frac{2}{h_{Wetter} - h_{WS}}\right)^{0.11} \f]
<!-- #mf: Klammersetzung in der Gleichung nochmal gegenchecken -->

\f$fkWind \f$: &ensp; Korrekturfaktor für die Windgeschwindigkeit [-] \n
\f$h_{Wetter} \f$: &ensp; Höhe der Wetterstation [m ü NN]  \n
\f$h_{WS} \f$: &ensp; Wasserspiegellage am Querprofil [m ü NN] \n

Der Sättigungsdampfdruck \f$p_S\f$ an der Wasseroberfläche berechnet sich aus der Wassertemperatur
\f$T_W\f$ [°C] nach der Magnus Formel zu:
<!-- #mf: überprüfen, ob QSim in °C oder K rechnet, sollte eigtl. °C sein -->

\f[p_S = p_0 \cdot e^\left({\frac{c_2 \cdot T_W}{c_3 + T_W}}\right) \f]

\f$p_S \f$: &ensp;	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [mbar] \n
\f$p_0 \f$: &ensp;	Dampfdruck bei der Wassertemperatur 0; \f$ p(T_W = 0) \f$ = 6,10780 [mbar] \n
\f$c_2 \f$: &ensp;	spezifische Verdampfungswärme = 17,08085 [-] \n
\f$c_3 \f$: &ensp;	spezifische Gaskonstante = 234,175 [°C] \n

Der Partialdampfdruck der Luft \f$p_D\f$ bei gemessener Lufttemperatur am trockenen 
Thermometer \f$T_{L, Tr}\f$ [°C] berechnet sich äquivalent zu \f$p_S\f$ unter Berücksichtigung der 
relativen Luftfeuchtigkeit \f$F_{Rel}\f$ [%]:

\f[ p_D = p_0 \cdot e^\left({\frac{c_2 \cdot T_{L, Tr}}{c_3+T_W}}\right) \cdot \frac{F_{Rel}}{100} \f]


\f$p_D\f$: 		Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer [mbar] \n
\f$p_0\f$:	 	\f$ p(TW=0) \f$ = 6,10780 [mbar] \n
\f$c_2\f$:		empirische Konstante = 17,08085 [-] \n
\f$c_3\f$:		empirische Konstante = 234,175 [K]  \n
\f$F_{rel}\f$:	relative Luftfeuchte [%] \n

Aus der latenten Wärme \f$q_V\f$ kann die Verdunstungsrate \f$h_V\f$ [m/s] über folgenden 
Zusammenhang berechnet werden:

\f[h_V = \frac{q_V}{\rho_W \cdot c_V} \f]


\f$h_V\f$:		Verdunstungsrate [m/s] \n
\f$q_V\f$:		Wärmestromdichte aus Verdunstung (latente Wärme) [\f$ kJ/(h \cdot m²) \f$] \n
\f$\rho_W\f$:	Dichte des Wassers = 1.000 [kg/m³] \n 
\f$c_V\f$:		latente Verdampfungswärme  von Wasser [kJ/kg] \n

Die latente Verdampfungswärme \f$c_V\f$ von Wasser berechnet sich in Abhängigkeit der 
Wassertemperatur \f$T_W\f$ [°C] und gibt an, wieviel Energie benötigt wird, um einen Kilogramm 
flüssiges Wasser zu verdunsten:
 
\f[ c_V = 595,24 - 0,569 \cdot T_W \f]

\f$c_V\f$:	latente Verdampfungswärme  von Wasser [kJ/kg] \n
\f$c_4\f$:	empirische Konstante = 595,24 [kJ/kg] \n
\f$c_5\f$:	empirische Konstante = 0,569 [kJ/(kg*K)] \n
<!-- #mf c4 und c5 tauchen nicht in der Formel auf; im Code gegenchecken -->


# Konvektion {#lnk_konvektion}#

Konvektion ist der direkte Wärmeaustausch zwischen Luft und Wasseroberfläche. Sie findet nur 
bei unterschiedlichen Temperaturen von Luft und Wasseroberfläche statt und ist, wie auch die 
Verdunstung, abhängig von der Windgeschwindigkeit. Der Einfluss der Konvektion auf die
Wärmehaushaltsbilanz eines Gewässers ist meist sehr viel geringer als der der Verdunstung. In
Abhängigkeit der Temperaturverhältnisse zwischen Luft und Wasser kann der Konvektionsterm positive 
oder negative Werte annehmen. Von den verschiedenen Ansätzen wird in QSim der Ansatz nach LAWA 
(1991) verwendet, für den sich der konvektive Wärmestrom nach folgender Formel berechnet:

 
\f[ q_K = q_V \cdot \frac{T_W - T_{L,Tr}}{1,53 \cdot (p_S - p_D)} \f]

\f$q_K\f$:	Wärmestromdichte aus Konvektion [\f$ kJ/(h \cdot m2) \f$]  \n
\f$q_V\f$:	Wärmestromdichte aus Verdunstung [\f$ kJ/(h \cdot m2) \f$] \n
\f$ T_W \f$:	Wassertemperatur [°C] \n
\f$T_{L,Tr}\f$:	Lufttemperatur, gemessen am trockenen Thermometer [°C]  \n
\f$p_S\f$:	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [mbar] \n
\f$p_D\f$:	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer [mbar] \n


# Wärmestromdichte aus direkter Einleitung {#lnk_waermeeinleitung} #

Neben den bisher genannten mehr oder weniger natürlichen Komponenten der Wärmehaushaltsbilanz 
muss die Wärmestromdichte aus direkter Einleitung berücksichtigt werden. Diese umfasst den 
Wärmeeintrag durch Kühlwassereinleitung, aber auch die Erwärmung durch den Zufluss meist 
wärmerer Nebengewässer. Eine Abkühlung durch Grundwasserzustrom könnte auch
über eine Einleitung abgeschätzt werden. 

 
\f[q_E = c_W \cdot \rho_W \cdot \nu_E \cdot \Delta T \f]
<!-- #mf: prüfen ob nu und Delta richtig sind -->

\f$q_E\f$:		Wärmestromdichte aus direkter Einleitung in kJ/(h m^2) \n
\f$c_W\f$:		spez. Wärmekapazität von Wasser = 4,1868 103 J/(kg K) \n
\f$\rho_W\f$:	Dichte des Wassers = 1.000 kg/ m3 \n
\f$\nu_E\f$:	Einleitgeschwindigkeit = QE/AE (Punktquelle) bzw. Q_L/LE (Linienquelle) in m/s \n
* \f$QE\f$:		streckenbezogene Wassermenge der punktförmigen Einleitung [m³*s-1*m-1] \n
* \f$AE\f$: \n	
* \f$Q_L\f$:	streckenbezogene Wassermenge der linienförmigen Einleitung [m³*s-1*m-1] \n
* \f$LE\f$:	 \n
<!-- #mf QE, AE, qE, LE sollten unter nu_E eingerückt sein; überprüfen + ist eine Einrückung 
ohne bullet points möglich? -->

\f$ \Delta_T \f$:	Aufwärmspanne, Temperaturdifferenz (T1-T2) in K  \n

Zur Vereinfachung der Berechnung werden für eine punktförmige Einleitung ein homothermer 
(gleichmäßig warmer) Wasserkörper sowie die sofortige vollständige Durchmischung der Ströme 
angenommen, obwohl sich unterhalb der Einleitungsstelle je nach Art des Fließvorgangs mehr oder 
minder ausgeprägte Zonen und Schichten unterschiedlicher Temperatur ausbilden. Für die meisten
Aufgabenstellungen (z.B. großräumige Wärmebilanzen) ist diese Vereinfachung jedoch zulässig.

Der Einfluss der Linienquellen (diffuser Eintrag) wird vor der Berechnung der Temperaturänderung 
durch die Wärmestromdichten wie folgt berücksichtigt:

 
\f[ T_W = T_{W-1} + \frac{(T_L - T_{W-1}) \cdot Q_L}{FLAE} \cdot \Delta t \f]

<!-- #mf: in Variablenbeschreibung: müsste da nicht °C stehen? es wird doch nicht in K gerechnet? -->

\f$T_W\f$:		Wassertemperatur nach dem Zeitschritt Δt [K] \n
\f$T_{W-1}\f$:	Wassertemperatur vor dem Zeitschritt Δt [K] \n
\f$T_L\f$:		Wassertemperatur der linienförmigen Einleitung [K] \n
<!-- #mf: K sollte überall durch °C ersetzt werden, no?? -->
\f$Q_L\f$:		streckenbezogene Wassermenge der linienförmigen Einleitung [m3*s-1*m-1] \n
\f$FLAE\f$:  	Querschnittsfläche des Gewässers [m2] \n
\f$\Delta_t\f$:	Zeitschritt [h] \n
<!-- #mf: Delta_t steht in h, Q_L steht in m3/s; Gleichung evtl. noch mit 86400 multiplizieren -->

Dies gilt analog für alle anderen Stoffe, für die ebenfalls ein diffuser Eintrag berücksichtigt wird.


# Rand- und Anfangsbedingungen #

Von entscheidender Bedeutung für die Berechnung der lokalen Wärmeflüsse sind die 
meteorologischen Bedingungen:

Sonnenstand, Bewölkung, Lufttemperatur und -feuchte sowie die Windgeschwindigkeit.\n\n

Diese Wetterdaten werden bei der eingabe() von Subroutine wetter_readallo() aus der Datei 
<a href="./exp/WETTER.txt" target="_blank">WETTER.txt</a> gelesen.
\n\n
! Wetterdaten für Waermebilanz in diesem Zeitschritt wurden in randbedingungen_setzen() ermittelt\n
! call wettles_wetter()  ! ersetzt wettles(), interpoliert Wetterdaten für den aktuellen Zeitpunkt\n
! call temperl_wetter()  ! ersetzt Temperl(), berechnet Lufttemperatur und legt sie in tlmax_T ab.\n
! call strahlg_wetter()  ! berechnet aus der Globalstrahlung den Strahlungsanteil, 
der im Gewässer ankommt.
\n\n
Im 1-dimensionalen QSim besteht die Möglichkeit <b>Wärmeeinleitungen</b> z.B. durch Kraftwerke 
direkt in der Temperaturberechnung zu berücksichtigen. Im mehrdimensionalen T-QSim muss dies 
über Randbedingungen vorgegeben werden. D. h. einen Ausströmrand an dem der Volumenstrom 
entnommen wird und einem Einströmrand, an dem das erwärmte Wasser ins Gewässer(Modellgebiet) 
zurückfließt.

Veröffentlichungen/weitere Dokumentation
----------------------------------------

Eine Modellbeschreibung in englischer Sprache ist im Anhang D des IKSR Berichts 
<a href="http://bibliothek.bafg.de/index.asp?detsuche_systematik=online+280" target="_blank">
Estimation of the effects of climate change scenarios on future Rhine water temperature development </a> zu finden.

Eine Vorgängerversion dieser Dokumentation mit einer ausführlichen Beschreibung dieses 
Modellbausteins einschließlich der Angabe sämtlicher Formeln findet sich in 
der\n <a href="./pdf/Temperatur_Doku_Volker.pdf" target="_blank">Dokumentation Temperatur</a> von Volker Kirchesch \n

Zitierte Literatur 
------------------

* ABBASI, A., F.O. ANNOR & N. VAN DE GIESEN: A framework to simulate small shallow inland Water bodies in semi-arid regions. - Advances in Water Resources, 110, 77-96 (2017)

* Delclaux, F., A. Coudrain & T. Condom: Evaporation estimation on Lake Titicaca: a synthesis review and modelling. – Hydrological Processes 21, 1664–1677 (2007)

* Poß, G.: Untersuchungen zum Abwärmetransport aus Fließgewässern an die Atmosphäre. -  Kernforschungszentrum Karlsruhe GmbH, ISSN 0303-4003 (1983)

* Priestley, C. H. B. & R. J. Taylor: On the assessment of surface heat flux and evaporation using large-scale parameters. -  Monthly Weather Review (100), 81-92, (1972)

* Sweers, H. E. : A nomogram to estimate the heat-exchange coefficient at the air-water interface as a function of wind speed and temperature; a critical survey of some literature. - Journal of Hydrology 30, 375-401 (1976)

* WMO, WORLD METEOROLOGICAL ORGANIZATION: Measurement and Estimation of Evaporation and Evapotranspiration. - Technical Note No. 83 (WMO-No. 201, TP. 105). Geneva (1966)





Textquelle: wtemp-prozess.md ; Codesources: TEMPERW.f90, temperw_huelle.f95 ;  zurück: \ref lnk_wtemp
