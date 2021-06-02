Wassertemperatur - Prozesse {#lnk_wtemp_prozesse} 
=====================

# Bilanz {#TempAenderung} #
Die Grundlage aller verbreiteten Methoden zur Berechnung der Erwärmung eines 
Fließgewässers ist die vereinfachte Wärmehaushaltsgleichung. 
Auf deren Grundlage wird die Wärmehaushaltsbilanz des Gewässers erstellt, 
mit der die Temperaturänderung des Gewässers pro Zeiteinheit quantifiziert werden kann. 
Damit lautet die vereinfachte Wärmehaushaltsgleichung:

\f[ \frac{\delta T_w}{\delta t} = \frac{q_S - q_V - q_K + q_{US} + q_U - q_E }{c_w * h * \rho_w}   \f]    


## Verdunstung {#lnk_verdunstung}##
===============

Verdunstung entsteht beim Druckausgleich des Dampfdrucks zwischen Wasseroberfläche 
und der darüber liegenden Luftschicht. 
Ist der Dampfdruck an der Wasseroberfläche größer als der der Luft, verdunstet Wasser aus 
dem Gewässer. 
In Abhängigkeit von den Dampfdruckverhältnissen kann der Verdunstungsterm in der 
Wärmehaushaltsbilanz aber auch negative Werte annehmen. 
Da für diesen Vorgang abhängig von der Wassertemperatur mehr oder weniger Verdampfungswärme 
benötigt wird, kühlt sich der Wasserkörper ab. 
Im umgekehrten Fall kondensiert Wasser aus der Luft ins Gewässer. Die bei diesem 
Phasenübergang erster Ordnung aufgenommene oder abgegebene Energiemenge wird auch als 
latente (lateinisch: „verborgen“) Wärme bezeichnet.

In QSim erfolgt die Berechnung der latenten Wärme nach dem Ansatz von Dalton (1803), 
wobei die Verdunstungsrate eine Funktion der Windgeschwindigkeit und der Differenz der 
Sättigungsdampfdrücke an der Phasengrenze Wasser-Luft ist. Die Berechnung beruht in diesem 
Fall auf einer empirisch ermittelten Formel, der sogenannten „Wind-Formel“. Die Konstanten 
a und b der Wind-Formel beziehen sich auf eine Referenz der World Meteorological Organisation 
(WMO, 1966).

\f[ q_V= (a+b*v_{wind} )*(p_S-p_D )*p_(L,Ort)/p_(L,Meer) *c \f]    \label{equ_qV}


Der Sättigungsdampfdruck pS an der Wasseroberfläche berechnet sich aus der Wassertemperatur TW [K] nach der Magnus Formel zu:
p_S=p_0*e^((c_2*T_W)/(c_3+T_W ))	[16]


pS	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [mbar], Formel Fehler! Verweisquelle konnte nicht gefunden werden.] p0	p(TW=0) = 6,10780 [mbar]
c2	spezifische Verdampfungswärme = 17 ,08085 [-]
c3	spezifische Gaskonstante = 234 ,175 [°C]

Der Partialdampfdruck der Luft pD bei gemessener Lufttemperatur am trockenen Thermometer TL [°C] berechnet sich äquivalent zu pS unter Berücksichtigung der relativen Luftfeuchtigkeit FRel [%]:
p_D=〖(p〗_0*e^((c_2*T_L)/(c_3+T_W )))*F_Rel/ 100	[177]


pD	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer [mbar]
p0	p(TW=0) = 6,10780 [mbar]
c2	empirische Konstante = 17,08085 [-]
c3	empirische Konstante = 234,175 [K] 
Frel	relative Luftfeuchte [%]

Aus der latenten Wärme qV kann die Verdunstungsrate hV [m/s] über folgenden Zusammenhang berechnet werden:
h_V=q_V/(〖(ρ〗_W*c_V))   	[18]


hv	Verdunstungsrate [m/s]
qV	Wärmestromdichte aus Verdunstung (latente Wärme) [kJ/(h*m²)]
ρW	Dichte des Wassers = 1.000 [kg/m³]
cV	latente Verdampfungswärme  von Wasser [kJ/kg], siehe Formel Fehler! Verweisquelle konnte nicht gefunden werden.] 
Die latente Verdampfungswärme cV von Wasser berechnet sich in Abhängigkeit der Wassertemperatur TW [°C] und gibt an, wieviel Energie benötigt wird, um einen Kilogramm flüssiges Wasser zu verdunsten:

 
[19 ]


cV	latente Verdampfungswärme  von Wasser [kJ/kg]
c4	empirische Konstante = 2501,7 [kJ/kg]
c5	empirische Konstante = 2,366 [kJ/(kg*K)]


Konvektion ist der direkte Wärmeaustausch zwischen Luft und Wasseroberfläche. Sie findet nur bei unterschiedlichen Temperaturen von Luft und Wasseroberfläche statt und ist, wie auch die Verdunstung, abhängig von der Windgeschwindigkeit. Der Einfluss der Konvektion auf die Wärmehaushaltsbilanz eines Gewässers ist meist sehr viel geringer als der der Verdunstung. In Abhängigkeit der Temperaturverhältnisse zwischen Luft und Wasser kann der Konvektionsterm positive oder negative Werte annehmen. Von den verschiedenen Ansätzen wird in QSim der Ansatz nach LAWA (1991) verwendet, für den sich der konvektive Wärmestrom nach folgender Formel berechnet:

 
[20 ]


qK	Wärmestromdichte aus Konvektion [kJ/(h*m2)]
qV	Wärmestromdichte aus Verdunstung nach Formel Fehler! Verweisquelle konnte nicht gefunden werden. [kJ/(h*m2)] TW	Wassertemperatur [°C]
TL,Tr	Lufttemperatur, gemessen am trockenen Thermometer [°C]
pS	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [Formel Fehler! Verweisquelle konnte nicht gefunden werden.] [hPa]
pD	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer [hPa, Formel Fehler! Verweisquelle konnte nicht gefunden werden.]  

Neben den bisher genannten mehr oder weniger natürlichen Komponenten der Wärmehaushaltsbilanz muss die Wärmestromdichte aus direkter Einleitung berücksichtigt werden. Diese umfasst den Wärmeeintrag durch Kühlwassereinleitung, aber auch die Erwärmung durch den Zufluss meist wärmerer Nebengewässer bzw. die Abkühlung durch Grundwasserzustrom. 

 
[21]


qE	Wärmestromdichte aus direkter Einleitung in kJ/(h m2)
cW	spez. Wärmekapazität von Wasser = 4,1868 103 J/(kg K)
ρW	Dichte des Wassers = 1.000 kg/ m3
vE	Einleitgeschwindigkeit = QE/AE (Punktquelle) bzw. qE/LE (Linienquelle) in m/s
	QE	streckenbezogene Wassermenge der punktförmigen Einleitung [m³*s-1*m-1]
	AE	
	qE	streckenbezogene Wassermenge der linienförmigen Einleitung [m³*s-1*m-1]
	LE	
ΔT	Aufwärmspanne, Temperaturdifferenz (T1-T2) in K

Zur Vereinfachung der Berechnung werden für eine punktförmige Einleitung ein homothermer (gleichmäßig warmer) Wasserkörper sowie die sofortige vollständige Durchmischung der Ströme angenommen, obwohl sich unterhalb der Einleitungsstelle je nach Art des Fließvorgangs mehr oder minder ausgeprägte Zonen und Schichten unterschiedlicher Temperatur ausbilden. Für die meisten Aufgabenstellungen (z.B. großräumige Wärmebilanzen) ist diese Vereinfachung jedoch zulässig.

Der Einfluss der Linienquellen (diffuser Eintrag) wird vor der Berechnung der Temperaturänderung durch die Wärmestromdichten wie folgt berücksichtigt:

 
[22]


Tm	Wassertemperatur nach dem Zeitschritt Δt [K]
Tm-1	Wassertemperatur vor dem Zeitschritt Δt [K]
TL	Wassertemperatur der linienförmigen Einleitung [K]
QL	streckenbezogene Wassermenge der linienförmigen Einleitung [m3*s-1*m-1]
FLAE	Querschnittsfläche des Gewässers [m2]
Δt	Zeitschritt [h]

Dies gilt analog für alle anderen Stoffe, für die ebenfalls ein diffuser Eintrag berücksichtigt wird.

Tabelle 4: Bei der Modellierung der Wassertemperatur benutzte Variablenwerte
Bezeichnung im Text	Bezeichnung im Code	Einheit	Bedeutung	Wert in QSim	Referenz
a		m*s-1*hPa-1	Koeffizient für den windunabhängigen Teil der Verdunstung	0,13	WMO 1966
AE					
b		m*s-1*hPa-1	Koeffizient für den zu überlagernden windbedingten Anteil	0,0936	WMO 1966
c		-	Faktor aus der Umrechnung der hV von mm/d in m/h	1/24.000	
cV	VDW	kJ*kg-1	latente Verdampfungswärme von Wasser	Formel Fehler! Verweisquelle konnte nicht gefunden werden.

cW	speWKW	J*kg-1* K-1	spezifische Wärmekapazität von Wasser	4,1868	
FLAE	flae	m2	Querschnittsfläche des Gewässers		
Frel	IDWe	%	relative Luftfeuchte		
h	Btiefe	m	mittlere Wassertiefe		
hv	HR	m*s-1	Verdunstungsrate		
LE					
pD	pdltt	mbar	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer	Formel Fehler! Verweisquelle konnte nicht gefunden werden.]

pL,Meer		mbar	Luftdruck bezogen auf Meereshöhe		
pL,Ort		mbar	Luftdruck auf Ortshöhe		
pS	sddw	mbar	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche	Formel Fehler! Verweisquelle konnte nicht gefunden werden.

qE		m³*s-1*m-1			
QE		m³*s-1*m-1	streckenbezogene Wassermenge der punktförmigen Einleitung		
qE	EWAERM	kJ*h-1*m-2	Wärmestromdichte aus direkter Einleitung	Formel Fehler! Verweisquelle konnte nicht gefunden werden.

qK		kJ*h-1*m-2	Wärmestromdichte aus Konvektion	Formel Fehler! Verweisquelle konnte nicht gefunden werden.

QL		m³*s-1*m-1	streckenbezogene Wassermenge der linienförmigen Einleitung		
qS		kJ*h-1*m-2	Wärmestromdichte aus Strahlung		
q(TW,t)	WSTRWS	kJ*h-1*m-2	Wärmestromdichte aus dem Austausch an der Wasseroberfläche		
qU		kJ*h-1*m-2			
qUS		kJ*h-1*m-2			
qV	WV	W*m-2	Wärmestromdichte aus Verdunstung	Formel Fehler! Verweisquelle konnte nicht gefunden werden.

ρW	roh2o	kg*m-3	Dichte des Wassers	1.000	
σ	stbk	kJ*m-2 *k-4	Stefan-Boltzmann-Konstante	2.0411 * 10-7	
t		h	Zeiteinheit		
TL	ETEMP	K	Wassertemperatur der linienförmigen Einleitung		
TL,Tr	TEMPL	K	Lufttemperatur gemessen am trockenen Thermometer	Messwert	
TW	TEMPW	K	Tiefengemittelte Wassertemperatur	Messwert	
vE	tflie	m*s-1	Einleitgeschwindigkeit		
vwind	fkwind	m*s-1	Windgeschwindigkeit	Messwert	



Überarbeitung:
============
Berechnungsverfahren für die Verdunstung
Für die Berechnungsverfahren nach WMO, Sweers und Rimsha-Donchenko wird der Ansatz nach Dalton verwendet. Die drei Verfahren haben unterschiedliche Werte für die Konstanten a und b.
Ansatz nach Dalton unter Berücksichtigung der Windgeschwindigkeit:
E= (a+b*v_wind )*(p_S-p_D )*p_(L,Ort)/p_(L,Meer) *1/24000
[1]


E	Verdunstungsrate [m*h-1]
a, b	empirische Konstanten [-]
vwind	Windgeschwindigkeit in 2 m über der Wasseroberfläche [m*s-1]
pS	Sättigungsdampfdruck bei der Wassertemperatur an der Wasseroberfläche [hPa] 
pD	Partialdampfdruck bei der Lufttemperatur, gemessen am trockenen Thermometer [hPa] 
pL,Ort	Luftdruck bezogen auf Ortshöhe [hPa]
pL,Meer	Luftdruck bezogen auf Meereshöhe [hPa]

E errechnet sich nach obiger Gleichung in mm/d. Die Umrechnung in m/h erfolgt durch Multiplikation mit 1/24000.

In QSim verwendete Werte für a und b 
a	b	Literatur
0.13	0.0936	WMO (1966)
0.153	0.063	SWEERS (1976)
0.211	0.103	RIMSHA-DONCHENKO (1957)
AUS: G.POß ( 1983)


Berechnungsansätze für die Verdunstung ohne Berücksichtigung des Winds
nach PRIESTLEY-TAYLOR (1972)
E=b1*  ((p_S-p_D ))/ps*∆/(∆+γ)*(abs(R_n))/λ	[2]


nach dem Hargreaves-Verfahren, DELCLAUX ET AL. (2007)
E=a3*  ((p_S-p_D ))/ps*(T_L+b3)*(abs(R_n))/λ	[3]


Δ		Steigung der Sättigungsdampfdruck-Kurve [hPa*°C-1]
λ		Verdampfungswärme von Wasser [KJ*m-3]	
γ		Psychrometer-Konstante [hPa*°C-1]
		γ=(〖cp〗_air*p)/(C_(v_t)*v)
		Cpair	spezifische Wärmekapazität von Luft: 1.005 [KJ*kg-1*K-1]
		P	atmosphärischer Druck [hPa]
		v	Molmassenverhältnis von Wasser und Luft [-]
		Cv_t	Verdampfungswärme von Wasser [KJ*Kg-1]
b1, a3, b3		empirische Konstanten (2.805; 0.04; 27.375) [-]
Rn		Nettostrahlung [kJ*m-2*h-1]
		R_n=q_s+q_Us-q_U
			qs 	Wärmestromdichte aus Strahlung [kJ*m-2*h-1]
			qUs 	Wärmestromdichte aus vom Sediment reflektierter Strahlung 
				[kJ*m-2*h-1]
qU	Wärmestromdichte aus Temperaturdifferenz zwischen Sediment und
	Wasser [kJ*m-2*h-1]
TL		Lufttemperatur [°C]

Die Einführung des Quotienten  ((p_S-p_D ))/ps in die Originalformeln führte beim Vergleich zwischen Modellergebnissen und Messungen zu deutlich besseren Ergebnissen.
Die Konstanten b1, a3, b3 sind Kalibrierungsdaten.


 
Berechnung der Strahlungsabsorption in den einzelnen vertikalen Schichten
Gegenüber dem von der BfG übergebenen Code wurde hier die langwellige Strahlung in zwei Wellenlängenbereiche unterteilt, wobei erst einmal der Extinktionskoeffizient für beide Bereiche als gleich angenommen wurde (4 m-1, wie im Originalcode). Eine Verbesserung wäre hier sicherlich die Einführung der in Tabelle 1 gelisteten Werte. Des Weiteren wurde  die von der Sohle reflektierte Strahlung in jeder vertikalen Schicht berücksichtigt. Dies war im Ausgangscode nicht der Fall und stellt damit eine Ungenauigkeit dar. Der Reflexionsanteil sollte laut Literatur nicht wie bisher 80% sondern besser nur 20% betragen.
Absorption der Globalstrahlung und atmosphärische Gegenstrahlung
Bei der einfallenden Globalstrahlung wird zwischen drei Wellenlängenbereichen unterschieden:
Der Wellenlängenbereich <=700 nm (UV-und fotosynthetisch aktive Strahlung):
〖GS〗_(PARS,k)=GS*(f_UV+f_PARS )	[4]


und die Wellenlängenbereiche >700 <=910 nm und >910 nm. Diese beiden Wellenlängenbereiche gelten auch für die langwellige Gegenstrahlung.
〖SL1〗_k=GS*〖fL〗_1+G*〖fL〗_1/(〖fL〗_1+〖fL〗_2 )
〖SL2〗_k=GS*〖fL〗_2+G*〖fL〗_2/(〖fL〗_1+〖fL〗_2 )	[5]


GS		Globalstrahlung an der Gewässeroberfläche [kJ*m-2*h-1]
G		langwellige atmosphärische Gegenstrahlung
fUV, fPARS	Anteil der UV- und fotosynthetisch aktiven Strahlung an der
		Globalstrahlung [-]
fL1		Anteil der Strahlung der Wellenlänge 700-910 nm an der
Globalstrahlung [-]
fL2		Anteil der Strahlung der Wellenlänge >910 nm an der
Globalstrahlung [-]

Die Strahlungsintensität in der Tiefe z errechnet sich für die drei Wellenlängenbereiche zu: 
〖GS〗_(PARS,k+1)=〖GS〗_(PARS,k)*exp⁡(-extk*z) 
〖SL1〗_(k+1)=〖SL1〗_k*exp⁡(-〖extkL〗_1*z)       
〖GS〗_(PARS,k+1)=〖GS〗_(PARS,k)*exp⁡(-〖extkL〗_2*z)  für j=1, n-1	[6]


GSPARS,k, SL1k, SL2k	Strahlung der drei Wellenlängenbereiche am Anfang der jeweiligen vertikalen Schicht [kJ*m-2*h-1]

GSPARS,k+1, SL1k+1, SL2k+1	Strahlung am Ende der jeweiligen vertikalen Schicht
(Schichtdicke = z) [kJ*m-2*h-1]
z				Dicke der jeweiligen vertikalen Schichten [m]
j				Laufvariable für die vertikalen Schichten
n				Anzahl der vertikalen Schichten
k				kennzeichnet Schichtanfang (1) und Schichtende (2)
Extk, extkL1, extkL2		Lichtextinktionskoeffizient für Licht der drei 
Wellenlängenbereiche [m-1]
				

Die Wärmestromdichte qs,j in kJ*m-2*h-1 durch die einfallende Strahlung errechnet sich somit für alle Schichten aus:
Für j = 1, n-1:
 q_(s,j)=〖(GS〗_(PARS,k)-〖GS〗_(PARS,k+1))+(〖SL1〗_k-〖SL1〗_(k+1))
             +(〖SL2〗_k-〖SL2〗_(k+1))

〖GS〗_(PARS,k)=〖GS〗_(PARS,k+1)
〖SL1〗_k=〖SL1〗_(k+1)
〖SL2〗_k=〖SL2〗_(k+1)
	[7]


Tab.1: Aufteilung der Strahlung und entsprechende Extinktionskoeffizienten
(ABBASI ET AL. 2017):
Wellenlänge	Anteil fi an der gesamten Strahlung  [-]	Extinktionskoeffizient [m-1]
< 400	0.046 	Wie PARS 
400 - 700	0.45 
(eigene Messungen)	Wird berechnet
700 - 910	0.21 	2.92 (eigene Messungen ~4) 

>910	0.294 	93 


Absorption der von der Gewässersohle reflektierten Strahlung
Für die drei Wellenlängenbereiche errechnet sich die von der Gewässersohle reflektierte Strahlung aus:

〖GSr〗_PARS=GS*PSREFS*(f_UV+f_PARS )*exp⁡(-extk*H)
SL1r=(GS*〖fL〗_1*PSREFS+G*PSREFS*〖fL〗_1/(〖fL〗_1+〖fL〗_2 ))
                 *exp⁡(-〖extkL〗_1*H)
SL2r=(GS*〖fL〗_2*PSREFS+G*PSREFS*〖fL〗_2/(〖fL〗_1+〖fL〗_2 ))
                 *exp⁡(-〖extkL〗_2*H)
	[8]


GSrPARS	reflektierte Strahlungsmenge der Wellenlängen <=700 nm an der
Gewässersohle [kJ*m-2*h-1]
SL1r		reflektierte Strahlungsmenge der Wellenlängen >700 <=910 nm an der
Gewässersohle [kJ*m-2*h-1]
SL1r		reflektierte Strahlungsmenge der Wellenlängen >910 nm an der
 Gewässersohle [kJ*m-2*h-1]
PSREFS		Reflektionsanteil der auf die Gewässersohle auftreffenden Strahlung [-]
H		Gewässertiefe [m]

Die Strahlungsmenge die von der reflektierten Strahlung die Gewässeroberfläche wieder erreicht ergibt sich aus: 
〖GSr〗_(PARS,k)=〖GSr〗_PARS*exp⁡(-extk*H)
〖SL1r〗_k=SL1r*exp⁡(-〖extkL〗_1*H)
〖SL2r〗_k=SL2r*exp⁡(-〖extkL〗_2*H)
	[9]














Die Wärmstromdichte qUS,j  kJ*m-2*h-1aus der reflektierten Strahlung  errechnet sich für die einzelnen Schichten aus:
Für j = 1, n-1:
k=1
 q_(US,j)=〖(GSr〗_(PARS,k+1)-〖GSr〗_(PARS,k))+(〖SL1r〗_(k+1)-〖SL1r〗_k)
             +(〖SL2r〗_(k+1)-〖SL2r〗_k)
mit: 
〖GSr〗_(PARS,k+1)=〖GSr〗_(PARS,k)*exp⁡(xtk*z)
〖SL1r〗_(k+1)=〖SL1r〗_k*exp⁡(-〖extkL〗_1*z)
〖SL2r〗_(k+1)=〖SL2r〗_k*exp⁡(-〖extkL〗_2*z)

〖GSr〗_(PARS,k)=〖GSr〗_(PARS,k+1)
〖SL1r〗_k=〖SL1r〗_(k+1)
〖SL2r〗_k=〖SL2r〗_(k+1)
	[10]



GSrPARS,k, SL1rk, SL2rk	reflektierte Strahlung der drei Wellenlängenbereiche am Anfang der jeweiligen vertikalen Schicht [kJ*m-2*h-1]
GSrPARS,k+1, SL1rk+1, SL2rk+1	Strahlung am Ende der jeweiligen vertikalen Schicht
(Schichtdicke = z) [kJ*m-2*h-1]
z				Dicke der jeweiligen vertikalen Schichten [m]
j				Laufvariable für die vertikalen Schichten
n				Anzahl der vertikalen Schichten
Extk, extkL1, extkL2		Lichtextinktionskoeffizient für Licht der drei 
Wellenlängenbereiche [m-1]
				




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



&nbsp aus Datei: wtemp-prozess.md; Code in Datei TEMPERW.f90
