Chelicorophium - Prozesse {#lnk_corophium_prozesse}
==========================

_Letzte Änderung am 26.10.2022_

\warning Momentan ist das Modul ausgeschaltet, das heißt, Corophium wird 
nicht simuliert. Es handelt sich um eine alte Dokumentation.


# Entwicklung von Chelicorophium - Überblick

Die Entwicklung der Chelicorophiumpopulation als Startgeneration G1 am 01.01. 
mit drei Vermehrungszyklen wird in QSim in Abhängigkeit von der Jahreszeit 
berücksichtigt. Um die unterschiedlichen Lebensdauern der einzelnen Generationen 
abbilden zu können, rechnet QSim im Jahresverlauf mit insgesamt 5 Generationen.

Wie Abb. 1 zeigt, spielen dabei die drei Zeitpunkte TagG1 (nrg1) (15.4.), 
TagG2 (nrg2) (15.6.) und TagG3 (nrg3) (15.8.) eine maßgebliche Rolle. Sie werden 
analog zu dem unter Makrophyten beschriebenen Vorgehen aus gesetzten Tagen und 
Monaten als Tage des Jahres berechnet. Am Start wurden für Chelicorophium 
100 Individuen pro m² eingegeben. An TagG1 bildet diese Startgeneration G1 
Eier (=G2) und stirbt selbst ab, an TagG2 bildet die Generation G2 Eier (=G3), 
wobei ihr Gewicht reduziert wird. An TagG3 bilden schließlich die Generationen 
G2 (=G4) und G3 (=G5) Eier, G2 stirbt ab und G3 bleibt bestehen. Nach dem TagG1 
(15.4.) zeigen alle Generationen einen leichten Verlust mit der Zeit, ab dem 
15.8. steigt die Mortalitätsrate generationsspezifisch an, nur G5 hat weiterhin 
vergleichsweise geringe Verluste und kann so die Ausgangspopulation für das 
nächste Jahr bilden. Mit jeder Generation steigt die Abundanz von 
Chelicorophium zunächst ca. um einen Faktor 10 an.

![Entwicklung der 5 Generationen von Corophium in QSim im Jahresverlauf, Überblick](img/corophium_Generationen_Entwicklung.png)
\n\n


# Dichte und Filtration
Vor jeder weiteren Berechnung wird die Ausgangsindividuendichte auf der Sohle 
und der Böschung bestimmt sowie die Anzahl der Individuen.

Zunächst werden die Chelicorophiumdichten auf Böschung (coroi(ior)) und Sohle 
(corois(ior)) ermittelt, indem die Summe über alle 5 Generationen (i = 1 bis 5) 
gebildet wird.

    (1) coroi(ior) = coro(ior,1)+coro(ior,2)+coro(ior,3)+coro(ior,4) &
    (2)  +coro(ior,5)        

    (3)  corois(ior) = coros(ior,1)+coros(ior,2)+coros(ior,3)+coros(ior,4) &
    (4)  +coros(ior,5)      

Daraus werden wiederum die in dem modellierten Gewässerabschnitt vorhandenen 
absoluten Chelicorophiumzahlen auf Böschung und Sohle (corg bzw. corsg) 
ermittelt, wobei die Länge des Gewässerabschnittes (elen) und die Böschungs- 
und Sohllängen (lboem, bsohlm) verwendet werden.

    (5)  corg = coroi(ior)*2.*lboem(ior)*elen(ior)   
    (6)  corsg = corois(ior)*bsohlm(ior)*elen(ior)    

Das von Chelicorophium filtrierte Wasservolumen [m³/s] (volfm³) ergibt sich 
schließlich durch die folgenden beiden Formeln:

    (7)  volfl = rfil*corg+rfil*corsg    
    (8)  volfm3 = volfl*tflie/1000    
 
oder vereinfacht durch:

    (9) volfm³ = rfil * (corg+corsg) *tflie/1000

mit 

rfil = Filtrierrate in Liter pro Individuum und Tag (= 0,12) \n
tflie = Fließzeit, entspricht eingestellter Zeitschrittweite (d)

Das im Abschnitt vorhandene Wasser [m³] (vol) ergibt sich durch

    (10) vol = flae(ior)*elen(ior)   

mit:

flae(ior) = Querschnittsfläche des Abschnitts \n
elen(ior) = Elementlänge

Der Anteil des Wassers (ohne Berücksichtigung von Q), der von Chelicorophium 
in der Fließzeit filtriert wird (hconf), ergibt sich durch

    (11) hconf = volfm3/vol         

Die Variable hconf kann dabei maximal einen Wert von 1 annehmen.

Die Biomassen der jeweiligen Algengruppen werden mit dem Faktor hconf 
multipliziert (Ergebnis: algcok, algcog, algcob).

    (11)  algcok(ior) = aki(ior)*hconf 
    (12)  algcog(ior) = agr(ior)*hconf 
    (13)  algcob(ior) = abl(ior)*hconf 

Anschließend werden für den aktuell modellierten Tag (nrs) und die Zeitpunkte 
TagG1, TagG2, und TagG3 (nrg1, nrg2, nrg3) aus den vorgegebenen Monats- und 
Tageswerten (siehe Tabelle oben) jeweils die Tage des Jahres ausgerechnet 
(analog zur Berechnung von Tagakt bei den Makrophyten).

    (14) if(monats<=2)then 
    (15)  nrs = itags+31*(monats-1) 
    (16)  else
    (17)  nrs = (itags+31*(monats-1)-int(0.4*monats+2.3)) 

    (18) if(ig1mon<=2)then 
    (19)  nrg1 = ig1tag+31*(ig1mon-1) 
    (20)  else
    (21)  nrg1 = (ig1tag+31*(ig1mon-1)-int(0.4*ig1mon+2.3)) 

    (22)  if(ig2mon<=2)then 
    (23)  nrg2 = ig2tag+31*(ig2mon-1) 
    (24)  else 
    (25)  nrg2 = (ig2tag+31*(ig2mon-1)-int(0.4*ig2mon+2.3)) 

    (26)  if(ig3mon<=2)then 
    (27)  nrg3 = ig3tag+31*(ig3mon-1) 
    (28) else 
    (29)  nrg3 = (ig3tag+31*(ig3mon-1)-int(0.4*ig3mon+2.3)) 


# Generation 2

Falls der aktuell modellierte Tag (nrs) kleiner als TagG1 (nrg1) ist, erfolgt 
keine weitere Berechnung. Falls der aktuell modellierte Tag gleich TagG1 
(Saisonstart) ist, wird beim ersten Berechnungsdurchlauf (itco1) mit der 
Berechnung der Nachkommen der Jahresgründungspopulation begonnen. 

    (30)  if(nrs<nrg1)cycle
    (31)  if(nrg1==nrs)then
    (32)  izeiger=1


Die Anzahl an Eiern (anzEi) wird aus der Chelicorophiumdichte der ersten 
Generation (coro(ior,1)) sowie dem Anteil weiblicher Individuen G1 und G2 
ab 15.8. (fweib1) abgeleitet (34) und ergibt multipliziert mit der Schlupfrate 
(schlupf) die Dichte der zweiten Generation (coro(ior,2)) (35), gleichzeitig 
wird die Dichte der ersten Generation auf Null gesetzt (sie stirbt ab) (36). 
Zunächst erfolgt die Berechnung für die Böschungspopulation:

    (33)  if(izeiger==1)then !Nachkommen der Jahresgruendungspopulation nco = 1 
    (34)  anzei = (-13.4+6.95*lw1)*coro(ior,1)*fweib1 
    (35)  coro(ior,2) = anzei*schlupf 
    (36)  coro(ior,1) = 0.0 

Da die Werte für fweib1 und lw1 bekannt sind, ergibt sich aus Zeile (34) 

-> anzei = 18,92 * coro(ior,1)

und aus Zeile (35) 

-> coro(ior,2) = 13,24 * coro(ior,1)

Die gleiche Berechnung wird anschließend für die Population der Sohle 
durchgeführt:

    (37)  anzei = (-13.4+6.95*lw1)*coros(ior,1)*fweib1 -> 18,92 * coros(ior,1)
    (38) coros(ior,2) = anzei*schlupf -> 13,24 * coros(ior,1)
    (39)  coros(ior,1) = 0.0 


Bei jedem weiteren Berechnungsdurchlauf für einen Tag des Jahres zwischen 
TagG1 (nrg1) (einschließlich) und TagG2 (nrg2) nimmt die Chelicorophiumdichte 
mit jeder Stunde leicht ab (Annahme TFlie: 1 h (1/24 d):

    (40)  if(nrs>nrg1.and.nrs<nrg2)izeiger = 2
    (41)  if(izeiger==2)then 
    (42)  coro(ior,2) = coro(ior,2)*exp(-kmonach*tflie) -> coro(ior,2)*exp(-0,000417)
    (43)  coros(ior,2) = coros(ior,2)*exp(-kmonach*tflie) -> coros(ior,2)*exp(-0,000417)

mit:

kmonach = Mortalitätsrate vor 15.08. (0,01) \n
tflie = Zeitschrittweite (Annahme 1h (1/24 d)) \n

# Generation 3

Falls der aktuell modellierte Tag gleich TagG2, wird beim ersten 
Berechnungsdurchlauf (itco2) mit der Berechnung der Nachkommen der 
G2-Population begonnen. Die Anzahl an Eiern (anzEi) wird aus der 
Chelicorophiumdichte der zweiten Generation (coro(ior,2)) abgeleitet und 
ergibt mal einer Schlupfrate (schlupf) die Dichte der dritten Generation 
(coro(ior,3)), gleichzeitig wird die Dichte der zweiten Generation um den 
Faktor 0,3 vermindert (ein Teil stirbt ab). Zunächst erfolgt die Berechnung 
für die Böschungspopulation:

    (44)  if(nrg2==nrs)then izeiger = 3

    (45)  if(izeiger==3)then 
    (46)  anzei = (-13.4+6.95*lw2)*coro(ior,2)*fweib2 -> 11,88*coro(ior,2)
    (47)  coro(ior,3) = anzei*schlupf -> 8,32*coro(ior,2)
    (48)  coro(ior,2) = coro(ior,2)*0.3 

Die gleichen Berechnungen finden dann auch für die Population der Sohle statt:

    (49) anzei = (-13.4+6.95*lw2)*coros(ior,2)*fweib2 -> 11,88*coros(ior,2)
    (50) coros(ior,3) = anzei*schlupf -> 8,32*coros(ior,2)
    (51) coros(ior,2) = coros(ior,2)*0.3 

Bei jedem weiteren Berechnungsdurchlauf für einen Tag des Jahres zwischen 
TagG2 (nrg2) (einschließlich) und TagG3 (nrg3) nimmt die Chelicorophiumdichte 
der Generationen 2 und 3 mit jeder Stunde leicht ab (Annahme TFlie: 
1 h (1/24 d):

    (52)  if(nrs>nrg2.and.nrs<nrg3)izeiger = 4
    (53)  if(izeiger==4)then 
    (54)  coro(ior,2) = coro(ior,2)*exp(-kmonach*tflie) -> coro(ior,2)*exp(-0,000417)
    (55)  coros(ior,2) = coros(ior,2)*exp(-kmonach*tflie)  -> coros(ior,2)*exp(-0,000417)
    (56)  coro(ior,3) = coro(ior,3)*exp(-kmonach*tflie) -> coro(ior,3)*exp(-0,000417)
    (57)  coros(ior,3) = coros(ior,3)*exp(-kmonach*tflie) -> coros(ior,3)*exp(-0,000417)

# Generationen 4 und 5

Falls der aktuell modellierte Tag gleich TagG3, wird beim ersten 
Berechnungsdurchlauf (itco30) mit der Berechnung der Nachkommen der G2- und 
G3-Population begonnen. Die Anzahl an Eiern (anzEi) wird aus der 
Chelicorophiumdichte der zweiten und dritten Generationen (coro(ior,2)) 
abgeleitet und ergibt mal einer Schlupfrate (schlupf) die Dichte der vierten 
und fünften Generation (coro(ior,4), coro(ior,5)), gleichzeitig wird die 
Dichte der zweiten Generation Null gesetzt. Zunächst erfolgt die Berechnung 
für die Böschungspopulationen:

    (58)  if(nrg3==nrs)then izeiger = 5
    (59)  if(izeiger==5)then 

    (60)  anzei = (-13.4+6.95*lw1)*coro(ior,2)*fweib1 -> coro(ior,2) * 18,92
    (61)  coro(ior,4) = anzei*schlupf -> coro(ior,2) * 13,24
    (62)  coro(ior,2) = 0.0 
  
    (63)  anzei = (-13.4+6.95*lw2)*coro(ior,3)*fweib2 -> coro(ior,3) * 11,88
    (64)  coro(ior,5) = anzei*schlupf -> coro(ior,3) * 8,32

und wird sinngemäß auch für die Chelicorophiumdichten an der Sohle durchgeführt.

    (65)  anzei = (-13.4+6.95*lw1)*coros(ior,2)*fweib1 -> coros(ior,2) * 18,92
    (66)  coros(ior,4) = anzei*schlupf -> coros(ior,2) *13,24
    (67)  coros(ior,2) = 0.0 
    (68)  anzei = (-13.4+6.95*lw2)*coros(ior,3)*fweib2 -> coros(ior,3) * 11,88
    (69)  coros(ior,5) = anzei*schlupf -> coros(ior,3) * 8,32

Bei jedem weiteren Berechnungsdurchlauf für einen Tag des Jahres größer gleich 
TagG3 nimmt die Chelicorophiumdichte der Generationen 3, 4 und 5 mit jeder 
Stunde entsprechend einer eigenen Mortalitätsrate ab:

    (70)  coro(ior,4) = coro(ior,4)*exp(-kmor1*tflie) -> coro(ior,4)*exp(-0,23*tflie)
    (71)  coros(ior,4) = coros(ior,4)*exp(-kmor1*tflie) -> coros(ior,4)*exp(-0,23*tflie)
  
    (72)  coro(ior,3) = coro(ior,3)*exp(-kmor2*tflie) -> coro(ior,3)*exp(-0,115*tflie)
    (73)  coros(ior,3) = coros(ior,3)*exp(-kmor2*tflie) -> coros(ior,3)*exp(-0,115*tflie)

    (74)  coro(ior,5) = coro(ior,5)*exp(-kmor3*tflie) -> coro(ior,5)*exp(-0,011*tflie)
    (75)  coros(ior,5) = coros(ior,5)*exp(-kmor3*tflie) -> coros(ior,5)*exp(-0,011*tflie)


 
![Jahresgang von Chelicorophium mit angenommener Startdichte von 100 (nur Böschung)   (A = Generationen 1 bis 5 / B = Gesamtpopulation)](img/corophium_Jahresgang.png)
\n\n


Die Modellierung von Chelicorophium erscheint relativ einfach, auch wenn sie 
mehrere Generationen während eines Jahresganges abbildet. Das Wachstum von 
Chelicorophium erfolgt nur in Abhänigkeit der Ausgangskonzentration, es werden 
keine Einflussfaktoren (Temperatur, Futterverfügbarkeit etc.) verwendet. Die 
Verlustraten sind ebenfalls unbeeinflusst von irgendwelchen Faktoren.


 
## QSim-Veröffentlichungen, die den ...-Baustein beschreiben und/oder anwenden: 
...
 

\n\n

Textquelle: corophium-prozess.md; Codesource: coroph.f90; zurück: \ref lnk_corophium