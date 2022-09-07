Simulationsverfahren {#lnk_simulationsverfahren}
========================

# Grundgleichung {#lnk_grundgleichung}
<!-- Link ehemals "EinfNum" -->

In QSim werden alle güterelevanten Variablen inkl. der planktisch lebenden 
Organismen als im Wasser gelöste Konzentrationen modelliert.

Die Veränderungen dieser Konzentrationen wird summarisch mit der untenstehenden 
partiellen Differentialgleichung beschrieben:

\f[ 
  \underbrace {\frac{\partial c_m}{\partial t}}_{lokale Aenderung} = 
  \underbrace {Q_m ( c_1 \ldots c_m \ldots c_M, x_i, t )  }_{Stoffumsatz}
  \underbrace {
- \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
+ \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
   }_{Stofftransport}
\f]

Diese Grundgleichung beschreibt die zeitliche Änderung der m-ten Konzentration 
\f$ c_m \f$ an einem festen Punkt im Raum. 
Diese Konzentrationsbilanzen stellen den Verfahrenskern von QSim dar; 
datentechnisch werden diese Konzentrationen als \ref lnk_var_planktisch gespeichert. 
Unter dem vorstehenden Link findet sich eine Auflistung, aus der hervorgeht um welche "Stoffe" es sich dabei handelt.
\n
Darüber hinaus bilanziert QSim auch \ref lnk_var_benthisch
bei denen allerdings der Stofftransport-Anteil der obigen Gleichung entfällt.

# Ursachen der Konzentrations-Änderungen: {#lnk_ursachen_konz_aenderung}

## Stoffumsatz 
<!-- {#lnk_stoffumsatz} -->

Der gesamte Stoffumsatz, also alle biologischen Stoffwechselvorgänge und 
chemischen Reaktionen werden in der obigen Gleichung in einer Änderungsrate 
\f$ Q_m \f$ zusammengefasst. Diese hängt von anderen (ebenfalls als 
Konzentrationen modellierten) Variablen sowie von Ort (*x*) und Zeit (*t*) ab. 

Die voranstehende Gleichung kann auch als Massenbilanz des "Stoffes" *m* gelesen 
werden. Dann ist \f$ Q_m \f$ je nach Vorzeichen eine Quelle oder Senke.
Der Index *m* zählt über alle 1 bis M (z. Zt. M = 76) transportierten 
Konzentrationen (\ref lnk_var_planktisch).
<!-- Anzahl der transp. Konz. checken, es müssten jetzt < 76 sein -->

Die Prozessbeschreibungen, die hier formal in der Änderungsrate \f$ Q_m \f$ zusammengefasst wurden, die aber den Kern des Gütemodells ausmachen,
werden in den einzelnen Modulbeschreibungen im Detail erläutert.


##  Stofftransport  
 
Die Verfrachtung (Advektion) und Vermischung (Diffusion) der Konzentrationen
infolge des Fließvorgangs des Wassers wird zusammenfassend als 
<b>\ref lnk_stofftransport </b> benannt.

Die Advektion bewirkt, dass das Strömungsfeld \f$ v_i \f$ eine Konzentration 
\f$ c_m \f$ von anderenorts (\f$x_i \f$) herantransportiert, wo diese andere 
Werte aufweist. 
Der Index *i* zählt über alle drei Raumrichtungen. Sein doppeltes Auftreten 
besagt, dass über ihn zu summieren ist.
<!-- #mf: das mit dem doppelten Auftreten verstehe ich nicht -->

Die Diffusion wird hier mithilfe eines Diffusionstensors \f$ D_{ij} \f$ 
beschrieben. Sie bewirkt, dass sich räumliche Konzentrationsunterschiede 
ausgleichen.
Die Indezes *i* und *j* zählen über alle drei Raumrichtungen;
ihr doppeltes Auftreten besagt, dass über beide zu summieren ist.

Die numerische Näherung des Transportprozesses in der Kopplung mit einem 
ebenfalls simulierten Strömungsfeld beschreibt der Abschnitt 
\ref lnk_stofftransport.


# Trennung der numerischen Lösung {#lnk_fracStep}

Die voranstehende Grundgleichung wird für ihre numerische Lösung getrennt. 
In QSim (1D und 3D) wird zuerst der Stoffumsatz, dann der Transport berechnet. 
Dies sei anhand eines Wassertropfens erläutert, der während eines Zeitschritts 
auf einem gewissen Weg im Wasserkörper fortbewegt wird:
Während des Zeitintervalls laufen in besagtem Wassertropfen biochemische
Stoffumwandlungsvorgänge ab.
Die Simulation in QSim läuft nun so ab, dass der Wasertropfen quasi am Ort 
festgehalten wird, an dem er sich zu Beginn des Zeitschritts befand.
Dort wird zunächst der Stoffumsatz unter den an diesem Ort herrschenden 
Bedingungen simuliert. Danach wird der Tropfen vom Transport-Algorithmus an den 
Ort gesetzt, den er am Ende des Zeitschritts erreicht.


# Algorithmische Umsetzung {#lnk_numerik}

\image html NumericalAspects_WyrwaSchoel_final5.png
Die numerische Lösung der o.g. partiellen Differentialgleichung für die Änderung 
einer Konzentration geschieht auf folgendem Wege:

In einem Fractional-step-Verfahren werden die Stoffumsätze und der 
Stofftransport nacheinnander simuliert, ohne im gleichen Zeitschritt aufeinander
rückwirken zu können.

\image html NumericalAspects_WyrwaSchoel_final6.png
Dabei können die Einzelprozesse mit unterschiedliche Zeitschrittweiten 
abgebildet werden, die allerdings ganzzahlige Vielfache voneinader sein müssen.

Die Stoffumsätze (aufgelistet in: \ref lnk_ueberblick) werden numerisch als 
\subpage lnk_wachstum_zerfall modelliert.
Die numerische Umsetzung der \ref lnk_stofftransport - Modellierung wird im 
Vortrag
<a href="http://bibliothek.bafg.de/dokumente/Online%20377.ppt" target="_blank">
Numerical aspects in offline coupling of biochemical reaction modules with advection-diffusion simulations</a>
näher erläutert.

<!-- #mf: folgende Zeilen rauslöschen oder überarbeiten
Die Näherungsgüte dieses auch als "fractional step" bezeichneten Verfahrens 
diskutiert die Dokumentation
<a href="./pdf/transportdoku_3.pdf" target="_blank"> ???? noch nicht geschrieben ??? </a>
-->

# Transport {#lnk_transport_numerik}
Eine Einführung in den Transport ist [hier](\ref lnk_stofftransport) zu finden.
<!-- #mf: klappt der Link zu lnk_stofftransport? (ref eingefügt) -->

Die folgenden drei Unterkapitel gehen auf informationstechnische Details 
der mehrdimensionalen hydraulischen Treiber ein:

1. \subpage lnk_transport_casu 
2. \subpage lnk_transport_untrim
3. \subpage lnk_transport_schism

aus Datei: simulationsverfahren-doc.md ; zurück zu \ref lnk_modelldetails
