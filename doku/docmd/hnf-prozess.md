Heterotrophe Nanoflagellaten (HNF) - Prozesse {#lnk_hnf_prozesse}
==============================================

Die HNF Biomasse gemessen an ihrem Kohlenstoffanteil [μg C * l-1]\n
wächst an infolge der Aufnahme von Bakterien und \n
nimmt ab infolge Mortalität, Respiration und Exkretion  \n

ausserdem werden die heterotrophe Nanoflagellaten \n
vom Zooplankton ( Rotatorien, konsum() ) und \n
von den benthischen Filtrierern (Dreissena Muscheln dreissen() ) gefressen. \n

# Bilanzgleichung HNF

\f{equation}{ 
  \frac{dHNF}{dt} = \mu_{HNF} \cdot HNF - g_{HNF,Rot} - g_{HNF,DR}
\f}, 

\f$ HNF \f$:  HNF Biomasse [\f$ \ugCL \f$] \n
\f$ \mu_{HNF} \f$:  Netto-HNF Wachstumsrate [\f$ \dinv \f$] \n
\f$ g_{HNF,Rot} \f$: Grazingrate der HNF durch Rotatorien [\f$ \mgCLd \f$] \n
\f$ g_{HNF,DR} \f$:  Grazingrate der HNF durch Dreissena [\f$ \mgCLd \f$] \n
\n\n 

# Wachstum {#lnk_prozess-b}
Die Netto HNF Wachstumsrate µHNF wird wie folgt berechnet:
 
\f{equation}{ 
  \mu_{HNF} = up_{HNF} - r_{HNF} - e_{HNF} - m_{HNF}
\f}, 

\f$up_{HNF}\f$:	Aufnahmerate der HNF [\f$ \dinv \f$] \n
\f$r_{HNF}\f$:	Respirationsrate der HNF [\f$ \dinv \f$] \n
\f$e_{HNF}\f$:	Exkretionsrate der HNF [\f$ \dinv \f$] \n
\f$m_{HNF}\f$:	altersspezifische Mortalität an HNF [\f$ \dinv \f$] \n
\n\n

# Aufnahmerate 
Im Modell ernähren sich HNF ausschließlich von Bakterien (Fenchel 1982). 
Entsprechend hängt ihre Aufnahmerate von der Bakterienbiomasse und der 
Wassertemperatur ab:
 
\f{equation}{ 
  up_{HNF} = up_{HNF,max} \cdot \frac{BAC}{BAC + K_{S,BAC}} \cdot f_{HNF}(T)
\f}, 


\f$up_{HNF,max}\f$: Maximale Aufnahmerate der HNF [\f$ \dinv \f$] \n
\f$BAC\f$:	Bakterienbiomasse [\f$\mgCL\f$] \n
\f$K_{S,BAC}\f$:  Halbsättigungskonstante der HNF für Bakterienbiomasse [\f$\mgCL\f$] \n
\f$f_{HNF}(T)\f$: Temperaturabhängiger Faktor der HNF [-] \n
\n\n

Im Gegensatz zu den meisten Untersuchungen werden im Modell die maximale 
Aufnahmerate der HNF sowie die Halbsättigungskonstante der HNF für 
Bakterienbiomasse nicht auf die Anzahl an Bakterien und HNF bezogen, sondern 
auf ihre Biomassen. Beide Variablen hängen stark von den Größen der in den 
Experimenten verwendeten Bakterien und HNF ab. Für diesen Modellansatz wurden 
die HNF-Wachstumsexperimente von JÜRGENS (1992) zu Grunde gelegt. Sein 
experimenteller Ansatz geben die in situ Bedingungen des HNF Wachstums im Rhein 
am besten wieder. Jürgens benutzte die im Rhein dominante Chrysomonade Spumella 
und relativ kleine Bakterien als Nahrung. Nähere Details zur Entwicklung des 
HNF-Moduls in BERGFELD (2002).

Die Temperaturabhängigkeit der HNF wurde nach BARETTA-BEKKER et al. (1998) 
modelliert:

\f{equation}{ 
 f_{HNF}(T) = Q_{10,HNF}^{(T - 20) \cdot 0,1}  
\f}, 

\f$Q_{10,HNF}\f$: Temperaturkoeffizient für HNF [-] \n
\f$T\f$:	Wassertemperatur [°C] \n
\n\n

Die Verlustrate der Bakterien durch HNF-Grazing kann aus der HNF-Aufnahmerate 
berechnet werden:
 
\f{equation}{ 
 g{BAC,HNF} = up_{HNF,BAC} \cdot \frac{HNF}{BAC}  
\f}, 

\f$g{BAC,HNF}\f$: Grazingrate der Bakterien durch HNF [mgC*m-3*d-1] \n
\f$up_{HNF,BAC}\f$: Aufnahmerate der HNF an Bakterien [\f$\gCmqd\f$] \n
\f$HNF\f$:	HNF-Biomasse [\f$\gCmq\f$] \n
\f$BAC\f$:	Bakterienbiomasse [\f$\gCmq\f$] \n
\n\n

Die HNF Respiration und Exkretion hängen von der aufgenommenen, aber nicht 
assimilierten Bakterienbiomasse ab:

\f{equation}{ 
 r_{HNF} = up_{HNF} \cdot (1 - Y_{HNF}) \cdot (1- f_{e,HNF}) + r_{HNF,G} 
\f}, 
 
\f{equation}{ 
 e_{HNF} = up_{HNF} \cdot (1 - Y_{HNF} \cdot e_{HNF})  
\f}, 

\f$r_{HNF}\f$:  Respirationsrate der HNF [d-1] \n
\f$up_{HNF}\f$: Aufnahmerate der HNF an Bakterien [d-1] \n
\f$Y_{HNF}\f$:	Ertragskoeffizient der HNF [-] \n
\f$f_{e,HNF}\f$: Exkretionsfaktor der HNF [-] \n
\f$r_{HNF,G},G\f$: Grundrespirationsrate der HNF [d-1] \n
\f$e_{HNF}\f$:  Exkretionsrate der HNF [d-1] \n
\n\n

# Rand- und Anfangsbedingungen 

QSim-Veröffentlichungen, die den BausteinA beschreiben und/oder anwenden:

- Kapitel <a href="./pdf/HNF_QSimDoku.pdf" target="_blank">
8.Heterotrophe Nanoflagellaten</a> Ausschnitt aus:

- <a href="./pdf/2002_Bergfeld_Dissertation_RhMoSa.pdf" target="_blank">
Dissertation Tanja Bergfeld</a>\n


\n\n

Textquelle: hnf-prozess.md; Codesource: hnf.f90; zurück: \ref lnk_hnf
