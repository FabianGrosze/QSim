Stofftransport QSim3D  {#lnk_stofftransport_3d}
=====================

# Konzept: Konzentrations-Transport 

In QSim werden im Wasser gelöste Stoffe, planktisch lebende Organismen und deren 
Eigenschaften als Konzentrationen modelliert. D. h. es wird angenommen,
dass selbige nur vernachlässigbare Bewegungen gegenüber dem umgebenden Wasser ausführen.
QSim arbeitet mit 72 tiefengemittelten und 11 tiefenaufgelösten Transportkonzentrationen,
deren Bedeutung in \ref lnk_var_planktisch näher beschrieben wird.


# Advektions-Diffusions-Gleichung
Die gesamte Änderung einer Konzentration wird aufgespalten in die Änderung 
infolge von Stoffumsatz und diejenige infolge von
Transport durch das fließende Wasser. In diesem Zweig geht es jetzt nur noch 
um den Transportanteil:

\f[
  \underbrace {\frac{\partial c_m}{\partial t}}_{Aenderung infolge Transport} =
- \underbrace {v_i \frac{\partial c_m}{\partial x_i} }_{Advektion}
+ \underbrace {\frac{\partial}{\partial x_i} D_{ij} \frac{\partial c_m}{\partial x_j}}_{Diffusion}
\f]

Die obige Gleichung (Transportgleichung) für die <i>m</i>-te Konzentration \f$ c_m \f$ 
gilt allgemein für die Advektion durch ein 3-dimensionales Strömungsfeld \f$ v_i \f$.
Mit <i>t</i> wird die Zeit bezeichnet und mit \f$ x_i \f$ ein Ort im 3-dimensionalen Raum. 
Der Index <i>i</i> zählt über alle drei Raumrichtungen; sein doppeltes Auftreten 
besagt, dass über ihn zu summieren ist.
Die Vermischung wird vom Diffusionstensors \f$ D_{ij} \f$ bewirkt, der genauso 
wie der Strömungsvektor von Ort und Zeit abhängig sein kann.


# Räumliche Integration
Die Integration der Transportgleichung in eine tiefen-, breiten- und 
querschnitts-gemittelte Formulierung wird in der speziell dazu erstellten 
<a href="./pdf/transport_dimensionsreduziert2016.pdf" target="_blank">
Ausarbeitung </a> hergeleitet.


# Antreibendes Strömungsfeld
Das in der Transportgleichung verwendete Geschwindigkeitsfeld wird für QSim 
vorab mithilfe einer numerischen Strömungssimulation 
(CFD, Computational Fluid Dynamics) von einem sogenanntes 
HN(Hydro-Numerisches)-Modell berechnet, das in diesem Zusammenhang auch als 
"hydraulischer Treiber" bezeichnet wird. Bei der Verbindung von 
Strömungssimulation und Gütemodell handelt es sich um eine \ref lnk_Kopplung .


QSim-1D verwendet als hydraulischen Treiber Hydrax.

QSim-3D kann die Ergebnisse verschiedener hydraulischer Treiber nutzen:\n

- <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  
   target="_blank">casu</a>.
   Die von o.g. HN-Modellen binär, verlustbehaftet komprimierten 
   Transportinformationen werden von holen_trans() mit der C++ Funktion 
   trans_read_() gelesen. \ref lnk_transport_casu \n
- UnTRIM (BAW) \ref lnk_transport_untrim \n
- SCHISM <a href="http://voss-wiki.bafg.de/instanzen/schismwiki/doku.php/start" target="_blank">Wiki</a> (BfG),
  <a href="http://ccrm.vims.edu/schism/" target="_blank">Homepage</a> (VIMS) 
  \ref lnk_transport_schism \n


# Numerischen Näherungslösungen
Die näherungsweise Lösungen der Advektions-Diffusons-Gleichung 
(Transportgleichung) werden im Abschnitt \ref lnk_numerik vorgestellt.

# Datentechnische Umsetzung
siehe \subpage lnk_hydraul_rb
 
Textquelle: stofftransport_3d.md ; Codesources: stofftransport.f95 ;  
zurück: \ref lnk_stofftransport
 