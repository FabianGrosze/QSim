Transportinformationen von casu (Strombahnursprünge) {#lnk_transport_casu}
====================================================

Die Transportinformationen werden von holen_trans() in die \ref lnk_hydraul_rb 
eingelesen.\n
Transportinformationen für den Stofftransport bestehen aus den
Ecknummern der vier(drei) Knoten des Elements aus dem die Strombahn kommt und je
4(3) Wichtungsfaktoren, die angegeben, welcher Anteil vom dazugehörenden Knoten 
kommt. Die Summe der Wichtungsfaktoren muss immer genau 1.0 sein.

Dies entspricht der Angabe des Strombahnursprungs (in einem Elementlokalen 
Koordinatensystem) und konstruiert auf diese Weise eine Euler-Lagrange Methode 
(ELM) für die Advektion.

\f[
  C^{n+1}_k = {\sum_{i=1}^{3(4)}} C_{Nr(i)}^n \cdot  w(i)
\f]

Bisher ist nur Advektion realisiert worden, die Diffusion steht bisher noch aus.
Es ist geplant, dies im Zusammenhang mit der Messung und Kalibrierung der realen 
Diffusivitäten und der Ermittlung der numerischen Diffusivität durchzuführen.

mit 

| Formelzeichen | Variablen-Name | Beschreibung | Dimension | Wertebereich |
| ------------- | -------------- | ------------ | --------- | ------------ |
| \f$ C \f$  | planktonic_variable \ref lnk_tiefengemittelte_plankt_var | als Konzentration modellierte planktische Variable |  |  |
| \f$ i \f$     | - | Zähler über alle Ecken des Elements | - | 1-3(4) |
| \f$ n \f$     | - | Zeitschritt-Zähler | - | - |
| \f$ k \f$     | - | Konzentrations-Nummer | - | - |
| \f$ Nr(i) \f$ | ieck() | globle Knotennummer der Elementecke | - | |
| \f$ w(i) \f$  | wicht() | Wichtungsfaktor (d.h. elementlokale Koordinaten des Strombahnursprungs im Ursprungselement) | - | 0-1 |


Es wurde die Möglichkeit geschaffen, durch \subpage lnk_kombitransi die 
hydraulischen Voraussetzungen für die Gütesimulation abschnittsweise zu 
simulieren. Das heißt: Der Hydraulische Treiber rechnet das Jahr z. B. 
monatsweise in 12 parallelen Rechenläufen (mit wenigen Tagen Überlappung),
deren Ergebnisse dann im nachhinein zu einem kontinuerlichen Datensatz der
Transportinformationen kombiniert werde.

Zum Lesen des Netzes aus dem Modellverzeichnis bedient sich netz_lesen() der 
Subroutinen points(), elements() und edges()

Textquelle: stofftransport_casu.md ; Codesources: stofftransport_casu.f95 ;  
zurück: \ref lnk_datenstruktur oder \ref lnk_transport_numerik
 