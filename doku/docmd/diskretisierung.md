Netz / Orts-Diskretisierung {#lnk_diskretisierung}
============================

Sowohl in QSim1D als auch in QSim3D wird die horizontalen Diskretisierung, 
also des Netzes (3D) oder der Gewässer-Stränge (1D)
vom Hydraulischen Treiber übernommen. Im 3D werden benutzt:

- <a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>, \ref lnk_transport_casu
- UnTRIM (Datenübergabe von der BAW), \ref lnk_transport_untrim
- SCHISM in Arbeit, \ref lnk_transport_schism 


Die Verwendung einer auf die Stofftransportsimulation abgestimmten 
Diskretisierung bietet Vorteile bei der Genauigkeit und der Rechenzeit. Dazu 
erforderlich ist aber eine Interpolation der hydraulischen Ergebnisse
auf das Güte-Netz. Die Implementierung dieser Interpolation ist nicht ganz 
unaufwändig und der dadurch erzielbare Vorteil nicht sicher einschätzbar. 
Bisher realisiert wurde in dieser Frage nur eine einfache Vergröberung von 
Deltares.

siehe auch: \ref lnk_stofftransport_3d


Zum Lesen des Netzes aus dem Modellverzeichnis bedient sich netz_lesen() der 
Subroutinen points(), elements() und edges()

aus Datei stofftransport.f95 ; zurück zu \ref lnk_modellerstellung
Steinbruch Numerik

# Advektion {#lnk_advektion}
\f[
 c_{m,k}(t+\Delta t) = A_{kl}(\Delta t^{adv}){c}_{m,l}^{diff,react} \quad ; \quad
 {c}_{m,k}^{diff,react}=D_{kl}(\Delta t^{diff}){c}_{m,l}^{react} \quad ; \quad
 {c}_{m,k}^{react}= Q_m(c_1(t) \ldots c_M(t), \underline {x}, t, \Delta t^{react})
\f]

Textquelle: diskretisierung.md ; Codesources: stofftransport.f95 ;  
zurück: \ref lnk_stofftransport_3d oder \ref lnk_modellerstellung
