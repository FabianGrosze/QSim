Lokale Modell-Eigenschaften  {#lnk_lokale_parameter}
===========================

Lokale Modelleigenschaften werden in QSim3D jeweils einer Zone zugeordnet.

Die zonenweise Verteilungen werden mithilfe der Datei \ref lnk_modell_g3 
vorgegeben.

Beispiel: <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>


Die Unterteilung des Modellgebiets in Zonen wird von GERRIS durchgeführt und in 
der Datei
Beispiel: <a href="./exp/ELEMENTE.txt" target="_blank">ELEMENTE.txt</a> abgelegt 
(UNTRIM-Antrieb).
Nach der Elementnummer enthält jede Datenzeile die Koordinaten des 
Elementzentrums, die Zonennummer und die Randnummer (0=kein Rand).


In QSim1D erfolgt die Zuordnung zu Strängen oder Strangabschnitten.

Dazu wird die Datei MODELLG.txt verwendet.


Textquelle: lokale_parameter.md ; Codesources: zonen.f95 ;  
zurück: \ref lnk_modellerstellung
 
