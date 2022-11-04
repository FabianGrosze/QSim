Coliforme Bakterien/Keime (Hygiene) {#lnk_coliform}
===================================

\warning Der Hygiene-Baustein ist zurzeit ausgeschaltet. Coliforme Bakterien 
können daher nicht gerechnet werden. Der Text stammt aus einer früheren
Version der Dokumentation und ist nicht mit dem Code gegengecheckt.

Fäkalcoliforme Bakterien (i. d. R. Escherichia coli) vermehren sich nicht im 
Gewässer, sondern sterben in der Umwelt rasch ab und zeigen somit eine relativ 
frische Verunreinigung mit Fäkalien an. Gesamtcoliforme Bakterien hingegen 
überleben länger und können sich in organisch reichhaltigen Materialien 
vermehren oder sind sogar ausschließlich in der Umwelt beheimatet. Entsprechend 
zeigt diese Gruppe eine mögliche Einschwemmung älterer Fäkalien (Klärschlamm, 
Jauche, Gülle) und deutet damit auch auf allgemeine Eutrophierungstendenzen hin. 
Von QSim wird lediglich die Entwicklung fäkalcoliformer Bakterien im 
Untersuchungsgebiet betrachtet.

Als besonders schwierig hat sich die Quantifizierung der Eintragspfade der 
fäkalcoliformen Bakterien (Coli-Bakterien) in das jeweilige Gewässer erwiesen. 
Die gewässerinternen Umsatzprozesse der Coli-Bakterien sind im Wesentlichen 
bestimmt durch die stark dezimierende Wirkung des Sonnenlichts (UV-Strahlung), 
ein Wachstum findet nicht statt. 

Es wurde in QSim ein erster Ansatz zur Beschreibung der Mortalitätsrate von 
Coli-Bakterien in Abhängigkeit von der UV-Strahlung implementiert, der aber 
noch überarbeitet werden muss.


\image html badestelle_rhein.png "Krankheitskeime im Wasser können für badenden Menschen gefährlich sein."


 Weitere Details zum Baustein sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_coliform_prozesse : Erläuterung der im Schwermetall-Baustein 
implementierten Prozesse 

- \subpage lnk_coliform_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_coliform_umsetzung : Details zum Code und der numerischen Umsetzung 


Textquelle: coliform-doc.md ; Codesources: coliform_huelle.f95 und coliform.f90 ;  
zurück: \ref lnk_weitere_stoffe
 