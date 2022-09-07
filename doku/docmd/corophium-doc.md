Chelicorophium {#lnk_corophium}
==========

\warning Momentan ist das Modul ausgeschaltet, das heißt, Corophium wird 
nicht simuliert. Es handelt sich um eine alte Dokumentation.


In QSim wird der Wegfraß des Phytoplanktons durch benthische Filtrierer in den 
Bausteinen Dreissena und Chelicorophium umgesetzt. Beide Arten, die Zebramuschel 
Dreissena polymorpha und der Schlickkrebs Chelicorophium curvispinum, sind 
sog. aktive Filtrierer, die das feine organische Material aktiv herbeistrudeln 
und feine und feinste partikuläre organische Substanzen und damit auch die 
planktischen (im Wasser schwebenden) Algen aus dem Wasserkörper filtrieren. 
Beide benthischen Filtrierer sind Neozoen, die in Fließgewässern sehr hohe 
Dichten erreichen und die Benthosgemeinschaft dominieren können. Die 
Modellierung der benthischen Filtrierer in QSim dient nicht der Beschreibung 
der Entwicklung von benthischen Filtrierern in einem Gewässer, sondern der 
Quantifizierung ihres Fraßdrucks auf das Phytoplankton.

Im Gegensatz zur Zebramuschel wurde die Populationsentwicklung beim 
Schlickkrebs im Modell nur mit einfachen Annahmen basierend auf Rajagopal et. 
al. (1999) umgesetzt. Dabei wird ausschließlich ihre Filtrierrate im 
Jahresgang berechnet, in weitere Bausteine wie dem Sauerstoff oder organischem 
Kohlenstoff ist Chelicorophium nicht eingebunden. 

Es werden drei Generationen während eines Jahresgangs abgebildet, das Wachstum 
von Chelicorophium ist nur von seiner Ausgangsindividuendichte am Modellstart 
abhängig. Keine anderen Einflussfaktoren wie die Wassertemperatur oder die 
Futterverfügbarkeit wurden im Baustein umgesetzt, die Verlustraten sind 
ebenfalls unbeeinflusst von äußeren Faktoren. Bei den Simulationsläufen wird 
jeweils für den 01.01. eines jeden Jahres eine Individuendichte für 
Chelicorophium unterschieden nach den Standorten Böschung und Sohle 
eingegeben. Dabei kann das modellierte Gewässer in verschiedene Abschnitte 
unterteilt werden, um die Verteilung der Chelicorophiumabundanzen für Sohle 
und Böschung entlang des Flusses abzubilden. 

Die Start- und Folgegenerationen sterben zu unterschiedlichen Zeitpunkten ab, 
die letzte Generation bildet die Basis für die Startgeneration im nächsten 
Jahr. Durch die hohe Dominanz von Chelicorophium in der Donau wurde der 
Baustein im Rahmen der EU-Studie Donau (BfG-1740, 2013) hinzugefügt, um die 
Algenentwicklung mit QSim abbilden zu können.


Weitere Informationen:
- \subpage lnk_corophium_prozesse
- \subpage lnk_corophium_vars
- \subpage lnk_corophium_umsetzung


Veröffentlichungen:
- Rajagopal, S., Van der Velde, G., Paffen, B. G. P., Van den Brink, F. W. B., & Bij de Vaate, A. (1999). Life history and reproductive biology of the invasive amphipod Corophium curvispinum (Crustacea: Amphipoda) in the Lower Rhine. Archiv für Hydrobiologie, 305-325.


<hr>
Textquelle: corophium-doc.md ; Codesource: coroph.f90 ; 
zurück: \ref lnk_konsumenten


