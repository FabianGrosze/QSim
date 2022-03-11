Dreissena {#lnk_dreissena}
==========

In QSim wird der Wegfraß des Phytoplanktons durch benthische Filtrierer in den 
Bausteinen Dreissena und Chelicorophium umgesetzt. Beide Arten, die Zebramuschel 
Dreissena polymorpha und der Schlickkrebs Chelicorophium curvispinum, sind sog. 
aktive Filtrierer, die das feine organische Material aktiv herbeistrudeln und feine 
und feinste partikuläre organische Substanzen und damit auch die planktischen (im 
Wasser schwebenden) Algen aus dem Wasserkörper filtrieren. Beide benthischen 
Filtrierer sind Neozoen, die in Fließgewässern sehr hohe Dichten erreichen und die 
Benthosgemeinschaft dominieren können. Die Modellierung der benthischen Filtrierer 
in QSim dient nicht der Beschreibung der Entwicklung von benthischen Filtrierern in 
einem Gewässer, sondern der Quantifizierung ihres Fraßdrucks auf das Phytoplankton.

Die Populationsentwicklung von Dreissena ist in QSim basierend auf der 
Literaturstudie von Seredszus (1998) relativ detailliert modelliert worden. Es 
werden zwei Generationen abgebildet und die Populationsentwicklung ist abhängig von 
Temperatur, Nahrung und Ablaichvorgängen. Bei den Simulationsläufen werden am 
Modellstart am 01.01. jeweils die Biomasse (g/m²) der Muscheln sowie das mittlere 
Gewicht einer Muschel (mg C) für Sohle und Böschung für die 0. und 1. Kohorte 
eingetragen. Dabei kann das modellierte Gewässer in verschiedene Abschnitte 
unterteilt werden, um die Verteilung der Muschelabundanzen für Sohle und Böschung 
entlang des Flusses abzubilden. 

Die 0. Kohorte beinhaltet alle Tiere mit einer mittleren Schalenlänge bis 8 mm und 
die 1. Kohorte alle mit einer mittleren Länge größer 8 mm. Die adulten Tiere (1. 
Kohorte) erzeugen Larven, die nach dem Festsetzen die 0. Kohorte bilden. Die 0. 
Kohorte ist frühestens vier Wochen nach dem ersten Ablaichtermin von Bedeutung, 
also ab Ende Juni jeden Jahres. Somit steigen die Individuendichte und damit auch 
der dazugehörige Fraßdruck der Zebramuschel im Verlauf eines Jahres an. Die 
verschiedenen Kohorten wurden eingeführt, um die größenabhängige Filtrierleistung 
der Muscheln abbilden zu können. Der Dreissena-Baustein wurde entwickelt, um die 
Algenentwicklung entlang der Mosel 1994 mit QSim abbilden zu können (Schöl et al. 
1999).


Weitere Informationen:
- \subpage lnk_dreissena_prozesse
- \subpage lnk_dreissena_vars
- \subpage lnk_dreissena_umsetzung


Veröffentlichungen:
- Seredszus, Fabian (1998): Populationsdynamische Grundlagen zur mathematischen 
Modellierung der Filtrierleistung und Populationsdynamik von Muscheln. 
Literaturstudie erstellt im Auftrag der BfG, 71 Seiten.


<hr>
Textquelle: dreissena-doc.md ; Codesource: dreissen.f90 ; 
zurück: \ref lnk_konsumenten; Vorläuferversion \subpage Dreissena


