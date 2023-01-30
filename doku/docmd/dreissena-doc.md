Dreissena {#lnk_dreissena}
==========

In QSim wird der Wegfraß des Phytoplanktons durch benthische Filtrierer im 
Baustein Dreissena umgesetzt. Die Zebramuschel *Dreissena polymorpha* ist ein 
sogenannter aktiver Filtrierer, die Art strudelt feines organisches Material 
aktiv herbei und filtriert feine und feinste partikuläre organische Substanzen 
und damit auch die planktischen (im Wasser schwebenden) Algen aus dem 
Wasserkörper. *Dreissena polymorpha* ist ein Neozoe, die in Fließgewässern sehr 
hohe Dichten erreichen und die Benthosgemeinschaft dominieren kann. 
Die Modellierung der benthischen Filtrierer in QSim dient nicht der Beschreibung 
der Entwicklung von benthischen Filtrierern in einem Gewässer, sondern der 
Quantifizierung ihres Fraßdrucks auf Phytoplankton und Schwebstoff.

Die Populationsentwicklung von Dreissena ist in QSim basierend auf der 
Literaturstudie von Seredszus (1998) relativ detailliert modelliert worden. Es 
werden mit der 0. und 1. Kohorte zwei Generationen abgebildet und die 
Populationsentwicklung ist abhängig von Temperatur, Nahrung und 
Ablaichvorgängen. Zusätzlich gibt es eine gewichtsabhängige Mortalitätsrate von 
Dreissena. Bei den Simulationsläufen werden am Modellstart am 01.01. jeweils die 
Biomasse (g/m²) der Muscheln sowie das mittlere Gewicht einer Muschel (mg C) für 
Sohle und Böschung für die 0. und 1. Kohorte eingetragen. Dabei kann das 
modellierte Gewässer in verschiedene Abschnitte unterteilt werden, um die 
Verteilung der Muschelabundanzen für Sohle und Böschung entlang des Flusses 
abzubilden. 

Die 0. Kohorte beinhaltet alle Tiere mit einer mittleren Schalenlänge bis 8 mm 
und die 1. Kohorte alle mit einer mittleren Länge größer 8 mm. Die adulten Tiere 
(1. Kohorte) erzeugen Larven, die nach dem Festsetzen die 0. Kohorte bilden. 
Die 0. Kohorte ist frühestens vier Wochen nach dem ersten Ablaichtermin von 
Bedeutung, also ab Ende Juni jeden Jahres. Somit steigen die Individuendichte 
und damit auch der dazugehörige Fraßdruck der Zebramuschel im Verlauf eines 
Jahres an. Die verschiedenen Kohorten wurden eingeführt, um die größenabhängige 
Filtrierleistung der Muscheln abbilden zu können. Der Dreissena-Baustein wurde 
entwickelt, um die Algenentwicklung entlang der Mosel 1994 mit QSim abbilden zu 
können (Schöl et al. 1999)

![<i>Dreissena polymorpha</i>](img/Dreissena_sp5_RalfRombach_BfGU4.jpg)

<small>Quelle: Foto von Ralf Rombach/U4</small>
\n\n

Weitere Informationen:
- \subpage lnk_dreissena_prozesse
- \subpage lnk_dreissena_vars
- \subpage lnk_dreissena_umsetzung




<hr>
Textquelle: dreissena-doc.md ; Codesource: dreissen.f90 ; 
zurück: \ref lnk_konsumenten


