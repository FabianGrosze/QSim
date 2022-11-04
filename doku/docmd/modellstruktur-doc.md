Modellstruktur {#lnk_modellstruktur}
========================

<!-- Inhalt soll sein 1D/3D relevante Dinge -->

# Verbindung zwischen QSim-1D und QSim-3D {#lnk_verbindung_1d_3d}

Die beiden Programme der QSim-Familie unterscheiden sich in der räumlichen 
Auflösung des Wasserkörpers (vgl. Abb. im Abschnitt \ref lnk_qsim_aufbau).
Dementsprechend werden für QSim auch die Namen QSim1D und QSim3D verwendet.

Im Abschnitt [Aufbau von QSim](\ref lnk_qsim_aufbau) wurde die unterschiedliche 
räumliche Auflösung der hydrodynamischen Treiber von QSim erwähnt. 
\n\n

## 1, 2, 3 Dimensionen {#lnk_dimensionen}
Die Bezeichnung 1D und 3D der beiden QSim's steht für die ein-dimensionale 
respektive drei-dimensionale Raumauflösung der Modelle. 
Dies ist deswegen erklärungsbedürftig, weil mit beiden Berechnungswerkzeugen 
unterschiedliche zwei-dimensionale Simulationen ausgeführt werden können.

QSim-<b>1D</b> basiert auf einer querschnittsgemittelten Hydraulik (HYDRAX). 
Die horizontale Erstreckung eines Gewässers wird dadurch immer als Linie erfasst. 
Diese Linie wird im Modell in Abschnitte (= "Stränge") diskretisiert, die aus 
mehreren Knoten bestehen. Jedem Knoten ist ein Querprofil zugeordnet. 
Sollen z.B. Altarme mit erfasst werden, können mehrere Stränge angelegt werden, 
die in einem Gewässernetz zusammengefasst werden (siehe dazu auch *Hydrax-Doku*). 

In QSim1D gibt es die Option eine Tiefenauflösung mancher Variablen abzuschätzen.
Diese "2D"-Option wird vor allem gewählt, um Temperaturschichtungen simulieren zu 
können. 2D bei QSim-1D meint daher <b>2D-breitengemittelt</b>.
<!-- #mf: Link zur Hydraxdoku einfügen, bzw. besser zur Dokuportal-Seite auf der 
sie verlinkt ist, damit Link nicht an mehreren Stellen bearbeitet werden muss --> 

In QSim1D ist es möglich Buhnen zu simulieren. Dies geschieht über die 
\subpage lnk_stillwasserzonen.


QSim-<b>3D</b> basiert auf hydraulischen Treibern, welche die horizontale 
Erstreckung mit einer flächigen Auflösung/Diskretisierung anhand von Drei- und 
Vierecksnetzen erfassen. In der BfG, Referatu U2 werden dabei hauptsächlich 
flache  Gewässer betrachtet.
Wird die Wassertiefe mit nur einer Schicht modelliert, gelangt man zu 
<b>2D-tiefengemittelten</b> Simulationen.
Erst durch die Hinzunahme der schon in QSim-1D angelegten Tiefenauflösung gelangt 
man zu wirklich drei-dimensionalen 
Raumauflösungen, die aber nur in geschichteten Wasserkörpern erforderlich sind.

<!-- #mf: check, if header is level 2 now and does work -->
## Verbindung von QSim-3D mit QSim-1D {#lnk_huellen}

Die Idee von QSim-3D ist es, dieselbe Gewässergüte-Simulation wie QSim-1D 
durchzuführen.

Nur die räumliche Auflösung des Wasserkörpers erfolgt 2D-tiefengemittelt oder 
voll 3-dimensional in QSim-3D statt 1D oder 2D-breitengemittelt wie in QSim-1D.

Die Simulation der im Gewässer lokal ablaufenden Prozesse (lokaler 
stoffumsatz()) bleibt identisch.

Der aktuell verwendete QSim1D-source-code wird im Abschnitt \ref lnk_download 
bereitgestellt.

Um mit denselben Subroutinen arbeiten zu können, die auch das Programm QSim 
verwendet, wurden hier Hüllroutinen geschaffen, deren einzige Aufgabe darin 
besteht, die in QSim-3D geänderte Datenstruktur an die QSim-Subroutinen zu 
übergeben.

Die wichtigste Umstellung ist dabei, dass die Hüllroutinen punktweise aufrufbar 
sind, während die QSim-Subroutinen strangweise arbeiten. Beim Aufruf durch die 
Hüllroutine wird die QSim-Subroutine in den Glauben versetzt,
einen Strang zu bearbeiten, der nur aus einem Profil besteht.

Ausserdem werden die Programmdateien der Hüllroutinen zur Dokumentation der 
QSim-Subroutinen genutzt.

Die QSim-1D Namen werden in QSim3D im module_QSimDatenfelder.f95 vereinbart.

Die Tiefenverteilung von QSim1D (welche dort als 2D bezeichnet wird) ist dort 
nicht komplett für alle Konzentrationen formuliert. 
Eine Übernahme ins mehrdimensionale ist nicht geplant.
QSim3D arbeitet z. Zt. (Mai 2018) noch durchgängig tiefenintegriert.


# Offline Kopplung von Strömungssimulation und Stoff-Transport {#lnk_Kopplung}

Bei einer Offline Kopplung kommunizieren die Strömungssimulation und das 
Gütemodell offline, d. h. mittels fest abgespeicherter Dateien.
Der hydraulische Treiber ist eine eigenständige Software, welche die 
Strömung simuliert und seine Ergebnisse abspeichert.
Das Gütemodell ist eine ebenfalls eigenständige Software, die nach dem 
hydraulischen Treiber gestartet wird und dessen Ergebnisse "offline" einliest.
Voraussetzung für diese Informationsübertragung nur in die eine Richtung ist, 
dass die im Gütemodell betrachteten Wasserinhaltsstoffe keinen (oder nur einen
vernachlässigbaren) Einfluss auf den Strömungsvorgang haben. 

Die offline-Kopplung hat sich in QSim bewährt. 
Viel Rechenzeit lässt sich dadurch sparen, dass in der Praxis viele 
Gütesimulationen auf der Basis einzelner hydraulischer Berechnungen 
durchgeführt werden. 
Müsste für jede Gütesimulationen eine eigene hydraulische Berechnung angefertigt
werden, wäre eine online-Kopplung schneller.
Ausserdem ergibt die offline-Kopplung eine klare Schnittstelle in der
interdisziplinären Zusammenarbeit.

QSim-3D verwendet die folgenden hydraulischen Treiber: \n
<a href="http://www.wasserimunterricht.de/wyrwa/casu12.html"  target="_blank">casu</a>,\n UnTRIM und \n
<a href="http://voss-wiki.bafg.de/instanzen/schismwiki/doku.php/start" target="_blank">SCHISM</a> (in Arbeit).

Der Stofftransport-Baustein in QSim übernimmt das Berechnungsnetz vom 
hydraulischen Treiber; an dessen Struktur muss auch der Transportalgorithmus 
speziell angepasst werden.
Da die verwendeten hydraulischen Treiber zur Simulation des Transports von Salz 
und suspendierten Sedimenten über eigene Löser der 
Advektions-Diffusions-Gleichung (Transportgleichung) verfügen,
ergibt sich die Möglichkeit, durch Übergabe von verfahrensabhängigen 
Transportinformationen im Gütemodell einiges an Rechenaufwand einzusparen.
Details werden im Abschnitt \ref lnk_stofftransport_3d näher ausgeführt. 


aus Datei: modellstruktur-doc.md ; zurück zu \ref lnk_modelldetails
