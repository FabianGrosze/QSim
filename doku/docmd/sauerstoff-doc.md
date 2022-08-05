Sauerstoff {#lnk_sauerstoff}
=========

Der Sauerstoffgehalt in einem Gewässer ist ein lebenswichtiger Faktor für viele 
Gewässerorganismen und deren Besiedlung der Gewässer, er prägt außerdem den 
Stoffumsatz. Der im Wasser gelöste Sauerstoff (dissolved oxygen) dient als 
Indikator für den Zustand eines Gewässers, auch für die Qualität des Gewässers 
als Lebensraum für verschiedene Organismen. 
Die Sauerstoffsättigungskonzentration ist von der Wassertemperatur abhängig, da 
die physikalische Löslichkeit des Sauerstoffs im Wasser mit der Zunahme der 
Temperatur sinkt. Daneben wirken sich weitere Komponenten auf die 
Sauerstoffkonzentration im Gewässer aus, die in einem zweistufigen 
Berechnungsverfahren bilanziert werden. 

In jedem Zeitschritt wird in einer ersten Stufe ein Zwischenwert des 
Sauerstoffgehalts infolge von biochemischen Zehrungs- und Produktionsprozessen 
berechnet. Folgende Prozesse führen in QSim zu einer Veränderung im 
Sauerstoffgehalt:

<!-- in Liste Links einfügen via * [Wort](\ref lnk_prozess); -->
* die Sauerstoffproduktion durch die Photosyntheseleistungen von 
  Kiesel-, Grün- und Blaualgen und Makrophyten
* der Sauerstoffverbrauch durch die Respiration von Kiesel-, Grün- und Blaualgen 
  und Makrophyten
* der Sauerstoffverbrauch durch mikrobielle Oxidation organischer 
  Kohlenstoffverbindungen 
* der Sauerstoffverbrauch durch die Nitrifikation in der Wassersäule
* der Sauerstoffverbrauch durch die Respiration des Zooplanktonplanktons 
  (Rotatorien) und der benthischen Filtrierer (Dreissena) 
* der Sauerstoffverbrauch im Sediment
<!-- früher noch in der Liste: Respiration HNF, der Baustein ist aber abgeschaltet -->

In einer zweiten Stufe wird ausgehend von dem Zwischenwert die Be- bzw. 
Entlüftung über die Gewässeroberfläche ermittelt. Dies geschieht auf Grundlage 
der temperaturabhängigen Sauerstoffsättigungskonzentration und dem als 
Zwischenwert ermittelten Sauerstoffgehalt. An Gewässern mit Wehren wird im Falle 
eines Wehrüberfalls zudem der Einfluss dieser Bauwerke auf den Sauerstoffgehalt 
mitberechnet. Die Veränderungen im Sauerstoffgehalt aus punktförmigen Quellen 
werden wie in allen Bausteinen über die Mischungsrechnungen an den 
Einleitstellen berücksichtigt.

Details zum Sauerstoff-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_sauerstoff_prozesse : Erläuterung der im Algen-Baustein 
implementierten Prozesse 

- \subpage lnk_sauerstoff_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_sauerstoff_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n 

Textquelle: sauerstoff-doc.md; Codesource: oxygen.f90; 
zurück: \ref index
