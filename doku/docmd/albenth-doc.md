Benthische Algen {#lnk_albenth}
================

\warning Momentan ist das Modul ausgeschaltet, das heißt, benthische Algen 
werden nicht simuliert. Es handelt sich um eine alte Dokumentation.


In OSim kann der Einfluss von benthischen Algen auf die Gewässergüte unter 
Berücksichtigung der Lichtintensität, der Extinktion + Wassertiefe*, der 
Nährstoff- und Temperaturbedingungen abgebildet werden. Es werden dabei zwei 
Gruppen, benthische Kiesel- und Grünalgen, unterschieden. Das Modul der 
benthischen Algen kann für einzelne Gewässer-Abschnitte unabhängig aktiv oder 
inaktiv geschaltet werden. Für diese können jeweils strang- oder abschnittsweise 
eigene Dichten des Vorkommens [g Biomasse/m²] am Sediment eingegeben werden, wenn 
die Berechnung aktiv geschaltet ist. 

Ausgegeben werden die Sauerstoffproduktion und der Sauerstoffverbrauch [mg/l] 
sowie die Biomasse der benthischen Algen insgesamt [g/m²].


Details zum Benthische Algen-Modul sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_albenth_prozesse : Erläuterung der im Benthische Algen-Baustein 
implementierten Prozesse 

- \subpage lnk_albenth_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_albenth_umsetzung : Details zum Code und der numerischen Umsetzung 

\n\n

Textquelle: albenth-doc.md ; Codesources: albenth.f90
