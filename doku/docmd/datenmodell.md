Datenmodell {#lnk_datenmodell}
===========

# Bestandteile des Datenmodells
Um das Modell und die damit durchzuführende Berechnung zu definieren
werden von QSim3D die folgenden Dateien eingelesen.

Dabei wird das Konzept verfolgt, soweit wie möglich, die selben Dateiformate 
zu verwenden wie QSim1D.

Hier werden daher nur die Änderungen beschrieben die sich im Bezug zur

<a href = "./pdf/Schnittstelle_QSIM2004.pdf" target = 
"_blank">Schnittstellenbeschreibung 2004 Gerris-QSim</a>
<a href = "./pdf/schnittstellenbeschreibung2006_gerris.pdf" target = 
"_blank">Schnittstellenbeschreibung 2006 Gerris-QSim</a>

ergeben.

| Datei-Name\n(Spezifikation verlinkt) | Format | Änderung zu QSim | Kurz-Beschreibung | wird gelesen von (Details) |
| ---------- | ----- | ----- | ----- | ----- |
| Gütemodell |       |       |       |       |
| \subpage lnk_ereig_g  | ascii | keine | \ref lnk_ereignissteuerung | ereigg_modell(), ereigg_Randbedingungen_lesen() |
| \subpage lnk_modell_a | ascii | keine | es werden von QSim3D nur noch Modellname und die Geologischen Breiten- und Längenkoordinaten daraus gelesen | modella() |
| \subpage lnk_wetter_txt | ascii | keine | \ref lnk_wetter_rb | wetter_readallo0() |
| \subpage lnk_modell_g3 | ascii | ja, ehemals ModellG.txt | \ref lnk_lokale_parameter und \ref lnk_anfangsbedingungen | modellg() |
|<a href="./exp/APARAM_200314.txt" target="_blank">APARAM.txt</a> | ascii | keine | \ref lnk_globale_parameter | aparam_lesen() |
|<a href="./exp/e_extnct.dat" target="_blank">e_extnct.dat</a> | ascii |  keine      | \subpage lnk_extnct_rb | e_extnct_lesen() |
| Hydraulischer Treiber |       |       |       |       |
|<a href="./exp/points" target="_blank">points</a> <br>  | ascii | neu, wird von casu (transinfo) ausgegeben. Nicht die converti-Version verwenden! | Infos zu den knoten: Lage (x+y), Höhe (z), Zonen_nummer(n), Randnummer, Zellfläche | points() |
|<a href="./exp/file.elements" target="_blank">file.elements</a> | ascii | converti, casu-eingabe-Datei | Vermaschung Netz, wird zur Ergebnis-Darstellung benötigt | elements() |
|<a href="./exp/transinfometa" target="_blank">transinfo/meta</a> | ascii | neu, wird von casu ausgegeben | Meta-Infos zu den Transportinformationen | transinfo_sichten() |
| transinfo/t* | binär | neu, wird von casu ausgegeben | \ref lnk_transport_numerik | holen_trans() |
| Ausgabesteuerung |
|<a href="./exp/ganglinien_knoten.txt" target="_blank">ganglinien_knoten.txt</a> | ascii | neu | Knotennummern, an denen Ganglinien ausgegeben werden sollen | ganglinien_lesen(), ganglinien_zeitschritt(), ganglinien_schliessen() |
|<a href="./exp/ausgabezeitpunkte.txt" target="_blank">ausgabezeitpunkte.txt</a> | ascii | neu | Zeitpunkte, zu denen komplette Konzentrationsfelder ausgegeben werden sollen | ausgabezeitpunkte() |
|<a href="./exp/ausgabekonzentrationen.txt" target="_blank">ausgabekonzentrationen.txt</a> ausgabekonzentrationen_beispiel.txt | ascii | neu | Angabe, welche Konzentrationen als Ganglinen und als komplette Konzentrrationsfelder ausgegeben werden sollen (Liste zum Ankreuzen x=ja 0=nein). Die Liste aller verfügbaren Variablenn wird von qsim3d als "ausgabekonzentrationen_beispiel.txt" bei jedem lauf angegeben. | ausgabekonzentrationen() |
|<a href="./exp/kilonummer" target="_blank">kilonummer</a> | ascii | neu | Zuordnung von Längsschnittknoten zu Kilometern | \ref lnk_laengsschnitt |
|<a href="./exp/email.txt" target="_blank">email.txt</a> | ascii | neu | eine Zeile mit der Emailadresse unter der/die Bearbeiter/in über das Ende des Rechenlaufes unterrichtet werden möchte | modell_vollstaendig() |


Ein- und Ausgabe sind nicht parallelisiert, d.h. sie laufen nur auf Prozess 0 !

\subpage lnk_kopfzeile


Textquelle: datenmodell.md ; Codesources: eingabe.f95 ;  
zurück: \ref lnk_modellerstellung
