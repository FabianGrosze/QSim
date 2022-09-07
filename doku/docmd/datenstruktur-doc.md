Datenstruktur {#lnk_datenstruktur}
==============

Hier werden die in QSim simulierten Variablen in Gruppen eingeteilt.
In den Tabellen der Gruppen sind Code-Name, Einheit etc. der Variablen
zusammengefasst.

# Variablenfelder {#lnk_variablenfelder}

In Qsim3D wurden Variablenfelder, Datenfelder und Parameter gemäß ihrer Funktion 
in den untenstehenden Gruppen zusammengefasst.
In QSim1D existiert diese Gruppierung noch nicht. 
Die Namen der Variablenfelder aus QSim1D wurden in QSim3D übernommen. 
Siehe dazu: \subpage lnk_huellen


1. \subpage lnk_var_planktisch, die im fließenden Wasser transportiert werden. 
    Diese Datenfelder werden nach jedem parallelen Stoffumsetzungs-Schritt 
	zusammengesammelt (MPI_gather in gather_planktkon()),um dann transportiert 
	(stofftransport()) zu werden. 
    Nach dem gemeinsamen Transport (Advektions-Diffusions-Simulation)
    werden die Datenfelder wieder auf die parallelen Prozesse verteilt 
    (MPI_scatter in scatter_planktkon()). \n 

2. \subpage lnk_var_benthisch, beschreibt Eigenschaften der Gewässersohle
    Diese Variablen sind ortsfest und 2-dimensional. 
    Die Initialisierungswerte werden vor dem ersten Zeitschritt verteilt 
	(scatter_benth()).
    Eingesammelt werden müssen diese Variablenfelder nur zum Zweck der Ausgabe 
	(gather_benth()). \n

3. \subpage lnk_uebergabewerte hält Daten, die dem Datenaustausch zwischen den 
    verschiedenen Stoffumsetzungsmodulen dienen und für die kein Transport 
	berechnet werden muss. Es handelt sich um 0D, 2D und 3D Variablen.\n
    Dieses Modul hält auch die \ref lnk_globale_parameter .\n
    Nach dem Einlesen/Initialisierung müssen die Werte auf die parallelen 
	Prozesse verteilt werden (scatter_uebergabe()).
    Eingesammelt werden müssen diese Variablenfelder nur zum Zweck der Ausgabe 
	(gather_uebergabe()). \n

4. Randdaten, d.h Vorgaben, die sich im Verlauf der Simulationszeitschritte 
    infolge externer Setzungen verändern, sind in die folgenden Datenstrukturen 
	aufgeteilt:
    \ref lnk_randbedingungen, \n
    \ref lnk_hydraul_rb , deren Änderung vom Hydraulischen Treiber vorab berechnet 
	wurde und \n
    \ref lnk_wetter_rb \n
    Ihre Werte müssen vor jedem Stoffumsetzungs-Zeitschritt verteilt werden 
	(scatter_RB()). Ein Widereinsammeln ist nicht erforderlich. \n

5. Je nach hydraulischem Treiber werden unterschiedliche Datenfelder für die 
    Übernahme verwendet.\n\n
     - \subpage lnk_transport_numerik \n
     - Zellrandflüsse aus Untrim\n
     - Anbindung von SCHISM steht noch aus. \n

6. \subpage lnk_steuerparameter werden Programmintern zur Ablaufsteuerung und 
    zur Verfahrensauswahl benutzt. \n

7. Die Modellparameter von QSim sind [hier](\ref lnk_globale_parameter) 
    zusammengefasst. \n


# Benennung der Variablen und Parameter im QSim-Code {#lnk_benennung}

Der Bennenung von Variablen und Parametern im QSim-Code unterliegt eine 
gewisse Struktur, die in der folgenden Tabelle zusammengefasst ist.
Achtung: Die Tabelle ist unvollständig und im Aufbau. Zum Teil wird von der 
Struktur abgewichen.

| Zusatz  |  Platz        |  Beispiel |  Bedeutung |
|-------- |---------------|-----------|------------|
| a       | vorangestellt | apfl | ?? |
| b       | vorangestellt | bgesN     | Buhnenfeld |
| e       | vorangestellt | egesN     | Einleiter (?) |
| e und h | vor- und nachgestellt     | egesNh | ?? |
|  h      | vorangestellt | hgesN     | geht über alle Stränge (?) |
|  L      | nachgestellt  | gesnL     | Linieneinleiter (?) |
|  LH     | nachgestellt  | gesnLH    | Linieneinleiter (?) |
|  max    | nachgestellt  | pflmax    | Maximum über ?? |
|  min    | nachgestellt  | pflmin    | Minimum über ?? |
|  mis    | nachgestellt  | pflmis    | ?? |
|  mxs    | nachgestellt  | pflmxs    | ?? |
|  s      | vorangestellt | svx0      | ?? |
|  s      | nachgestellt  | gesns     | Variable lokal in Subroutine |
|  sum    | vorangestellt | sumpfl    | Summe über ?? |
|  t      | nachgestellt  | vo2t      | jeweilige Größe am jeweiligen Profil (am Ende der jeweiligen Knotenschleife) |
|  y      | nachgestellt  | vx0y      | für Mittelung (?) |
|  z      | nachgestellt  | hgesnz    | tiefenaufgelöst |
|  zt     | nachgestellt  | gesnzt    | ?? |
|  zw     | vorangestellt | zwgesN    | Zwischengröße |
|  z_z    | nachgestellt  | hgesnz_z  | dreifach indiziert (?)  |

#  
 
aus Datei: datenstruktur-doc.md ; zurück:\ref index
