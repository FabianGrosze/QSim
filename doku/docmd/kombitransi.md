Zusammenfügen von Teilzeiträumen {#lnk_kombitransi}
================================

Das Programm >>kombi<<
dient der Kombination von Transinfo-Verzeichnissen

Wenn ein längerer Zeitabschnitt (z.B. ein Jahresgang)
für die hydraulische Simulation in Teile (Zeitabschnitte) zerlegt wurde,
um ihn auf mehreren Prozessoren parallel rechnen zu können,
ist es nach Abschluss aller Teil-Rechenläufe erforderlich, die
Transportinformationen für die Gütesimulation mit QSim-3D in ein Verzeichnis 
zusammenzuführen, so dass ein durchgängiger Jahresgang entsteht.

Dies ist möglich, weil das Erinnerungsvermögen von Impuls und Wasserstand, die 
vom hydraulischen Treiber simuliert werden,
in einem Ästuar kaum läger zurückreicht als einen Tag.
Konzentrationsverteilungen, die im Gütemodell simuliert werden, haben in 
einem Ästuar mit mehreren Monaten Wasseraufenthaltszeit ein viel längeres 
Gedächtnis.

Textquelle: kombitransi.md ; Codesources: kombitransi.f95 ;  
zurück: \ref lnk_transport_numerik
