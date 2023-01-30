Stofftransport  {#lnk_stofftransport}
=================

In Fließgewässern gelöste Stoffe sowie planktisch lebende Organismen, die 
vernachlässigbare Bewegungen gegenüber dem umgebenden Wasser ausführen, werden 
mit der Strömungsgeschwindigkeit transportiert. Dabei kommt es nicht nur zur 
konvektiven Verlagerung, sondern auch zu dispersiven Vermischungen. Diese 
beruhen auf einer Reihe physikalischer Effekte und führen dazu, dass die 
Maximalkonzentration mit zunehmender Fließstrecke verringert wird, aber 
gleichzeitig, dass das mit Stoffen versetzte Wasservolumen anwächst 
(Abbildung 1). Je nachdem um welche Art von Stoff es sich handelt kann die 
Wasserqualität maßgeblich beeinträchtigt, aber auch die natürlichen Funktionen 
aquatischer Ökosysteme beeinflusst werden. Demnach ist der Stofftransport in 
Fließgewässern ein relevanter Schlüsselprozess, welcher in QSim im 
„Transportbaustein“ umgesetzt wird. 

Durch die Lösung der 1D-Advektions-Dispersions-Gleichung werden die im 
Querschnitt gemittelten Konzentrationsänderungen berechnet. Für die Berechnung 
des Advektionsanteils kann zwischen drei verschiedenen numerischen Verfahren 
(Cubic interpolated Pseudo-Particle – CIP, Lax-Wendroff-Verfahren und Quadratic 
Upstream Interpolation for Convective Kinematics with Estimated Upstream Terms – 
QUICKEST) gewählt werden, während der Dispersionsanteil entsprechend mit dem 
Crank-Nicolson oder McCormack-Verfahren gelöst wird. Für die Auswahl des 
Dispersionskoeffizienten stehen vier Gleichungen zur Verfügung. Zusätzlich 
besteht die Möglichkeit bei mit Buhnen geprägten Fließgewässern laterale 
Austauchprozesse zu berücksichtigen, um den Einfluss der Stillwasserzonen auf 
die Stoffkonzentration widerzuspiegeln. 

![Schematische Darstellung der advektiven und dispersiven Prozesse beim Stofftransport.](img/transport_schema_adv_disp.png)

Die Details zum Stofftransport werden in dieser Dokumentation (zunächst) getrennt 
beschrieben:

- \subpage lnk_stofftransport_1d
mit
  - \subpage lnk_transport_1d_vars
  - \subpage lnk_transport_1d_umsetzung
- \subpage lnk_stofftransport_3d

Textquelle: stofftransport.md ; Codesources: stofftransport.f95 ;  
zurück: \ref index
 
