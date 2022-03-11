Rotatorien {#lnk_rotatorien}
==========

Anstatt die Gesamheit des Zooplanktons zu modellieren, werden in QSim lediglich 
Rädertierchen (Rotatorien) modelliert.



Die Rotatorien ernähren sich ausschließlich von Algen und üben somit Fraßdruck auf das
Phytoplankton des Modells aus. 


Wie alle Organismen, können auch Rotatorien ihre Nahrung nicht vollständig verwerten,
so dass sie einen Teil der Nahrung als Faeces wieder ausscheiden.
Aus dem assimilierten Teil ihrer Nahrung decken sie ihren Energiebedarf für
Grunderhaltung und Wachstum.
Das Modell berücksichtigt eine natürliche Absterberate, die bei Sauerstoffmangel
im Wasser ansteigen kann.
Alle Umsatzraten sind zudem temperaturabhängig, so dass eine Erhöhung der
Umgebungstemperatur zu einer Beschleunigung der Raten führt.


Die Eigenbewegung der Rotatorien ist gegenüber der Fließgeschwindigkeit des 
Gewässers vernachlässigbar, so dass sie im Modell passiv mit der Strömung verdriftet 
werden.


Weitere Informationen:
- \subpage lnk_rotatorien_equ
- \subpage lnk_rotatorien_pars
- \subpage lnk_rotatorien_num


Veröffentlichungen:
- [Schoel et al., 2002](./pdf/Schoel_et_al_2002rhein.pdf)
- [Schoel et al., 1999](./pdf/Schoel_et_al_1999mosel-saar.pdf)



<hr>
Textquelle: rotatorien-doc.md ; Codesource: konsum_kern.f90 ; 
zurück: \ref lnk_konsumenten


