Global radiation {#lnk_strahlung}
=========

Global radiation is a key factor in the heat balance of a water body.
However, the global radiation also plays a crucial role for primary producers
and thus the productivity of a water body.

In QSim, the radiation is used to simulate the following processes:

- Heat balance of the waterway <br>
 *(wavelength 290 - 4000 nm)* ;
- Growth of algae and macrophytes by photosynthesis <br>
 _(wavelength 400 – 700 nm)_ ;
- Photolytic death of faecal coliforms <br>
 _(wavelength 290 - 380 nm)_ . 

<!-- #todo: Wellenlängenangaben erscheinen noch nicht kursiv auf kompilierter
Website --> 
 
The individual radiation components are calculated from global radiation
and cloud cover. Global radiation and cloud cover are based on measurement data 
and are fed into the model as boundary conditions.
 
Details on the calculation of the radiation components and its implementation in 
the program code are described in the following sections:

- \subpage lnk_strahlung_prozesse
- \subpage lnk_strahlung_vars
- \subpage lnk_strahlung_umsetzung

Text source: strahlung-doc.md; Code sources: strahlg.f90, sasu.f95; 
go back to: \ref lnk_waerme

<!-- #mf: noch etwas die Prozesse oben ausführen? Evtl. noch die verschiedenen
Strahlungskomponenten unterscheiden und kurz erläutern, dafür evtl. allg. Text aus dem 
nächsten Reiter rausziehen -->
