Sediment-Flüsse {#lnk_sediment}
===============

\warning Momentan ist das Modul ausgeschaltet, das heißt, ein 
Stoffaustausch mit dem Sediment findet nicht statt. Der Text stammt aus einer 
früheren Version der Dokumentation.


Das ab QSim-Version 12.30 (2010) im Gewässergütemodell enthaltene Sediment-Modul 
sedflux() beschreibt die Nährstoffflüsse zwischen Sediment und Wasserkörper 
sowie den Sauerstoffverbrauch im Sediment eines Fließgewässers. Es basiert auf 
dem Modell von DITORO (2001). Danach beinhaltet die Beschreibung eines 
Sediment-Stoffflux-Modells drei Teilprozesse, nämlich die Ablagerung von 
partikulärem organischem Material (Kohlenstoff, Stickstoff und Phosphor) aus 
der Wassersäule, die Mineralisation (Diagenese) des abgelagerten Materials und 
schließlich die Reaktion der so entstandenen Zwischenprodukte im Sediment sowie 
einen Teilfluss der Stoffe zurück in das überliegende Wasser. Ferner 
beeinflussen die Diffusion von gelösten organischen Verbindungen in das 
Sediment und ihre Oxidation die Nährstoffflüsse und den Sauerstoffverbrauch im 
Sediment. Das in QSim integrierte Sediment-Modul teilt das Sediment in zwei 
Schichten, eine aerobe Schicht H1, deren Dicke sich aus dem Sauerstoffverbrauch 
in dieser Schicht ergibt und meist nur wenige Millimeter dick ist, und eine 
darunterliegende anaerobe Schicht H2 (konstant 10 cm). Das Modul ist ausführlich 
im BfG-Bericht 1843 (2016, DOI: 10.5675/BfG-1843) beschrieben.

Nachstehend das Beispiel der gelösten organischen 
Kohlenstoffverbindungen:

<center>
 \image html Sedi_orgC.png ""
</center>

Weitere Details zum Baustein sind in den folgenden Abschnitten beschrieben:

- \subpage lnk_sediment_prozesse : Erläuterung der im Schwermetall-Baustein 
implementierten Prozesse 

- \subpage lnk_sediment_vars : Auflistung der verwendeten Formelzeichen und Variablen 

- \subpage lnk_sediment_umsetzung : Details zum Code und der numerischen Umsetzung 


Textquelle: sediment-doc.md; Codesource: SedFlux.f90, sedflux_huelle.f95; 
zurück: \ref index 
