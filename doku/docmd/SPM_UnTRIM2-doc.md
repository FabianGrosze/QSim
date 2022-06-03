Modul zum Einlesen des Schwebstoffs aus UnTRIM²-SediMorph in QSim3D {#lnk_SPM_UnTRIM2}
==========

## Einführung
In QSim setzt sich der simulierte Schwebstoffgehalt aus organischem und anorganischem bzw. mineralischem Material zusammen. Das organische Material besteht dabei aus totem Material (`CP1` und `CP2`) sowie Algen (`aki`, `agr`, `abl`) und Zooplankton (`zooind`; alle umgerechnet in mg/L). Diese Größen sowie die Summe aus mineralischem und totem organischem Schwebstoff (`SS`) werden zu Beginn der Simulation über die eingegebenen Randwerte initialisiert. Die organischen Schwebstoffgrößen unterliegen neben Transportprozessen auch biogeochemischen Transformationsprozessen.

In QSim3D wird `SS` bisher über die eingespeisten Randwerte vorbelegt. Das hydrodynamisch-morphologische Modell UnTRIM²-SediMorph simuliert neben den Volumentransporten ebenfalls die Schwebstoffmenge (in kg), aufgeteilt auf vorab definierte Korngrößenfraktionen. Das hier beschriebene Modul ermöglicht diese Schwebstoffmengen in QSim3D einzulesen und in die entsprechenden Konzentrationen umzurechnen, um den Einfluss des mineralischen Schwebstoffs auf das Lichtklima besser abbilden zu können.

Bei der aktuellen Umsetzung ist zu beachten, dass die Zusammenführung des toten organischen Materials und des mineralischen Schwebstoffs in `SS` in QSim im Zusammenspiel mit dem Belegen von `SS` mit dem eingelesenen (mineralischen) Schwebstoff aus UnTRIM²-SediMorph bedeutet, dass es hier eine Inkonsistenz gibt, die nur behoben werden kann, indem in Zukunft die toten organischen Schwebstoffanteile und der mineralische Schwebstoff in QSim voneinander separiert werden.

Weitere Details zum Modul sind in den folgenden Abschnitten beschrieben:
- \subpage lnk_SPM_UnTRIM2_umsetzung : Erläuterung der Umsetzung
- \subpage lnk_SPM_UnTRIM2_bedienung : Erläuterung der Bedienung

Aus Datei: SPM_UnTRIM2-doc.md

Code in Datei: module_suspendedMatter.f90



