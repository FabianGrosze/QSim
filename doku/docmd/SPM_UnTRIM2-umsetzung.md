Modul zum Einlesen des Schwebstoffs aus UnTRIM²-SediMorph in QSim3D - Umsetzung {#lnk_SPM_UnTRIM2_umsetzung}
===============================

Das Einlesen des mit UnTRIM²-SediMorph simulierten Schwebstoffs wird in der aktuellen Umsetzung über den Modellschalter `iEros` angesteuert:
- `iEros >= 0`: Verhalten wie bisher,
- `iEros  < 0`: QSim liest für jeden Simulationszeitschritt die Masse der `nClasses = abs(iEros)` feinsten Korngrößenfraktionen ein und berechnet aus diesen die Schwebstoffkonzentration, welche in QSim auf `SS` geschrieben wird. Anschließend wird `SSalg` unter Verwendung dieses `SS` berechnet.

In der aktuellen Umsetzung muss der hydrodynamische Antrieb dafür die folgenden Größen enthalten:
- `nMesh2_suspension_classes` (Dim.): Anzahl der verfügbaren Korngrößenfraktionen (incl. der Gesamtschwebstoffmenge),
- `Mesh2_face_Schwebstoffmenge_2d` (Var.): die über die Wassersäule integrierte und über den Ausgabezeitschritt gemittelte Schwebstoffmasse (in kg) für alle Fraktionen (incl. Gesamtschwebstoff), \f$ M_{SS,i} \f$,
- `Mesh2_face_mittleres\Wasservolumen_2d` (Var.): das über den Ausgabezeitschritt gemittelte Volumen der Wassersäule (\f$m^3\f$), \f$V_{SS}\f$, welches das Referenzvolumen für den Schwebstoff darstellt.

Im Fall von `iEros < 0` prüft QSim3D zunächst, ob die Anzahl der zu lesenden Korngrößenfraktionen kleiner oder gleich der verfügbaren Korngrößenfraktionen ist: `nClasses <= nClassesFile - 1`. Wenn dies gewährleistet ist, liest QSim3D zu jedem Berechnungszeitschritt die Massen der `nClasses` feinsten Korngrößenfraktionen ein, summiert diese und rechnet diese in eine Konzentration (\f$C_{SS}\f$) um:

\f[ M_{SS}(t) = \sum_{i=1}^{nClasses} M_{SS,i}(t), \f]
\f[ C_{SS}(t) = \frac{M_{SS}(t)}{V_{SS}(t)}. \f]


Da im Fall `iEros < 0` die Variablen `SS` und `SSalg` zu jedem Zeitschritt unter Verwendung der eingelesenen Daten im gesamten Modellgebiet neu berechnet werden, unterliegen beide Größen dann keinem Transport in QSim3D.

aus Datei: SPM_UnTRIM2-umsetzung.md; 

Code in Datei module_suspendedMatter.f90
