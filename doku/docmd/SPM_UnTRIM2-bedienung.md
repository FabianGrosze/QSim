Modul zum Einlesen des Schwebstoffs aus UnTRIM²-SediMorph in QSim3D - Bedienung {#lnk_SPM_UnTRIM2_bedienung}
===============================

Die Belegung von `SS` aus den Ergebnissen des hydrodynamischen Antriebs wird (wie in lnk_SPM_UnTRIM2_umsetzung beschrieben) über den Modellschalter `iEros < 0` aktiviert. Dabei beschreibt `nClasses = abs(iEros)` die Anzahl der einzulesenden Korngrößenfraktionen, beginnend bei der feinsten. Über GERRIS können `iEros` aktuell nur die Werte 0 (mit Erosion) oder 1 (ohne Erosion) zugewiesen werden. Die Verwendung von `iEros < 0` ist somit nur eine Übergangslösung, solange QSim nicht flexibel um zusätzliche Modellschalter (analog zu zusätzlichen Güteparametern) erweitert werden kann.\par

Derzeit muss `iEros < 0`  durch manuelles Editieren der `EREIGG.txt` gesetzt werden. Hierzu muss vor dem Abschicken eines QSim3D-Batch-Jobs auf dem HPC der sechste Wert in der sechsten Zeile von `EREIGG.txt` verändert werden. Dies ist im unten gezeigten Auszug des Dateikopfes beispielhaft dargestellt. Anschließend muss der QSim3D-Batch-Job mittels `sbatch` manuell abgeschickt werden.

<center>
\image html UnTRIM2_SPM_EREIGG.png "Dateikopf der `EREIGG.txt` mit aktiviertem Einlesen der zwei feinsten Korngrößenfraktionen (s. rote Markierung)."
</center>

aus Datei: SPM_UnTRIM2-umsetzung.md; 

Code in Datei module_suspendedMatter.f90
