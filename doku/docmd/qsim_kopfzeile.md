Versions-Kopfzeile der QSim-Dateien {#lnk_kopfzeile}
===================================

Die Versions-Kopfzeile gibt die Version des beteiligten QSim-Programms an.
Sie ist in der Regel die erste Zeile der QSim-Dateien wie \ref lnk_modell_a 
"ModellA.txt".

Die Zeile hat maximal 255 Zeichen.

Die Anführungszeichen in der folgenden Beschreibung kennzeichnen konstante Texte.


| Feld | Inhalt | Erläuterung |
| ---- | ------ | ----------- |
| 1 | "*V" | Zeilenkennung der Versions-Kopfzeile |
| 2 | "QSim" | Kennzeichnung der Datei als QSim-Schnittstelle |
| 3 | Typ | Bezeichnung des Dateityps, z.&nbsp;B. "ModellA" |
| 4 | QSim-Version | Im Fall einer Eingangsdatei die Version des QSim-Programms, das die verwendete Definitionsdatei (EreigGParam.xml o.&nbsp;ä.) erzeugt hat.\n
  Im Fall einer Ausgangsdatei die Version des QSim-Programms, das die Datei berechnet hat. |
| 5 | Weitere Angaben | Optional weitere Angaben, z.&nbsp;B. die Version des Gerris-Programms, das die Datei erzeugt hat. Diese Angaben werden von QSim nicht berücksichtigt. |


Textquelle: qsim_kopfzeile.md ; Codesources: eingabe.f95 ;  
zurück: \ref lnk_datenmodell
 