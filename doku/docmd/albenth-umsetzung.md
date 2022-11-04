Benthische Algen - Umsetzung {#lnk_albenth_umsetzung}
========================= 

## Herkunft ##
Autor: Volker Kirchesch

## Schnittstellenbeschreibung ##
call albenth(\ref schwi, \ref tflie, \ref tempw, \ref tiefe, \ref vmitt, 
             \ref vno3, \ref vnh4, \ref gelp               &
		   , \ref albewg, \ref alberg, \ref elen, \ref flae, \ref ior, 
		   \ref anze, \ref aggmax, \ref agksn, \ref agksp       &
		   , \ref si, \ref akksn, \ref akksp, \ref akkssi, \ref akgmax, 
		   \ref albewk, \ref alberk, \ref abegm2, \ref abekm2  &
		   ,\ref vabfl, \ref cmatgr, \ref cmatki, \ref akchl, \ref agchl,
		   \ref extk, \ref ilang, \ref mstr           &
		   ,.false., 0)

\n\n

## Verknüpfung zu anderen Modulen

An andere Module werden die folgenden Größen übergeben:
- albewk und albewg: Berechneter Zuwachs Biomasse benthische Kiesel-/Grünalgen
  [mg Biomasse/L]
- alberk und alberg: Berechneter Respirationsverlust Biomasse benthischer 
  Kiesel-/Grünalgen [mg Biomasse/L]

Es bestehen Verknüpfungen zu folgenden Modulen/Subroutinen:

- Ncyc (N-Bilanz): aus Differenz zwischen Wachstum und Respriration (auf mg/l 
  bezogen) wird unter der Annahme einer maximalen Nährstoffkonzentration 
  entsprechend den Angaben in der Parameterliste für planktische Algen der 
  Einfluss des N-Entzugs durch das Wachstum benthischer Algen berücksichtigt
- Ph (pH Wert Berechnung): entsprechend dem BM:C der Algengruppe wird das bei 
  der Respiration gebildete und beim Wachstum aufgenommene CO2 bilanziert
- Po4s (P-Bilanz): aus der Differenz zwischen Wachstum und Respriration 
  (auf mg/l bezogen) wird unter der Annahme einer maximalen 
  Nährstoffkonzentration entsprechend den Angaben in der Parameterliste für 
  planktische Algen der Einfluss des P-Entzugs durch das Wachstum benthischer 
  Algen berücksichtigt
- Oxygen (O-Bilanz): für das Wachstum und die Respiration werden jeweils die 
  Sauerstofffreisetzung bzw. Zehrung berechnet, wobei das NH4/NO3-Verhältnis, 
  wie es für die jeweilige Algengruppe bei planktischen Algen verwendet wird, 
  ebenfalls zur Umrechnung herangezogen wird
  
\note Es gibt derzeit keine Beziehung zu dem Sedimentmodul, checken ob plausibel 
(O2 Bildung dort sollte ggf. Einfluss haben können?)


Textquelle: albenth-umsetzung.md; Code: albenth.f90; zurück \ref lnk_albenth
