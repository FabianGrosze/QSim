Makrophyten - Umsetzung {#lnk_makrophyt_umsetzung}
===============================

\warning Momentan ist das Modul ausgeschaltet, das heißt, Makrophyten werden 
nicht simuliert. Es handelt sich um eine alte Dokumentation.


## Herkunft 
Unterprogramm zur Berechnung des Makrophytenwachstums und dessen Einfluss auf 
den Stoff- und Sauerstoffhaushalt eines Fließgewässers 

Autor: Volker Kirchesch

entnommen aus Version qsim1xxx

## Schnittstellenbeschreibung 
Die Makrophyten-Biomasse wird in der Subroutine mphyt() berechnet, welche
die folgenden Abhängigkeiten hat:

    call mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie                    &
	     ,itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi    &
	     ,pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,ifehl,ifhStr       &
   	     ,.false.,0)
 

### Verwendung in anderen Modulen
 
1.	Makrophytendichte AMak,TFlie (pfl) -> orgC.f90 (Umrechnung auf mg/l C ?), 
    NCyc: Ammoniumoxidation auf Oberfläche der Makrophyten
2.	Max. Makrophytendichte AMak,, max, TFlie (pflmax) -> für nächsten Zeitschr.
3.	Sauerstoffproduktion durch Wasserpflanzen O2,Mak (po2p) -> Oxygen.f90 O2-Bilanz
4.	Sauerstoffzehrung  durch Wasserpflanzen (po2r) -> Oxygen.f90 O2-Bilanz
 
 
 
## IT-Realisierung
...




\n\n

Textquelle: baustein-umsetzung.md; Codesources: mphyt.f90 ; 
zurück \ref lnk_makrophyt oder ... 
