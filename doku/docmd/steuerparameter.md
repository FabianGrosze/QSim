Steuerparameter {#lnk_steuerparameter}
===============

*in Arbeit*

QSim1D
=====

| Variable 	| Funktion 	| 
|-----------|-----------|
| iwied 	| ??? 		| 
| ilbuhn   	| ???    	| 
| iph 		| mit pH Berechnung | 
| flag(ior)/=4| Einleitung? | 
| | | 

      if(ischwer==1)ipps = 37
      if(ilang==0)then
      ieinsh(mstr) = iein !! Einleitungen am Strang
      ieinLs(mstr)= ieinL 

QSim3D
=====

| Variable 	| Funktion 	| 
|-----------|-----------|
| hydro_trieb 	| ??? 		| 
| knotenanzahl   	| ???    	| 



-----------------------------------------------------------------------

!.....Schalter für "Regeln bei Kraftwerksbetrieb"                       
!               iRHKW = 1 > Betrieb der HKW's unterliegt gewissen Regeln
      iRHKW = 0 
!                                                                       
!...Dicke der vertikalen Schichten                                      
!                                                                       
      maus = 0
      iend = 0
      iwied = 0 
      ilang = 0 
      ilbuhn = 0 
      jlauf = 0 
      jtag = 0 


zurück zu \ref lnk_Datentechnik ; Quelle: steuerparameter.md