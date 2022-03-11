Variablen- und Parametersammlung{#lnk_varpar}
=================================
!> <h1>Mehr Datenfelder QSim</h1>
!! Das Modul varpar ist ein temporäre Liste, in welcher die restl. Vars/Pars 
!! landen, die nicht bereits im modul QSimDatenfelder oder modul model
!! enthalten sind. Außerdem befinden sich hier die Variablen, für die ein 
!! "broken Link" angegeben wird (die noch gefixt werden müssen)
!! Die Deklarationen sind nonsense
!!\n\n
module varpar
implicit none

!> <h1> Noch nicht deklarierte Var/Pars befinden sich hier </h1>
!! die sollten an anderer Stelle deklariert werden
!!
!> \anchor awmit Mittlerer Extinktionskoeffizient von Wasser; \f$ \epsm{W} \f$ [Einheit]
    real :: awmit

	
end module

