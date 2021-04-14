!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

  subroutine CBSB5(BL0, BL0t, obsb, obsbt, ior, mstr)



 real                                 ::  k_BSB
 real, Dimension(1000)                ::  obsb

 save deltat

      deltat = 5.
                                                       

!########################
!    BSB5-Berechnung
!########################

      hc_wert = min(0.95,obsb(ior)/Bl0)  
      k_BSB = (-1.*log(1.-hc_wert))/deltat        ! Abbaurate 1. Ordnung berechnet aus dem alten Zeitschritt

      obsbt = BL0t*(1.-exp(-k_BSB*deltat))

  end subroutine CBSB5
