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

  subroutine verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,iformVert,i) 

!##############################################
!    Berechnung der Schwermetallkonzentrationen
!    Cd, Zn, Cu, Ni
!    
!    Stand: 06.04.2018
!    Autor: Volker Kirchesch
!##############################################


      real, Dimension(2)                   :: VTKoeff_Zn, VTKoeff_Cad, VTKoeff_Cu, VTKoeff_Ni
 
!#################################################################
! Konstanten und Exponenten zur Berechnung der Verteilungsfunktion
!#################################################################
 
     if(iformVert==1)then  ! DWA-Modell
       c1Zn = 144.
       e1Zn = 1.038
       c2Zn = 17769.
       e2Zn = 0.673
       c3Zn = 42.
       e3Zn = 0.056
       c4Zn = 0.308
       e4Zn = -1.
       c5Zn = -101.
       e5Zn = 3.5

       c1Cu = 45.
       e1Cu = 0.496
       c2Cu = 2541.
       e2Cu = 0.807
       c3Cu = 13.
       e3Cu = 0.172
       c4Cu = -1660.
       e4Cu = 0.459
       c5Cu = 0.0
       e5Cu = 3.2

       c1Cad = 49090.
       e1Cad = 1.586
       c2Cad = 12556.
       e2Cad = 0.641
       c3Cad = 17.
       e3Cad = -0.023
       c4Cad = -251483.
       e4Cad = 1.835
       c5Cad = 0.0
       e5Cad = 3.8

       c1Ni = 21.
       e1Ni = 0.548
       c2Ni = 1666.
       e2Ni = 0.872
       c3Ni = 63.
       e3Ni = 0.205
       c4Ni = -294.
       e4Ni = 0.810
       c5Ni = 0.0
       e5Ni = 4.2


!########################################################################################
! Berechnung der Verteilungskoeffizienten VTKoeff_Zn, VTKoeff_Cu, VTKoeff_Cad, VTKoeff_Ni
! VTKoff.. in l/g
!########################################################################################

        VTKoeff_Zn(i) = (c1Zn/hcSS**e1Zn) + (c2Zn/hcSS**e2Zn)*((hcph/9.)**(c3Zn/hcSS**e3Zn))          &
                        + ((c4Zn/hcSS**e4Zn)+c5Zn)*(((hcph-4.)/5.)**e5Zn-((hcph-4.)/5.)**(e5Zn-1)) 
 
        VTKoeff_Cu(i) = (c1Cu/hcSS**e1Cu) + (c2Cu/hcSS**e2Cu)*((hcph/9.)**(c3Cu/hcSS**e3Cu))          &
                        + ((c4Cu/hcSS**e4Cu)+c5Cu)*(((hcph-4.)/5.)**e5Cu-((hcph-4.)/5.)**(e5Cu-1)) 

        VTKoeff_Cad(i) = (c1Cad/hcSS**e1Cad) + (c2Cad/hcSS**e2Cad)*((hcph/9.)**(c3Cad/hcSS**e3Cad))   &
                         + ((c4Cad/hcSS**e4Cad)+c5Cad)*(((hcph-4.)/5.)**e5Cad-((hcph-4.)/5.)**(e5Cad-1)) 

        VTKoeff_Ni(i) = (c1Ni/hcSS**e1Ni) + (c2Ni/hcSS**e2Ni)*((hcph/9.)**(c3Ni/hcSS**e3Ni))          &
                        + ((c4Ni/hcSS**e4Ni)+c5Ni)*(((hcph-4.)/5.)**e5Ni-((hcph-4.)/5.)**(e5Ni-1)) 

   else    ! Deltares 2010
     
     VTKoeff_Zn(i) = 102.
     VTKoeff_Cu(i) = 46.
     VTKoeff_Cad(i) = 116. 
     VTKoeff_Ni(i) = 9.
!     VTKoeff_Pb(i) = 603.
!     VTKoeff_As(i) = 263.     
   endif          

  end subroutine verteilungskoeff
