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

      SUBROUTINE erosion_kern(tflie,TIEFEs,RAUs,VMITTs        &
		                     ,SSeross,sss,ssalgs,dsedHs       &
						     ,tauscs,M_eross,n_eross,sedrohs  &
		                     ,kontroll_kern,jjj)
                                                                     
!     UNTERPROGRAMM ZUR Bestimmung der Erosionsrate                     
!     AUTOR:VOLKER KIRCHESCH                                            
!     STAND:31.07.2019                                                    
!     Restrukturierung Jens Wyrwa   Nov.2021                                                          
                                                                       
!     sedroh   -  Rohdichte des Sediments [Kg*m-3] 
!     dReros   -  Erosionsrate in kg/(m2) je Zeitschritt
!     dsedH    -  Sohlhöhenänderung in mm    
                                                                       
      implicit none
      logical :: kontroll_kern
      integer :: jjj 
      real    :: tflie,TIEFEs,RAUs,VMITTs,SSeross,sss,ssalgs,tauscs,M_eross,n_eross,sedrohs
	  real    :: roh2o, g, UST, taus, dReros
	  real dsedHs !! Sohlhöhenänderung im aktuellen Zeitschritt

      if(kontroll_kern)then
	     print*,'erosion_kern TIEFE,RAU,VMITT,',TIEFEs,RAUs,VMITTs
		 print*,'tausc,M_eros,n_eros,sedroh=',tauscs,M_eross,n_eross,sedrohs
      endif
      roh2o = 1000. 
      G = 9.81 

! #### Berechnung der Sohlschubspannung ####
 
      UST = ( ((G/RAUs)**0.5) / (TIEFEs**0.166667) )*abs(VMITTs) 
      taus = (ust**2)*roh2o 
                                                                       
! #############################################################

      if((taus .gt. tauscs).and.(tauscs.gt.0.0).and.(sedrohs.gt.0.0).and.(tiefes.gt.0.0))then
         dReros = M_eross*((taus-tauscs)/tauscs)**n_eross                                     
         dReros = dReros*tflie*86400. 
         dsedHs = 1000.0* dReros/sedrohs
         SSeross = (dReros/tiefes) 
         SSs = SSs+SSeross*1000. 
         SSalgs = SSalgs+SSeross*1000.
      endif
	
      if(kontroll_kern)print*,'erosion_kern taus,SS,SSalg,SSeros,dsedH,jjj=',  &
	                    taus,SSs,SSalgs,SSeross,dsedHs,jjj
                                                                       
  END subroutine erosion_kern                                           

