!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualit�t
!
!   Copyright (C) 2020 Bundesanstalt f�r Gew�sserkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie k�nnen es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation ver�ffentlicht, weitergeben und/oder modifizieren. 
!   Die Ver�ffentlichung dieses Programms erfolgt in der Hoffnung, da� es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT F�R EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------


!> Sedimentbelastung wird aus der Belastung des suspendierten Sediments berrechnet.\n
!! Dazu wird �ber die erosionsfreien Zeitschritte gemittelt.\n
!! Nach dem Ende der Erosion wird die Mittelung fortgesetzt.\n\n
!! file: sedimentbelastung.f90 zur�ck: \ref lnk_schwermetalle
  subroutine Sedimentbelastung(SSalgs  						&
                               ,hgsZns,hglZns,ZnSeds		&
							   ,hgsCads,hglCads,CadSeds	    &
                               ,hgsCus,hglCus,CuSeds		&
							   ,hgsNis,hglNis,NiSeds		&
							   ,hgsAss,hglAss,AsSeds		&
							   ,hgsPbs,hglPbs,PbSeds        &
                               ,hgsCrs,hglCrs,CrSeds		&
							   ,hgsFes,hglFes,FeSeds		&
							   ,hgsHgs,hglHgs,HgSeds		&
							   ,hgsMns,hglMns,MnSeds        &
                               ,hgsUs,hglUs,USeds			&
							   ,anzZeits,SSeross            &
							   ,kontroll ,jjj)

      implicit none
      logical                  :: kontroll
      integer                  :: jjj,anzZeits
      real                     :: SSalgs,SSeross
      real                     :: hgsZns,hglZns,ZnSeds
      real                     :: hgsCads,hglCads,CadSeds
      real                     :: hgsCus,hglCus,CuSeds
      real                     :: hgsNis,hglNis,NiSeds
      real                     :: hgsAss,hglAss,AsSeds
      real                     :: hgsPbs,hglPbs,PbSeds
      real                     :: hgsCrs,hglCrs,CrSeds
      real                     :: hgsFes,hglFes,FeSeds
      real                     :: hgsHgs,hglHgs,HgSeds
      real                     :: hgsMns,hglMns,MnSeds
      real                     :: hgsUs,hglUs,USeds

      ! counting timesteps without erosion	  
	  if (SSeross.le.0.0)anzZeits=anzZeits+1

	  if(anzZeits.le.0)then ! no deposition yet, sediment equals suspension
	     !print*,'Sedimentbelastung timecounter anzZeits .le. zero'
	     ZnSeds = 1000 * (hgsZns-hglZns)/SSalgs
	     CadSeds= 1000*(hgsCads-hglCads)/SSalgs
	     CuSeds = 1000 * (hgsCus-hglCus)/SSalgs
	     NiSeds = 1000 * (hgsNis-hglNis)/SSalgs
	     AsSeds = 1000 * (hgsAss-hglAss)/SSalgs
	     PbSeds = 1000 * (hgsPbs-hglPbs)/SSalgs
	     CrSeds = 1000 * (hgsCrs-hglCrs)/SSalgs
	     FeSeds = 1000 * (hgsFes-hglFes)/SSalgs
	     HgSeds = 1000 * (hgsHgs-hglHgs)/SSalgs
	     MnSeds = 1000 * (hgsMns-hglMns)/SSalgs
	     USeds  = 1000 * (hgsUs-hglUs)  /SSalgs
	     !stop
	  else
	     ! compute sedimentcontent from median of water content in erosionfree timesteps
	     ZnSeds = ( ZnSeds*(anzZeits-1)+(1000 * (hgsZns-hglZns)/SSalgs) )/real(anzZeits)
	     CadSeds= (CadSeds*(anzZeits-1)+(1000*(hgsCads-hglCads)/SSalgs) )/real(anzZeits)
	     CuSeds = ( CuSeds*(anzZeits-1)+(1000 * (hgsCus-hglCus)/SSalgs) )/real(anzZeits)
	     NiSeds = ( NiSeds*(anzZeits-1)+(1000 * (hgsNis-hglNis)/SSalgs) )/real(anzZeits)
	     AsSeds = ( AsSeds*(anzZeits-1)+(1000 * (hgsAss-hglAss)/SSalgs) )/real(anzZeits)
	     PbSeds = ( PbSeds*(anzZeits-1)+(1000 * (hgsPbs-hglPbs)/SSalgs) )/real(anzZeits)
	     CrSeds = ( CrSeds*(anzZeits-1)+(1000 * (hgsCrs-hglCrs)/SSalgs) )/real(anzZeits)
	     FeSeds = ( FeSeds*(anzZeits-1)+(1000 * (hgsFes-hglFes)/SSalgs) )/real(anzZeits)
	     HgSeds = ( HgSeds*(anzZeits-1)+(1000 * (hgsHgs-hglHgs)/SSalgs) )/real(anzZeits)
	     MnSeds = ( MnSeds*(anzZeits-1)+(1000 * (hgsMns-hglMns)/SSalgs) )/real(anzZeits)
	     USeds  = (  USeds*(anzZeits-1)+(1000 * (hgsUs-hglUs)  /SSalgs) )/real(anzZeits)
	  endif

      return

      end subroutine Sedimentbelastung