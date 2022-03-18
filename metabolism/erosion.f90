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

      SUBROUTINE erosion(ss,ssalg,SSeros,dsedH,tausc,M_eros,n_eros,sedroh                              &
                         ,tflie,tiefe,rau,vmitt,anze,mstr,ilang,iwied,kontroll ,jjj)                          
                                                                       
!     UNTERPROGRAMM ZUR Bestimmung der Erosionsrate                     
                                                                       
                                                                       
!     AUTOR:VOLKER KIRCHESCH                                            
!     STAND:31.07.2019                                                    
!     Restrukturierung Jens Wyrwa   Nov.2021                                                          
                                                                       
!     sedroh   -  Rohdichte des Sediments [Kg*m-3] 
!     dReros   -  Erosionsrate in kg/m2/sec                      
                                                                       
      use allodim                                                   
      implicit none
      logical                                 :: kontroll,kontroll_kern
      integer                                 :: anze, jjj, iwied, mstr, ilang, ior
	  real                                    :: tflie
      real, Dimension(ialloc2)                :: tiefe, ss, ssalg, SSeros
      real, Dimension(ialloc2)                :: vmitt, rau 
	  real, Dimension(azStrs,ialloc2)         :: tausc
	  real, Dimension(azStrs,ialloc2)         :: M_eros
	  real, Dimension(azStrs,ialloc2)         :: n_eros
	  real, Dimension(azStrs,ialloc2)         :: sedroh
	  real, Dimension(azStrs,ialloc2)         :: dsedH

      !print*,mstr,' erosion',size(tausc,1),azStrs,size(tausc,2),ialloc2    
!                                                                       
!...Sedimentparameter                                                   
!
      ! M_eros(mstr,:) = 7.5e-4      !! Eingebbar machen!     ###
      ! n_eros(mstr,:) = 3.2         !! Eingebbar machen!     ###
      ! tausc(mstr,:)  = 1.25        !! Eingebbar machen!     ###
      ! sedroh(mstr,:) = 1450.       !! Eingebbar machen!     ###
                                                                      
      if(iwied==0)then
        do ior=1,anze+1
            dsedH(mstr,ior) = 0.0
        enddo
      endif 
       
      do ior = 1,anze+1
        !!wy if(ilang==0)exit
		
		kontroll_kern= .false.
		if(kontroll .and. ior==jjj)then
		   kontroll_kern= .true.
		   !print*,"vor erosion_kern ss(",jjj,"),ssalg=",ss(jjj),ssalg(jjj)
		endif
        
		call erosion_kern(tflie,TIEFE(ior),RAU(ior),VMITT(ior)  &
		                 ,SSeros(ior),ss(ior),ssalg(ior),dsedH(mstr,ior)  &
						 ,tausc(mstr,ior),M_eros(mstr,ior),n_eros(mstr,ior),sedroh(mstr,ior)  &
		                 ,kontroll_kern,jjj)
		
		if(kontroll_kern)then
		   !print*,"nach erosion_kern ss(",jjj,"),ssalg=",ss(jjj),ssalg(jjj)
		endif
      enddo ! do ior
	
      !if(kontroll)print*,mstr,jjj,' erosion computing tausc(jjj,1),SSeros,SS,SSalg=',  &
	  !                   tausc(jjj,1), SSeros(jjj), SS(jjj), SSalg(jjj)
                                                                       
  END subroutine erosion                                           

