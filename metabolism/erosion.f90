! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> Bestimmung der Erosionsrate
!! @author Volker Kirchesch Jens Wyrwa
!! @date 01.11.2021
subroutine erosion(ss, ssalg, SSeros, dsedH, tausc, M_eros, n_eros, sedroh,   &
                   tflie, tiefe, rau, vmitt, anze, mstr, iwied,               &
                   control, jjj)
   use module_alloc_dimensions
   implicit none
   
   logical                                 :: control,kontroll_kern
   integer                                 :: anze, jjj, iwied, mstr, ior
   real                                    :: tflie
   real, dimension(ialloc2)                :: tiefe, ss, ssalg, SSeros
   real, dimension(ialloc2)                :: vmitt, rau
   real, dimension(azStrs,ialloc2)         :: tausc
   real, dimension(azStrs,ialloc2)         :: M_eros
   real, dimension(azStrs,ialloc2)         :: n_eros
   real, dimension(azStrs,ialloc2)         :: sedroh !< Rohdichte des Sediments [Kg*m-3]
   real, dimension(azStrs,ialloc2)         :: dsedH
   
   external :: erosion_kern
   
  
   !print*,mstr,' erosion',size(tausc,1),azStrs,size(tausc,2),ialloc2
   
   ! Sedimentparameter
   ! M_eros(mstr,:) = 7.5e-4      !! Eingebbar machen!     ###
   ! n_eros(mstr,:) = 3.2         !! Eingebbar machen!     ###
   ! tausc(mstr,:)  = 1.25        !! Eingebbar machen!     ###
   ! sedroh(mstr,:) = 1450.       !! Eingebbar machen!     ###
   
   if (iwied == 0) then
      do ior = 1,anze+1
         dsedH(mstr,ior) = 0.0
      enddo
   endif
   
   do ior = 1,anze+1
      kontroll_kern = .false.
      if (control .and. ior == jjj) then
         kontroll_kern = .true.
         !print*,"vor erosion_kern ss(",jjj,"),ssalg=",ss(jjj),ssalg(jjj)
      endif
      
      call erosion_kern(tflie,TIEFE(ior),RAU(ior),VMITT(ior)  &
                        ,SSeros(ior),ss(ior),ssalg(ior),dsedH(mstr,ior)  &
                        ,tausc(mstr,ior),M_eros(mstr,ior),n_eros(mstr,ior),sedroh(mstr,ior)  &
                        ,kontroll_kern,jjj)
      
      if (kontroll_kern) then
         !print*,"nach erosion_kern ss(",jjj,"),ssalg=",ss(jjj),ssalg(jjj)
      endif
   enddo
   
   if(control) then 
      print*, mstr,jjj,' erosion computing tausc(jjj,1),SSeros,SS,SSalg=',  &
              tausc(jjj,1), SSeros(jjj), SS(jjj), SSalg(jjj)
   endif
   
end subroutine erosion
