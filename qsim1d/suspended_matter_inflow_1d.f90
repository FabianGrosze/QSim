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

!> Berechnung der Schwebstoffkonzentration
!! @author Volker Kirchesch
!! @date 22.09.2011
subroutine suspended_matter_inflow_1d(ss, fssgr, vkigr, antbl, akbcm, agbcm, abbcm, &
                                      fssgrs, ssl, ess, echla, ezind, mstr, ieinls, &
                                      qeinll, qeinl, vabfl, iorle, iorla, jiein,    &
                                      flae, anze, flag, tflie)
   
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: ss      !< suspended matter in river
   real,    intent(inout), dimension(ialloc2) :: fssgr   !<
   real,    intent(in),    dimension(ialloc2) :: vkigr   !<
   real,    intent(in),    dimension(ialloc2) :: antbl   !<
   real,    intent(in),    dimension(ialloc2) :: akbcm   !<
   real,    intent(in),    dimension(ialloc2) :: agbcm   !<
   real,    intent(in),    dimension(ialloc2) :: abbcm   !<
   real,    intent(in)                        :: fssgrs  !< 
   real,    intent(inout), dimension(ialloc1) :: ssl     !< TODO (Schoenung): should be intent(in) only
   real,    intent(in),    dimension(ialloc1) :: ess     !<
   real,    intent(in),    dimension(ialloc1) :: echla   !<
   real,    intent(in),    dimension(ialloc1) :: ezind   !<
   integer, intent(in)                        :: mstr    !<
   integer, intent(in),    dimension(azstrs)  :: ieinls  !<
   real,    intent(inout), dimension(ialloc1) :: qeinll  ! TODO (Schoenung): should be intent(in) only
   real,    intent(in),    dimension(ialloc1) :: qeinl   !< 
   real,    intent(in),    dimension(ialloc2) :: vabfl   !<
   integer, intent(in),    dimension(ialloc1) :: iorle   !<
   integer, intent(in),    dimension(ialloc1) :: iorla   !<
   integer, intent(in),    dimension(ialloc2) :: jiein   !<
   real,    intent(in),    dimension(ialloc2) :: flae    !<
   integer, intent(in)                        :: anze    !<
   integer, intent(in),    dimension(ialloc2) :: flag    !<
   real,    intent(in)                        :: tflie   !< timestep [d]

   ! --- local variables ---
   integer  :: iein,ieinL,ior, j, ior_flag, m, ihcq, ji
   real     :: hcfssg, hcq, hcss, hcsse, hcqe
   real     :: akie, able, agre, hcfssge

   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinl = 1, ieinls(mstr)
      if (qeinll(ieinl)>=0.0 .and. ssl(ieinl) == -1.)cycle
      do ior = 1,anze+1
         if (iorle(ieinl) < ior)cycle
         if (iorla(ieinl) <= ior .and. iorle(ieinl)>=ior) then
            if (qeinll(ieinl) <= 0.0) ssl(ieinl) = 0.0
            ss(ior) = ss(ior) + ((ssl(ieinl)-ss(ior))*qeinll(ieinl)/flae(ior))*tflie*86400.
         endif
      enddo 
   enddo
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1                   
      ior = j
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0.and.vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcq = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcq = 1 ! konzentration an der einleitstelle
         ! ist gleich der konzentration der einleitung
         
         hcss = ss(ior-m)       ! umbenennen der benötigten variablen
         hcfssg = fssgr(ior-m)
         hcq = vabfl(ior-m)
         if (hcq < 0.0)hcq = abs(hcq)
         if (hcq == 0.0 .or. ihcq == 1)hcq = 1.e-10
         
         do ji = 1,jiein(ior)   ! beginn einleitungsschleife
            hcqe = max(0.0,qeinl(iein))
            hcsse = ess(iein)
            if (hcsse < 0.0) then
               hcsse = hcss
            else
               if (echla(iein)>=0.0) then
                  ! todo (schönung, june 2023)
                  ! this means algae in source are distributed the same as main river
                  akie = (echla(iein) * vkigr(ior-1)/1000.) * (akbcm(ior-1)/caki)
                  agre = (echla(iein) * (1.-vkigr(ior-1) - antbl(ior-1))/1000.) * (agbcm(ior-1)/cagr)
                  able = (echla(iein) * antbl(ior-1)/1000.) * (abbcm(ior-1)/cabl)
                  hcsse = hcsse - (akie + agre + able)
               endif
               if (ezind(iein)>=0.0) hcsse = hcsse - (ezind(iein)*grot/1000.)
            endif
            hcfssge = fssgrs
            ss(ior) = (hcq*hcss+hcqe*hcsse)/(hcq+hcqe)
            fssgr(ior) = (hcq*hcfssg+hcqe*hcfssge)/(hcq+hcqe)
            
            hcq = hcq+qeinl(iein)
            iein = iein+1
            hcss = ss(ior)
            hcfssg = fssgr(ior)
         enddo ! ende einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            ss(ior) = ss(ior+1)
            fssgr(ior) = fssgr(ior+1)
         endif
      endif
   
   enddo 
   
   
end subroutine suspended_matter_inflow_1d
