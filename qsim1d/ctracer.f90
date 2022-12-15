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

!> Berechnung der Einleitungen bei Tracerberechnungen

subroutine ctracer(tempw, flag, anze, qeinl, etemp, vabfl, jiein, ilbuhn, nkzs)
                   
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(1000) :: tempw
   integer, intent(in),    dimension(1000) :: flag
   integer, intent(in)                     :: anze
   real,    intent(in),    dimension(100)  :: qeinl
   real,    intent(in),    dimension(100)  :: etemp
   real,    intent(in),    dimension(1000) :: vabfl
   integer, intent(in),    dimension(1000) :: jiein
   integer, intent(in)                     :: ilbuhn
   integer, intent(out),   dimension(1000) :: nkzs 
   
   ! TODO (Schönung) This subroutine must not be allowed to alter 'nkzs'
   
   ! --- local variables
   integer :: iein, j, ji, ior, ior_flag, m, ihcq
   real    :: hctemp, hcq, hcqe, hcte
   
   iein = 1
   do j = 1,anze+1
      nkzs(j) = 1
      ior = j
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. &
          vabfl(ior+1) > 0.0 .and. flag(ior+1) == 4) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (ilbuhn == 1) then
      
      else if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior) < 0.0)m = -1            ! if(vabfl(ior-1)<0.0.and.vabfl(ior)<0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcq = 1
         hctemp = tempw(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior) ! Beginn Einleiterschleife
            hcQE = max(0.0,qeinl(iein))
            
            hcTE = etemp(iein)
            if (hcTE < 0.0)hcTE = 0.0
            tempw(ior) = (abs(hcQ)*hctemp+hcQE*hcTE)/(hcQ+hcQE)
            hcQ = hcQ+qeinl(iein)
            hctemp = tempw(ior)
            iein = iein+1
         enddo ! Ende Einleiterschleife
         
         if (ior_flag == 1) then
            ior = ior-1
            tempw(ior) = tempw(ior+1)
         endif
      endif
   enddo ! Ende Knotenschleife
   return
end
