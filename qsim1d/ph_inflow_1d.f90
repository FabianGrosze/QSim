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

!> Mischung der Einleiter und Hauptfluss für pH-Wert, m-Wert, Calcium und 
!! Leitfähigkeit
!! @author Volker Kirchesch
!! @date 11.06.1993

subroutine ph_inflow_1d(vph, lf, ca, mw, pw, elfL, caL, eph, elf, eca, emw,  &
                        tempw, mstr, ieinLs, qeinlL, qeinl, vabfl, iorLe,    &
                        iorLa, jiein, flae, anze, flag, tflie,               &
                        control, jjj)
   use module_alloc_dimensions
   use module_ph, only: pwert
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: vph      !< pH [-]
   real,    intent(inout), dimension(ialloc2) :: lf       !< Leitfähigkeit [µS/cm]
   real,    intent(inout), dimension(ialloc2) :: ca       !< Calcium [mg/l]
   real,    intent(inout), dimension(ialloc2) :: mw       !< m-Wert [mmol/l]
   real,    intent(inout), dimension(ialloc2) :: pw       !< p-Wert [mmol/l]
   real,    intent(inout), dimension(ialloc1) :: elfL     !< Leitfähigkeit in Linienquelle [µS/cm]
   real,    intent(inout), dimension(ialloc1) :: caL      !< Calcium in Linienquelle [mg/l]
   real,    intent(in),    dimension(ialloc1) :: eph      !< pH im Einleiter [-]
   real,    intent(in),    dimension(ialloc1) :: elf      !< Leitfähigkeit im Einleiter [µS/cm]
   real,    intent(in),    dimension(ialloc1) :: eca      !< Calcium im Einleiter [mg/l]
   real,    intent(in),    dimension(ialloc1) :: emw      !< m-Wert im Einleiter [mmol/l]
   real,    intent(in),    dimension(ialloc2) :: tempw    !< Wassertemperatur [°C]
   integer, intent(in)                        :: mstr     !< aktueller Strang
   integer, intent(in),    dimension(azStrs)  :: ieinLs   !< Anzahl der Linienquellen je Strang
   real,    intent(in),    dimension(ialloc1) :: qeinlL   !< 
   real,    intent(in),    dimension(ialloc1) :: qeinl    !< 
   real,    intent(in),    dimension(ialloc2) :: vabfl    !< 
   integer, intent(in),    dimension(ialloc1) :: iorLe    !<
   integer, intent(in),    dimension(ialloc1) :: iorLa    !<
   integer, intent(in),    dimension(ialloc2) :: jiein    !< 
   real,    intent(in),    dimension(ialloc2) :: flae     !<
   integer, intent(in)                        :: anze     !< 
   integer, intent(in),    dimension(ialloc2) :: flag     !<
   real,    intent(in)                        :: tflie    !< Zeitschritt [d]
   logical, intent(in)                        :: control !< debugging
   integer, intent(in)                        :: jjj      !< debugging
   
   ! --- local variables ---
   integer     :: iein, ieinL, ior, j, ior_flag, m, ihcQ, ji
   real        :: hcmw, hcca, hcvph, hclf
   real        :: mue, hk, lgh, hcvh, hcQ, hchE
   real        :: hcQE, hcphE, hclfE, hcmwE, hccaE, vhneu
   
   
   
   !Crot = 0.45
   !CDR = 0.38
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior) cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL) >= ior) then
            if (qeinlL(ieinL) <= 0.0) then
               elfL(ieinL) = 0.0
               caL(ieinL)  = 0.0
            endif
            
            if (elfL(ieinL) >= 0.0) then
               Lf(ior) = Lf(ior)+((elfL(ieinL)-Lf(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            endif
            
            if (CaL(ieinL) >= 0.0) then
               Ca(ior) = Ca(ior)+((CaL(ieinL)-Ca(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            endif
         endif
      enddo ! Ende Knotenschleife
   enddo   ! Ende Schleife Linienquellen
   
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1  ! Beginn Schleife Ortspunkte
      ior = j
      
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior + 1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         ! Umbenennen der benötigten Variablen; 1D
         hcmw  = mw(ior-m)
         hcca  = ca(ior-m)
         hcvph = vph(ior-m)
         hclf  = lf(ior-m)
         
         mue = 1.7e-5 * hclf
         hk = (0.5 * sqrt(mue)) / (1. + 1.4 * sqrt(mue))
         lgh = hcvph - hk
         hcvh = 10**(-lgh)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            
            hcphE = eph(iein)
            if (hcphE < 0.0) hcphE = hcvph
            
            hclfE = elf(iein)
            if (hclfE < 0.0) hclfE = hclf
            mue = 1.7e-5 * hclfE
            hk = (0.5 * sqrt(mue)) / (1. + 1.4*sqrt(mue))
            lgh = hcphE - hk
            hchE = 10**(-lgh)
            
            hcmwE = emw(iein)
            if (hcmwE < 0.0) hcmwE = hcmw
            
            hccaE = eca(iein)
            if (hccaE < 0.0) hccaE = hcca
            
            vhneu   = (hcQ * hcvh + hcQE * hchE)  / (hcQ + hcQE)
            mw(ior) = (hcQ * hcmw + hcQE * hcmwE) / (hcQ + hcQE)
            lf(ior) = (hcQ * hclf + hcQE * hclfE) / (hcQ + hcQE)
            ca(ior) = (hcQ * hcca + hcQE * hccaE) / (hcQ + hcQE)
            
            mue = 1.7e-5 * lf(ior)
            hk = (0.5 * sqrt(mue)) / (1. + 1.4*sqrt(mue))
            vph(ior) = (-1. * log10(vhneu)) + hk
            
            hcQ = hcQ + qeinl(iein)
            iein = iein + 1
            hcmw = mw(ior)
            hcca = ca(ior)
            hcvph = vph(ior)
            hclf = lf(ior)
            call pwert(mw(ior),vph(ior),lf(ior),tempw(ior),pw(ior))
         enddo ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            vph(ior) = vph(ior+1)
            mw(ior)  = mw(ior+1)
            lf(ior)  = lf(ior+1)
            ca(ior)  = ca(ior+1)
         endif
      endif ! Ende Einleitungs-flag
   enddo
end subroutine ph_inflow_1d