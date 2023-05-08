subroutine oxygen_inflow_1d(vo2, vo2z, o2L, eo2, etemp, tempwz, mstr, nkzs,   &
                            dh2d, ieinLs, qeinlL, qeinl, vabfl, iorLe, iorLa, &
                            jiein, flae, anze, flag, tflie)
                          
   use aparam, only: Caki, Cagr, Cabl
   use allodim
   
   implicit none
   
   real,    intent(inout), dimension(ialloc2)         :: vo2      !< Sauerstoff im Hauptfluss
   real,    intent(inout), dimension(ialloc5,ialloc2) :: vo2z     !< Sauerstoff im Hauptfluss
   real,    intent(in),    dimension(ialloc1)         :: o2L      !< Sauerstoff im Linieneinleiter
   real,    intent(in),    dimension(ialloc1)         :: eo2      !< Sauerstoff im Punkteinleiter
   real,    intent(in),    dimension(ialloc1)         :: etemp    !< Temperatur im Punkteinleiter
   real,    intent(in),    dimension(ialloc5,ialloc2) :: tempwz   !< Wassertemperatur
   integer, intent(in)                                :: mstr     !< aktuelle Strangnumme
   integer, intent(in),    dimension(ialloc2)         :: nkzs     !< Anzahl Tiefenschichten am Knoten
   real,    intent(in)                                :: dh2d     !< Dicke einer Tiefenschicht
   integer, intent(in),    dimension(azStrs)          :: ieinLs   !<
   real,    intent(inout), dimension(ialloc1)         :: qeinlL   ! TODO (schoenung): should be intent(in) only
   real,    intent(in),    dimension(ialloc1)         :: qeinl    !< 
   real,    intent(in),    dimension(ialloc2)         :: vabfl    !<
   integer, intent(in),    dimension(ialloc1)         :: iorLe    !<
   integer, intent(in),    dimension(ialloc1)         :: iorLa    !<
   integer, intent(in),    dimension(ialloc2)         :: jiein    !<
   real,    intent(in),    dimension(ialloc2)         :: flae     !<
   integer, intent(in)                                :: anze     !<
   integer, intent(in),    dimension(ialloc2)         :: flag     !<
   real,    intent(in)                                :: tflie    !< Zeitschritt [d]
   
   ! --- local variables ---
   integer                           :: iein, ieinL, ior, j, ior_flag, m, ihcQ, ji, nkz, vo2vor
   real                              :: hcvo2, hco2E, hcq, hcQE, rohe, hcte
   real, dimension(ialloc5, ialloc2) :: hcvo2z
   real, dimension(ialloc5)          :: hco2Ez, d, cpart
   
   ! functions
   real                              :: density_1d
   external                          :: dichte, einleiter_misch
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL)>=0.0 .and. o2L(ieinL) == -1.) cycle

      do ior = 1,anze+1
         if (iorLe(ieinL) < ior) cycle
         
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) qeinlL(ieinL) = 0.0
            
            ! depth resolved
            do nkz = 1,nkzs(ior)
               vo2z(nkz,ior) = vo2z(nkz,ior)+((o2L(ieinL)-vo2z(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            enddo
            
            ! depth avaraged
            vo2(ior) = vo2(ior)+((o2L(ieinL)-vo2(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
         endif
      enddo
   enddo
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1, anze+1
      ior = j
      
      ior_flag = 0
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hcvO2 = vo2(ior-m)     ! Umbenennen der benötigten Variablen; 1D
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do nkz = 1,nkzs(ior)
            hcvo2z(nkz,ior) = vo2z(nkz,ior-m)
         enddo
      
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hco2E = eo2(iein)
            if (hco2E < 0.0) then
               ! 1D
               hco2E = hcvo2
               
               ! 2D
               do nkz = 1, nkzs(ior)
                  hco2Ez(nkz) = hcvo2z(nkz,ior)
               enddo
            else
               do nkz = 1,nkzs(ior)
                  hco2Ez(nkz) = hco2E
               enddo
            endif
            
            if (etemp(iein) > -9.9) then
               hcTE = etemp(iein)
               ! Dichte im Einleiter
               rohE = density_1d(hcTE)
               
               ! Dichte im Vorfluter
               call dichte(tempwz, nkzs, ior, D)
               
               ! Berechnung der vertikalen Einmischung
               call einleiter_misch(nkzs,ior,hcvo2z,Cpart,hcQ,hcQE,hco2E,rohE,D,dH2D)
               vo2z(1:nkzs(ior),ior) = Cpart(1:nkzs(ior))
            else
               do nkz = 1,nkzs(ior)      ! 2D
                  vo2z(nkz,ior) = (hcQ*hcvo2z(nkz,ior)+hcQE*hco2Ez(nkz))/(hcQ+hcQE)
               enddo
            endif
            
            vo2vor = vo2(ior)
            
            vo2(ior) = (hcQ * hcvo2 + hcQE * hco2E) / (hcQ + hcQE)
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcvo2 = vo2(ior)
            
            do nkz = 1,nkzs(ior)
               hcvo2z(nkz,ior) = vo2z(nkz,ior)
            enddo
         enddo ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            vo2(ior) = vo2(ior+1)
            
            do nkz = 1,nkzs(ior)
               vo2z(nkz,ior) = vo2z(nkz,ior+1)
            enddo
         endif
         
      endif   ! Ende Einleitungs-flag
   enddo

end subroutine oxygen_inflow_1d