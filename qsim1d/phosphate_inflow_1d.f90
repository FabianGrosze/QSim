subroutine phosphate_inflow_1d(gelp, gesP, pl0, Q_PK, Q_PG, Q_PB, hgesPz,  &
                               gelPz, gPL, gesPL, egesP, eP, epl0, mstr,   &
                               ieinLs, qeinlL, qeinl, vabfl, iorLa, iorLe, &
                               jiein, flae, anze, nkzs, flag, tflie)
   use allodim
   use aparam, only : Qmx_PK, Qmx_PG, Qmx_PB
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)               :: gelp   !< gelöster Phosphor
   real,    intent(inout), dimension(ialloc2)               :: gesP   !< Gesamtphosphor
   real,    intent(inout), dimension(ialloc2)               :: pl0    !< P:C-Verhältnis   
   real,    intent(inout), dimension(ialloc2)               :: Q_PK   !< Phosphor in Kieselalgen
   real,    intent(inout), dimension(ialloc2)               :: Q_PG   !< Phosphor in Grünalgen
   real,    intent(inout), dimension(ialloc2)               :: Q_PB   !< Phosphor in Blaualgen
   real,                   dimension(azStrs,ialloc5,ialloc2):: hgesPz !< 
   real,    intent(inout), dimension(ialloc5,ialloc2)       :: gelPz  !< gelöster Phosphor tiefenaufgelöst
   real,    intent(inout), dimension(ialloc1)               :: gPL    !< gelöster Phosphor in Linienquelle  
   real,    intent(inout), dimension(ialloc1)               :: gesPL  !< Gesamtphosphor in Lininenquelle
   real,    intent(in),    dimension(ialloc1)               :: egesP  !< Gesamtphosphor im Einleiter
   real,    intent(in),    dimension(ialloc1)               :: eP     !< 
   real,    intent(in),    dimension(ialloc5)               :: epl0   !< P:C-Verhältnis im Einleiter
   integer, intent(in)                                      :: mstr   !< akuteller Strang
   integer, intent(in),    dimension(azStrs)                :: ieinLs !< Anzahl der Linienquellen je Strang
   real,    intent(in),    dimension(ialloc1)               :: qeinlL !<
   real,    intent(in),    dimension(ialloc5)               :: qeinl  !<
   real,    intent(inout), dimension(ialloc2)               :: vabfl  !<
   integer,                dimension(ialloc1)               :: iorLa  !<
   integer,                dimension(ialloc1)               :: iorLe  !<
   integer, intent(in),    dimension(ialloc2)               :: jiein  !<
   real,    intent(in),    dimension(ialloc2)               :: flae   !< Oberfläche
   integer, intent(in)                                      :: anze   !< Anzahl der Abschnitte im aktuellen Strang
   integer,                dimension(ialloc2)               :: nkzs   !< Anzahl der Tiefenschichten
   integer, intent(in),    dimension(ialloc2)               :: flag   !<
   real,    intent(in)                                      :: tflie  !< Zeitschritt [d]
   
   ! --- local variables ---
   integer             :: ieinL, ior, nkz, iein, j, ior_flag, m, ihcQ, ji
   integer             :: nkzs_alt, nkzs_neu
   real                :: hcgelP, hcgesP, hcp10, hcQ
   real                :: hcQE, hcgelpE, hcgespE, hcpl0E , hcpl0
   real, dimension(50) :: gelPz_neu, gesPz_neu, hcgelPz
   real, dimension(50) :: hcgelpEz, hcgespEz, hcgesPz
   
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) then
               gPL(ieinL) = 0.0
               gesPL(ieinL) = 0.0
            endif
            
            ! --- 2D ---
            do nkz = 1,nkzs(ior)
               if (qeinlL(ieinL)>=0.0 .and. gPL(ieinL) == -1) then
               else
                  gelPz(nkz,ior) = gelPz(nkz,ior)+((gPL(ieinL)-gelPz(nkz,ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif
            enddo
            
            ! --- 1D ---
            if (qeinlL(ieinL)>=0.0 .and. gPL(ieinL) == -1) then
            else
               gelP(ior) = gelP(ior)+((gPL(ieinL)-gelP(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            endif
            
            if (qeinlL(ieinL)>=0.0 .and. gesPL(ieinL) == -1) then
            else
               gesP(ior) = gesP(ior)+((gesPL(ieinL)-gesP(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            endif
         endif
      enddo
   enddo
   
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1                ! Schleife longitudinale Gitterpunkte
      ior = j
      ior_flag = 0
      
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         hcgelP = gelP(ior-m)
         hcgesP = gesP(ior-m)
         hcpl0  = pl0(ior-m)
         hcQ    = vabfl(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         nkzs_alt = nkzs(ior-m)
         nkzs_neu = nkzs(ior)
         
         do nkz = 1, nkzs_alt
            gelPz_neu(nkz) = gelPz(nkz,ior-m)
            if (gesP(ior) > 0.0) gesPz_neu(nkz) = hgesPz(mstr,nkz,ior-m)
         enddo
         
         ! call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,gelPz_neu,hcgelPz)
         ! if(gesP(ior)>0.0)call z_gitter_einl(dh2D,ior,nkzs_alt,nkzs_neu,gesPz_neu,hcgesPz)
         do nkz = 1,nkzs_neu
            hcgelPz(nkz) = gelPz_neu(nkz)
            if (gesP(ior) > 0.0) hcgesPz(nkz) = gesPz_neu(nkz)
         enddo
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcgelpE = eP(iein)
            hcgespE = egesP(iein)
            hcpl0E = epl0(iein)
            
            if (hcpl0E  < 0.0) hcpl0E  = hcpl0
            if (hcgelpE < 0.0) hcgelpE = hcgelp
            if (hcgespE < 0.0) hcgespE = hcgesp
            do nkz = 1, nkzs(ior)
               hcgelpEz(nkz) = eP(iein)
               if (hcgelpEz(nkz) < 0.0)hcgelpEz(nkz) = hcgelpz(nkz)
               hcgespEz(nkz) = egesP(iein)
               if (hcgespEz(nkz) < 0.0)hcgespEz(nkz) = hcgespz(nkz)
            enddo
            do nkz = 1,nkzs(ior)      ! 2D
               gelpz(nkz,ior) = (hcQ*hcgelpz(nkz)+hcQE*hcgelpEz(nkz))/(hcQ+hcQE)
               if (gesP(ior) > 0.0)hgesPz(mstr,nkz,ior) = (hcQ*hcgespz(nkz)+hcQE*hcgespEz(nkz))/(hcQ+hcQE)
            enddo
            
            gelp(ior) = (hcQ * hcgelp      + hcQE * hcgelpE) / (hcQ+hcQE)
            pl0(ior)  = (hcQ * hcpl0       + hcQE * hcpl0E)  / (hcQ+hcQE)
            Q_PK(ior) = (hcQ * Q_PK(ior-m) + hcQE * Qmx_PK)  / (hcQ+hcQE)
            Q_PG(ior) = (hcQ * Q_PG(ior-m) + hcQE * Qmx_PG)  / (hcQ+hcQE)
            Q_PB(ior) = (hcQ * Q_PB(ior-m) + hcQE * Qmx_PB)  / (hcQ+hcQE)
            if (gesP(ior) > 0.0) then
               gesP(ior) = (hcQ*hcgesP+hcQE*hcgespE)/(hcQ+hcQE)
            endif
            hcQ = hcQ + qeinl(iein)
            iein = iein+1
            hcgelP = gelP(ior)
            hcgesP = gesP(ior)
            hcpl0 = pl0(ior)
            
            do nkz = 1,nkzs(ior)
               hcgelpz(nkz) = gelPz(nkz,ior)
               hcgesPz(nkz) = hgesPz(mstr,nkz,ior)
            enddo
         enddo
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            gelp(ior) = gelp(ior+1)
            if (gesP(ior+1)>=0.0)gesP(ior) = gesP(ior+1)
            pl0(ior) = pl0(ior+1)
            Q_PK(ior) = Q_PK(ior+1)
            Q_PG(ior) = Q_PG(ior+1)
            Q_PB(ior) = Q_PB(ior+1)
            
            do nkz = 1,nkzs(ior)
               gelPz(nkz,ior) = gelPz(nkz,ior+1)
               if (gesP(ior) > 0.0)hgesPz(mstr,nkz,ior) = hgesPz(mstr,nkz,ior+1)
            enddo
         endif
      
      endif
   enddo
   
end subroutine phosphate_inflow_1d