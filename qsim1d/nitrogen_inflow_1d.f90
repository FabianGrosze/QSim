subroutine nitrogen_inflow_1d(vnh4, vno2, vno3, gesN, vx0, vx02, nl0, Q_NK,    &
                              Q_NG, Q_NB, hFluN3, mstr, eNH4L, eNO2L, eNO3L,   &
                              gesNL, eNH4, eNO2, eNO3, ex0, ex02, eGesN, enl0, &
                              ieinLs, qeinlL, qeinl, vabfl, iorLe, iorLa,      &
                              jiein, flae, anze, flag, tflie)
   
   use allodim
   use aparam, only: qmx_NK, qmx_NG, qmx_NB
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)        :: vnh4   !< ammonium
   real,    intent(inout), dimension(ialloc2)        :: vno2   !< nitrite 
   real,    intent(inout), dimension(ialloc2)        :: vno3   !< nitrate
   real,    intent(inout), dimension(ialloc2)        :: gesN   !< total nitrogen
   real,    intent(inout), dimension(ialloc2)        :: vx0    !< nitrosomonas
   real,    intent(inout), dimension(ialloc2)        :: vx02   !< nitrobacter
   real,    intent(inout), dimension(ialloc2)        :: nl0    !< ratio N:C in organic material 
   real,    intent(inout), dimension(ialloc2)        :: Q_NK   !< Stickstoffanteil in Kieselangen
   real,    intent(inout), dimension(ialloc2)        :: Q_NG   !< Stickstoffanteil in Grünalgen
   real,    intent(inout), dimension(ialloc2)        :: Q_NB   !< Stickstoffanteil in Blaualgen
   real,    intent(inout), dimension(azStrs,ialloc2) :: hFluN3 !<
   integer, intent(in)                               :: mstr   !< current stretch 
   real,    intent(inout), dimension(ialloc1)        :: eNH4L  !< inflow ammonium (diffuse source)
   real,    intent(inout), dimension(ialloc1)        :: eNO2L  !< inflow nitrite (diffuse source)
   real,    intent(inout), dimension(ialloc1)        :: eNO3L  !< inflow nitrate (diffuse source)
   real,    intent(inout), dimension(ialloc1)        :: gesNL  !< inflow total nitrogen (diffuse source)
   real,    intent(in),    dimension(ialloc1)        :: eNH4   !< inflow ammonium (point source)
   real,    intent(in),    dimension(ialloc1)        :: eNO2   !< inflow nitrite (point source)
   real,    intent(in),    dimension(ialloc1)        :: eNO3   !< inflow nitrate (point source)
   real,    intent(in),    dimension(ialloc1)        :: ex0    !< inflow nitrosomonas (point source)
   real,    intent(in),    dimension(ialloc1)        :: ex02   !< inflow nitrobacter (point source)
   real,    intent(in),    dimension(ialloc1)        :: eGesN  !< inflow total nitrogen (point source)
   real,    intent(in),    dimension(ialloc1)        :: enl0   !<
   integer, intent(in),    dimension(azStrs)         :: ieinLs !< number of diffuse sources per stretch
   real,    intent(in),    dimension(ialloc1)        :: qeinlL !<
   real,    intent(in),    dimension(ialloc1)        :: qeinl  !<
   real,    intent(in),    dimension(ialloc2)        :: vabfl  !<
   integer, intent(in),    dimension(ialloc1)        :: iorLe  !< 
   integer, intent(in),    dimension(ialloc1)        :: iorLa  !<
   integer, intent(in),    dimension(ialloc2)        :: jiein  !< Anzahl der Einleiter je Knoten
   real,    intent(in),    dimension(ialloc2)        :: flae   !< Oberfläche
   integer, intent(in)                               :: anze   !< Anzahl Abschnitte im aktuellen Strang
   integer, intent(in),    dimension(ialloc2)        :: flag   !<
   real,    intent(in)                               :: tflie  !< Zeitschritt [d]
   
   
   ! --- local variables ---
   integer  :: ieinL, ior, j, ji, ior_flag, m, ihcQ, iein
   real     :: hcvnh41, hcvno21, hcvno31, hcvx01, hcvx021, hcnl01, hcgesN1 
   real     :: hcvnh4, hcvno2, hcvno3, hcvx0, hcvx02, hcnl0, hcgesN, hcFluN3
   real     :: hcQE, hcNH4E, hcNO2E, hcNO3E, hcx0E, hcx02E, hcgesNE, hcnl0E
   real     :: hcq   
   
   ! TODO FG: removed initialisation with 0 of the following variables and
   !          added them to save-variables in analogy to other routines
   save hcvnh41, hcvno21, hcvno31, hcvx01, hcvx021, hcnl01
   ! TODO FG: added variable as save-variables in analogy to other routines
   save hcgesN1
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior) cycle
         
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) then
               eNH4L(ieinL) = 0.0
               eNO2L(ieinL) = 0.0
               eNO3L(ieinL) = 0.0
               gesNL(ieinL) = 0.0
            endif
            
            if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            
               if (qeinlL(ieinL)>=0.0 .and. eNH4L(ieinL) == -1) then
               else
                  vNH4(ior) = vNH4(ior)+((eNH4L(ieinL)-vNH4(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif
               
               if (qeinlL(ieinL)>=0.0 .and. eNO2L(ieinL) == -1) then
               else
                  if (vNO2(ior)>=0.0) vNO2(ior) = vNO2(ior) + ((eNO2L(ieinL)-vNO2(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif

               if (qeinlL(ieinL)>=0.0 .and. eNO3L(ieinL) == -1) then
               else
                  vNO3(ior) = vNO3(ior)+((eNO3L(ieinL)-vNO3(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif

               if (qeinlL(ieinL)>=0.0 .and. gesNL(ieinL) == -1) then
               else
                  if (gesN(ior)>=0.0)                    &
                      gesN(ior) = gesN(ior)+((gesNL(ieinL)-gesN(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
               endif
        
            endif
         endif
      enddo
   enddo
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1
      
      ior = j
      if (vabfl(ior) >= 0.0 .and. vabfl(ior+1) < 0.0) then
         hcvnh41 = vnh4(ior)
         hcvno21 = vno2(ior)
         hcvno31 = vno3(ior)
         hcvx01  = vx0(ior)
         hcvx021 = vx02(ior)
         hcnl01  = nl0(ior)
         if (gesN(ior) > 0.0) hcgesN1 = gesN(ior)
      endif
      
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
         
         ! Umbenennen der benötigten Variablen
         hcvnh4 = vnh4(ior-m)        
         hcvno2 = vno2(ior-m)
         hcvno3 = vno3(ior-m)
         hcvx0  = vx0(ior-m)
         hcvx02 = vx02(ior-m)
         hcnl0  = nl0(ior-m)
         hcgesN = gesN(ior-m)
         hcFluN3 = hFluN3(mstr,ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         if (ihcQ == 1) then
            hcvnh4 = hcvnh41
            hcvno2 = hcvno21
            hcvno3 = hcvno31
            hcvx0  = hcvx01
            hcvx02 = hcvx021
            hcnl0  = hcnl01
            hcgesN = hcgesN1
         endif
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcNH4E  = eNH4(iein)
            hcNO2E  = eNO2(iein)
            hcNO3E  = eNO3(iein)
            hcx0E   = ex0(iein)
            hcx02E  = ex02(iein)
            hcgesNE = egesN(iein)
            hcnl0E  = enl0(iein)
            if (hcNH4E  < 0.0) hcNH4E  = hcvNH4
            if (hcNO2E  < 0.0) hcNO2E  = hcvNO2
            if (hcNO3E  < 0.0) hcNO3E  = hcvNO3
            if (hcx0E   < 0.0) hcx0E   = hcvx0
            if (hcx02E  < 0.0) hcx02E  = hcvx02
            if (hcgesNE < 0.0) hcgesNE = hcgesN
            if (hcnl0E  < 0.0) hcnl0E  = hcnl0
            
            vNH4(ior) = (hcQ*hcvNH4 + hcQE*hcNH4E) / (hcQ+hcQE)
            vNO2(ior) = (hcQ*hcvNO2 + hcQE*hcNO2E) / (hcQ+hcQE)
            vNO3(ior) = (hcQ*hcvNO3 + hcQE*hcNO3E) / (hcQ+hcQE)
            vx0(ior)  = (hcQ*hcvx0  + hcQE*hcx0E)  / (hcQ+hcQE)
            vx02(ior) = (hcQ*hcvx02 + hcQE*hcx02E) / (hcQ+hcQE)
            if (gesN(ior) > 0.0) then
               gesN(ior) = (hcQ*hcgesN + hcQE*hcgesNE) / (hcQ+hcQE)
            endif
            nL0(ior)  = (hcQ*hcnl0       + hcQE*hcnl0E) / (hcQ+hcQE)
            Q_NK(ior) = (hcQ*Q_NK(ior-m) + hcQE*Qmx_NK) / (hcQ+hcQE)
            Q_NG(ior) = (hcQ*Q_NG(ior-m) + hcQE*Qmx_NG) / (hcQ+hcQE)
            Q_NB(ior) = (hcQ*Q_NB(ior-m) + hcQE*Qmx_NB) / (hcQ+hcQE)
            hFluN3(mstr,ior) = hcFluN3
            
            hcQ    = hcQ + qeinl(iein)
            iein   = iein+1
            hcvnh4 = vnh4(ior)
            hcvno2 = vno2(ior)
            hcvno3 = vno3(ior)
            hcvx0  = vx0(ior)
            hcvx02 = vx02(ior)
            hcnl0  = nl0(ior)
            hcgesN = gesN(ior)
            
         enddo 
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            vNH4(ior) = vNH4(ior+1)
            vNO2(ior) = vNO2(ior+1)
            vNO3(ior) = vNO3(ior+1)
            vx0(ior)  = vx0(ior+1)
            vx02(ior) = vx02(ior+1)
            if (gesN(ior+1) > 0.0) gesN(ior) = gesN(ior+1)
            nL0(ior)  = nL0(ior+1)
            Q_NK(ior) = Q_NK(ior+1)
            Q_NG(ior) = Q_NG(ior+1)
            Q_NB(ior) = Q_NB(ior+1)
         endif
      endif ! Ende Einleitungs-flag
   enddo

end subroutine nitrogen_inflow_1d