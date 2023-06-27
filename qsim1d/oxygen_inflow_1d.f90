subroutine oxygen_inflow_1d(vo2, o2L, eo2, etemp, mstr, ieinLs, qeinlL, qeinl, &
                            vabfl, iorLe, iorLa, jiein, flae, anze, flag, tflie)
                          
   use module_aparam
   use module_alloc_dimensions
   
   implicit none
   
   real,    intent(inout), dimension(ialloc2) :: vo2      !< Sauerstoff im Hauptfluss
   real,    intent(in),    dimension(ialloc1) :: o2L      !< Sauerstoff im Linieneinleiter
   real,    intent(in),    dimension(ialloc1) :: eo2      !< Sauerstoff im Punkteinleiter
   real,    intent(in),    dimension(ialloc1) :: etemp    !< Temperatur im Punkteinleiter
   integer, intent(in)                        :: mstr     !< aktuelle Strangnumme
   integer, intent(in),    dimension(azStrs)  :: ieinLs   !<
   real,    intent(inout), dimension(ialloc1) :: qeinlL   ! TODO (schoenung): should be intent(in) only
   real,    intent(in),    dimension(ialloc1) :: qeinl    !< 
   real,    intent(in),    dimension(ialloc2) :: vabfl    !<
   integer, intent(in),    dimension(ialloc1) :: iorLe    !<
   integer, intent(in),    dimension(ialloc1) :: iorLa    !<
   integer, intent(in),    dimension(ialloc2) :: jiein    !<
   real,    intent(in),    dimension(ialloc2) :: flae     !<
   integer, intent(in)                        :: anze     !<
   integer, intent(in),    dimension(ialloc2) :: flag     !<
   real,    intent(in)                        :: tflie    !< Zeitschritt [d]
   
   ! --- local variables ---
   integer :: iein, ieinL, ior, j, ior_flag, m, ihcQ, ji
   real    :: hcvo2, hco2E, hcq, hcQE,  hcte
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL)>=0.0 .and. o2L(ieinL) == -1.) cycle

      do ior = 1,anze+1
         if (iorLe(ieinL) < ior) cycle
         
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) qeinlL(ieinL) = 0.0
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
         
         hcvO2 = vo2(ior-m)
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0)hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1)hcQ = 1.e-10
         
         do ji = 1,jiein(ior)
            hcQE = max(0.0,qeinl(iein))
            hco2E = eo2(iein)
            if (hco2E < 0.0) hco2E = hcvo2
            if (etemp(iein) > -9.9) hcTE = etemp(iein)
            
            vo2(ior) = (hcQ * hcvo2 + hcQE * hco2E) / (hcQ + hcQE)
            hcQ = hcQ+qeinl(iein)
            iein = iein+1
            hcvo2 = vo2(ior)
         enddo
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            vo2(ior) = vo2(ior+1)
         endif
         
      endif
   enddo

end subroutine oxygen_inflow_1d



