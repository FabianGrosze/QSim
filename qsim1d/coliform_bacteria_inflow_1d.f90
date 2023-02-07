subroutine coliform_bacteria_inflow_1d(coli, doscf, mstr, colil, ecoli, ieinls, &
                                       qeinll, qeinl, vabfl, iorle, iorla,      &
                                       jiein, flae, anze, flag, tflie)   
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)  :: coli   !< coliform bacteria
   real,    intent(inout), dimension(ialloc2)  :: doscf  !<  
   integer, intent(in)                         :: mstr   !< current stretch 
   real,    intent(inout), dimension(ialloc1)  :: coliL  !< inflow coliform bacteria (diffuse sources)
   real,    intent(in),    dimension(ialloc1)  :: ecoli  !< inflow coliform bacteria (point sources)
   integer, intent(in),    dimension(azStrs)   :: ieinLs !< number of diffuse sources per stretch
   real,    intent(in),    dimension(ialloc1)  :: qeinlL !< 
   real,    intent(in),    dimension(ialloc1)  :: qeinl  !<
   real,    intent(in),    dimension(ialloc2)  :: vabfl  !<
   integer, intent(in),    dimension(ialloc1)  :: iorLe  !< 
   integer, intent(in),    dimension(ialloc1)  :: iorLa  !<
   integer, intent(in),    dimension(ialloc2)  :: jiein  !< number of point sources per node
   real,    intent(in),    dimension(ialloc2)  :: flae   !< area
   integer, intent(in)                         :: anze   !< number of segments in current stretch
   integer, intent(in),    dimension(ialloc2)  :: flag   !< 
   real,    intent(in)                         :: tflie  !< timestep [d]
   
   ! --- local variables ---
   integer  :: ieinl, ior, iein, m, ihcq, ji
   real     :: hccoli, hcdoscf, hcq, hcqe, hccolie
   
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL) >= 0.0 .and. coliL(ieinL) == -1.) cycle
      
      do ior = 1, anze+1
         if (iorLe(ieinL) < ior) cycle
         
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL) >= ior) then
            if (qeinlL(ieinL) > 0.0) then
               coli(ior) = coli(ior) + ((coliL(ieinL)-coli(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
            endif
         endif
      enddo
   enddo
      
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do ior = 1, anze+1
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         ! Umbenennen der benötigten Variablen
         hccoli = coli(ior-m)     
         hcdoscf = doscf(ior-m)
         hcq = vabfl(ior-m)
         if (hcq < 0.0) hcq = abs(hcq)
         if (hcq == 0.0 .or. ihcq == 1)hcq = 1.e-10
         
         do ji = 1,jiein(ior)   
            hcqe = max(0.0, qeinl(iein))
            hccolie = ecoli(iein)
            if (hccolie < 0.0) hccolie = hccoli
            coli(ior)  = (hcq*hccoli+hcqe*hccolie)/(hcq+hcqe)
            ! annahme: einleitung mit doscf = 0
            doscf(ior) = (hcq*hcdoscf)/(hcq+hcqe)    
            hcq = hcq + qeinl(iein)
            iein = iein+1
            hccoli = coli(ior)
            hcdoscf = doscf(ior)
         enddo 
      endif 
   enddo
   
   return
  
end subroutine coliform_bacteria_inflow_1d
