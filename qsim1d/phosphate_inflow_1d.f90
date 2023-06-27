subroutine phosphate_inflow_1d(gelp, gesp, pl0, q_pk, q_pg, q_pb,          &
                               gpl, gespl, egesp, ep, epl0, mstr,          &
                               ieinls, qeinll, qeinl, vabfl, iorla, iorle, &
                               jiein, flae, anze, flag, tflie)
   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2)  :: gelp   !< gelöster phosphor
   real,    intent(inout), dimension(ialloc2)  :: gesp   !< gesamtphosphor
   real,    intent(inout), dimension(ialloc2)  :: pl0    !< p:c-verhältnis   
   real,    intent(inout), dimension(ialloc2)  :: q_pk   !< phosphor in kieselalgen
   real,    intent(inout), dimension(ialloc2)  :: q_pg   !< phosphor in grünalgen
   real,    intent(inout), dimension(ialloc2)  :: q_pb   !< phosphor in blaualgen
   real,    intent(inout), dimension(ialloc1)  :: gpl    !< gelöster phosphor in linienquelle  
   real,    intent(inout), dimension(ialloc1)  :: gespl  !< gesamtphosphor in lininenquelle
   real,    intent(in),    dimension(ialloc1)  :: egesp  !< gesamtphosphor im einleiter
   real,    intent(in),    dimension(ialloc1)  :: ep     !< 
   real,    intent(in),    dimension(ialloc5)  :: epl0   !< p:c-verhältnis im einleiter
   integer, intent(in)                         :: mstr   !< akuteller strang
   integer, intent(in),    dimension(azstrs)   :: ieinls !< anzahl der linienquellen je strang
   real,    intent(in),    dimension(ialloc1)  :: qeinll !<
   real,    intent(in),    dimension(ialloc5)  :: qeinl  !<
   real,    intent(inout), dimension(ialloc2)  :: vabfl  !<
   integer,                dimension(ialloc1)  :: iorla  !<
   integer,                dimension(ialloc1)  :: iorle  !<
   integer, intent(in),    dimension(ialloc2)  :: jiein  !<
   real,    intent(in),    dimension(ialloc2)  :: flae   !< oberfläche
   integer, intent(in)                         :: anze   !< anzahl der abschnitte im aktuellen strang
   integer, intent(in),    dimension(ialloc2)  :: flag   !<
   real,    intent(in)                         :: tflie  !< zeitschritt [d]
   
   ! --- local variables ---
   integer :: ieinl, ior, iein, j, ior_flag, m, ihcq, ji
   real    :: hcgelp, hcgesp, hcq
   real    :: hcqe, hcgelpe, hcgespe, hcpl0e , hcpl0

   
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinl = 1, ieinls(mstr)
      do ior = 1,anze+1
         if (iorle(ieinl) < ior)cycle
         if (iorla(ieinl) <= ior .and. iorle(ieinl)>=ior) then
            if (qeinll(ieinl) <= 0.0) then
               gpl(ieinl) = 0.0
               gespl(ieinl) = 0.0
            endif
            
            if (qeinll(ieinl)>=0.0 .and. gpl(ieinl) == -1) then
            else
               gelp(ior) = gelp(ior)+((gpl(ieinl)-gelp(ior))*qeinll(ieinl)/flae(ior))*tflie*86400.
            endif
            
            if (qeinll(ieinl)>=0.0 .and. gespl(ieinl) == -1) then
            else
               gesp(ior) = gesp(ior)+((gespl(ieinl)-gesp(ior))*qeinll(ieinl)/flae(ior))*tflie*86400.
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
      ior_flag = 0
      
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcq = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcq = 1 ! konzentration an der einleitstelle
         ! ist gleich der konzentration der einleitung
         
         hcgelp = gelp(ior-m)
         hcgesp = gesp(ior-m)
         hcpl0  = pl0(ior-m)
         hcq    = vabfl(ior-m)
         if (hcq < 0.0) hcq = abs(hcq)
         if (hcq == 0.0 .or. ihcq == 1) hcq = 1.e-10
                  
         do ji = 1,jiein(ior)   ! beginn einleitungsschleife
            hcqe = max(0.0,qeinl(iein))
            hcgelpe = ep(iein)
            hcgespe = egesp(iein)
            hcpl0e = epl0(iein)
            
            if (hcpl0e  < 0.0) hcpl0e  = hcpl0
            if (hcgelpe < 0.0) hcgelpe = hcgelp
            if (hcgespe < 0.0) hcgespe = hcgesp
            
            gelp(ior) = (hcq * hcgelp      + hcqe * hcgelpe) / (hcq+hcqe)
            pl0(ior)  = (hcq * hcpl0       + hcqe * hcpl0e)  / (hcq+hcqe)
            q_pk(ior) = (hcq * q_pk(ior-m) + hcqe * qmx_pk)  / (hcq+hcqe)
            q_pg(ior) = (hcq * q_pg(ior-m) + hcqe * qmx_pg)  / (hcq+hcqe)
            q_pb(ior) = (hcq * q_pb(ior-m) + hcqe * qmx_pb)  / (hcq+hcqe)
            if (gesp(ior) > 0.0) then
               gesp(ior) = (hcq*hcgesp+hcqe*hcgespe)/(hcq+hcqe)
            endif
            hcq = hcq + qeinl(iein)
            iein = iein+1
            hcgelp = gelp(ior)
            hcgesp = gesp(ior)
            hcpl0 = pl0(ior)
            
         enddo
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            gelp(ior) = gelp(ior+1)
            if (gesp(ior+1)>=0.0)gesp(ior) = gesp(ior+1)
            pl0(ior) = pl0(ior+1)
            q_pk(ior) = q_pk(ior+1)
            q_pg(ior) = q_pg(ior+1)
            q_pb(ior) = q_pb(ior+1)
         endif
      
      endif
   enddo
   
end subroutine phosphate_inflow_1d