subroutine water_temperature_inflow_1d(tempw, etemp, ewaerm, etempl, mstr,   &
                                 ieinLs, qeinlL, qeinl, vabfl, iorLe, iorLa, &
                                 jiein, flae, anze, flag, tflie)
   
   use allodim
   implicit none
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: tempw
   real,    intent(in),    dimension(ialloc1) :: etemp
   real,    intent(inout), dimension(ialloc1) :: ewaerm
   real,    intent(in),    dimension(ialloc1) :: etempl
   integer, intent(in)                        :: mstr    !< aktueller Strang
   integer, intent(in),    dimension(azStrs)  :: ieinLs  !< Anzahl der Linienquellen je Strang
   real,    intent(inout), dimension(ialloc1) :: qeinlL  !< 
   real,    intent(in),    dimension(ialloc1) :: qeinl   !< 
   real,    intent(in),    dimension(ialloc2) :: vabfl   !< 
   integer, intent(in),    dimension(ialloc1) :: iorLe   !<
   integer, intent(in),    dimension(ialloc1) :: iorLa   !<
   integer, intent(in),    dimension(ialloc2) :: jiein   !< Anzahl der Einleiter je Knoten
   real,    intent(in),    dimension(ialloc2) :: flae    !< Oberfläche
   integer, intent(in)                        :: anze
   integer, intent(in),    dimension(ialloc2) :: flag    !<
   real,    intent(in)                        :: tflie   !< Zeitschritt [d] 

   ! --- local variables ---
   integer :: ieinl, ior, iein, j, ji, ior_flag, m, ihcq
   real    :: hctemp1, hctemp, hcq, hcwe, hcqe, hcte
   
   
   external :: dichte
   
   hctemp1 = 0.0
   
   ! -------------------------------------------------------------------------
   ! diffuse sources
   ! -------------------------------------------------------------------------
   do ieinl = 1, ieinls(mstr)
      if (qeinll(ieinl)>=0.0 .and. etempl(ieinl) == -9.99) cycle
      do ior = 1,anze+1
         if (iorle(ieinl) < ior)cycle
         if (iorla(ieinl) <= ior .and. iorle(ieinl)>=ior) then
            if (qeinll(ieinl) <= 0.0)qeinll(ieinl) = 0.0
            if (flae(ior) > 0.0 .and. etempl(ieinl) > 0.0) then
               tempw(ior) = tempw(ior)+((etempl(ieinl)-tempw(ior))*qeinll(ieinl)/flae(ior))*tflie*86400. 
            endif
         endif
      enddo
   enddo
   
   
   ! -------------------------------------------------------------------------
   ! point sources
   ! -------------------------------------------------------------------------
   iein = 1
   do j = 1, anze+1
      ior_flag = 0
      ior = j
      
      if (vabfl(ior) >=0.0 .and. vabfl(ior+1) < 0.0) hctemp1 = tempw(ior)
      
      
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior+1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then 
         m = 1
         ihcq = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0)m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0)ihcq = 1
         hctemp = tempw(ior-m)
         hcq = vabfl(ior-m)
         if (hcq < 0.0)hcq = abs(hcq)
         if (hcq == 0.0 .or. ihcq == 1) hcq = 1.e-10
         if (ihcq == 1) hctemp = hctemp1
         
         do ji = 1,jiein(ior) ! Beginn Einleiterschleife
            hcwe = 0.0
            hcqe = max(0.0,qeinl(iein))
            
            ! Wärmeeinleitung
            if (ewaerm(iein) > -9999.9) then 
               hcwe = ewaerm(iein)/4.2
               if (hcqe == 0.0) then
                  if (hcq > 1.e-10)tempw(ior) = hctemp+hcwe/hcq
               else  
                  tempw(ior) = hctemp + (hcwe/(hcq+hcqe))
               endif
            
            else    
               ! Temperatureinleitung
               hcte = etemp(iein)
               if (hcte < -9.8) hcte = hctemp
               tempw(ior) = (hcq * hctemp + hcte * hcqe) / (hcq + hcqe) 
            endif
            hcq = hcq+hcqe
            iein = iein+1
            hctemp = tempw(ior)
            
         enddo
        
        if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            tempw(ior) = tempw(ior+1)
         endif
      endif
   
   enddo
   
end subroutine water_temperature_inflow_1d