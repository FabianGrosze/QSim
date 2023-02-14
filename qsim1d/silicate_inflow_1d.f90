subroutine silicate_inflow_1d(si, q_sk, siL, esi, mstr, ieinLs,   &
                              qeinlL, qeinl, vabfl, iorLe, iorLa, &
                              jiein, flae, anze, flag, tflie)


   use allodim
   use aparam, only: qmx_sk
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: si
   real,    intent(inout), dimension(ialloc2) :: q_sk
   real,    intent(inout), dimension(ialloc1) :: siL ! TODO (schoenung, august 2022): should be intent(in)
   real,    intent(in),    dimension(ialloc1) :: esi
   integer, intent(in)                        :: mstr
   integer, intent(in),    dimension(azStrs)  :: ieinLs
   real,    intent(in),    dimension(ialloc1) :: qeinlL
   real,    intent(in),    dimension(ialloc1) :: qeinl
   real,    intent(in),    dimension(ialloc2) :: vabfl
   integer, intent(in),    dimension(ialloc1) :: iorLe
   integer, intent(in),    dimension(ialloc1) :: iorLa
   integer, intent(in),    dimension(ialloc2) :: jiein
   real,    intent(in),    dimension(ialloc2) :: flae
   integer, intent(in)                        :: anze
   integer, intent(in),    dimension(ialloc2) :: flag
   real,    intent(in)                        :: tflie
   
   ! --- local variables ---
   integer                  :: ieinl, ior, j, ior_flag, m , iein, ihcq, ji
   real                     :: hcsi, hcq, hcqe, hcsie   
   
   ! --------------------------------------------------------------------------
   ! diffuse sources
   ! --------------------------------------------------------------------------
   do ieinL = 1, ieinLs(mstr)
      if (qeinlL(ieinL)>=0.0 .and. SiL(ieinL) == -1.)cycle
      do ior = 1,anze+1
         if (iorLe(ieinL) < ior)cycle
         if (iorLa(ieinL) <= ior .and. iorLe(ieinL)>=ior) then
            if (qeinlL(ieinL) <= 0.0) SiL(ieinL) = 0.0
            Si(ior) = Si(ior)+((SiL(ieinL)-Si(ior))*qeinlL(ieinL)/flae(ior))*tflie*86400.
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
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 ! Konzentration an der Einleitstelle
         ! ist gleich der Konzentration der Einleitung
         
         ! Umbenennen der benötigten Variablen
         hcSi = Si(ior-m)     
         hcQ = vabfl(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hcSiE = eSi(iein)
            
            if (hcSiE < 0.0) then 
               hcSiE = hcSi
            endif
            
            Si(ior)   = (hcQ * hcSi        + hcQE * hcSiE)  / (hcQ+hcQE)
            Q_SK(ior) = (hcQ * Q_SK(ior-1) + hcQE * Qmx_SK) / (hcQ+hcQE)
            
            hcQ = hcQ + qeinl(iein)
            iein = iein+1
            hcSi = Si(ior)
         enddo
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            si(ior) = si(ior+1)
         endif
         
      endif  
   enddo

end subroutine silicate_inflow_1d