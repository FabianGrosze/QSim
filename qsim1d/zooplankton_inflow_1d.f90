subroutine zooplankton_inflow_1d(zooind, ezind, mstr, qeinl, vabfl, jiein,  &
                                 anze, flag, tflie)
   
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: zooind !< zooplankton in main river [Ind/l]
   real,    intent(in),    dimension(ialloc1) :: ezind  !< zooplanktion in point source [Ind/l]
   integer, intent(in)                        :: mstr   !< number of current stretch
   real,    intent(in),    dimension(ialloc1) :: qeinl  !< 
   real,    intent(in),    dimension(ialloc2) :: vabfl  !<
   integer, intent(in),    dimension(ialloc2) :: jiein  !<
   integer, intent(in)                        :: anze   !<
   integer, intent(in),    dimension(ialloc2) :: flag   !<
   real,    intent(in)                        :: tflie  !< timestep [d]
   
   ! --- local variables ---
   integer :: iein, j, ior, ior_flag, m, ihcq, ji
   real    :: hczoo1, hczoo, hcq, hcqe, hczooe
   
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   
   do j = 1, anze+1
      ior = j
      ior_flag = 0
      
      zooind(ior) = max(0., zooind(ior))
      
      if (vabfl(ior) >= 0.0 .and. vabfl(ior+1) < 0.0) then
         hczoo1 = zooind(ior)
      endif
      
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
         
         hczoo = zooind(ior-m)     ! Umbenennen der benötigten Variablen; 1D
         hcQ = abs(vabfl(ior-m))
        
         if (hcQ <= epsilon(hcq) .or. ihcQ == 1) hcQ = 1.e-10
         if (ihcQ == 1) hczoo = hczoo1
         
         
         do ji = 1,jiein(ior)   ! Beginn Einleitungsschleife
            hcQE = max(0.0,qeinl(iein))
            hczooE = ezind(iein)
            if (hczooE < 0.0) hczooE = hczoo
           
            zooind(ior) = (hcQ * hczoo + hcQE * hczooE) / (hcQ + hcQE)
            if (ezind(iein) > 0.0 .and. qeinl(iein) == 0.0) then
               zooind(ior) = ezind(iein)
            endif
            
            hcQ = hcQ + qeinl(iein)
            iein = iein + 1
            hczoo = zooind(ior)
         enddo ! Ende Einleitungsschleife
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior-1
            zooind(ior) = zooind(ior+1)
         endif
   
      endif  ! Ende Einleitungs-flag
   enddo
   
end subroutine zooplankton_inflow_1d