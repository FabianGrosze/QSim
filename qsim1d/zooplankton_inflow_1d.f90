subroutine zooplankton_inflow_1d(zooind, ezind, qeinl, vabfl, jiein, anze, &
                                 flag, tflie)
   
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: zooind !< zooplankton in river [Ind/l]
   real,    intent(in),    dimension(ialloc1) :: ezind  !< zooplankton in point source [Ind/l]
   real,    intent(in),    dimension(ialloc1) :: qeinl  !< discharge point source
   real,    intent(in),    dimension(ialloc2) :: vabfl  !< discharge river
   integer, intent(in),    dimension(ialloc2) :: jiein  !< number of boundaries at node
   integer, intent(in)                        :: anze   !< number of sections in current strech
   integer, intent(in),    dimension(ialloc2) :: flag   !< TODO
   real,    intent(in)                        :: tflie  !< timestep [d]
   
   ! --- local variables ---
   integer :: iein, j, ji,  ior, ior_flag, m, ihcq
   real    :: hczoo, hczoo1, hczooE, hcqe, hcq
   
   save hczoo1
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do j = 1,anze+1  !Beginn Knotenschleife
      ior = j
      ior_flag = 0
  
      if (vabfl(ior) >= 0.0 .and. vabfl(ior+1) < 0.0) then
         hczoo1   = zooind(ior)
      endif
      
      if (flag(ior) == 6 .and. vabfl(ior) < 0.0 .and. vabfl(ior+1) > 0.0) then
         ior = ior + 1
         ior_flag = 1
      endif
      
      if (flag(ior) == 4) then
         m = 1
         ihcQ = 0
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) < 0.0) m = -1
         if (vabfl(ior-1) < 0.0 .and. vabfl(ior) > 0.0) ihcQ = 1 
         
         hczoo = zooind(ior-m)
         if (hcQ < 0.0) hcQ = abs(hcQ)
         if (hcQ == 0.0 .or. ihcQ == 1) hcQ = 1.e-10
         
         if (ihcQ == 1) then
            hczoo = hczoo1
         endif
         
         do ji = 1, jiein(ior)
            hcQE = max(0.0, qeinl(iein))
            hczooE = ezind(iein)
            if (hczooE < 0.0) hczooE = hczoo
            zooind(ior) = (hcQ * hczoo + hcQE * hczooE) / (hcQ + hcQE)
            if (ezind(iein) > 0.0 .and. qeinl(iein) == 0.0) then
               zooind(ior) = ezind(iein)
            endif
            hcQ = hcQ + qeinl(iein)
            iein = iein + 1
            hczoo = zooind(ior)
         enddo
         
         if (ior_flag == 1) then
            iein = iein - jiein(ior)
            ior = ior - 1
            zooind(ior) = zooind(ior+1)
         endif
      endif
   enddo
   
   return
end subroutine zooplankton_inflow_1d
