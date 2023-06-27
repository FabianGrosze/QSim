subroutine hnf_inflow_1d(chnf,bvhnf,echnf,ebvhnf, qeinl, vabfl, jiein, anze, flag)
   
   use module_alloc_dimensions
   
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: chnf
   real,    intent(inout), dimension(ialloc2) :: bvhnf
   real,    intent(in),    dimension(ialloc1) :: echnf
   real,    intent(in),    dimension(ialloc1) :: ebvhnf
   real,    intent(in),    dimension(ialloc1) :: qeinl
   real,    intent(in),    dimension(ialloc1) :: vabfl  
   integer, intent(in),    dimension(ialloc2) :: jiein
   integer, intent(in)                        :: anze
   integer, intent(in),    dimension(ialloc2) :: flag
   
   ! --- local variables ---
   integer :: iein, ior, ji
   real    :: hcohnf, hcbhnf, hcq, sechnf, sebvhn, sqeinl
   
   
   ! --------------------------------------------------------------------------
   ! point sources
   ! --------------------------------------------------------------------------
   iein = 1
   do ior = 1,anze+1
      
      if (flag(ior) == 4) then
         hcohnf = chnf(ior-1)
         hcbhnf = bvhnf(ior-1)
         hcq = vabfl(ior-1)
         if (hcq < 0.0) hcq = 0.0
         
         do ji = 1,jiein(ior)
            if (echnf(iein) <= 0.0) then
               sechnf = hcohnf
               sebvhn = hcbhnf
            endif
            
            sechnf = echnf(iein)
            
            if (ebvhnf(iein) <= 0.0) then
               sebvhn = hcbhnf
            else
               sebvhn = ebvhnf(iein)
            endif
            
            sqeinl = qeinl(iein)
            
            if (sqeinl < 0.0) sqeinl = 0.0
            
            if (hcq == 0.0 .and. sqeinl == 0.0) then
               chnf(ior)  = hcohnf
               bvhnf(ior) = hcbhnf
            else
               chnf(ior)  = (hcq * hcohnf + sechnf * sqeinl) / (hcq + sqeinl)
               bvhnf(ior) = (hcq * hcbhnf + sebvhn * sqeinl) / (hcq + sqeinl)
            endif
            
            hcq = hcq + qeinl(iein)
            iein = iein + 1
            hcohnf = chnf(ior)
            hcbhnf = bvhnf(ior)
         enddo
      
      endif
   enddo
   
end subroutine hnf_inflow_1d