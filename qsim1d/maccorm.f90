subroutine maccorm(u, elen, flag, dl, deltat, anze, nkz, isub_dtx, isgn)
   
   use module_alloc_dimensions
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout), dimension(ialloc2) :: u
   real,    intent(in),    dimension(ialloc2) :: elen
   integer, intent(in),    dimension(ialloc2) :: flag
   real,    intent(in),    dimension(ialloc2) :: dl
   real,    intent(in)                        :: deltat
   integer, intent(in)                        :: anze
   integer, intent(in)                        :: nkz
   integer, intent(in)                        :: isub_dtx
   integer, intent(in), dimension(ialloc2,50) :: isgn
   
   ! --- local variables ---
   integer                              :: i, itime_sub
   real                                 :: ux_nx, ux1, s1, s2, s3
   real, dimension(ialloc2)             :: nenner, calpha, dlmit, uneu_1 
   double precision, dimension(ialloc2) :: ss1, ss2
   
   
   do itime_sub = 1, isub_dtx
      ! if(isgn(anze,nkz)==-1)u(anze+1) = temp0
      ! if(isgn(anze,nkz)==1)u(1) = temp0
      ux1 = u(1)
      ux_nx = u(anze+1)
      
      !$omp parallel do
      do i = 1,anze+1
         if (isgn(i,nkz) > 0) then
            if (i > 1 .and. i < anze+1) then
               calpha(i) = elen(i) / elen(i-1)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i-1)**2
               dlmit(i) = (dl(i-1)+dl(i)) / 2.
               s1 = u(i-1)
               s2 = u(i)
               s3 = u(i+1)
            endif
            
            if (i == 1) then
               s1 = u(i)
               s2 = u(i)
               s3 = u(i+1)
               calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i)**2
               dlmit(i) = (dl(i)+dl(i))/2.
            endif
            
            if (i > 1) then
               if (flag(i) == 6 .or. i == anze+1) then
                  s1 = u(i-1)
                  s2 = u(i)
                  s3 = u(i)
                  calpha(i) = elen(i-1)/elen(i-1)
                  nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i-1)**2
                  dlmit(i) = (dl(i-1) + dl(i-1)) / 2.
               endif
            endif
         
         else ! isgn(i,nkz) gleich -1 (rueckstroemung)
            if (i > 1 .and. i < anze+1) then
               calpha(i) = elen(i)/elen(i-1)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i-1)**2
               dlmit(i) = (dl(i-1) + dl(i)) / 2.
               s1 = u(i+1)
               s2 = u(i)
               s3 = u(i-1)
            endif
            
            if (flag(i) == 6 .or. i == 1) then
               s1 = u(i+1)
               s2 = u(i)
               s3 = u(i)
               
               calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i))*elen(i)**2
               dlmit(i) = (dl(i) + dl(i))/2.
            endif
            
            if (i == anze+1) then
               s1 = u(i)
               s2 = u(i)
               s3 = u(i-1)
               
               calpha(i) = elen(i-1)/elen(i-1)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i-1)**2
               dlmit(i) = (dl(i-1)+dl(i-1))/2.
            endif
            
            if (i == 1) then
               s1 = u(i+1)
               s2 = u(i)
               s3 = u(i)
               calpha(i) = elen(i)/elen(i)
               nenner(i) = 0.5 * calpha(i) * (1.+calpha(i)) * elen(i)**2
               dlmit(i) = (dl(i)+dl(i))/2.
            endif
         endif
         
         ss1(i) = dlmit(i)*(s3-(1.+calpha(i))*s2+calpha(i)*s1)/nenner(i)
         if (flag(i) == 4) ss1(i) = 0.0
         uneu_1(i) = u(i) + ss1(i) * deltat/isub_dtx
      enddo
      !$omp end parallel do
      
      if (isgn(1,nkz) == 1)uneu_1(1) = ux1
      
      if (isgn(anze+1,nkz) == -1)uneu_1(anze+1) = ux_nx
      
      !$omp parallel do
      do i = 1,anze+1
         if (isgn(i,nkz) > 0) then
            if (i > 1 .and. i < anze+1) then
               s1 = uneu_1(i-1)
               s2 = uneu_1(i)
               s3 = uneu_1(i+1)
            endif
            
            if (i == 1) then
               s1 = uneu_1(i)
               s2 = uneu_1(i)
               s3 = uneu_1(i+1)
            endif
            
            if (i > 1) then
               if (flag(i) == 6 .or. i == anze+1) then
                  s1 = uneu_1(i-1)
                  s2 = uneu_1(i)
                  s3 = uneu_1(i)
               endif
            endif
         
         else 
         ! isgn(i,nkz) gleich -1 (rueckstroemung)
            if (i > 1 .and. i < anze+1) then
               s1 = uneu_1(i+1)
               s2 = uneu_1(i)
               s3 = uneu_1(i-1)
            endif
            
            if (flag(i) == 6 .or. i == 1) then
               s1 = uneu_1(i+1)
               s2 = uneu_1(i)
               s3 = uneu_1(i)
            endif
            
            if (i == anze+1) then
               s1 = uneu_1(i)
               s2 = uneu_1(i)
               s3 = uneu_1(i-1)
            endif
         endif
         
         ss2(i) = dlmit(i) * (s3-(1.+calpha(i)) * s2 + calpha(i)*s1) / nenner(i)
         if (flag(i) == 4) ss2(i) = 0.0
         u(i) = u(i) + ((ss1(i)+ss2(i)) / 2.) * deltat/isub_dtx
      enddo
      !$omp end parallel do
      
      if (isgn(1,nkz) == 1) u(1) = ux1
      if (isgn(anze+1,nkz) == -1) u(anze+1) = ux_nx
   enddo
   
   return
end subroutine maccorm