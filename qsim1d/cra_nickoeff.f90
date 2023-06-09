subroutine cra_nickoeff(elen, r1, r2, r3, dl, flag, deltat, anze,  &
                        imarker, icranickoeff)
   
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in),  dimension(ialloc2) :: elen
   real,    intent(out), dimension(ialloc2) :: r1, r2, r3
   real,    intent(in),  dimension(ialloc2) :: dl
   integer, intent(in),  dimension(ialloc2) :: flag
   real,    intent(in)                      :: deltat
   integer, intent(in)                      :: anze
   integer, intent(out), dimension(ialloc2) :: imarker
   integer, intent(out)                     :: icranickoeff
   
   ! --- local variables ---
   integer :: ior
   real    :: dlmit, alpha, nenner
  
   
   icranickoeff = 1

   imarker(1) = 0
   ! if (flag(2) == 4) imarker(1) = 3
   r2(1) = dl(1) * deltat / elen(1)**2
   r3(1) = dl(1) * deltat / elen(1)**2
   
   do ior = 2,anze
      imarker(ior) = 0
      
      ! if(flag(ior)==4.and.flag(ior+1)==4)imarker(ior) = 3
      if (flag(ior) == 6) imarker(ior) = 1
      
      if (flag(ior) == 4) imarker(ior) = 2
      
      if (imarker(ior) == 2) then
         r2(ior) = dl(ior) * deltat / elen(ior)**2
         r3(ior) = dl(ior) * deltat / elen(ior)**2
      else if (imarker(ior) == 1) then
         r1(ior) = dl(ior) * deltat / elen(ior-1)**2
         r2(ior) = 2. * dl(ior) * deltat / elen(ior-1)**2
      else
         alpha = elen(ior) / elen(ior-1)
         nenner = 0.5 * alpha * (1 + alpha) * elen(ior-1)**2
         dlmit = (dl(ior-1) + dl(ior))/2.
         r1(ior) = alpha * dlmit * deltat / nenner
         r2(ior) = (1+alpha) * dlmit * deltat / nenner
         r3(ior) = dlmit * deltat / nenner
      endif
   enddo
   
   imarker(anze+1) = 0
   r1(anze+1) = dl(anze) * deltat / elen(anze)**2
   r2(anze+1) = 2. * dl(anze) * deltat / elen(anze)**2
   
   
   return
end subroutine cra_nickoeff