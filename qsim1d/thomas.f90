subroutine thomas(a, b, c, d, anze, U, imarker)
   use allodim
   implicit none
   
   ! --- dummy arguments ---
   double precision, intent(in),    dimension(ialloc2) :: a, b, c, d
   integer,          intent(in)                        :: anze
   real,             intent(inout), dimension(ialloc2) :: U
   integer,          intent(in),    dimension(ialloc2) :: imarker
   
   ! --- local variables ---
   integer                              :: i
   double precision, dimension(ialloc2) :: bp, dp
   double precision                     :: m
   
   bp(1) = b(1)
   dp(1) = d(1)
   
   do i = 2,anze+1
      m = a(i) / bp(i-1)
      bp(i) = b(i) - m * c(i-1)
      dp(i) = d(i) - m * dp(i-1)
   enddo
   
   U(anze+1) = dp(anze+1) / bp(anze+1)
   
   do i = anze,1,-1
      if (imarker(i) /= 2 .and. imarker(i) /= 3) then
         U(i) = (dp(i) - c(i) * U(i+1)) / bp(i)
      endif
   enddo
   
   return
end subroutine thomas