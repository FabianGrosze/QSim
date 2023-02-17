subroutine thomas(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,nkz,itime)
   
   implicit none
   
   integer                           :: nkz, mstr, ktrans, i, iwahld
   integer                           :: itime, itags, anze
   integer, dimension(1000)          :: imarker, flag
   real, dimension(1000)             :: U
   double precision, dimension(1000) :: a,b,c,d, bp, dp
   double precision                  :: m
   
   bp(1) = b(1)
   dp(1) = d(1)
   do i = 2,anze+1
      m = a(i)/bp(i-1)
      bp(i) = b(i)-m*c(i-1)
      dp(i) = d(i)-m*dp(i-1)
   enddo
   U(anze+1) = dp(anze+1)/bp(anze+1)
   do i = anze,1,-1
      if (imarker(i) == 3) then
         cycle
      else if (imarker(i) == 2) then
         cycle
      else
         U(i) = (dp(i)-c(i)*U(i+1))/bp(i)
      endif
   enddo
   return
end subroutine thomas