!> Berechnung einer tridiagonal-Matrix
!! @author Volker Kirchesch
!! @date 23.01.1994
subroutine trimat(a,b,c,d,anze,U,flag,mstr,ktrans,imarker,iwahlD,itags,monats,uhrz,nkz,nkzs,itime)
   ! Koeffizienten: a,b,c  

   implicit none
   
   integer                           :: nkz, mstr, monats, ktrans, iwahld
   integer                           :: itime, itags, ior, anze
   real                              :: uhrz
   integer, dimension(1000)          :: flag, imarker,nkzs
   real,    dimension(1000)          :: U
   double precision, dimension(1000) :: a, b, c, d, l, mm, y
   
   mm(1) = a(1)
   do ior = 1,anze
      l(ior) = c(ior)/mm(ior)
      if (imarker(ior) == 2)mm(ior) = a(ior)
      mm(ior+1) = a(ior+1)-l(ior)*b(ior)
   enddo
   y(1) = d(1)
   do ior = 2,anze+1
      if (imarker(ior) == 2) then
         Y(ior) = d(ior)
         cycle
      endif
      y(ior) = d(ior)-l(ior-1)*y(ior-1)
   enddo
   if (mm(anze+1) > 0.0)U(anze+1) = y(anze+1)/mm(anze+1)
   do ior = anze,1,-1 ! hier
      if (imarker(ior) == 2)cycle
      if (imarker(ior) == 1) then
         U(ior) = Y(ior)/mm(ior)
         cycle
      endif
      U(ior) = (y(ior)-b(ior)*U(ior+1))/mm(ior)
      if (U(ior) > 0.0 .and. abs(U(ior)) < 1.e-15)U(ior) = 1.e-15  ! Falls Fehler evt. hier
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U(ior) < 0.0)U(ior) = 0.0
      endif
   enddo
   if (mm(anze+1) == 0.0)U(anze+1) = U(anze)
   return
end subroutine trimat