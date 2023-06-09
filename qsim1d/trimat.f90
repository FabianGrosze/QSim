!> Calculation of a tridiagonal matrix
!! @author Volker Kirchesch
!! @date 23.01.1994
subroutine trimat(a, b, c, d, anze, U, ktrans, imarker)
   ! Koeffizienten: a,b,c  

   implicit none
   
   ! --- dummy arguments ---
   double precision, intent(in),    dimension(1000) :: a, b, c, d
   integer,          intent(in)                     :: anze
   real,             intent(inout), dimension(1000) :: U
   integer,          intent(in)                     :: ktrans
   integer,          intent(in),    dimension(1000) :: imarker
   
   ! --- local variables ---
   integer                           :: ior
   double precision, dimension(1000) :: l, mm, y

   
   mm(1) = a(1)
   do ior = 1,anze
      l(ior) = c(ior)/mm(ior)
      if (imarker(ior) == 2) mm(ior) = a(ior)
      mm(ior+1) = a(ior+1) - l(ior) * b(ior)
   enddo
   
   y(1) = d(1)
   do ior = 2,anze+1
      if (imarker(ior) == 2) then
         y(ior) = d(ior)
         cycle
      endif
      y(ior) = d(ior) - l(ior-1) * y(ior-1)
   enddo
   
   if (mm(anze+1) > 0.0) U(anze+1) = y(anze+1)/mm(anze+1)
   do ior = anze,1,-1 ! hier
      if (imarker(ior) == 2)cycle
      if (imarker(ior) == 1) then
         U(ior) = Y(ior)/mm(ior)
         cycle
      endif
      U(ior) = (y(ior) - b(ior) * U(ior+1)) / mm(ior)
      if (U(ior) > 0.0 .and. abs(U(ior)) < 1.e-15) then
         ! Falls Fehler evt. hier
         U(ior) = 1.e-15 
      endif
      
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U(ior) < 0.0) U(ior) = 0.0
      endif
   enddo
   
   if (mm(anze+1) == 0.0) U(anze+1) = U(anze)
   return
end subroutine trimat