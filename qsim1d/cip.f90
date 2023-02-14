!> Calculating 1D Advection equation based on an cubic Interpolation
!!
!! Solver RCIP as in:
!! F. Xiao et al., Constructing oscillation preventing
!! scheme for advection equation by rational function.
!! Computer Physics Communications 93, 1-12 (1996)
subroutine cip(U, CUx, DX, DT, NX, xpoint, flag, nkz, nkzs,ktrans,kktrans,itime,mstr    &
               ,iwahld,nkztot_max, ianze_max,itags,isgn,m,vx_Cr,Uhrz,monats,temp0,deltat)

   use allodim
   implicit none
   
   integer                     :: nkz, nkztot_max, mstr, monats, m3
   integer                     :: m2, m1, ktrans, kktrans, j
   integer                     :: jj, iwahld, itime, itags, iein
   real                        :: b, vx_cr, umin, umax, uhrz
   real                        :: temp0, qmin, qmax, p, hconp
   real                        :: dt, deltat, cx, a
   integer                     :: NX, I, ianze_max
   integer, dimension(1000)    :: nkzs, flag
   integer, dimension(1000,50) :: m, isgn
   real,    dimension(1000)    :: DX, U,CUx, CU_neu,U_neu, Ulin
   real,    dimension(1000,50) :: xpoint
   save CU_neu, U_neu, Ulin,a,b
   
   iein = 0
   cx = 0.0
   j = 2
   if (m(1,nkz) > 0) then
      if (isgn(m(1,nkz),nkz) == -1)j = 1
   endif
   !$omp parallel do
   do i = j,nx+1
      m1 = m(i,nkz)
      if (flag(i) == 4) then
         U_neu(i) = U(i)
         Ulin(i) = U(i)
         CU_neu(i) = (U(i+1)-U(i))/dx(i)
         cycle
      endif
      
      ! if(m1==1)then
      !    m2 = m1
      if (isgn(m(i,nkz),nkz) == -1 .and. i == nx+1) then
         m2 = m1
      else
         m2 = m(i,nkz)-isgn(m(i,nkz),nkz)
      endif
      if (m2 > nx+1 .and. m1 == nx+1)m2 = m1
      m3 = m2
      if (isgn(m(i,nkz),nkz) == -1) m3 = m1
      a = (CUx(m1)+CUx(m2))/(-dx(m3)*isgn(m(i,nkz),nkz))**2+(2.*(U(m1)-U(m2)))/(-dx(m3)*isgn(m(i,nkz),nkz))**3
      b = (3.*(U(m2)-U(m1)))/(-dx(m3)*isgn(m(i,nkz),nkz))**2-(2.*CUx(m1)+CUx(m2))/(-dx(m3)*isgn(m(i,nkz),nkz))
      ! if(isgn(m(i,nkz),nkz)==1)then
      !    if(nkzs(i-1)<nkz)then
      !       U_neu(i) = U(i)
      !       cycle
      !    endif
      ! else if(isgn(m(i,nkz),nkz)==-1)then
      !    if(nkzs(i+1)<nkz)then
      !       U_neu(i) = U(i)
      !       cycle
      !    endif
      ! endif
      U_neu(i) = a*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz))**3    &
               + b*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz))**2    &
               + CUx(m1)*(-xPoint(i,nkz)*isgn(m(i,nkz),nkz)) &
               + U(m1)
      CU_neu(i) = 3.*a*(-xpoint(i,nkz)*isgn(m(i,nkz),nkz))**2 &
                + 2*b*(-xpoint(i,nkz)*isgn(m(i,nkz),nkz))     &
                + CUx(m1)
      Ulin(i) = U(m2)+((U(m1)-U(m2))/dx(m3))*(dx(m3)-xpoint(i,nkz))
      Umax = max(U(m1),U(m2))
      Umin = min(U(m1),U(m2))
      ! if(U_neu(i)>Umax)U_neu(i) = Umax
      ! if(U_neu(i)<Umin)U_neu(i) = Umin
      qmax = Umax-Ulin(i)
      qmin = Umin-Ulin(i)
      p = U_neu(i)-Ulin(i)
      if (p == 0.0)cx = 0.0
      if (p > 0.0) then
         hconp = qmax/p
         cx = 1.
         if (hconp < cx)cx = hconp
      endif
      if (p < 0.0) then
         hconp = qmin/p
         cx = 1.
         if (hconp < cx)cx = hconp
      endif
      U_neu(i) = Ulin(i)+cx*(U_neu(i)-Ulin(i))
      if (U_neu(i) > 0.0 .and. abs(U_neu(i)) < 1.e-15)U_neu(i) = 1.e-15
      if (ktrans /= 1 .and. ktrans /= 57) then
         if (U_neu(i) < 0.0)U_neu(i) = 0.0
      endif
   enddo  ! Ende Knotenschleife
   !$OMP END PARALLEL Do
   
   if (nx > 1) then  ! Belegung des ersten Knotens
      if (isgn(1,nkz) == 1 .or. isgn(1,nkz) == 0)U_neu(1) = U(1)
      if (isgn(nx+1,nkz) == -1)U_neu(nx+1) = U(nx+1)
   else  ! Strang besteht aus einem Knoten
      U_neu(1) = U(1)
      U_neu(NX+1) = U_neu(1)
   endif
   Ulin(1) = U_neu(1)
   j = 1
   !     if(isgn(1,nkz)==1)j = 2
   jj = 1
   !     if(isgn(nx+1,nkz)==-1)jj = 0
   !$omp parallel do
   do i = j,nx+jj
      U(i) = U_neu(i)
      CUx(i) = CU_neu(i)
   enddo
   !$OMP END PARALLEL Do
   return
end subroutine cip