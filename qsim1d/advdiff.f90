! --------------------------------------------------------------------------- !
!  qsim - programm zur simulation der wasserqualität                          !
!                                                                             !
!  copyright (c) 2022                                                         !
!  bundesanstalt für gewässerkunde                                            !
!  koblenz (deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  dieses programm ist freie software. sie können es unter den bedingungen    !
!  der gnu general public license, version 3, wie von der free software       !
!  foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  die veröffentlichung dieses programms erfolgt in der hoffnung, dass es     !
!  ihnen von nutzen sein wird, aber ohne irgendeine garantie, sogar ohne die  !
!  implizite garantie der makrtreife oder der verwendbarkeit für einen        !
!  bestimmten zweck.                                                          !
!                                                                             !
!  details finden sie in der gnu general public license.                      !
!  sie sollten ein exemplar der gnu general public license zusammen mit       !
!  diesem programm erhalten haben.                                            !
!  falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  programmiert von                                                           !
!  1979 bis 2018   volker kirchesch                                           !
!  seit 2011       jens wyrwa, wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !

!> advdiff
!! @author volker kirchesch
!! @date 27.01.2009
subroutine advdiff(anze, elen, vmitt, uvert, dl, flag, ktrans, u, deltat,  &
                   sumdet, itime, izeits, mstr, iwied, iwahld, nkz, tflie, &
                   jpoin1, isub_dtx, imac, iverfahren, kktrans, ianze_max, &
                   mtracer, iwsim)
   use module_alloc_dimensions
   implicit none
   
   ! --- dummy arguments ---
   integer, intent(in)                           :: anze
   real,    intent(inout), dimension(ialloc2)    :: elen
   real,    intent(inout), dimension(ialloc2)    :: vmitt
   real,    intent(inout), dimension(50,ialloc2) :: uvert
   real,    intent(in),    dimension(ialloc2)    :: dl
   integer, intent(in),    dimension(ialloc2)    :: flag
   integer, intent(in)                           :: ktrans
   real,    intent(inout), dimension(ialloc2)    :: u
   real,    intent(inout)                        :: deltat
   real,    intent(inout)                        :: sumdet
   integer, intent(in)                           :: itime
   integer, intent(in)                           :: izeits
   integer, intent(in)                           :: mstr
   integer, intent(inout)                        :: iwied
   integer, intent(in)                           :: iwahld
   integer, intent(in)                           :: nkz
   real,    intent(in)                           :: tflie
   integer, intent(in)                           :: jpoin1
   integer, intent(in)                           :: isub_dtx
   integer, intent(in),   dimension(azstrs)      :: imac
   integer, intent(in)                           :: iverfahren
   integer, intent(in)                           :: kktrans
   integer, intent(in)                           :: ianze_max
   integer, intent(inout)                        :: mtracer
   integer, intent(in)                           :: iwsim
   
   ! --- local variables ---
   real                                     :: s2, s1, dx2, dx1
   integer                                  :: ithomas, icranickoeff, ior
   integer, dimension(ialloc2,50)           :: m, isgn
   real,    dimension(ialloc2)              :: cux
   real,    dimension(ialloc2,50)           :: xpoint
   real,    dimension(50,ialloc2)           :: vmittt_1
   real,    dimension(:,:,:),   allocatable :: uvertt_1, cu
   real,    dimension(:,:,:,:), allocatable :: cuz
   
   external :: basispoint, cip, lax_wendroff, quickest, crank_nicolson, maccorm
   
   save cuz, vmittt_1, uvertt_1, isgn, m, cux, xpoint, cu
   
   
   ! Lösung der Tridiagonalmatrix
   ! 0: nach Gauß
   ! 1: nach dem Thomas-Verfahren
   ithomas = 0  
   icranickoeff = 0
   
   
   
   if (iwahld == 2) then
      if (.not. allocated(cuz))      allocate(cuz(1:kktrans, 1:azstrs, 1, 1:ianze_max))
      if (.not. allocated(uvertt_1)) allocate(uvertt_1(azstrs, 1, ianze_max+1))
   else
      if (.not. allocated(cu))       allocate(cu(1:kktrans, 1:azstrs, 1:ianze_max+1))
   endif

   deltat = tflie*86400./izeits
   if (iwsim == 4 .and. mtracer == 0)iwied = 0
   
   ! cu zu beginn der simulation fuer cip-verfahren
   if (iwied == 0) then 
      do ior = 1,anze+1
         if (ior == 1) then
            s1 = u(ior)
            s2 = u(ior+1)
            dx1 = elen(ior)
            dx2 = 0.0
         else if (ior == anze+1 .or. flag(ior+1) == 4) then
            s1 = u(ior-1)
            s2 = u(ior)
            dx1 = elen(ior-1)
            dx2 = 0.0
         else
            s1 = u(ior-1)
            s2 = u(ior+1)
            dx1 = elen(ior-1)
            dx2 = elen(ior)
         endif
         
         if (iwahld == 1) then
            cu(ktrans,mstr,ior) = (s2-s1)/(dx1+dx2)
         else
            cuz(ktrans,mstr,nkz,ior) = (s2-s1)/(dx1+dx2)
         endif
      enddo
   endif
   
   if (iwied == 1) then
      do ior = 1,anze+1
         
         if (ior == 1) then
            s1 = u(ior)
            s2 = u(ior+1)
            dx1 = elen(ior)
            dx2 = 0.0
         else if (ior == anze+1 .or. flag(ior+1) == 4) then
            s1 = u(ior-1)
            s2 = u(ior)
            dx1 = elen(ior-1)
            dx2 = 0.0
         else
            s1 = u(ior-1)
            s2 = u(ior+1)
            dx1 = elen(ior-1)
            dx2 = elen(ior)
         endif
         if (iwahld == 1) then
            if (ior == 1)cu(ktrans,mstr,ior) = (s2-s1)/(dx1+dx2)
         endif
         if (iwahld == 2) then
            if (cuz(ktrans,mstr,nkz,ior) == 0.0 .or. ior == 1) cuz(ktrans,mstr,nkz,ior) = (s2-s1)/(dx1+dx2)
         endif
      enddo
   endif
   
   if (iwsim == 4) then
      mtracer = 1
      iwied = 1
   endif
   
   select case(iverfahren)
      case (1) ! cip
         if (ktrans == 1 .and. nkz == 1) sumdet = sumdet + deltat
         
         if (jpoin1 /= 1) then
            call basispoint(anze,flag,deltat,vmitt,uvert,elen, &
                            xpoint,m, nkz, iwahld, isgn)
         endif
         
         if (iwahld == 1) then
            cux(1:anze+1) = cu(ktrans,mstr,1:anze+1)
         else
            cux(1:anze+1) = cuz(ktrans,mstr,nkz,1:anze+1)
         endif
         
         call cip(u, cux, elen, anze, xpoint, flag, nkz, ktrans, isgn, m)
         
         if (iwahld == 1) then
            cu(ktrans,mstr,1:anze+1) = cux(1:anze+1)
         else
            cuz(ktrans,mstr,nkz,1:anze+1) = cux(1:anze+1)
         endif
      
      case(2) ! lax wendroff
         call lax_wendroff(u, vmitt, uvert, elen, deltat, anze, flag, &
                           nkz, ktrans, isgn, iwahld)
      
      case(3)  ! ultimate quickest
         call quickest(u, vmitt, uvert, elen, deltat, anze, flag, ktrans, &
                        nkz, iwahld, isgn, iwsim)
   end select
   ! ende gitterschleife
   
   if (itime == izeits .and. ktrans == 1) then
      if (iwahld == 1) vmittt_1(mstr,1:anze+1) = vmitt(1:anze+1)
      if (iwahld == 2) uvertt_1(mstr,nkz,1:anze+1) = uvert(nkz,1:anze+1)
   endif
   
   
   ! Löesung des Dispersionsterms
   ! if (isgn(2,nkz) == 1) u(1) = temp0
   ! if (isgn(anze+1,nkz) == -1) u(anze+1) = tempn
   ! if (ktrans /= 44) then
   if (imac(mstr) == 0) then
      call crank_nicolson(u, elen, flag, dl, deltat, anze, icranickoeff, &
                          ithomas, ktrans, nkz, isub_dtx, isgn)
   else
      call maccorm(u, elen, flag, dl, deltat, anze, nkz, isub_dtx, isgn)
   endif
   ! endif
   
   
   ! if (iwied == 1 .and. iflri(mstr) == (-1)) then  
   !    ! Umdrehen bei Fließumkehr
   !    jr =anze + 2 
   !    if (iwahld == 1) then
   !       do ior = 1,anze+1
   !          jr = jr-1
   !          cu(ktrans,mstr,ior) = cu(ktrans,mstr,jr)
   !       enddo
   !    else
   !       do ior = 1,anze+1
   !          jr = jr-1
   !          cuz(ktrans,mstr,nkz,ior) = cuz(ktrans,mstr,nkz,jr)
   !       enddo
   !    endif
   ! endif
   
   ! if (itime == izeits) then   ! löschen
   !    if (isgn(2,nkz) == 1) u(1) = temp0
   !    if (isgn(anze,nkz) == -1) u(anze+1) = tempn
   ! endif
   ! if (iwahld == 2) then
   !    deallocate(cuz)
   !    deallocate(imarkercuz)
   !    deallocate(uvertt_1)
   ! else if (iwahld == 1) then
   !    deallocate(cu)
   ! endif
   
   return
end subroutine advdiff
