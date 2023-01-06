! --------------------------------------------------------------------------- !
!  QSim - Programm zur Simulation der Wasserqualität                          !
!                                                                             !
!  Copyright (C) 2022                                                         !
!  Bundesanstalt für Gewässerkunde                                            !
!  Koblenz (Deutschland)                                                      !
!  http://www.bafg.de                                                         !
!                                                                             !
!  Dieses Programm ist freie Software. Sie können es unter den Bedingungen    !
!  der GNU General Public License, Version 3, wie von der Free Software       !
!  Foundation veröffentlicht, weitergeben und/oder modifizieren.              !
!                                                                             !
!  Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, dass es     !
!  Ihnen von Nutzen sein wird, aber ohne irgendeine Garantie, sogar ohne die  !
!  implizite Garantie der Makrtreife oder der Verwendbarkeit für einen        !
!  bestimmten Zweck.                                                          !
!                                                                             !
!  Details finden Sie in der GNU General Public License.                      !
!  Sie sollten ein Exemplar der GNU General Public License zusammen mit       !
!  diesem Programm erhalten haben.                                            !
!  Falls nicht, siehe http://www.gnu.org/licenses/.                           !
!                                                                             !
!  Programmiert von                                                           !
!  1979 bis 2018   Volker Kirchesch                                           !
!  seit 2011       Jens Wyrwa, Wyrwa@bafg.de                                  !
! --------------------------------------------------------------------------- !
!> SUBROUTINE read_mesh_schism()
!!    get SCHISM mesh and partitioning \n
!!    check for necessary variables  \n
!!    elev=iof_hydro(1) in param.nml \n
!!    (dahv)=iof_hydro(16) in param.nml \n
!!    hvel_side=iof_hydro(26) in param.nml \n
!! \n\n
!! aus Datei read_mesh_schism.f95; zurück zu \ref lnk_transport_schism
subroutine read_mesh_schism() !meinrang.eq.0
   use netcdf
   use modell
   use schism_glbl, only: su2, sv2, tr_el, eta2, nvrt, ns_global, tr_nd,         &
                          np,npa,ne,nea,nea2,ns,nsa,iegrpv,                      &
                          ne_global, np_global, ielg, iplg, islg, RNDAY, dt,     &
                          rkind, xnd, ynd, dp00, kbp00, i34, elnode, isidenode,  &
                          itrtype,irange_tr,isbnd,isbe,trth,trobc,natrm,ntrs,    &
                          snx, sny, distj,ntracers,kbe,isdel,delj,               &
                          start_year,start_month,start_day,start_hour,utc_start, &
                          nrec,nspool,kz,ics,xlon,ylat,area,elside,ic3,isbs,     &
                          wsett,iwsett,tr_nd0,saltmax,saltmin,tempmax,tempmin,   &
                          ssign,kbs,iegl2,isten_qual2,                           &
                          ihdif,ihconsv,isconsv,itur,itr_met,                    &
                          h_tvd,eps1_tvd_imp,eps2_tvd_imp,                       &
                          ip_weno,courant_weno,ntd_weno,nquad,                   &
                          epsilon1,epsilon2,ielm_transport,                      &
                          ztot,sigma,in_dir,len_in_dir,nxq
                          
  use schism_msgp, only:  comm, nproc, myrank

   implicit none
   include 'netcdf.inc'
   
   integer                           :: i,j,k,n,m,mm, nr
   integer                           :: istat, ierr
   character (len = longname)        :: dateiname,systemaufruf
   character(len = 72)               :: fgb,fgb2  ! Processor specific global output file name
   character(len = 400)              :: textline
   integer                           :: ne_l,nea_l,nea2_l, np_l,npa_l, ns_l,nsa_l ! numbers on each process
   integer                           :: irank
   integer                           :: nmax, nmin, tn, bn
   real                              :: dtout
   !real,allocatable,dimension (:)    :: manni
   integer,allocatable,dimension(:,:):: nm2,nm3
   integer,allocatable,dimension(:)  :: kbp00_global , i34_global, rrr
   real                              :: xmax, xmin, ymax, ymin, zmax, zmin, xmaxi, xmini, ymaxi, ymini, zmaxi, zmini
   real                              :: rzone, distmax, distmin, sum_area, totalarea, dx,dy
   real    dummy,xummy,yummy
   integer nummy, rank_min,rank_max,id_min,id_max, kkk,nnn, lfdb,ll,lll,jsj,ndo
   integer total_edge_number,total_element_number,total_node_number
   integer  :: eck(4)
   
   character(len=2),save :: mid,stab
   real(rkind),save :: alphaw,btrack_nudge,coricoef,dfh0,dfv0,dramp,dramp_ss,drampbc,drampwafo,drampwind,  &
                       dtb_max,dtb_min,dzb_min,eos_a,eos_b,epsilon3,gen_wsett,h0,h1_bcc,h1_pp,h2_bcc,      &
                       h2_pp,h_bcc1,h_c,h_s,h_massconsv,hmin_airsea_ex,hmin_man,hmin_radstress,            &
                       hmin_salt_ex,hvis_coef0,hw_depth,hw_ratio,                                          &
                       loadtide_coef,                                                                      &
                       prmsl_ref,rearth_eq,rearth_pole,rho0,rinflation_icm,rlatitude,rmaxvel,rtol0,        &
                       s1_mxnbt,s2_mxnbt,sed_class,sfea0,shapiro0,shw,slam0,slr_rate,small_elad,           &
                       step_nu_tr,tdmin_pp1,tdmin_pp2,theta_b,theta_f,thetai,turbinj,turbinjds,            &
                       vclose_surf_frac,vdmax_pp1,vdmax_pp2,vdmin_pp1,vdmin_pp2,velmin_btrack,vnf1,vnf2,   &
                       vnh1,vnh2,wtiminc,xlsc0
   integer,save     :: fwvor_streaming,fwvor_gradpress,fwvor_breaking,fwvor_advz_stokes,fwvor_advxy_stokes,        &
                       flag_fib,eco_class,cur_wwm,fwvor_wveg,fwvor_wveg_nl,i_hmin_airsea_ex,i_hmin_salt_ex,        &
                       i_prtnftl_weno,ibc,ibcc_mean,                                           &
                       ibdef,ibtp,ibtrack_test,ic_elev,icou_elfe_wwm,ielad_weno,ieos_pres,ieos_type,if_source,     &
                       iflux,iflux_out_format,iharind,ihfskip,ihhat,ihorcon,ihot,ihydraulics,iloadtide,imm,        &
                       indvel,inter_mom,inu_elev,inu_uv,inunfl,                                             &
                       inv_atm_bnd,ipre,ipre2,iprecip_off_bnd,irouse_test,isav,ishapiro,itransport_only,           &
                       iunder_deep,iupwind_mom,iwbl,iwind_form,iwindoff,izonal5,kr_co,level_age,     &
                       mdc2,meth_sink,moitn0,msc2,mxitn0,                                                          &
                       max_subcyc,nadv,nchi,ncor,niter_shap,nramp_elev,nstep_ice,nstep_wwm,ntracer_age,            &
                       ntracer_gen,nu_sum_mult,nws,shorewafo,wafo_obcramp
   !integer,parameter :: natrm=12 !# of _available_ tracer models at the moment (including T,S)
   integer :: flag_ic(12),inu_tr(12),iadjust_mass_consv0(12),lev_tr_source(12)

   namelist /CORE/ IPRE,IBC,IBTP,NTRACER_GEN,NTRACER_AGE,SED_CLASS,ECO_CLASS,NSPOOL,IHFSKIP,MSC2,MDC2,DT,RNDAY
   namelist /OPT/ gen_wsett,flag_fib,ics,rearth_pole,rearth_eq,indvel, &
     &imm,ibdef,ihot,ihydraulics,izonal5,slam0,sfea0,iupwind_mom,ihorcon, &
     &hvis_coef0,ishapiro,shapiro0,niter_shap,ihdif,thetai,drampbc, &
     &dramp,nadv,dtb_min,dtb_max,h0,nchi,dzb_min, &
     &hmin_man,ncor,rlatitude,coricoef,nws,wtiminc,iwind_form, &
     &drampwind,iwindoff,ihconsv,isconsv,itur,dfv0,dfh0,h1_pp,h2_pp,vdmax_pp1, &
     &vdmax_pp2,vdmin_pp1,vdmin_pp2,tdmin_pp1,tdmin_pp2,mid,stab,xlsc0, &
     &ibcc_mean,flag_ic,start_year,start_month,start_day,start_hour,utc_start, &
     &itr_met,h_tvd,eps1_tvd_imp,eps2_tvd_imp,ip_weno, &
     &courant_weno,ntd_weno,nquad,epsilon1,epsilon2,epsilon3,ielad_weno,small_elad, &
     &i_prtnftl_weno,inu_tr,step_nu_tr,vnh1,vnh2,vnf1,vnf2, &
     &moitn0,mxitn0,rtol0,iflux,iflux_out_format,inter_mom,h_bcc1,inu_elev,inu_uv, &
     &ihhat,kr_co,rmaxvel,velmin_btrack,btrack_nudge,ibtrack_test,irouse_test, &
     &inunfl,shorewafo,ic_elev,nramp_elev,inv_atm_bnd,prmsl_ref,s1_mxnbt,s2_mxnbt, &
     &iharind,icou_elfe_wwm,drampwafo,nstep_wwm,hmin_radstress,turbinj,turbinjds,alphaw, &
     &fwvor_advxy_stokes,fwvor_advz_stokes,fwvor_gradpress,fwvor_breaking, &
     &fwvor_streaming,fwvor_wveg,fwvor_wveg_NL,wafo_obcramp, &
     &iwbl,cur_wwm,if_source,dramp_ss,ieos_type,ieos_pres,eos_a,eos_b,slr_rate, &
     &rho0,shw,isav,nstep_ice,iunder_deep,h1_bcc,h2_bcc,hw_depth,hw_ratio, &
     &level_age,vclose_surf_frac,iadjust_mass_consv0,ipre2, &
     &ielm_transport,max_subcyc,i_hmin_airsea_ex,hmin_airsea_ex,itransport_only,meth_sink, &
     &iloadtide,loadtide_coef,nu_sum_mult,i_hmin_salt_ex,hmin_salt_ex,h_massconsv,lev_tr_source, &
     &rinflation_icm,iprecip_off_bnd

      !print*,meinrang,' read_mesh_schism starts ',proz_anz

!#### read param.nml #############################################################################!
         ! read param.out.nml
      write(dateiname,"(2A,I4.4,3A)")trim(modellverzeichnis),"outputs_schism/param.out.nml"
      open(15,file = dateiname,delim = 'apostrophe',status = 'old',iostat = istat)
      if (istat /= 0)then
         write(fehler,*)meinrang,' opening ',trim(dateiname),' failed'
         call qerror(fehler)
      endif
      read(15,nml = CORE)
      deltat = int(dt)
      read(15,nml = OPT)
      if (meinrang == 0) then !! nur prozessor 0
         print*,"ihdif,ihconsv,isconsv,itur,itr_met  = ",ihdif,ihconsv,isconsv,itur,itr_met
         print*,"h_tvd,eps1_tvd_imp,eps2_tvd_imp     = ",h_tvd,eps1_tvd_imp,eps2_tvd_imp
         print*,"ip_weno,courant_weno,ntd_weno,nquad = ",ip_weno,courant_weno,ntd_weno,nquad
         print*,"epsilon1,epsilon2,ielm_transport    = ",epsilon1,epsilon2,ielm_transport
         print*,"read_mesh_schism: param.out.nml DT,RNDAY = ",DT,deltat,RNDAY
         close(15)
      endif ! process0 only
      call mpi_barrier (mpi_komm_welt, ierr)

!#### read local_to_global_0000 files ############################################################!
      write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/local_to_global_000000"
      i=len_trim(dateiname)
      write(dateiname(i-5:i),'(i6.6)') meinrang
      open(10+meinrang,file=trim(dateiname),status='old',iostat = istat)
      if (istat /= 0)then
         write(fehler,*)'opening ',trim(dateiname),' failed'
         call qerror(fehler)
      endif
!     header info 
      read(10+meinrang,*)ns_global,ne_global,np_global,nvrt,nproc,ntracers ! ,ntrs(:) ??? !global info
      read(10+meinrang,'(A)')textline ! ; print*,meinrang,trim(textline)  !write(10,*)'Header:'
      n_elemente     = ne_global
      number_plankt_point=n_elemente
      knotenanzahl2D = np_global
      kantenanzahl   = ns_global
      num_lev=nvrt
      if (meinrang == 0)print*,'read_mesh_schism: local_to_global ns_global,ne_global,np_global,num_lev= '  &
                             ,ns_global,ne_global,np_global,num_lev
      if(proz_anz .ne. nproc )then
         print*,trim(dateiname)
         print*,meinrang,' proz_anz .ne. nproc ',proz_anz,nproc
         call qerror('incompatible process number SCHISM - QSim')
      endif
      read(10+meinrang,*)ne,nea,nea2
      if(allocated(ielg)) deallocate(ielg); allocate(ielg(nea)); ielg=0
      do i=1,ne
         read(10+meinrang,*)j,ielg(j)
      enddo
      read(10+meinrang,*)np,npa
      if(allocated(iplg)) deallocate(iplg); allocate(iplg(npa)); iplg=0
      do i=1,np
         read(10+meinrang,*)j,iplg(j)
      enddo
      read(10+meinrang,*)ns,nsa
      if(allocated(islg)) deallocate(islg); allocate(islg(nsa)); islg=0
      do i=1,ns
         read(10+meinrang,*)j,islg(j)
      enddo
      print*,meinrang,'local_to_global: elements',ne,nea,nea2,' points',np,npa,' sides/edges',ns,nsa
      call mpi_barrier (mpi_komm_welt, ierr)
      call mpi_reduce(ns,total_edge_number,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(ne,total_element_number,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(np,total_node_number,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'total_edge_number,total_element_number,total_node_number='  &
                            ,total_edge_number,total_element_number,total_node_number
      call mpi_barrier (mpi_komm_welt, ierr)

      read(10+meinrang,'(A)')textline ! ; print*,meinrang,trim(textline)  !write(10+meinrang,*)'Header:'
      read(10+meinrang,*)start_year,start_month,start_day,start_hour,utc_start
      read(10+meinrang,*)nrec,dummy,nspool,nvrt,kz, &
     &         h0,h_s,h_c,theta_b,theta_f,ics
      if(allocated(ztot)) deallocate(ztot); allocate(ztot(nvrt))
      if(allocated(sigma)) deallocate(sigma); allocate(sigma(nvrt))
      do k=1,kz-1
         read(10+meinrang,*)ztot(k)
      enddo !k
      do k=1,nvrt-kz+1
         read(10+meinrang,*)sigma(k)
      enddo !k
      read(10+meinrang,*)i,j
      if((i.ne.np).or.(j.ne.ne))call qerror('(i.ne.np).or.(j.ne.ne)')
      if(ics==1) then
         allocate(xnd(npa),ynd(npa),dp00(npa),kbp00(npa))
         xmin=1.0e9 ; xmax=-1.0*xmin
         ymin=1.0e9 ; ymax=-1.0*ymin
         do m=1,np
            read(10+meinrang,*)xnd(m),ynd(m),dp00(m),kbp00(m)
            if(xmin.gt.xnd(m))xmin=xnd(m);if(xmax.lt.xnd(m))xmax=xnd(m)
            if(ymin.gt.ynd(m))ymin=ynd(m);if(ymax.lt.ynd(m))ymax=ynd(m)
         enddo !m
      else !lat/lon
         allocate(xlon(npa),ylat(npa),dp00(npa),kbp00(npa))
         do m=1,np
            read(10+meinrang,*)xlon(m),ylat(m),dp00(m),kbp00(m)
            xlon(m)=xlon(m)/180.d0*3.141592653589793 ; ylat(m)=ylat(m)/180.d0*3.141592653589793
         enddo !m
      endif !ics
      allocate(i34(nea),elnode(4,nea))
      allocate(kbe(nea),area(nea),ic3(4,nea),elside(4,nea),ssign(4,nea))
      sum_area=0.0
      kbe=0
      area=0.0
      do m=1,ne
         read(10+meinrang,*)i34(m),(elnode(mm,m),mm=1,i34(m))
         read(10+meinrang,*)kbe(m),area(m),ic3(1,m),ic3(2,m),ic3(3,m),ic3(4,m)
         sum_area=sum_area+area(m)
         read(10+meinrang,*)elside(1,m),elside(2,m),elside(3,m),elside(4,m),ssign(1,m),ssign(2,m),ssign(3,m),ssign(4,m)
         !if(kbe(m)<1)print*,meinrang,m,' read_mesh_schism local_to_global: kbe(m)<1',kbe(m)
      enddo !m
      sum_area=sum_area/1000000 ! km²
      allocate(isidenode(2,nsa),isdel(2,nsa))
      allocate(distj(nsa),delj(nsa),snx(nsa),sny(nsa),isbs(nsa),kbs(nsa))
      do i=1,ns
         read(10+meinrang,*)j,isidenode(1:2,j)
         if(j/=i)call qerror('read_mesh_schism: local_to_global, isidenode j/=i')
         read(10+meinrang,*)distj(i),delj(i),snx(i),sny(i),isbs(i),kbs(i),isdel(1,i),isdel(2,i)
      enddo !i
      close(10+meinrang) ! local_to_global_....
      print*,meinrang,' area [km²]=',sum_area,' bottom left=',xmin,ymin,' top right=',xmax,ymax,' nodes,elements=',np,ne
      call mpi_reduce(xmax,xmaxi,1,MPI_DOUBLE_PRECISION,mpi_max,0,mpi_komm_welt,ierr)
      call mpi_reduce(ymax,ymaxi,1,MPI_DOUBLE_PRECISION,mpi_max,0,mpi_komm_welt,ierr)
      call mpi_reduce(xmin,xmini,1,MPI_DOUBLE_PRECISION,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(ymin,ymini,1,MPI_DOUBLE_PRECISION,mpi_min,0,mpi_komm_welt,ierr)
      call mpi_reduce(sum_area,totalarea,1,MPI_FLOAT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_barrier (mpi_komm_welt, ierr)
      if (meinrang==0)print*,'area[km²]=',totalarea,' bottom left=',xmini,ymini,' top right=',xmaxi,ymaxi
      
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_reduce(nea2,maxel,1,MPI_INT,mpi_max,0,mpi_komm_welt,ierr)
      call MPI_Bcast(maxel,1,MPI_INT,0,mpi_komm_welt,ierr)
      if (meinrang==0)print*,'0 read_mesh_schism: max number Elements on one proc. maxel=',maxel


 !#### read global_to_local.prop #################################################################!
      !integer,save,allocatable :: iegl2(:,:)      ! Global-to-local element index table (2-tier augmented)
      if(allocated(iegl2)) deallocate(iegl2); allocate(iegl2(2,ne_global)); iegl2=0
      !integer,save,allocatable :: iegrpv(:)    ! Global element to resident processor vector
      if(allocated(iegrpv)) deallocate(iegrpv); allocate(iegrpv(ne_global)) ; iegrpv=0
      allocate(ipgl_rank(np_global)) ; allocate(ipgl_id(np_global))
      call mpi_barrier (mpi_komm_welt, ierr)
      if(meinrang==0) then
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/global_to_local.prop"
         open(32,file=trim(dateiname),status='unknown')
         do i=1,ne_global
            !read(32,'(i8,1x,i6,1x,i6,1x,i6)')j,nummy,iegl2(1,i),iegl2(2,i)
            !write(32,'(i8,1x,i6,1x,i6,1x,i6)')ie,iegrpv(ie),iegl2(1,ie),iegl2(2,ie)
            read(32,*)j,iegrpv(i),iegl2(1,i),iegl2(2,i)
         enddo !i
         do i=1,np_global
            read(32,*)j, ipgl_rank(i), ipgl_id(i)
            !read(32,'(i8,1x,i6,1x,i6)')j, ipgl_rank(i), ipgl_id(i)
         enddo !i
         close(32)
      endif ! meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)
      call MPI_Bcast(ipgl_rank,np_global,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(ipgl_id,np_global,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(iegrpv,ne_global,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(iegl2,2*ne_global,MPI_INT,0,mpi_komm_welt,ierr)
      rank_min=777777 ; rank_max=-777777 ; id_min=777777 ; id_max=-777777 ;j=0
      do i=1,np_global
         if(rank_min.gt.ipgl_rank(i))rank_min=ipgl_rank(i)
         if(rank_max.lt.ipgl_rank(i))rank_max=ipgl_rank(i)
         if(ipgl_rank(i).eq.meinrang)then
            j=j+1
            if(id_min.gt.ipgl_id(i))id_min=ipgl_id(i)
            if(id_max.lt.ipgl_id(i))id_max=ipgl_id(i)
         endif
      enddo !i
      !print*,meinrang,' global_to_local.prop ranks=',rank_min,rank_max,' id=',j,id_min,id_max,np,npa
      call mpi_reduce(j,k,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(np,m,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      call mpi_reduce(npa,n,1,MPI_INT,mpi_sum,0,mpi_komm_welt,ierr)
      if(meinrang==0)print*,'mpi_sum=',k,m,n,'np_global',np_global

      !check partition consitency
      j=0
      do i=1,ne
         if(iegrpv(ielg(i)).ne.meinrang)then
            j=j+1
            print*,meinrang,' element rank mismatch local=',i,' global=',ielg(i),' iegrpv=',iegrpv(ielg(i))
         endif
      enddo
      print*,meinrang,' global-local element-rank missmatch ',j,' of ',ne

!#### allocate mesh properties QSim ###########################################################################!
      if (meinrang == 0) then !! nur prozessor 0
         allocate(ne_sc(proz_anz), np_sc(proz_anz), ns_sc(proz_anz),stat = istat)
         if (istat /= 0) call qerror('Allocation error: np')
         ! nodes knoten
         allocate( iplg_sc(proz_anz,knotenanzahl2D) )
         allocate (knoten_x(knotenanzahl2D),knoten_y(knotenanzahl2D),knoten_z(knotenanzahl2D),        &
                  knoten_zone(knotenanzahl2D),knoten_rang(knotenanzahl2D),knot_ele(knotenanzahl2D),   &
                  knoten_rand(knotenanzahl2D),knoten_flaeche(knotenanzahl2D),                         &
                  knoten_trocken(knotenanzahl2D),knot_kant(knotenanzahl2D),stat = istat )
         knoten_z=-777777.7 ; knoten_flaeche=0.0
         knoten_zone=0; knoten_rang=0; knoten_rand=0; knot_ele=0; knoten_trocken=0 ; knot_kant=0
         allocate( kbp00_global(knotenanzahl2D) ,stat = istat)
         if (istat /= 0) call qerror('Allocation error: kbp00_global')
         ! elements
         allocate( ielg_sc(proz_anz,number_plankt_point) ) !! too large, who cares?
         allocate (element_x(number_plankt_point), element_y(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (element_x failed')
         allocate (element_rand(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (element_rand failed')
         element_rand(:)=0
         if(.not.allocated(inflow))allocate (inflow(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (inflow failed')
         inflow(:)=.false.
         allocate (element_rang(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (element_rang failed')
         element_rang(:)=0
         allocate (element_zone(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (element_zone failed')
         element_zone(:)=-1
         allocate (cornernumber(number_plankt_point), stat = istat )
         if (istat /= 0) call qerror('allocate (cornernumber( failed')
         allocate (elementnodes(number_plankt_point,4), stat = istat )
         if (istat /= 0) call qerror('allocate (elementnodes( failed')
         elementnodes(:,:)=-1
         allocate (elementedges(number_plankt_point,4), stat = istat )
         if (istat /= 0) call qerror('allocate (elementedges( failed')
         elementedges(:,:)=-1
         allocate(element_trocken(number_plankt_point),stat=istat);
         if (istat /= 0) call qerror('allocate element_trocken( failed')
         element_trocken=0

         ! edges kanten
         allocate( islg_sc(proz_anz,kantenanzahl) )
         allocate( nm2(4,number_plankt_point),nm3(4,number_plankt_point), stat = istat)
         allocate( top_node(kantenanzahl), bottom_node(kantenanzahl), stat = istat)
         top_node(:) = -1; bottom_node(:) = -1
         allocate( left_element(kantenanzahl),right_element(kantenanzahl), stat = istat)
         left_element(:) = -1; right_element(:) = -1
         allocate( edge_normal_x(kantenanzahl), edge_normal_y(kantenanzahl), &
                   cell_bound_length(kantenanzahl), stat = istat)
         edge_normal_x(:) = -77.7; edge_normal_y(:) = -77.7; cell_bound_length(:) = -77.7
         allocate(ed_vel_x(kantenanzahl),ed_vel_y(kantenanzahl),stat=istat)
         if (istat /= 0) call qerror('allocate ed_vel_( failed')
         ed_vel_x=0.0; ed_vel_y=0.0

         print*,'read_mesh_schism: allocated QSim arrays'
      endif ! meinrang==0
      
!#### Reconstruct connectivity table ###########################################################################!
   call mpi_barrier (mpi_komm_welt, ierr)
   maxstack=0
   if (meinrang == 0) then !! prozessor 0 only
      ! reread on process 0
      write(dateiname,'(4A)')trim(modellverzeichnis),'outputs_schism','/','local_to_global_000000'
      lfdb = len_trim(dateiname)
      do irank = 1,proz_anz,1 ! all ranks
         write(dateiname(lfdb-5:lfdb),'(i6.6)') irank-1
         open(10, file = dateiname, status = 'old', iostat = istat)
         print*,"read_mesh_schism reread:  dateiname ",trim(dateiname),' ',irank,' of ',proz_anz
         if (istat /= 0) call qerror('read_mesh_schism reread: open 10 failed')
         if ( .not. zeile(10))call qerror('Lesefehler 1')
         if ( .not. zeile(10))call qerror('Lesefehler 2')
         read(10,*)ne_l,nea_l,nea2_l
         do k = 1,ne_l
            read(10,*)j,ielg_sc(irank,k)
            element_rang(ielg_sc(irank,k)) = irank-1 ! element_rang(ielg_sc(irank,k))+1  !
         enddo !k
         read(10,*)np_l,npa_l
         !print*,'irank,np_l=',irank,np_l,trim(adjustl(dateiname))
         do k = 1,np_l
            read(10,*)j,iplg_sc(irank,k)
            knoten_rang(iplg_sc(irank,k)) = irank-1 ! knoten_rang(iplg_sc(irank,k))+1 ! 
         enddo !k
         read(10,*)ns_l,nsa_l
         do k = 1,ns_l
            read(10,*)j,islg_sc(irank,k)
         enddo !k
         ! print*,irank,"read_mesh_schism: np_l,ne_l,ns_l=",np_l,ne_l,ns_l
         ! take care that maxstack is large enough for everything !ne_l,nea_l,nea2_l, np_l,npa_l, ns_l,nsa_l
         if (maxstack < nea_l)maxstack = nea_l
         if (maxstack < npa_l)maxstack = npa_l
         if (maxstack < nsa_l)maxstack = nsa_l
         ! Header:
         if ( .not. zeile(10))call qerror('get_local_to_global Header missing')!read(10+meinrang,*) !'Header:'
         read(10,*)start_year,start_month,start_day,start_hour,utc_start
         if (irank == 1)print*,'Start Date = ',start_year,start_month,start_day,start_hour,utc_start
         jahr = start_year
         monat = start_month
         tag = start_day
         stunde = int(start_hour)
         minute = int( start_hour*60.0 - int(start_hour)*int(60) )
         sekunde = int( start_hour*3600.0 - int(start_hour)*int(3600) )
         ! version='v10'
         read(10,*)nummy,dummy,nummy,nnn,kkk  ! ,h0,h_s,h_c,theta_b,theta_f,ics
         ! read(10+meinrang,*)nrec,real(dt*nspool),nspool,nvrt,kz,h0,h_s,h_c,theta_b,theta_f,ics
         !read(10,*)(ztot(k),k = 1,kz-1),(sigma(k),k = 1,nvrt-kz+1)
         do k = 1,kkk-1
            read(10,'(A)')textline
         enddo
         do k = 1,nnn-kkk+1
            read(10,'(A)')textline
         enddo
         read(10,*)np_sc(irank),ne_sc(irank)
         if (np_sc(irank) /= np_l) call qerror('np_sc(irank) /= np_l')
         if (ne_sc(irank) /= ne_l) call qerror('ne_sc(irank) /= ne_l')
         ns_sc(irank)=ns_l
         print*,irank,'read_mesh_schism: np_sc,ne_sc,ns_sc=',np_sc(irank),ne_sc(irank),ns_sc(irank)

         do m=1,np_sc(irank)
            read(10,*,iostat = istat)knoten_x(iplg_sc(irank,m)), knoten_y(iplg_sc(irank,m)),     &
                                     knoten_z(iplg_sc(irank,m)), kbp00_global(iplg_sc(irank,m))
            if (istat /= 0)call qerror('read(10,*) ( knoten_x... failed')
         enddo !m points
         
         ! Reconstruct connectivity table
         do m=1,ne_sc(irank)  !m elemente
            j=ielg_sc(irank,m)
!           read(10+meinrang,*)i34(m),(elnode(mm,m),mm=1,i34(m))
            read(10,*,iostat = istat)cornernumber(j),(nm2(mm,m),mm = 1,cornernumber(j))
            if (istat /= 0)call qerror('read(10,*) ( cornernumber... failed')
!           read(10+meinrang,*)kbe(m),area(m),ic3(1,m),ic3(2,m),ic3(3,m),ic3(4,m)
            read(10,'(A)',iostat = istat)textline
!           read(10+meinrang,*)elside(1,m),elside(2,m),elside(3,m),elside(4,m),ssign(1,m),ssign(2,m),ssign(3,m),ssign(4,m)
            read(10,*,iostat = istat)nm3(1,m),nm3(2,m),nm3(3,m),nm3(4,m)
            if (istat /= 0)call qerror('read_mesh_schism: elside nm3 elementedges ... failed')
!           #!  integer,save,allocatable :: elnode(:,:)      ! Element-node tables
!           #!  integer,save,allocatable :: elside(:,:)             ! Element-side tables
!           #!   integer , allocatable , dimension (:,:) :: elementnodes, elementedges
            do mm = 1,cornernumber(j)
               i = nm2(mm,m) ! local node number
               k = nm3(mm,m) ! local side number
               if (i > np_sc(irank) .or. i <= 0) then
                  print*,',nm2(1....4,',m,')) = ',nm2(1,m),nm2(2,m),nm2(3,m),nm2(4,m)
                  print*,',i,np(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j) = ',  &
                         i,np_sc(irank),irank,nm2(mm,m),mm,m,j,cornernumber(j)
                  call qerror('cornernumber error in read_mesh_schism')
               endif
               elementnodes(j,mm) = iplg_sc(irank,i)
               elementedges(j,mm) = islg_sc(irank,k)
            enddo ! all mm element corners
         enddo !m elemente
         do i=1,ns_sc(irank) !i edges/sides
            read(10,*,iostat = istat)nummy,tn,bn
!###        read(10+meinrang,*)j,isidenode(1:2,j)
            if(istat /= 0)call qerror('read_mesh_schism: read(10,*)nummy,tn,bn... failed')
            if(nummy /= i)then
               print*,'read_mesh_schism: wrong edge/side',i,nummy,tn,bn
               call qerror('read_mesh_schism: wrong edge/side number')
            endif
            if (istat /= 0)call qerror('read(10,*) ( distj(i),delj(i)... failed')
            if ((tn <= 0) .or. (tn > np_sc(irank)))print*,irank,tn,np_sc(irank)," tn wrong"
            if ((bn <= 0) .or. (bn > np_sc(irank)))print*,irank,bn,np_sc(irank)," bn wrong"
            top_node(   islg_sc(irank,i)) = iplg_sc(irank,tn)
            bottom_node(islg_sc(irank,i)) = iplg_sc(irank,bn)
            read(10,*,iostat = istat)dummy,dummy,dummy,dummy,dummy,dummy,tn,bn
            left_element( islg_sc(irank,i)) = ielg_sc(irank,tn)
            right_element(islg_sc(irank,i)) = ielg_sc(irank,tn)
            !#!###        read(10+meinrang,*)distj(i),delj(i),snx(i),sny(i),isbs(i),kbs(i),isdel(1,i),isdel(2,i)
            !#!  integer,save,allocatable :: isdel(:,:)             ! Side-element tables
            !#!  integer,save,allocatable :: isidenode(:,:)      ! Side-node tables
            !#!   integer , allocatable , dimension (:) :: top_node,bottom_node,  left_element,right_element
            !!! vnor1=su2(k,j)*snx(j)+sv2(k,j)*sny(j)
            !!! vnor2=su2(k-1,j)*snx(j)+sv2(k-1,j)*sny(j)
            !!! flux_adv_hface(k,j)=(zs(k,j)-zs(k-1,j))*distj(j)*(vnor1+vnor2)/2 !normal * area = flux (in local x-direction)
         enddo !i edges/sides
         close(10)
      enddo ! all irank
      deallocate( nm2 ); deallocate( nm3 )
      
      do n = 1,knotenanzahl2D !! check if all nodes are read
         if(knoten_z(n)<-15000.0)then
            print*,n, ' strange node (read_mesh_schism) ',knoten_x(n),knoten_y(n),knoten_z(n)
            call qerror('point error in read_mesh_schism')
         endif
      enddo ! all n points
      do n = 1,kantenanzahl !! check integrity of side-node connectivity
         if ((top_node(n) <= 0) .or. (top_node(n) > knotenanzahl2D))print*,"top_node(",n,")wrong",top_node(n)
         if ((bottom_node(n) <= 0) .or. (bottom_node(n) > knotenanzahl2D))print*,"bottom_node(",n,")wrong",bottom_node(n)
         knot_kant(top_node(n))=knot_kant(top_node(n))+1
         knot_kant(bottom_node(n))=knot_kant(bottom_node(n))+1
      end do ! all n sides=edges
      summ_ne = 0 ! needed by vtk output
      do n = 1,number_plankt_point! all n elements
         summ_ne = summ_ne+cornernumber(n)+1
         do j=1,cornernumber(n)
            knot_ele(elementnodes(n,j))=knot_ele(elementnodes(n,j))+1
         end do ! all j corners
      end do ! all n elements
      
      !distmax = -999999999999.9
      !distmin = 999999999999.9
      !do n = 1,kantenanzahl ! all edges
      !   if (distmax <= cell_bound_length(n))distmax = cell_bound_length(n)
      !   if (distmin >= cell_bound_length(n))distmin = cell_bound_length(n)
      !end do ! all n edges/sides
      print*,'local_to_global_0000:'
      print*,'x-koordinate max+min', maxval(knoten_x),minval(knoten_x)
      print*,'y-koordinate max+min', maxval(knoten_y),minval(knoten_y)
      print*,'Sohlhöhe max+min', maxval(knoten_z),minval(knoten_z)
      print*,'maxstack = ',maxstack
      print*,'elements at nodes max+min', maxval(knot_ele),minval(knot_ele)
      !print*,'edge length distmax+distmin', distmax, distmin
   end if !! prozess 0 only
   call MPI_Bcast(maxstack,1,MPI_INT,0,mpi_komm_welt,ierr)
   
!#### do aquire_hgrid ################################################################################!
      if (meinrang == 0)then
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/hgrid.gr3"
         open(14,file = trim(dateiname),status = 'old',iostat = istat)
         if (istat /= 0)then
            print*,'mesh information expected on outputs_schism/hgrid.gr3 is missing'
            write(fehler,*)'read_mesh_schism: hgrid.gr3 open failure'
            call qerror(fehler)
         endif
         close(14)
      endif !meinrang == 0
      call mpi_barrier (mpi_komm_welt, ierr)
      ! nxq Cyclic index of nodes in an element (tri/quads) from schism_init
      do k=3,4 !elem. type
        do i=1,k  !local index
          do j=1,k-1 !offset
            nxq(j,i,k)=i+j
            if(nxq(j,i,k)>k) nxq(j,i,k)=nxq(j,i,k)-k
            if(nxq(j,i,k)<1.or.nxq(j,i,k)>k) then
              write(fehler,*)'INIT: nxq wrong',i,j,k,nxq(j,i,k)
              call qerror(fehler)
            endif
          enddo !j
        enddo !i
      enddo !k
      write(in_dir,"(2A)")trim(modellverzeichnis),"outputs_schism/"
      len_in_dir=len(in_dir)
      comm=mpi_komm_welt
      myrank=meinrang
      write(dateiname,"(2A)")trim(modellverzeichnis),"aquire_hgrid_output.txt"
      open(16,file = trim(dateiname))
      
      call aquire_hgrid(.true.)
      
      close(16)
      call mpi_barrier (mpi_komm_welt, ierr)
      if (meinrang == 0) print*,'aquire_hgrid finished in read_mesh_schism'

!#### read hgrid.gr3 into QSim data-structures ########################################################!
      if (meinrang == 0) then !! nur prozessor 0
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/hgrid.gr3"
         open(14,file = trim(dateiname),status = 'old',iostat = istat)
         if (istat /= 0)call qerror('read_mesh_schism: hgrid.gr3 open failure2')
         read(14,*); read(14,*) n,k
         if(n.ne.n_elemente)call qerror('hgrid.gr3 element number missmatch')
         if(k.ne.knotenanzahl2D)call qerror('hgrid.gr3 node number missmatch')
         do i = 1,knotenanzahl2D
            read(14,*)n,knoten_x(i),knoten_y(i),knoten_z(i)
            if (n /= i) call qerror('reading hgrid.gr3 nodes: something gone wrong')
         enddo
         xmax = -999999999999.9 ; xmin = 999999999999.9
         ymax = -999999999999.9 ; ymin = 999999999999.9
         zmax = -99999999       ; zmin = 99999999
         do i = 1,knotenanzahl2D
            if (xmax <= knoten_x(i))xmax = knoten_x(i)
            if (xmin >= knoten_x(i))xmin = knoten_x(i)
            if (ymax <= knoten_y(i))ymax = knoten_y(i)
            if (ymin >= knoten_y(i))ymin = knoten_y(i)
            knoten_z(n) = knoten_z(n)*(-1.0) ! bathymety elevation upwards in QSim
            if (zmax <= knoten_z(i))zmax = knoten_z(i)
            if (zmin >= knoten_z(i))zmin = knoten_z(i)
         end do ! alle i Knoten
         print*,'hgrid.gr3 bottom left=',xmin,ymin,' top right=',xmax,ymax,' z max/min ',zmax, zmin
         
         ! re-checking connectivity table
         do i = 1,n_elemente
            read(14,*,iostat = istat)n,j,(eck(mm),mm=1,j)
            if (istat /= 0) call qerror('read_mesh_schism: hgrid.gr3 element reading failure')
            if (n /= i) call qerror('reading hgrid.gr3 elements: something gone wrong')
            if(j.ne.cornernumber(i)) call qerror('cornernumber mismatch')
            do mm=1,j
               if(elementnodes(i,mm).ne.eck(mm))call qerror('read_mesh_schism: hgrid.gr3 elementnodes mismatch')
            enddo !mm
         enddo
         
         ! get boundaries
         read(14,*,iostat = istat)nnn
         print*,'Number of open boundaries=',nnn
         min_rand=1;max_rand=nnn
         read(14,*,iostat = istat)n
         print*,'Total number of open boundary nodes=',n
         knoten_rand(:)=0
         do i = 1,nnn
            read(14,*,iostat = istat)kkk
            print*,'Number of nodes for open boundary ',i,'=',kkk
            do j = 1,kkk
               read(14,*)k
               knoten_rand(k)=i
            enddo ! all kkk nodes
         enddo ! all nn open boundaries
         read(14,*,iostat = istat)m
         print*,'Number of land boundaries=',m
         read(14,*,iostat = istat)n
         print*,'Total number of land boundary nodes=',n
         do i = 1,m
            read(14,*,iostat = istat)kkk
            print*,'Number of nodes for land boundary ',i,i+nnn,'=',kkk
            do j = 1,kkk
               read(14,*,iostat = istat)k
               if (istat /= 0) call qerror('read_mesh_schism: hgrid.gr3  nodes for land boundary reading failure')
               !knoten_rand(k)=i+nnn
            enddo ! all kkk nodes
         enddo ! all nn open boundaries
         
         ! Elements only open boundaries!! ### ausrangiert ###
         element_rand(:)=0
         do i = 1,n_elemente
            k=0;j=0
            do mm=1,cornernumber(i)
               if(knoten_rand(elementnodes(i,mm)).gt.0)then
                  k=k+1
                  j=j+knoten_rand(elementnodes(i,mm))
               endif
            enddo !mm
            if(k.eq.2)then ! boundary-element must have two nodes on boundary
               do mm=1,cornernumber(i)
                  if(knoten_rand(elementnodes(i,mm)).gt.0)then
                     element_rand(i) = knoten_rand(elementnodes(i,mm))
                  endif
               enddo
               ! boundary-elements must have boundary-nodes on same boundary
               if (element_rand(i).ne.(j/2))element_rand(i)=0
            endif !k==2
         enddo ! all i elements
         
         ! wie schism, Randelement braucht nur einen Knoten auf dem Rand
         element_rand(:)=0
         inflow(:)=.false.
         do i = 1,n_elemente
            do mm=1,cornernumber(i)
               if(knoten_rand(elementnodes(i,mm)).gt.0) element_rand(i) = knoten_rand(elementnodes(i,mm))
            enddo
            if(element_rand(i)>0)inflow(i)=.true.
         enddo ! all i elements

         close(14)
      end if !! prozess 0 only
      call MPI_Bcast(min_rand,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(max_rand,1,MPI_INT,0,mpi_komm_welt,ierr)

      ! check node locations
      call MPI_Bcast(knotenanzahl2D,1,MPI_INT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(n_elemente,1,MPI_INT,0,mpi_komm_welt,ierr)
      if(.not.allocated(knoten_x)) allocate (knoten_x(knotenanzahl2D))
      if(.not.allocated(knoten_y)) allocate (knoten_y(knotenanzahl2D))
      call MPI_Bcast(knoten_x,knotenanzahl2D,MPI_FLOAT,0,mpi_komm_welt,ierr)
      call MPI_Bcast(knoten_y,knotenanzahl2D,MPI_FLOAT,0,mpi_komm_welt,ierr)
      do i=1,np
         dx=knoten_x(iplg(i))-xnd(i)
         dy=knoten_y(iplg(i))-ynd(i)
         if(sqrt(dx**2+dy**2).gt. 1.0)  &
            print*,meinrang,'ii=',i,iplg(i),'xx,yy=',knoten_x(iplg(i)),xnd(i),knoten_y(iplg(i)),ynd(i)
      enddo
      
      ! element center approximation
      call mpi_barrier (mpi_komm_welt, ierr)
      if (meinrang == 0) then !! nur prozessor 0
         element_x(:)=0.0
         element_y(:)=0.0
         do i=1,n_elemente
            do j=1,cornernumber(i)
               element_x(i)=element_x(i)+knoten_x(elementnodes(i,j))
               element_y(i)=element_y(i)+knoten_y(elementnodes(i,j))
            enddo ! all j corners
            element_x(i)=element_x(i)/(real(cornernumber(i)))
            element_y(i)=element_y(i)/(real(cornernumber(i)))
         enddo ! all i elements
      end if !! prozess 0 only
      
!#### check boundaries and set their properties ###########################################!

      !Hydro/grid_subs.F90:  ! isbs >0 if on open bnd (points to global segment #); =-1 if land bnd; =0 if internal
      if(.not.allocated(knoten_rand)) allocate (knoten_rand(knotenanzahl2D))
      call MPI_Bcast(knoten_rand,knotenanzahl2D,MPI_FLOAT,0,mpi_komm_welt,ierr)
      do i=1,ns
         if(isbs(i).gt.0)then
            if(knoten_rand(iplg(isidenode(1,i))).ne.isbs(i))call qerror('read_mesh_schism boundary number mismatch1')
            if(knoten_rand(iplg(isidenode(2,i))).ne.isbs(i))call qerror('read_mesh_schism boundary number mismatch2')
         endif ! open boundary side/edge
      enddo ! all i sides/edges
      !if(meinrang==0)then
      !   do i=1,knotenanzahl2D
      !      if(knoten_rand(i).gt.0)print*,i,'knoten_rand(i)',knoten_rand(i)
      !   enddo
      !endif!meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)
      
      ! apply element boundary marker
      allocate(isbe(ne),stat=istat)
      if(istat/=0) call qerror('read_mesh_schism: failed in alloc. isbe')
      do i=1,ne
         isbe(i)=0
         do j=1,i34(i)
            ! isbe(ie)=1 if any node of element ie lies on bnd; isbe(ie)=0 otherwise
            if(knoten_rand(iplg(elnode(j,i))) .gt. 0) isbe(i)=1
         enddo
         !if(isbe(i).ne.0)print*,meinrang,' Element=',i,ielg(i),' isbe=',isbe(i)
      enddo ! all i elements
      !if(meinrang==0)then
      !   do i = 1,n_elemente
      !      if(element_rand(i).ne.0)print*,'0 Element=',i,' element_rand=',element_rand(i)
      !   enddo
      !endif!meinrang==0
      call mpi_barrier (mpi_komm_welt, ierr)
      ! isbnd(:,:) ! local node to _global_ open bndry segment flags
      if(allocated(isbnd)) deallocate(isbnd); allocate(isbnd(-2:2,npa),stat=istat);
      if(istat/=0) call qerror('read_mesh_schism: isbnd allocation failure')
      isbnd=0
      !#grid_subs.f90 2230 ...
      do i=1,np
         isbnd(1,i)=knoten_rand(iplg(i)) ! boundary #
         isbnd(-1,i)=1                   ! only needs to work together with itrtype(jj,ibnd)==1
         !if(isbnd(1,i)>0)print*,meinrang,'read_mesh_schism: i,isbnd,iplg',i,isbnd(1,i),iplg(i)
      enddo
      !check side-node boundary integrity
      !Hydro/grid_subs.F90:  ! isbs >0 if on open bnd (points to global segment #); =-1 if land bnd; =0 if internal
      do i=1,ne
         do j=1,i34(i)
            if(isbs(elside(j,i))>0)then ! open boundaries only
               do ll=1,2 ! both nodes of each element side/edge
                  !do lll=1,2 !2 possible bnds
                  lll=1 ! only first boundary present
                  !if( isbnd(lll,isidenode(ll,elside(j,i))) /= isbs(elside(j,i)) ) then
                  !   print*,meinrang,'read_mesh_schism: isbnd\=isbs',  &
                  !   i,j,ll,elside(j,i),isidenode(ll,elside(j,i)),isbnd(lll,isidenode(ll,elside(j,i))),isbs(elside(j,i))
                  !endif
                  !enddo !lll
                  if(isbnd(1,isidenode(ll,elside(j,i))) /= isbs(elside(j,i)))then
                     print*,meinrang,'read_mesh_schism: isbnd /= isbs',  &
                     isbnd(1,isidenode(ll,elside(j,i))),isbs(elside(j,i)),i,j,ll,  &
                     elside(j,i),isidenode(ll,elside(j,i)),iplg(isidenode(ll,elside(j,i)))
                  endif ! 
               enddo ! ll
            endif ! isbs>0
         enddo ! j
      enddo ! i

      ! set boundary properties itrtype=1 - time series uniform along boundary ; trobc=1.0 - no nudging
      ! so tr_nd0 is not used, as needed only for itrtype=3 and intenionally left unallocated
      
      ntracers=number_plankt_vari ! number of concentrations , ntr !# of tracers (=ntracers)
      
      if(allocated(itrtype)) deallocate(itrtype); allocate(itrtype(natrm,max_rand),stat=istat);
      if(istat/=0) call qerror('read_mesh_schism: itrtype allocation failure')
      itrtype=1
      if(allocated(trobc)) deallocate(trobc); allocate(trobc(natrm,max_rand),stat=istat);
      if(istat/=0) call qerror('read_mesh_schism: trobc allocation failure')
      trobc=1.0
      irange_tr(2,:)=0 ! irange_tr(2,natrm) in schism_glbl
      irange_tr(1,:)=1 
      irange_tr(2,1)=ntracers ! all planctonic variables in first model
      ntrs=0
      ntrs(1)=ntracers
      
!#### set concentration and transport scheme properties ###########################################!

      ! real(rkind),save,allocatable :: wsett(:,:,:) 
      ! integer,save,allocatable :: iwsett(:) !iwsett(ntracers)
      if(allocated(wsett)) deallocate(wsett); allocate(wsett(ntracers,nvrt,nea),stat=istat);
      if(istat/=0) call qerror('read_mesh_schism: wsett allocation failure')
      wsett=0.0 ! no settling velocity
      if(allocated(iwsett)) deallocate(iwsett); allocate(iwsett(ntracers),stat=istat);
      if(istat/=0) call qerror('read_mesh_schism: iwsett allocation failure')
      iwsett=0  ! settling not activated
      
      saltmax=35.0
      saltmin=0.0
      tempmax=35.0
      tempmin=0.0
     
      !logical,save,allocatable :: isten_qual2(:)
      if(allocated(isten_qual2)) deallocate(isten_qual2); allocate(isten_qual2(ne),stat=istat)
      if(istat/=0) call qerror('read_mesh_schism: isten_qual2 allocation failure')
      isten_qual2=.true. ! assuming all sides have qualified stencils | subroutine CheckSten2

!#### read manning.gr3 ###########################################################################!
      if (meinrang == 0) then !! nur prozessor 0
         write(dateiname,"(2A)")trim(modellverzeichnis),"outputs_schism/manning.gr3"
         open(14,file = trim(dateiname),status = 'old',iostat = istat)
         !open(14,file = 'zone.gr3',status = 'old',iostat = istat)
         if (istat /= 0)then
            print*,'friction coefficients (Mannings n ~ bottom roughness)'
            print*,'expected on outputs_schism/manning.gr3 is missing'
            write(fehler,*)'read_mesh_schism: manning.gr3 open failure'
            call qerror(fehler)
         endif
         if(allocated(knoten_manni)) deallocate(knoten_manni); allocate(knoten_manni(knotenanzahl2D))
         read(14,*); read(14,*) n,k
         if(n.ne.n_elemente)call qerror('manning.gr3 element number missmatch')
         if(k.ne.knotenanzahl2D)call qerror('manning.gr3 node number missmatch')
         !print*,'read_mesh_schism: manning.gr3: n_elemente,knotenanzahl2D = ',n,k
         do i = 1,knotenanzahl2D
            read(14,*)n,xummy,yummy,knoten_manni(i)
            if (n /= i) call qerror('reading manning.gr3 nodes: something gone wrong')
         enddo
         zmax = -99999999       ; zmin = 99999999
         do i = 1,knotenanzahl2D
            if (zmax <= knoten_manni(i))zmax = knoten_manni(i)
            if (zmin >= knoten_manni(i))zmin = knoten_manni(i)
         end do ! alle i Knoten
         print*,'manning.gr3 Mannings n max/min=',zmax, zmin
         close(14)
      end if !! prozess 0 only
      
      call mpi_barrier (mpi_komm_welt, ierr)
      if (meinrang == 0) print*,'read_mesh_schism finished'
      return
end subroutine read_mesh_schism
      
