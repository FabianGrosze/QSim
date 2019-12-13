    ! Loop over records in file
    do ispool=1,nrec
      !Gather all ranks
      do irank=0,nproc-1
        !Open input file
        write(a_4,'(i4.4)') irank
        file63='schout_'//a_4//'_'//it_char(1:it_len)//'.nc'
        file63=adjustl(file63)
        iret=nf90_open(trim(file63),OR(NF90_NETCDF4,NF90_NOWRITE),ncid2)
        !write(99,*)'nvars=',nvars,file63

        do m=1,nvars
          ! get variable id (maybe not necessary)
          iret = nf90_inq_varid(ncid2,variable_nm(m),varid)
          !if (iret/=NF90_NOERR) then
          !  write(0,*) trim(file63),trim(nf90_strerror(iret))
          !  stop
          !endif
          !! alternatively use same id as in first file:
          !varid = m

          if(i23d(m)<=3) then !node
            data_count_2d(1)=np(irank)
            data_count_3d(2)=np(irank)
            data_count_4d(3)=np(irank)
          else if(i23d(m)<=6) then
            data_count_2d(1)=ne(irank)
            data_count_3d(2)=ne(irank)
            data_count_4d(3)=ne(irank)
          else
            data_count_2d(1)=ns(irank)
            data_count_3d(2)=ns(irank)
            data_count_4d(3)=ns(irank)
          endif

          if(i23d(m)==0) then !time
            data_start_1d(1)=ispool; data_count_1d(1)=1
            iret=nf90_get_var(ncid2,varid,hc_array,data_start_1d,data_count_1d) 
            time=hc_array(1)
            !write(99,*)'time=',time
          else if(mod(i23d(m)-1,3)==0) then !2D
            if(ivs(m)==1) then
              data_start_2d(1)=1; data_start_2d(2)=ispool 
              data_count_2d(2)=1
              iret=nf90_get_var(ncid2,varid,worka(1,1,1:data_count_2d(1)),data_start_2d,data_count_2d)
            else !vector
              data_start_3d(1:2)=1; data_start_3d(3)=ispool 
              data_count_3d(1)=2; data_count_3d(3)=1
              iret=nf90_get_var(ncid2,varid,worka(1:2,1,1:data_count_3d(2)),data_start_3d,data_count_3d)
            endif !ivs
          else !3D
            if(ivs(m)==1) then
              data_start_3d(1:2)=1; data_start_3d(3)=ispool
              data_count_3d(1)=nvrt; data_count_3d(3)=1
              iret=nf90_get_var(ncid2,varid,worka(1,:,1:data_count_3d(2)),data_start_3d,data_count_3d)
            else !vector
              data_start_4d(1:3)=1; data_start_4d(4)=ispool
              data_count_4d(1)=2; data_count_4d(2)=nvrt; data_count_4d(4)=1
              iret=nf90_get_var(ncid2,varid,worka(1:2,:,1:data_count_4d(3)),data_start_4d,data_count_4d)
            endif !ivs
          endif !i23d

          !Put into global index
          if(i23d(m)==0) then !do nothing
          else if(i23d(m)<=3) then !node
            do i=1,np(irank)
              nd=iplg(irank,i)
              outd(m)%data(1:2,:,nd)=worka(1:2,1:ubound(outd(m)%data,2),i)
            enddo !i
          else if(i23d(m)<=6) then
            do i=1,ne(irank)
              ie=ielg(irank,i)
              outd(m)%data(1:2,:,ie)=worka(1:2,1:ubound(outd(m)%data,2),i)
            enddo !i
          else
            do i=1,ns(irank)
              isd=islg(irank,i)
              outd(m)%data(1:2,:,isd)=worka(1:2,1:ubound(outd(m)%data,2),i)
            enddo !i
          endif !i23d
        enddo !m=1,nvars
        iret=nf90_close(ncid2)
      enddo !irank

