!---------------------------------------------------------------------------------------
!
!   QSim - Programm zur Simulation der Wasserqualität
!
!   Copyright (C) 2020 Bundesanstalt für Gewässerkunde, Koblenz, Deutschland, http://www.bafg.de
!
!   Dieses Programm ist freie Software. Sie können es unter den Bedingungen der 
!   GNU General Public License, Version 3,
!   wie von der Free Software Foundation veröffentlicht, weitergeben und/oder modifizieren. 
!   Die Veröffentlichung dieses Programms erfolgt in der Hoffnung, daß es Ihnen von Nutzen sein wird, 
!   aber OHNE IRGENDEINE GARANTIE, sogar ohne die implizite Garantie der MARKTREIFE oder der VERWENDBARKEIT FÜR EINEN BESTIMMTEN ZWECK. 
!   Details finden Sie in der GNU General Public License.
!   Sie sollten ein Exemplar der GNU General Public License zusammen mit diesem Programm erhalten haben. 
!   Falls nicht, siehe http://www.gnu.org/licenses/.  
!   
!	Programmiert von:
!	1979 bis 2018 Volker Kirchesch
!	seit 2011 Jens Wyrwa, Wyrwa@bafg.de
!
!---------------------------------------------------------------------------------------

    subroutine funkstar(abfls,vbsbs,vcsbs,vnh4s,vno2s,vno3s,gesNs,vx0s,vx02s,gelps,gesPs,sis,chlas,vkigrs               &
                    ,antbls,zooins,vphs,mws,cas,lfs,ssalgs,tempws,vo2s,CHNFs,BVHNFs,colis,DOSCFs,waers                &         
                    ,ischwer,glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis,istund                                 &
                    ,uhrz,RBtyp,NRSCHr,itags,monats,jahrs,cpfad,iwsim,ilang,iwied,mstrRB,azStrs,i_Rands               &
                    ,iw_max,iformVert,ifehl,ifmRB,ifmstr)

!   UNTERPROGRAMM ZUR Interpolation der Randbedingungen                   
!   AUTOR: VOLKER KIRCHESCH                                           
!   STAND: 03.01.2013  
!   variable Anzahl Randbedingungszeilen. Wyrwa Juli 2020
                 
      integer, parameter :: maxstrang_rb=100
      character (len = 255)                       :: cpfad 
      character (len=275)                         :: pfadstring , dummy
      integer                                     :: azStrs,RBNR, read_error
      integer, Dimension(azStrs,maxstrang_rb)     :: istund, RBtyp, NRSchr 
!      integer, Dimension(50,200,30)              :: mREC 
      integer, Dimension(:,:,:), allocatable      :: mREC 
      real, Dimension(2)                          :: VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni
      real, Dimension(azStrs,maxstrang_rb)                 :: vbsbs,vcsbs, vnh4s, vno2s, vno3s, gesNs, vx0s, vx02s
      real, Dimension(azStrs,maxstrang_rb)                 :: gelps, gesPs, sis, chlas, waers 
      real, Dimension(azStrs,maxstrang_rb)                 :: vkigrs, antbls, zooins, vphs, mws, cas, lfs, ssalgs
      real, Dimension(azStrs,maxstrang_rb)                 :: tempws, vo2s, CHNFs, BVHNFs, colis, DOSCFs, abfls  
      real, Dimension(azStrs,maxstrang_rb)                 :: glZns,gsZns,glCads,gsCads,glCus,gsCus,glNis,gsNis  

      integer:: maxlines_bc, num
      !real, Dimension(200,40000)                 :: uhrl 
      real, Dimension(:,:), allocatable           :: uhrl 
      !integer, Dimension(200,40000)              :: itagl, monatl, jahrl 
      integer, Dimension(:,:) , allocatable       :: itagl, monatl, jahrl 
      real, Dimension(:,:,:), allocatable         :: werts
      !integer, Dimension(40000)                  :: imstr, iRBNR, ianzW 
      integer, Dimension(:) , allocatable         :: imstr, iRBNR, ianzW 
      double precision                            :: R_NRS, R_NRS2, R_NRS1

      save ianRBs, mREC, werts, ianzW, itagl, monatl,jahrl, Uhrl, iRBNR, imstr,R_NRS, R_NRS2, R_NRS1, VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni

!   Anmerkung ipp=28 ist die Tracerkonzentration. Wird auf den Parameter tempw gelegt
!   Anmerkung ipp=29 konserv. Substanz wird auf tempw gelegt 
!   Anmerkung ipps=37 Schwermetalle   
!   iwsim = 4 -> Tracer
!   iwsim = 5 -> konserv. Substanz                                                   
                                                                       
!      open(unit=19,file='funkstar.tst') 

      ianzRB = 0
      maxlines_bc = 0
      ipps = 29
      if(ischwer==1)ipps = 37
      if(.not.allocated(mREC))allocate(mREC(1:azStrs,1:i_Rands,1:ipps))
      
!.....Einlesen aus EREIGG / read steering parameters and boundary conditions from file EREIGG.txt
      if(ilang==0)then
         close (92) 
         write(pfadstring,'(2A)')trim(adjustl(cpfad)),'EREIGG.txt'
         open(unit=92, file=pfadstring)
         ! first read to evaluate block size...
         rewind (92) 
         read(92,'(2x)')  ! neglect header
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         do i_Rand = 1, 200 ! Randbedingungsschleife Beginn  / all boundary conditions    
            ! read(92,9230,iostat=read_error)mstr,RBNR,istund(mstr,RBNR),NrSchr(mstr,RBNR)                                                
            read(92,*,iostat=read_error)mstr,RBNR,istund(mstr,RBNR),NrSchr(mstr,RBNR),dummy                                               
            if(read_error<0.0)exit ! no further boundary
            print*,'funkstar read(92 mstr,RBNR,istund,NrSchr,ident',mstr,RBNR,istund(mstr,RBNR),NrSchr(mstr,RBNR),adjustl(trim(dummy))
            !....warning                                                    
            if(NrSchr(mstr,RBNR).gt.40000)then 
               write(199,1899)RBNR,mstr 
               1899 format(2x,'Warnung: fuer die ',I3,'.Randbedingung des ',I2,'. Strangs existieren mehr als 40000 Datensaetze')              
            endif 
            if(NrSchr(mstr,RBNR).gt.maxlines_bc)maxlines_bc=NrSchr(mstr,RBNR)
            if(NrSchr(mstr,RBNR).le.0)cycle 
            do iwe = 1,NrSchr(mstr,RBNR)      ! spool through   
               read(92,*,iostat=read_error)dummy
            enddo
         enddo ! all i boundary conditions / Randbedingungsschleife Ende
         if(i_Rand.ge.200)then
		    print*,'too many boundary conditions in EREIGG.txt'
			stop 44
         endif		 

         !allocate bc-arrays
         if(.not.allocated(imstr))allocate(imstr(i_Rands))
         if(.not.allocated(iRBNR))allocate(iRBNR(i_Rands))
         if(.not.allocated(ianzW))allocate(ianzW(i_Rands))
         if(.not.allocated(werts))allocate(werts(i_Rands,ipps,maxlines_bc))
         if(.not.allocated(itagl))allocate(itagl(i_Rands,maxlines_bc))
         if(.not.allocated(monatl))allocate(monatl(i_Rands,maxlines_bc))
         if(.not.allocated(jahrl))allocate(jahrl(i_Rands,maxlines_bc))
         if(.not.allocated(uhrl))allocate(uhrl(i_Rands,maxlines_bc))

         ! second read to evaluate block size...
         NrSchr(:,:)=-1 ! initialize
         rewind (92)
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)') 
         read(92,'(2x)')
         do i_Rand = 1, 200 ! Randbedingungsschleife Beginn  / all boundary conditions  
            !cdoppelbelegung einer Randbedingung abfangen !!       NrSchr      
            read(92,*,iostat=read_error)mstr,RBNR,istund(mstr,RBNR),num                                                
            if(read_error<0.0) exit ! no further boundary
            if(mstr.gt.azStrs)stop 77
            if(RBNR.gt.maxstrang_rb)stop 78
            if(NrSchr(mstr,RBNR).eq.-1)then ! boundary only once
                NrSchr(mstr,RBNR)=num
            else
               print*,"funkstar: Randbedingung doppelt"
               stop 79 
            endif! =num
            if(NrSchr(mstr,RBNR).le.0)cycle 
            ianzRB = ianzRB+1                 ! Summenbildung der Randbedingungen  
            if(ianzRB.gt.i_Rands)stop 80
            imstr(ianzRB) = mstr                
            iRBNR(ianzRB) = RBNR 
            ianzW(ianzRB) = NrSchr(mstr,RBNR) 
            do iwe = 1,NrSchr(mstr,RBNR)      ! Einlesen der Randbedingungswerte für den Strang <mstr>, hier Schleifenbeginn 
               if(ischwer==0)then !without heavy metals
                  read(92,*,iostat=read_error)itagl(ianzRB,iwe),monatl(ianzRB,iwe),jahrl(ianzRB,iwe),uhrl(ianzRB,iwe)   &
                              ,(werts(ianzRB,ixpp,iwe),ixpp=1,28)
                  if(read_error<0.0)print*,'funkstar() Einlesen der Randbedingungswerte für den Strang',ianzRB,iwe,mstr,RBNR,NrSchr(mstr,RBNR)
                  if(iwsim==4.and.werts(ianzRB,28,iwe)<0.0)werts(ianzRB,28,iwe) = 0.0
               else ! ischwer!=0
                  read(92,9240)itagl(ianzRB,iwe),monatl(ianzRB,iwe),jahrl(ianzRB,iwe),uhrl(ianzRB,iwe)   &
                               ,(werts(ianzRB,ixpp,iwe),ixpp=1,ipps)
               endif
               uhrl(ianzRB,iwe) = int(uhrl(ianzRB,iwe))+((uhrl(ianzRB,iwe)-int(uhrl(ianzRB,iwe)))/0.6)  !Umrechnung der "Messwert-Uhrzeit" in Dezimalschreibweise
            enddo ! all iwe bc-lines
            mREC(mstr,ianzRB,:) = 0 ! initialize
         enddo                                 ! Randbedingungsschleife Ende 
         ianRBs = ianzRB 

 9230    format(I5,2x,I5,2x,I1,2x,I5) 
 9240    format(i2,2x,i2,2x,I4,2x,f5.2,2x,f13.6,2x,f6.2,2x,f6.2,2x,f6.2,2x                 &
              ,f6.3,2x,f5.2,2x,f5.2,2x,f8.5,2x,f8.5,2x,f6.3,2x,f5.2,2x,f5.2               &
              ,2x,f6.2,2x,f5.2,2x,f5.2,2x,f7.1,2x,f5.2,2x,f5.2,2x,f5.1,2x                 &
              ,f8.1,2x,f7.2,2x,f5.2,2x,f5.2,2x,f8.1,2x,f6.1,2x,E9.2,2x,f7.1               &
              ,2x,f9.3,2x,f7.1,2x,F9.2,2x,F9.2,2x,F8.4,2x,F8.4,2x,F7.3,2x                 &
              ,F7.3,2x,F7.3,2x,F7.3)                                                         
      endif ! ilang==0

        if(monats>2)then 
          NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3)) 
            else 
              NRS = ITAGS+31*(MONATS-1) 
        endif
                                                                       
        NRSJ = (Jahrs-1900)*365+int((Jahrs-1900)/4)            !Tage seit 1900 (Berücksichtigung der Schaltjahre
                                                                       
        R_NRS = NRS + NRSJ + Uhrz/24. 


    do ianzRB = 1,ianRBs                       !Schleife über alle Randbedingungen hier: Beginn
      mstr = imstr(ianzRB) 
      RBNR = iRBNR(ianzRB)
      NRS = 0 

      do ipp = 1,ipps                          ! Parameterschleife Beginn
        iee1 = -1
        iee2 = -1
                                                                       
        iREC = 1 
        if(ilang==1)iREC = mREC(mstr,ianzRB,ipp) 
        if(iREC.eq.0)iREC = 1

        manzW = ianzW(ianzRB)

          do iwe = iRec,manzW                 ! Beginn Werteschleife

           if(monatl(ianzRB,iwe)>2)then 
              NRS = (itagl(ianzRB,iwe)+31*(monatl(ianzRB,iwe)-1)-INT(0.4*monatl(ianzRB,iwe)+2.3)) 
                else 
                  NRS = itagl(ianzRB,iwe)+31*(monatl(ianzRB,iwe)-1) 
            endif

            NRSJ = (jahrl(ianzRB,iwe) - 1900)*365+int((jahrl(ianzRB,iwe)-1900)/4)
            R_NRS0 = NRS + NRSJ + uhrl(ianzRB,iwe)/24. 

            i_zeiger = 4
            if(ipp==1)i_zeiger = 1
            if(ipp==22)i_zeiger = 2
            if(ipp==27)i_zeiger = 3 
            zeiger_var: select case (i_zeiger)

        case(1)

            if(R_NRS0<=R_NRS)then
              iee1 = 1
              wert1 = werts(ianzRB,ipp,iwe) 
              mREC(mstr,ianzRB,ipp) = iwe
              R_NRS1 = R_NRS0                
                else
                  R_NRS2 = R_NRS0
                  iee2 = 1
                  wert2 = werts(ianzRB,ipp,iwe)
                  exit
              endif             
        case(2)

            if(iee1==-1.and.R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)==-9.99)then
              mREC(mstr,ianzRB,ipp) = iwe
              wert1 = werts(ianzRB,ipp,iwe)
                else if(R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)>-9.99)then
                  iee1 = 1
                  wert1 = werts(ianzRB,ipp,iwe) 
                  mREC(mstr,ianzRB,ipp) = iwe
                  R_NRS1 = R_NRS0                
                    else if(R_NRS0>R_NRS.and.iee2==-1.and.werts(ianzRB,ipp,iwe)==-9.99)then
                      wert2 = werts(ianzRB,ipp,iwe)
                        else if(R_NRS0>R_NRS.and.werts(ianzRB,ipp,iwe)>-9.99)then 
                          R_NRS2 = R_NRS0
                         iee2 = 1
                         wert2 = werts(ianzRB,ipp,iwe)
                         exit
              endif             

        case(3)

            if(iee1==-1.and.R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)==-9999.9)then
              mREC(mstr,ianzRB,ipp) = iwe
              wert1 = werts(ianzRB,ipp,iwe)
                else if(R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)>-9999.9)then
                  iee1 = 1
                  wert1 = werts(ianzRB,ipp,iwe) 
                  mREC(mstr,ianzRB,ipp) = iwe                
                    else if(R_NRS0>R_NRS.and.iee2==-1.and.werts(ianzRB,ipp,iwe)==-9999.9)then
                      wert2 = werts(ianzRB,ipp,iwe)
                        else if(R_NRS0>R_NRS.and.werts(ianzRB,ipp,iwe)>-9999.9)then 
                          R_NRS2 = R_NRS0
                         iee2 = 1
                         wert2 = werts(ianzRB,ipp,iwe)
                         exit
              endif             

        case(4)

            if(iee1==-1.and.R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)<0.0)then
              mREC(mstr,ianzRB,ipp) = iwe
              wert1 = werts(ianzRB,ipp,iwe)
                else if(R_NRS0<=R_NRS.and.werts(ianzRB,ipp,iwe)>=0.0)then
                  R_NRS1 = R_NRS0
                  iee1 = 1
                  wert1 = werts(ianzRB,ipp,iwe) 
                  mREC(mstr,ianzRB,ipp) = iwe                
                    else if(R_NRS0>R_NRS.and.iee2==-1.and.werts(ianzRB,ipp,iwe)<0.0)then
                      wert2 = werts(ianzRB,ipp,iwe)
                        else if(R_NRS0>R_NRS.and.werts(ianzRB,ipp,iwe)>=0.0)then 
                          R_NRS2 = R_NRS0
                         iee2 = 1
                         wert2 = werts(ianzRB,ipp,iwe)
                        exit
              endif             
           
       
        end select zeiger_var

          enddo                ! Ende Werteschleife


        if(iee1==1.and.iee2==-1)then
        Ywert = wert1
          else if(iee1==-1.and.iee2==1)then
            Ywert = wert2
              else if(iee1==-1.and.iee2==-1)then
                Ywert = wert1
                  else 
                    hcon1 = R_NRS2 - R_NRS1
                    hcon2 = R_NRS - R_NRS1
                    Ywert = wert1 + ((wert2 - wert1)/hcon1)*hcon2 
        endif    

      if(RBtyp(mstr,RBNR)==0.and.ipp==28.and.iwsim==4)then  ! Tracer                                                
        tempws(mstr,RBNR) = ywert
        if(iwied==0)ywert = 0.0 
        if(tempws(mstr,RBNR)<0.0)tempws(mstr,RBNR) = 0.0 
        cycle
      endif

      if(RBtyp(mstr,RBNR)==2.and.ipp==28.and.iwsim==4)then  ! Tracer                                                
        tempws(mstr,RBNR) = ywert 
        if(iwied==0)ywert = 0.0 
        if(tempws(mstr,RBNR)<0.0)tempws(mstr,RBNR) = 0.0 
        cycle
      endif

      if(RBtyp(mstr,RBNR)==1.and.ipp==28.and.iwsim==4)then  ! Tracer                                                
        tempws(mstr,RBNR) = ywert 
        if(iwied==0)ywert = 0.0 
        if(tempws(mstr,RBNR)<0.0)tempws(mstr,RBNR) = 0.0 
        cycle
      endif

      if(RBtyp(mstr,RBNR)==0.and.ipp==29.and.iwsim==5)then  ! konserv. Substanz                                                
        tempws(mstr,RBNR) = ywert
        cycle
      endif

      if(RBtyp(mstr,RBNR)==2.and.ipp==29.and.iwsim==5)then  ! konserv. Substanz                                                
        tempws(mstr,RBNR) = ywert 
        cycle
      endif

      if(RBtyp(mstr,RBNR)==1.and.ipp==29.and.iwsim==5)then  ! konserv. Substanz                                                
        tempws(mstr,RBNR) = ywert 
        cycle
      endif

      if(ipp==1)abfls(mstr,RBNR) = ywert 
      if(ipp==2)vbsbs(mstr,RBNR) = ywert 
      if(ipp==3)vcsbs(mstr,RBNR) = ywert 
      if(ipp==4)vnh4s(mstr,RBNR) = ywert 
      if(ipp==5)vno2s(mstr,RBNR) = ywert 
      if(ipp==6)vno3s(mstr,RBNR) = ywert 
      if(ipp==7)gesNs(mstr,RBNR) = ywert 
      if(ipp==8)vx0s(mstr,RBNR) = ywert 
      if(ipp==9)vx02s(mstr,RBNR) = ywert 
      if(ipp==10)gelps(mstr,RBNR) = ywert 
      if(ipp==11)gesPs(mstr,RBNR) = ywert
      if(ipp==12)sis(mstr,RBNR) = ywert 
      if(ipp==13)chlas(mstr,RBNR) = ywert 
      if(ipp==14)vkigrs(mstr,RBNR) = ywert 
      if(ipp==15)antbls(mstr,RBNR) = ywert 
      if(ipp==16)zooins(mstr,RBNR) = ywert 
      if(ipp==17)vphs(mstr,RBNR) = ywert 
      if(ipp==18)mws(mstr,RBNR) = ywert 
      if(ipp==19)cas(mstr,RBNR) = ywert 
      if(ipp==20)lfs(mstr,RBNR) = ywert 
      if(ipp==21)then
        ssalgs(mstr,RBNR) = ywert
        if(ssalgs(mstr,RBNR)<5.)ssalgs(mstr,RBNR) = 5. 
      endif 
      if(ipp==22)tempws(mstr,RBNR) = ywert 
      if(ipp==23)vo2s(mstr,RBNR) = ywert 
      if(ipp==24)CHNFs(mstr,RBNR) = ywert 
      if(ipp==25)BVHNFs(mstr,RBNR) = ywert 
      if(ipp==26)colis(mstr,RBNR) = ywert
      if(colis(mstr,RBNR)>=0.0)DOSCFs(mstr,RBNR) = 0.0 
      if(ipp==27)waers(mstr,RBNR) = ywert
      if(ipp==30)gsZns(mstr,RBNR) = ywert  
      if(ipp==31)glZns(mstr,RBNR) = ywert  
      if(ipp==32)gsCads(mstr,RBNR) = ywert  
      if(ipp==33)glCads(mstr,RBNR) = ywert  
      if(ipp==34)gsCus(mstr,RBNR) = ywert  
      if(ipp==35)glCus(mstr,RBNR) = ywert  
      if(ipp==36)gsNis(mstr,RBNR) = ywert  
      if(ipp==37)glNis(mstr,RBNR) = ywert  

   enddo                          ! Ende Parameterschleife 

                                                                     
      if(RBtyp(mstr,RBNR).eq.0)mstrRB = mstr 

  if(ischwer==1)then
    do i = 1,1

    hcSS = min(100.,ssalgs(mstr,RBNR))
    hcph = vphs(mstr,RBNR)

    call Verteilungskoeff(hcSS,hcph,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,iformVert,i)                                                                           

    enddo

    if(gsZns(mstr,RBNR)>0.0.and.glZns(mstr,RBNR)<=0.0)then
      glZns(mstr,RBNR) = gsZns(mstr,RBNR)/(1.+VTKoeff_Zn(1)*hcSS/1000.)    
       else if(gsZns(mstr,RBNR)<=0.0.and.glZns(mstr,RBNR)>0.0)then
         gsZns(mstr,RBNR) = glZns(mstr,RBNR)*(1+VTKoeff_Zn(1)*hcSS/1000.)
           else if(gsZns(mstr,RBNR)>0.0.and.glZns(mstr,RBNR)==gsZns(mstr,RBNR))then
             glZns(mstr,RBNR) = gsZns(mstr,RBNR)/(1.+VTKoeff_Zn(1)*hcSS/1000.)    
               else if(gsZns(mstr,RBNR)<glZns(mstr,RBNR))then
                 gsZns(mstr,RBNR) = glZns(mstr,RBNR)*(1+VTKoeff_Zn(1)*hcSS/1000.)
    endif 

   if(gsCads(mstr,RBNR)>0.0.and.glCads(mstr,RBNR)<=0.0)then
     glCads(mstr,RBNR) = gsCads(mstr,RBNR)/(1.+VTKoeff_Cad(1)*hcSS/1000.)    
       else if(gsCads(mstr,RBNR)<=0.0.and.glCads(mstr,RBNR)>0.0)then
         gsCads(mstr,RBNR) = glCads(mstr,RBNR)*(1+VTKoeff_Cad(1)*hcSS/1000.)
           else if(gsCads(mstr,RBNR)>0.0.and.glCads(mstr,RBNR)==gsCads(mstr,RBNR))then
             glCads(mstr,RBNR) = gsCads(mstr,RBNR)/(1.+VTKoeff_Cad(1)*hcSS/1000.)    
               else if(gsCads(mstr,RBNR)<glCads(mstr,RBNR))then
                 gsCads(mstr,RBNR) = glCads(mstr,RBNR)*(1+VTKoeff_Cad(1)*hcSS/1000.)
    endif 

   if(gsCus(mstr,RBNR)>0.0.and.glCus(mstr,RBNR)<=0.0)then
     glCus(mstr,RBNR) = gsCus(mstr,RBNR)/(1.+VTKoeff_Cu(1)*hcSS/1000.)    
       else if(gsCus(mstr,RBNR)<=0.0.and.glCus(mstr,RBNR)>0.0)then
         gsCus(mstr,RBNR) = glCus(mstr,RBNR)*(1+VTKoeff_Cu(1)*hcSS/1000.)
           else if(gsCus(mstr,RBNR)>0.0.and.glCus(mstr,RBNR)==gsCus(mstr,RBNR))then
             glCus(mstr,RBNR) = gsCus(mstr,RBNR)/(1.+VTKoeff_Cu(1)*hcSS/1000.)    
               else if(gsCus(mstr,RBNR)<glCus(mstr,RBNR))then
                 gsCus(mstr,RBNR) = glCus(mstr,RBNR)*(1+VTKoeff_Cu(1)*hcSS/1000.)
    endif 

   if(gsNis(mstr,RBNR)>0.0.and.glNis(mstr,RBNR)<=0.0)then
     glNis(mstr,RBNR) = gsNis(mstr,RBNR)/(1.+VTKoeff_Ni(1)*hcSS/1000.)    
       else if(gsNis(mstr,RBNR)<=0.0.and.glNis(mstr,RBNR)>0.0)then
          gsNis(mstr,RBNR) = glNis(mstr,RBNR)*(1+VTKoeff_Ni(1)*hcSS/1000.)
           else if(gsNis(mstr,RBNR)>0.0.and.glNis(mstr,RBNR)==gsZns(mstr,RBNR))then
             glNis(mstr,RBNR) = gsNis(mstr,RBNR)/(1.+VTKoeff_Ni(1)*hcSS/1000.)    
               else if(gsNis(mstr,RBNR)<glNis(mstr,RBNR))then
                 gsNis(mstr,RBNR) = glNis(mstr,RBNR)*(1+VTKoeff_Ni(1)*hcSS/1000.)

    endif
  endif

!...Fehlermeldung                                                       

    if(ischwer==1)then
      ifehl = 0
      if(ISNAN(gsZns(mstr,RBNR)).or.ISNAN(glZns(mstr,RBNR)))then 
        if(vphs(mstr,RBNR)<=0.0)ifehl = 31 
        if(ssalgs(mstr,RBNR)<=0.0)ifehl = 32
        ifmstr = mstr
        ifmRB = RBNR 
        exit 
      endif 
   endif

   enddo                          ! Ende Randbedingungsschleife 

!   deallocate(mREC)
!   deallocate(werts)
 

 End subroutine funkstar
