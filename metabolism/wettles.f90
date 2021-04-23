      subroutine wettles(itags,monats,jahrs,uhrz,glob,tlmax,tlmin,ro,wge,cloud,typw,imet     &
                         ,iwied,cpfad, ckenn_vers1)
!                                                                       
!                                                                       
!     UNTERPROGRAMM ZUR BERECHNUNG DER WETTERDATEN fÅr einen ZEITSCHRITT
!                                                                       
!                                                                       
!     AUTOR: VOLKER KIRCHESCH                                           
!                                                                       
!     STAND: 15.04.2003                                                 
!                                                                       
!                                                                       
      character (len=1)                      :: cwert
      character (len=2)                      :: ckenn_vers1
      character (len=40)                     :: MODELL, ERENAME  
      character (len=255)                    :: cpfad
      character (len=275)                    :: pfadstring 
      integer                                :: read_error
      integer, Dimension(20)                 :: iWSta, mwetts
      integer,Dimension(:,:), allocatable    :: itagw, monatw, jahrw

      real, Dimension(20)                     :: glob, tlmax, tlmin, ro, wge, cloud, typw     
      real, Dimension(:,:), allocatable       :: uhrzw
      real, Dimension(:,:,:), allocatable     :: wertw

      double precision                        :: R_NRS2, R_NRS1, R_NRS


      save itagw, monatw, jahrw, uhrzw, wertw, iwetts,R_NRS2, R_NRS1, R_NRS, IWSta, mWetts                                                                       
!                                                                       
!                                                                       
!#############################
!  Parameter
!#############################

!   IWETTS  : Anzahl der Wetterstationen
!   IPWS    : Anzahl der metereologischen Kenngrˆﬂen (hier: 7)
!   mWETTs  : Anzahl der Datens‰tze f¸r eine Wetterstation
!   imet    : Daten auf Tagesbasis (0); Daten auf Stundenbasis (1)
!   iWSta   : ID-Nummer der Wetterstation      
 

      ipws = 7 

     if(iwied==0)then

! Einlesen der Wetterdaten aus WETTER.TXT                               
                                                                       
                                                                       
      write(pfadstring,'(2A)')trim(adjustl(cpfad)),'WETTER.txt'
      open(unit=86, file=pfadstring)
      rewind (86) 

      read(86,'(A2)')ckenn_vers1
      if(ckenn_vers1/='*V')then 
        read(86,'(A40)')ERENAME 
          else
           read(86,'(A40)')MODELL 
           read(86,'(A40)')ERENAME 
      endif

      read(86,'(I2,2x,I1)')IWETTs,IMET 
                                                                       
      iWSta_mx = 1
      mWetts_mx = 1

      do iWETT = 1,iWETTs 
      read(86,'(I8,2x,I5)',iostat=read_error)IWSta(iwett),mWetts(iwett)
      if(read_error<0)exit 
      if(mWetts(iwett)>mWetts_mx)mWetts_mx = mWetts(iwett)
      if(iWSta(iwett)>iWSta_mx)iWSta_mx = iWSta(iwett)
                                                                       
      if(mWetts(iwett).eq.0)then 
      write(199,1996) 
 1996 format(2x,'Wetterdaten nicht oder unvollst‰ndig eingegeben') 
      endif 
!                                                                       
      do mWett = 1,mWetts(iwett) 
        read(86,'(a1)')cwert
      enddo
   enddo      

      if(.not.allocated(wertw))allocate(wertw(1:iWSta_mx,1:mWetts_mx,1:ipws))
      if(.not.allocated(itagw))allocate(itagw(1:iWSta_mx,1:mWetts_mx))
      if(.not.allocated(monatw))allocate(monatw(1:iWSta_mx,1:mWetts_mx))
      if(.not.allocated(jahrw))allocate(jahrw(1:iWSta_mx,1:mWetts_mx))
      if(.not.allocated(uhrzw))allocate(uhrzw(1:iWSta_mx,1:mWetts_mx))

      rewind (86)

      read(86,'(A2)')ckenn_vers1
      if(ckenn_vers1/='*V')then 
        read(86,'(A40)')ERENAME 
          else
           read(86,'(A40)')MODELL 
           read(86,'(A40)')ERENAME 
      endif
      read(86,'(I2,2x,I1)')IWETTs,IMET 

      do iWETT = 1,iWETTs 
      read(86,'(I8,2x,I5)',iostat=read_error)IWSta(iwett),mWetts(iwett)
      if(read_error<0)exit 
                                                                       
       if(imet==0)then
         hcTmx2 = -999.
         do mWett = 1,mWetts(iwett) 
           read(86,2013)itagw(iWSta(iwett),mwett),monatw(iWSta(iwett),mwett)                    &
                       ,jahrw(iWSta(iwett),mwett),(wertw(iWSta(iwett),mwett,ixw),ixw=1,7)
                                                                       
           if(wertw(iWSta(iwett),mwett,3).gt.hcTmx2)hcTmx2 = wertw(iWSta(iwett),mwett,3)                                       
                                                                       
         enddo 
                                                                       
!...Fehlermeldung keine Minimumtemperaturen an einer oder mehrer Wetterstationen
         if(hcTmx2==(-1.))then 
           ifehl = 4 
           ifhStr = IWETT 
           goto 989 
          endif 
            else             ! Messwerte auf Stundenbasis
              do mWett = 1,mWetts(iwett) 

                read(86,2023)itagw(iWSta(iwett),mwett),monatw(iWSta(iwett),mwett),jahrw(iWSta(iwett),mwett)       &
                             ,uhrzw(iWSta(iwett),mwett),(wertw(iWSta(iwett),mwett,ixw),ixw=1,7)                          
              enddo
         endif
      enddo
      close (86)
         else
     endif

 2013 format(i2,2x,i2,2x,I4,9x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f4.1,2x,f4.1)                                                 
 2023 format(i2,2x,i2,2x,I4,2x,f5.2,2x,f7.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f6.2,2x,f4.1,2x,f4.1)                                         


        if(monats>2)then 
          NRS = (ITAGS+31*(MONATS-1)-INT(0.4*MONATS+2.3)) 
            else 
              NRS = ITAGS+31*(MONATS-1) 
        endif
                                                                       
        NRSJ = (Jahrs-1900)*365+int((Jahrs-1900)/4)            !Tage seit 1900 (Ber¸cksichtigung der Schaltjahre
                                                                       
        if(imet==0)then
          R_NRS = NRS + NRSJ 
            else 
              R_NRS = NRS + NRSJ + Uhrz/24. 
        endif 

        do iwett = 1, iwetts
          do ipw = 1,ipws
            iee1 = -1
            iee2 = -1
     
            do mWett = 1, mWetts(iwett)       ! Beginn der Werteschleife
              if(imet==0)uhrzw(iWSta(iwett),mwett) = 0.0

!...Umrechnen der "Messwertuhrzeit" in Dezimalschreibweise              
                uhr_Dz = int(uhrzw(iWSta(iwett),mwett))+((uhrzw(iWSta(iwett),mwett)-int(uhrzw(iWSta(iwett),mwett)))/0.6)  


            if(monatw(iWSta(iwett),mwett)>2)then 
              NRS = (itagw(iWSta(iwett),mwett)+31*(monatW(iWSta(iwett),mwett)-1)-INT(0.4*monatW(iWSta(iwett),mwett)+2.3)) 
                else 
                  NRS = itagW(iWSta(iwett),mwett)+31*(monatW(iWSta(iwett),mwett)-1) 
            endif

            NRSJ = (jahrW(iWSta(iwett),mwett) - 1900)*365+int((jahrW(iWSta(iwett),mwett)-1900)/4)

            if(imet==0)then
              R_NRS0 = NRS + NRSJ 
                else
                  R_NRS0 = NRS + NRSJ + uhr_Dz/24. 
            endif

            if(iee1==-1.and.R_NRS0<=R_NRS.and.wertw(iWSta(iwett),mwett,ipw)<0.0)then
              wert1 = wertw(iWSta(iwett),mwett,ipw)
                else if(R_NRS0<=R_NRS.and.wertw(iWSta(iwett),mwett,ipw)>=0.0)then
                  R_NRS1 = R_NRS0
                  iee1 = 1
                  wert1 = wertw(iWSta(iwett),mwett,ipw) 
                    else if(R_NRS0>R_NRS.and.iee2==-1.and.wertw(iWSta(iwett),mwett,ipw)<0.0)then
                      wert2 = wertw(iWSta(iwett),mwett,ipw)
                        else if(R_NRS0>R_NRS.and.wertw(iWSta(iwett),mwett,ipw)>=0.0)then 
                          R_NRS2 = R_NRS0
                         iee2 = 1
                         wert2 = wertw(iWSta(iwett),mwett,ipw)
                         exit
              endif             

      enddo   ! Ende Werteschleife

  !+++ Interpolation++++
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

        if(ipw.eq.1)glob(iWSta(iwett)) = ywert 
        if(ipw.eq.2)tlmax(iWSta(iwett)) = ywert 
        if(ipw.eq.3)tlmin(iWSta(iwett)) = ywert 
        if(ipw.eq.4)ro(iWSta(iwett)) = ywert 
        if(ipw.eq.5)wge(iWSta(iwett)) = ywert 
        if(ipw.eq.6)cloud(iWSta(iwett)) = ywert 
        if(ipw.eq.7)typw(iWSta(iwett)) = ywert 
    enddo   ! Ende Parameterschleife

  enddo      ! Ende Statonenschleife 

 989 continue

  end subroutine WETTLES 
