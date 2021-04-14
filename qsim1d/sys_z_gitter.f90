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

subroutine sys_z_Gitter(azStrs,mstra,hanze,znkzs,hnkzs,dH2D,iFlRi,htempz,ho2z,hnh4z         &
                        ,hno2z,hno3z,hgelPz,hSiz,hakiz,hagrz,hablz,hchlaz,hchlkz            &
                        ,hchlgz,hchlbz,hgesPz,hgesNz,hQ_NKz,hQ_NBz,hQ_NGz,hCChlkz           &
                        ,hCChlbz,hCChlgz,itags,monats)

!    znkzs:  Anzahl der Gitterpunkte des alten Gitters
!    hnkzs:  Anzahl Gitterpunkte des neuen Gitters
!
!
!
integer                                :: azStrs,azStr, nkzs, nkzs_neu, nkz_alt,mstr
integer, Dimension(azStrs)             :: mstra, hanze, iFlRi
integer, Dimension(azStrs,1000)        :: znkzs, hnkzs 
real, Dimension(azStrs,50,1000)        :: htempz, ho2z, hnh4z, hno2z, hno3z, hgelPz, hSiz, hakiz, hagrz, hablz, hchlaz
real, Dimension(azStrs,50,1000)        :: hchlkz, hchlgz, hchlbz, hgesPz, hgesNz, hQ_NKz, hQ_NBz, hQ_NGz, hCChlkz
real, Dimension(azStrs,50,1000)        :: hCChlbz, hCChlgz
real, Dimension(50)                    :: y_var, hcTiefe, hcTiefe_neu, tiefe_neu, y_var_neu, dzMasse
!
!
!  open(unit=234,file='sys_z_gitter.tst') 
!  
!
  do azstr = 1,azstrs  ! Strangschleife
  mstr = mstra(azStr)
!
!....Rueckstroemung
      if(iFlRi(mstr).eq.(-1))then
      ibeg = hanze(mstr)+1
      iRe = 1
      istep = -1
      endif
!
      if(iFlRi(mstr).gt.(-1))then
      ibeg = 1
      iRe = hanze(mstr)+1
      istep = 1
      endif
!
  do ior = ibeg,iRe,istep ! Schleife über die longitudinalen Gitterpunkte
!
     if(hnkzs(mstr,ior)==1)cycle  !Bei 1D-Modellierung wird die Gitterbelegunsroutine übersprungen
!    Ermittlung der Tiefe des alten Gitters
      gstiefe_alt = (znkzs(mstr,ior)-1)*dH2D
!    Ermittlung der Tiefe des neuen Gitters
      gstiefe_neu = (hnkzs(mstr,ior)-1)*dH2D 
!
     dTiefe = gsTiefe_neu-gstiefe_alt   ! Tiefenänderung zwischen Tiefe im neuen und alten Zeitschritt
     hcdH2D = dTiefe/(znkzs(mstr,ior)-1)

     ifak = 1
!    if(nkzs_neu<nkzs_alt)ifak = -1

     nkzs_alt = znkzs(mstr,ior)
     nkzs_neu = hnkzs(mstr,ior)
     hcTiefe_neu(nkzs_alt) = 0.0                         ! hcTiefe_neu: TiefenStufen des Zwischengitters  

      do nkz = nkzs_alt-1,1,-1
          hcTiefe_neu(nkz) = hcTiefe_neu(nkz+1)+dH2D+hcdH2D 
!          Runden
          hc_Wert = hcTiefe_neu(nkz)*1000.
          ihc_Wert = int(hc_wert)
          dhc_Wert = hc_Wert - ihc_Wert
          if(dhc_Wert>0.5)then
!            hcTiefe_neu(nkz) = (ihc_Wert + 1)/1000. 
              else
!                hcTiefe_neu(nkz) = ihc_Wert/1000. 
          endif
       enddo

     do nkz=1,nkzs_neu
        Tiefe_neu(nkz) = gsTiefe_neu-(nkz-1)*dH2D  ! Tiefe_neu Tiefenstufen des neuen Gitters

!          Runden
          hc_Wert = Tiefe_neu(nkz)*1000.
          ihc_Wert = int(hc_Wert)
          dhc_Wert = hc_Wert - ihc_Wert
          if(dhc_Wert>0.5)then
!            Tiefe_neu(nkz) = (ihc_Wert + 1)/1000. 
              else
!                Tiefe_neu(nkz) = ihc_Wert/1000. 
          endif
     enddo

     if(nkzs_alt==nkzs_neu)cycle

  i_vars = 23
 i_zaehlv = 1   
  Do i_var = 1,i_vars
   zeiger_var: select case (i_zaehlv)
     case(1)
         do nkz = 1,nkzs_alt
           y_var(nkz) = htempz(mstr,nkz,ior)
         enddo
     case(2)
         do nkz = 1,nkzs_neu
           htempz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hO2z(mstr,nkz,ior)
         enddo
     case(3)
         do nkz = 1,nkzs_neu
           hO2z(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hNH4z(mstr,nkz,ior)
         enddo
     case(4)
         do nkz = 1,nkzs_neu
           hNH4z(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hNO2z(mstr,nkz,ior)
         enddo
     case(5)
         do nkz = 1,nkzs_neu
           hNO2z(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hNO3z(mstr,nkz,ior)
         enddo
     case(6)
         do nkz = 1,nkzs_neu
           hNO3z(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hgelPz(mstr,nkz,ior)
         enddo
     case(7)
         do nkz = 1,nkzs_neu
           hgelPz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hSiz(mstr,nkz,ior)
         enddo
     case(8)
         do nkz = 1,nkzs_neu
           hSiz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hakiz(mstr,nkz,ior)
         enddo
     case(9)
         do nkz = 1,nkzs_neu
           hakiz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hagrz(mstr,nkz,ior)
         enddo
     case(10)
         do nkz = 1,nkzs_neu
           hagrz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hablz(mstr,nkz,ior)
         enddo
     case(11)
         do nkz = 1,nkzs_neu
           hablz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hchlaz(mstr,nkz,ior)
         enddo
     case(12)
         do nkz = 1,nkzs_neu
           hchlaz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hchlkz(mstr,nkz,ior)
         enddo
     case(13)
         do nkz = 1,nkzs_neu
           hchlkz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hchlgz(mstr,nkz,ior)
         enddo
     case(14)
         do nkz = 1,nkzs_neu
           hchlgz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hchlbz(mstr,nkz,ior)
         enddo
     case(15)
         do nkz = 1,nkzs_neu
           hchlbz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hgesPz(mstr,nkz,ior)
         enddo
     case(16)
         do nkz = 1,nkzs_neu
           hgesPz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hgesNz(mstr,nkz,ior)
         enddo
     case(17)
         do nkz = 1,nkzs_neu
           hgesNz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hQ_NKz(mstr,nkz,ior)
         enddo
     case(18)
         do nkz = 1,nkzs_neu
           hQ_NKz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hQ_NBz(mstr,nkz,ior)
         enddo
     case(19)
         do nkz = 1,nkzs_neu
           hQ_NBz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hQ_NGz(mstr,nkz,ior)
         enddo
     case(20)
         do nkz = 1,nkzs_neu
           hQ_NGz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hCChlkz(mstr,nkz,ior)
         enddo
     case(21)
         do nkz = 1,nkzs_neu
           hCChlkz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hCChlbz(mstr,nkz,ior)
         enddo
     case(22)
         do nkz = 1,nkzs_neu
           hCChlbz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = hCChlgz(mstr,nkz,ior)
         enddo
     case(23)
         do nkz = 1,nkzs_neu
           hCChlgz(mstr,nkz,ior) = y_var_neu(nkz)
         enddo

   end select zeiger_var
  i_zaehlv = i_zaehlv+1
!
   call lin_interpolation(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
!
   enddo ! Ende Variablenschleife 
!
   enddo ! Ende Schleife über die longitudinalen Gitterpunkte
      enddo ! Ende Strangschleife

 

 end subroutine sys_z_Gitter
!
!***************************************
 subroutine lin_interpolation(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
!
real, Dimension(50) :: tiefe_neu, hctiefe_neu, y_var, y_var_neu
!
!  Bestimmung der neuen Gitterwerte durch lineare Interpolation
!
!
 i_neu = 1
 do nkz_alt = 1,nkzs_alt-1
     do nkz = i_neu,nkzs_neu
         if(nkz==1.and.tiefe_neu(nkz)>hcTiefe_neu(nkz_alt))then
           y_var_neu(nkz) = y_var(nkz_alt)
           cycle
          endif

         if(nkz==nkzs_neu.and.Tiefe_neu(nkz)<hctiefe_neu(nkzs_alt))then
         y_var_neu(nkz) = y_var(nkzs_alt)
         exit
         endif
          
         if(tiefe_neu(nkz)<=hcTiefe_neu(nkz_alt).and.Tiefe_neu(nkz)>=hcTiefe_neu(nkz_alt+1))then
         Stg = (y_var(nkz_alt+1)-y_var(nkz_alt))/(hcTiefe_neu(nkz_alt+1)-hcTiefe_neu(nkz_alt))
         Yachs = y_var(nkz_alt)-Stg*hcTiefe_neu(nkz_alt)
         y_var_neu(nkz) = Stg*Tiefe_neu(nkz)+Yachs

!         if(mstr==28.and.i_zaehlv==4.and.itags==23.and.monats==6)write(89,'(i4,2x,i2,2x,f5.2,2x,f6.3,2x,f6.3,2x,f5.2,2x,f5.2,2x,f6.3,2x,f7.4,2x,f7.4)')ior,nkz,tiefe_neu(nkz),y_var(nkz_alt+1),y_var(nkz_alt),hcTiefe_neu(nkz_alt+1),hcTiefe_neu(nkz_alt),y_var_neu(nkz),Stg,yachs

            cycle 
              else
                i_neu = nkz
                exit  
          endif
 enddo
     enddo

 end subroutine lin_interpolation    
