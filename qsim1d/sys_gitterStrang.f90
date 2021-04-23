 subroutine sys_gitterStrang(mstr,j_neu,j_alt,dH2D,tzt,o2zt,nh4zt,no2zt,no3zt,Pzt,gSizt,akizt,agrzt,ablzt        &
                             ,chlazt,chlkzt,chlgzt,chlbzt,gesPzt,gesNzt,Q_NKzt,Q_NBzt,Q_NGzt, CChlkzt, CChlbzt   &
                             ,CChlgzt, jnkz,i_EstRNR,itags,monats,uhrz,azStrs)


 
      integer                         :: nkzs_neu, nkz_alt,mstr, azStrs

      real, Dimension(azStrs,50,2)    :: tzt, o2zt, nh4zt, no2zt, no3zt, Pzt, gSizt, akizt, agrzt, ablzt, chlazt
      real, Dimension(azStrs,50,2)    :: chlkzt, chlgzt, chlbzt, gesPzt, gesNzt, Q_NKzt, Q_NBzt, Q_NGzt
      real, Dimension(azStrs,50,2)    :: CChlkzt, CChlbzt, CChlgzt
      real, Dimension(50)             :: y_var, hcTiefe, hcTiefe_neu, tiefe_neu, y_var_neu, zMasse


!  open(unit=234,file='sys_z_gitter.tst') 

!    Ermittlung der Tiefe des alten Gitters
      gstiefe_alt = (j_alt-1)*dH2D
!    Ermittlung der Tiefe des neuen Gitters
      gstiefe_neu = (j_neu-1)*dH2D 

     dTiefe = gsTiefe_neu-gstiefe_alt   ! Tiefen�nderung zwischen Tiefe im neuen und alten Zeitschritt
     hcdH2D = dTiefe/(j_alt-1)

     nkzs_alt = j_alt
     nkzs_neu = j_neu
     hcTiefe_neu(nkzs_alt) = 0.0                         ! hcTiefe_neu: TiefenStufen des Zwischengitters  

      do nkz = nkzs_alt-1,1,-1
          hcTiefe_neu(nkz) = hcTiefe_neu(nkz+1)+dH2D+hcdH2D 
       enddo

     do nkz=1,nkzs_neu
        Tiefe_neu(nkz) = gsTiefe_neu-(nkz-1)*dH2D  ! Tiefe_neu Tiefenstufen des neuen Gitters
     enddo


  i_vars = 23
  i_zaehlv = 1   
  Do i_var = 1,i_vars
   zeiger_var: select case (i_zaehlv)
     case(1)
         do nkz = 1,nkzs_alt
           y_var(nkz) = tzt(i_EstRNR,nkz,jnkz)
         enddo
     case(2)
         do nkz = 1,nkzs_neu
           tzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo 
         do nkz = 1,nkzs_alt
           y_var(nkz) = O2zt(i_EstRNR,nkz,jnkz)
         enddo
     case(3)
         do nkz = 1,nkzs_neu
           O2zt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = NH4zt(i_EstRNR,nkz,jnkz)
         enddo
     case(4)
         do nkz = 1,nkzs_neu
           NH4zt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = NO2zt(i_EstRNR,nkz,jnkz)
         enddo
     case(5)
         do nkz = 1,nkzs_neu
           NO2zt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = NO3zt(i_EstRNR,nkz,jnkz)
         enddo
     case(6)
         do nkz = 1,nkzs_neu
           NO3zt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = Pzt(i_EstRNR,nkz,jnkz)
         enddo
     case(7)
         do nkz = 1,nkzs_neu
           Pzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = gSizt(i_EstRNR,nkz,jnkz)
         enddo
     case(8)
         do nkz = 1,nkzs_neu
           gSizt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = akizt(i_EstRNR,nkz,jnkz)
         enddo
     case(9)
         do nkz = 1,nkzs_neu
           akizt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = agrzt(i_EstRNR,nkz,jnkz)
         enddo
     case(10)
         do nkz = 1,nkzs_neu
           agrzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = ablzt(i_EstRNR,nkz,jnkz)
         enddo
     case(11)
         do nkz = 1,nkzs_neu
           ablzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = chlazt(i_EstRNR,nkz,jnkz)
         enddo
     case(12)
         do nkz = 1,nkzs_neu
           chlazt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = chlkzt(i_EstRNR,nkz,jnkz)
         enddo
     case(13)
         do nkz = 1,nkzs_neu
           chlkzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = chlgzt(i_EstRNR,nkz,jnkz)
         enddo
     case(14)
         do nkz = 1,nkzs_neu
           chlgzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo
         do nkz = 1,nkzs_alt
           y_var(nkz) = chlbzt(i_EstRNR,nkz,jnkz)
         enddo
     case(15)
         do nkz = 1,nkzs_neu
           chlbzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = gesPzt(i_EstRNR,nkz,jnkz)
         enddo
    case(16)
         do nkz = 1,nkzs_neu
           gesPzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = gesNzt(i_EstRNR,nkz,jnkz)
         enddo
    case(17)
         do nkz = 1,nkzs_neu
           gesNzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = Q_NKzt(i_EstRNR,nkz,jnkz)
         enddo
    case(18)
         do nkz = 1,nkzs_neu
           Q_NKzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = Q_NBzt(i_EstRNR,nkz,jnkz)
         enddo
    case(19)
         do nkz = 1,nkzs_neu
           Q_NBzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = Q_NGzt(i_EstRNR,nkz,jnkz)
         enddo
    case(20)
         do nkz = 1,nkzs_neu
           Q_NGzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = CChlkzt(i_EstRNR,nkz,jnkz)
         enddo
    case(21)
         do nkz = 1,nkzs_neu
           CChlkzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = CChlbzt(i_EstRNR,nkz,jnkz)
         enddo
    case(22)
         do nkz = 1,nkzs_neu
           CChlbzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

         do nkz = 1,nkzs_alt
           y_var(nkz) = CChlgzt(i_EstRNR,nkz,jnkz)
         enddo
    case(23)
         do nkz = 1,nkzs_neu
           CChlgzt(i_EstRNR,nkz,jnkz) = y_var_neu(nkz)
         enddo

   end select zeiger_var

  i_zaehlv = i_zaehlv+1

   if(i_zaehlv<=23)then 
     call lin_interpolation2(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
   endif
   enddo ! Ende Variablenschleife 



 end subroutine sys_gitterStrang
!
!***************************************
 subroutine lin_interpolation2(y_var,y_var_neu,hctiefe_neu,tiefe_neu,nkzs_alt, nkzs_neu,i_zaehlv,itags,monats,mstr,ior)
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
            cycle 
              else
                i_neu = nkz
                exit  
          endif
 enddo
     enddo

 end subroutine lin_interpolation2   
!******************************************
