!> \page mphyt Makrophythen (Wasserpflanzen)
!!
!!<table><tr><td> 
!! \image html ElodeaNuttallii_Aspect_klein.jpg "" <a href="ElodeaNuttallii_Aspect.jpg">Bild groß</a> \n 
!! Bild-Autor: Christian Fischer  \n
!! Herkunft: <a href="https://commons.wikimedia.org/wiki/File:ElodeaNuttallii_Flowering.jpg">ElodeaNuttallii</a>  (12. Nov. 2019) \n
!! licensed under the 
!! <a href="https://en.wikipedia.org/wiki/en:Creative_Commons">Creative Commons</a> 
!! <a href="https://creativecommons.org/licenses/by-sa/3.0/deed.en">Attribution-Share Alike 3.0 Unported</a> license. 
!!</td><td>
!! \image html Illustration_Myriophyllum_spicatum_mini.jpg ""  <a href="Illustration_Myriophyllum_spicatum0.jpg">Bild groß</a> \n
!! aus: Prof. Dr. Otto Wilhelm Thomé Flora von Deutschland, Österreich und der Schweiz 1885, Gera, Germany
!! Herkunft: <a href="https://de.wikipedia.org/wiki/Datei:Illustration_Myriophyllum_spicatum0.jpg">Illustration_Myriophyllum_spicatum</a>  (12. Nov. 2019)
!!!</td><td>
!! \image html Nymphaea_alba_mini.jpg ""   \n 
!! public domain, Herkunft: <a href="https://en.wikipedia.org/wiki/File:Nymphaea_alba.jpg">Nymphaea_alba</a> (12. Nov. 2019)
!!</td></tr></table>
!! 
!! <h2>Herkunft</h2>
!!                                                                       
!! Berechnung des Makrophytenentwicklung im Jahresgang               
!!                                                                       
!! <h2>Teilprozesse</h2>
!!                          !                                                                       
!!     Berechnung der maximalen Wachstumsrate   \n                            
!!     Berechnung des Aufwuchses auf den Makrophyten         \n              
!!     Berechnung der Sauerstoffproduktion und -Respiration durch Makrophyten   \n    
!!     pmaxpfl   -       max. Bruttoproduktion der Wasserpflanzen       \n   
!!                       [mgO2/(gTG*h)]                               \n     
!!     Umrechnung des Pflanzentrockengewichtes von g/m2 in g/l       \n         
!!     Temperaturabhaengigkeit der Pflanzenphotosynthese             \n      
!!     po2p(ior) = po2p(ior)*(exp(-((tempw(ior)-18.)**2)/13.7**2))  \n 
!!                                                                                                                   
!! <h2>Dokumentation und Veröffentlichungen</h2>
!!
!! <h2>Schnittstellenbeschreibung / IT-Realisierung</h2>
!!      subroutine mphyt( \ref tiefe, \ref tempw, \ref anze, \ref po2p, \ref po2r, \ref pfldalg, \ref tflie         &\n
!!     &, \ref itags, \ref monats, \ref itstart, \ref mstart, \ref itmax, \ref mmax, \ref itend, \ref mend, \ref schwi          &\n
!!     &, \ref pflmin, \ref pflmax, \ref pfl, \ref sa, \ref su, \ref ilang, \ref extk, \ref mstr, \ref ifehl, \ref ifhstr             & \n                                                  
!!     &, \ref kontroll , \ref iglob ) \n
!! \n
!! Steuerung des Moduls mittels Zeilen in 
!! <a href="./exp/MODELLG.3D.txt" target="_blank">MODELLG.3D.txt</a>\n
!! siehe auch ModellGParam() \n
!! <table>
!! <tr><td> M-Zeile </td><td> Makrophyten </td><td> Makrophyten-Wachstum </td></th>
!! <tr><td> StartTag </td><td> Start-Tag </td><td> Tag des Wachstumsbeginns der Makrophyten </td></tr>
!! <tr><td> StartMonat </td><td> Start-Monat </td><td> Monat des Wachstumsbeginns der Makrophyten </td></tr>
!! <tr><td> MaxTag </td><td> Max.-Tag </td><td> Tag, an dem die Makrophytenbiomasse ihr Maximum hat </td></tr>
!! <tr><td> MaxMonat </td><td> Max.-Monat </td><td> Monat, in dem die Makrophytenbiomasse ihr Maximum hat </td></tr>
!! <tr><td> EndTag </td><td> Ende-Tag </td><td> Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum </td></tr>
!! <tr><td> EndMonat </td><td> Ende-Monat </td><td> Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat </td></tr>
!! </table>
!! \n
!! <table>
!! <tr><td> P-Zeile </td><td> Dichte der Makrophyten </td><td>- </td><td> Makrophyten-Dichte </td></th>
!! <tr><td> PflMin </td><td> min. Dichte (Winter) </td><td> g/m² </td><td> Minimale Dichte der Makrophyten im Winter </td></tr>
!! <tr><td> PflMax </td><td> max. Dichte (Sommer) </td><td> g/m² </td><td> Maximale Dichte der Makrophyten im Sommer </td></tr>
!! </table>
!!\n
!! Quelle mphyt_huelle.f95; zurück zu: \ref lnk_ueberblick

!> SUBROUTINE mphyt_huelle() wird beschrieben in: \ref mphyt \n
!! Quelle mphyt_huelle.f95
      SUBROUTINE mphyt_huelle(i)
      use modell                                                 
      use QSimDatenfelder
      implicit none
      integer :: i,nk
      real :: pflmin,pflmax
      real :: sa, su 

      iglob=(i+meinrang*part)
      nk=(i-1)*number_plankt_vari
      kontroll = iglob.eq.kontrollknoten

      tiefe(1:2) = rb_hydraul_p(2+(i-1)*number_rb_hydraul) ! Wassertiefe
      tempw(1:2) = planktonic_variable_p( 1+nk)  ! Wassertemperatur
      anze=1            ! Anzahl der Profile im aktuellen Strang
      po2p(1:2) = transfer_quantity_p(30+(i-1)*number_trans_quant) ! Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt
      po2r(1:2) = transfer_quantity_p(31+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt
      pfldalg(1:2) = 0.0 ! unbenutzte Variable
      tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim-3D) in real Tage (QSim-1D)

      itags=tag             ! Tag im Monat  module::modell zeitsekunde()
      monats=monat          ! Monat im Jahr module::modell zeitsekunde()
      itstart = zone(point_zone(iglob))%macrophyt%starttag
      mstart  = zone(point_zone(iglob))%macrophyt%startmonat
      itmax   = zone(point_zone(iglob))%macrophyt%maxtag
      mmax    = zone(point_zone(iglob))%macrophyt%maxmonat
      itend   = zone(point_zone(iglob))%macrophyt%endtag
      mend    = zone(point_zone(iglob))%macrophyt%endmonat
      schwi(1:2)=transfer_quantity_p(64+(i-1)*number_trans_quant)      ! Globalstrahlung in cal/(cm2*h) von strahlg() berechnet

      pflmin=zone(point_zone(iglob))%macrodicht%pflmin ! Minimale Dichte der Makrophyten im Winter
      pflmax=zone(point_zone(iglob))%macrodicht%pflmax ! Maximale Dichte der Makrophyten im Sommer
      pfl(1:2) = benthic_distribution_p(3+(i-1)*number_benth_distr) ! Trockengewicht Wasserpflanzen in g/m²
      sa = sonnenaufgang
      su = sonnenuntergang
      ! module_QSimDatenfelder.f95:    integer , parameter :: ilang=1
      extk(1:2) = transfer_quantity_p(54+(i-1)*number_trans_quant) ! mittlerer Extinktionskoeffizient
      mstr = 1        ! Strangzähler | nur ein Profil in einem Strang
      ifehl=0  ! if ISNAN(tempmt)(zwischenwert Wassertemperatur) > ifehl=24
      ifhStr=0 ! Strangnummer in dem der Fehler auftrat

      if(kontroll)print*,'vor mphyt() pfl,po2p,po2r=',pfl(1),po2p(1),po2r(1)
!----------------------------------------------------------------------------------
      call mphyt(tiefe,tempw,anze,po2p,po2r,pfldalg,tflie               &
     &,itags,monats,itstart,mstart,itmax,mmax,itend,mend,schwi          &
     &,pflmin,pflmax,pfl,sa,su,ilang,extk,mstr,ifehl,ifhstr             &                                                   
     &,kontroll ,iglob ) !!wy  
!----------------------------------------------------------------------------------
      if(kontroll)print*,'nach mphyt() pfl,po2p,po2r=',pfl(1),po2p(1),po2r(1)

      benthic_distribution_p(3+(i-1)*number_benth_distr) = pfl(1) ! Trockengewicht Wasserpflanzen in g/m²
      transfer_quantity_p(30+(i-1)*number_trans_quant) = po2p(1)  ! Sauerstoffproduktion durch Makrophyten in mgO2/l je Zeitschritt
      transfer_quantity_p(31+(i-1)*number_trans_quant) = po2r(1)  ! Sauerstoffverbrauch durch Makrophyten in mgO2/l je Zeitschritt

      RETURN 
      END subroutine mphyt_huelle

!! <ParamSetDef Id="QM" Text="Makrophyten" Help="Makrophyten-Wachstum" Scope="Strang">
!!   <Parameter Ident="StartTag" Text="Start-Tag" Unit="" Format="I2" Null="-1" Help="Tag des Wachstumsbeginns der Makrophyten" Min="1" Max="31" Default="" />
!!   <Parameter Ident="StartMonat" Text="Start-Monat" Unit="" Format="I2" Null="-1" Help="Monat des Wachstumsbeginns der Makrophyten" Min="1" Max="12" Default="" />
!!   <Parameter Ident="MaxTag" Text="Max.-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="31" Default="" />
!!   <Parameter Ident="MaxMonat" Text="Max.-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Maximum hat" Min="1" Max="12" Default="" />
!!   <Parameter Ident="EndTag" Text="Ende-Tag" Unit="" Format="I2" Null="-1" Help="Tag, an dem die Makrophytenbiomasse ihr Minimum erreicht hat. Hier endet das Makrophytenwachstum" Min="1" Max="31" Default="" />
!!   <Parameter Ident="EndMonat" Text="Ende-Monat" Unit="" Format="I2" Null="-1" Help="Monat, in dem die Makrophytenbiomasse ihr Minimum erreicht hat" Min="1" Max="12" Default="" />
!      type :: maphy
!         integer :: starttag,startmonat,maxtag,maxmonat,endtag,endmonat
!      end type maphy
!         type (maphy) :: macrophyt         ! M Makrophyten

!qsim.f90:      itstart = itsts(mstr)
!qsim.f90:      mstart = msts(mstr)
!qsim.f90:      itmax = itmaxs(mstr)
!qsim.f90:      mmax = mmaxs(mstr)
!qsim.f90:      itend = itends(mstr)
!qsim.f90:      mend = mends(mstr)
!qsim.f90:      if(ckenn.eq.'M')read(77,1031)itsts(mstr),msts(mstr),itmaxs(mstr),mmaxs(mstr),itends(mstr),mends(mstr)                



