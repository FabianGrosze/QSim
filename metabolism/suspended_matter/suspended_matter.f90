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
!> Berechnung der Schwebstoffkonzentration
!! @author Volker Kirchesch
!! @date 22.09.2011
subroutine suspended_matter(ss_s, ssdr_s, fssgr_s, aki_s, agr_s, abl_s, zooind_s,       &
                            tiefe_s, rau_s, vmitt_s, tausc_s, dorgss_s, dkimor_s,       &
                            dgrmor_s, dblmor_s, drfaek_s, drfaeg_s, drfaeb_s, drfaes_s, &
                            zexki_s, zexgr_s, zexbl_s, abszo_s, ischif_s,ieros, tflie,    &
                            ssalg_s, sedss_s, sedss_mq_s,                               &
                            control, jjj)
   
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout) :: ss_s      !< organische und anorganischer Schwebstoffe (ohne Algen und Zooplankton)
   real,    intent(inout) :: ssdr_s
   real,    intent(inout) :: fssgr_s
   real,    intent(in)    :: aki_s
   real,    intent(in)    :: agr_s
   real,    intent(in)    :: abl_s
   real,    intent(in)    :: zooind_s
   real,    intent(in)    :: tiefe_s
   real,    intent(in)    :: rau_s
   real,    intent(in)    :: vmitt_s
   real,    intent(in)    :: tausc_s
   real,    intent(in)    :: dorgss_s
   real,    intent(in)    :: dkimor_s
   real,    intent(in)    :: dgrmor_s
   real,    intent(in)    :: dblmor_s
   real,    intent(in)    :: drfaek_s
   real,    intent(in)    :: drfaeg_s
   real,    intent(in)    :: drfaeb_s
   real,    intent(in)    :: drfaes_s
   real,    intent(in)    :: zexki_s
   real,    intent(in)    :: zexgr_s
   real,    intent(in)    :: zexbl_s
   real,    intent(in)    :: abszo_s
   integer, intent(in)    :: ischif_s
   integer, intent(in)    :: ieros
   real,    intent(in)    :: tflie
   real,    intent(out)   :: ssalg_s   !< Gesamtschwebstoffe
   real,    intent(out)   :: sedss_s
   real,    intent(out)   :: sedss_mq_s
   logical, intent(in)    :: control  !< debugging
   integer, intent(in)    :: jjj      !< debugging
   
   ! --- local variables ---
   integer  :: ised, jsed
   real     :: exzo, sst, sss_old
   real     :: ust, v6, vges, sssed, zellv, hc, hc1, hc2, fssgrv
   real     :: ustkri, vkrit, qsgr, oc, oc0, wst, ceq
   
   real, parameter :: g = 9.81
   
   external :: print_clipping, schiff, sedimentation
   
   
   
   fssgrv = fssgr_s
   ust = (((1./rau_s)*sqrt(g))/(tiefe_s**0.16667))*abs(vmitt_s)
   ustkri = sqrt(tausc_s/1000.)
   vkrit = (ustkri*tiefe_s**0.166667)/((1./rau_s)*g)
   
   ! Einfluss der Schifffahrt
   if (ischif_s == 0) then
      vges = vmitt_s
   else
      call schiff(vmitt_s, tiefe_s, ischif_s, v6)
      vges = vmitt_s + v6
   endif
   
   sssed = fssgr_s * ss_s
   ised = 3 
   jsed = 1
   zellv = 0.
   call sedimentation(tiefe_s, ised, ust, qsgr, oc, oc0, tflie, &
                      wst, jsed, zellv, control, jjj)
   ceq = sssed*qsgr
   sedss_s = max(0.0,(sssed-ceq)) * oc
   sedss_mq_s = sedss_s
   if (ieros == 1 .and. vges > vkrit)sedss_s = 0.0
   
   exzo = zexki_s+zexgr_s+zexbl_s
   
   SSt = ss_s                           &
       - sedss_s                        &
       + exzo                           &
       + dkimor_s + dgrmor_s + dblmor_s &
       + abszo_s                        &
       - ssdr_s                         &
       + dorgss_s                       &
       + drfaek_s + drfaeg_s + drfaeb_s &
       + drfaes_s
   
   
   
   ! Neuberechnung des Faktors zur Berechnung der ablagerungsfreien Grenzkonzentration
   hc = exzo                             &
      - sedss_s                          &
      + dkimor_s + dgrmor_s + dblmor_s   &
      + abszo_s                          &
      - ssdr_s                           &
      + dorgss_s                         &
      + drfaek_s + drfaeg_s + drfaeb_s   &
      + drfaes_s
   
   hc1 = ss_s + hc
   hc2 = sssed + hc
       
   hc1 = max(0.0, hc1)
   hc2 = max(0.0, hc2)
   
   if (hc1 > 0.0) then
      fssgr_s = hc2/hc1
   else
      fssgr_s = 0.0
   endif
   
   if (sst < 0.0) then
      sss_old = sst
      ss_s = (ss_s/(ss_s+abs(sst-ss_s))) * ss_s
      call print_clipping("schweb_kern", "ss_s", sss_old, ss_s, "mg/l")
   else
      ss_s = sst
   endif
   
   
   ssalg_s = ss_s                  &
          + agr_s + aki_s + abl_s  &
          + (zooind_s * grot/1000.)
   return
end subroutine suspended_matter
