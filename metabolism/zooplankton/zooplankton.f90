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

!> Calculation of zooplankton dynamics
!!
!! @author: Volker Kirchesch
!! @date: 21.5.2015
subroutine zooplankton(zooind_s, aki_s, agr_s, abl_s, vo2_s,   &
                       tempw_s, chnf_s, tflie,                 &
                       ir_s, iras_s, zhnf_s, zbac_s, dzres1_s, &
                       abszo_s, dzres2_s, zexki_s, zexgr_s,    &
                       zexbl_s, algzok_s, algzog_s, algzob_s,  &
                       rmuas_s, rakr_s, rbar_s, hnfza_s,       &
                       control, jjj)

   use module_alloc_dimensions
   use module_aparam
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(inout) :: zooind_s !< zooplankton [Ind/l]
   real,    intent(in)    :: aki_s    !<
   real,    intent(in)    :: agr_s    !<
   real,    intent(in)    :: abl_s    !<
   real,    intent(in)    :: vo2_s    !< oxygen [mg/l]
   real,    intent(in)    :: tempw_s  !< water temperature [°C]
   real,    intent(in)    :: chnf_s   !<
   real,    intent(in)    :: tflie    !< timestep [d]
   real,    intent(out)   :: ir_s     !<
   real,    intent(out)   :: iras_s   !<
   real,    intent(out)   :: zhnf_s   !<
   real,    intent(out)   :: zbac_s   !<
   real,    intent(out)   :: dzres1_s !<
   real,    intent(out)   :: abszo_s  !<
   real,    intent(out)   :: dzres2_s !<
   real,    intent(out)   :: zexki_s  !<
   real,    intent(out)   :: zexgr_s  !<
   real,    intent(out)   :: zexbl_s  !<
   real,    intent(out)   :: algzok_s !<
   real,    intent(out)   :: algzog_s !<
   real,    intent(out)   :: algzob_s !<
   real,    intent(out)   :: rmuas_s  !<
   real,    intent(out)   :: rakr_s   !<
   real,    intent(out)   :: rbar_s   !<
   real,    intent(out)   :: hnfza_s  !<
   logical, intent(in)    :: control !< debugging
   integer, intent(in)    :: jjj      !< debugging

   ! --- local variables ---
   real :: rotc, up_crot, hc_irmax, clearrlog, clearr, volrot
   real :: clearr_ind, irmax_ind, fks, rot, ftmor, morrot
   real :: fting, filo2, filabio, hconf, hcaki
   real :: hconki, hcongr, hconbl, hconm
   real :: zass, ir_f, prodrot, rott, zooint, zooint_old, delzoo

   real, parameter :: dokrit  = 2.5
   real, parameter :: mormax  = 0.15  ! mormax = 0.32
   real, parameter :: assmxr  = 0.84  ! Verschoor et al. (2007)
   real, parameter :: respar  = 0.203
   real, parameter :: emort   = 2.00
   real, parameter :: eass    = 0.705 ! Verschoor et al. (2007)
   real, parameter :: thresr  = 1.12
   real, parameter :: thmorr  = 1.077
   real, parameter :: thing   = 1.08
   real, parameter :: zqz10   = 2.23
   real, parameter :: ztmax   = 26.1
   real, parameter :: ztopt   = 22.2
   real, parameter :: zellvgr = 320.0
   real, parameter :: zellvki = 645.0
   real, parameter :: zellvbl = 1000.0

   external :: print_clipping
   
   
   ! Umrechnung der Individienzahl in Biomasse (g/m3)
   rot = zooind_s * grot / 1000.
   rotc = grot * czoo   
   
   ! --------------------------------------------------------------------------
   ! Calculation of parameters if not given in AParam
   ! --------------------------------------------------------------------------
   ! up_crot: gewichtszpezifische maximale ingestionsrate µc^(-2/3)*d-1
   if (irmax < 0. .and. grot > 0.) then
      up_crot = 10**(-0.8377 * log10(rotc) + 0.3131)
   else
      up_crot = irmax
   endif
   hc_irmax = up_CROT * RotC**(2./3.)

   ! halfsatturation constant
   if (foptr < 0. .and. grot > 0.) then
      clearrlog = -0.9987 * log10(rotc) - 0.706
      ! clearance rate (1/h)
      clearr    = 1.e5 * (10**(clearrlog)*rotc**(2./3.))
      volrot = rotc * 1.e6 / 0.12
      clearr_ind = clearr * volrot / 1.e9
      irmax_ind = hc_irmax * volrot * 0.00012/24.
      fks = irmax_ind / clearr_ind
   else
      fks = foptr
   endif
   
   ! --------------------------------------------------------------------------
   ! basal respiration
   ! --------------------------------------------------------------------------
   rbar_s = zresg * thresR**(tempw_s - 20.)

   ! --------------------------------------------------------------------------
   ! mortality rate
   ! --------------------------------------------------------------------------
   ! --- influence of oxygen ---
   filo2 = vo2_s / dokrit
   
   ! --- influence of temperature ---
   fTmoR = thmorR**(tempw_s -20.)

   ! --- influence of alage ---
   ! filtrierbare Algenbiomasse (in mgC/l)
   filabio = aki_s * Caki + agr_s * Cagr + abl_s * Cabl
   hconF = max(0., min(1., filabio / (filabio + FKs)))

   if (aki_s + agr_s + abl_s <= epsilon(aki_s)) then
      hcaki = 1.e-6
   else
      hcaki = aki_s
   endif

   hconki = hcaki / (hcaki + agr_s + abl_s)
   hcongr = agr_s / (hcaki + agr_s + abl_s)
   hconbl = abl_s / (hcaki + agr_s + abl_s)
   hconf = hconf * (zaki * hconki + zagr * hcongr + zabl * hconbl)
   hconm = min(hconf, filo2)

   morrot = (-0.14 * hconm**2 - 0.0093 * hconm + mormax) * ftmor

   ! --------------------------------------------------------------------------
   ! growth
   ! --------------------------------------------------------------------------
   ! assimilationsrate
   if (hconF == 0.0) then
      zass = 0.0
   else
      zass = min(1., ASSmxR * exp(-EASS * hconF))
   endif

   ! influence of temperature
   fting = thing**(tempw_s - 20.)

   ir_f = hc_irmax * hconf
   prodrot = (zass - respar) * ir_f * fting - rbar_s
   ir_s   = ir_f * fting * tflie * rot
   iras_s = ir_f * fting

   ! ir_s - Ingestionsrate in mg/(l h)
   ! zHNF_s - Aufnahmerate der HNF
   ! zBAC_s - Aufnahmerate der Bakterien
   zBAC_s = 0.0
   if (ir_s /= 0.0) then
     zhnf_s = ir_s * chnf_s / (chnf_s + agr_s + aki_s + abl_s)
   else
     zhnf_s = 0.0
   endif

   ! --------------------------------------------------------------------------
   ! timestep
   ! --------------------------------------------------------------------------
   rott = rot * exp((prodrot-morRot)*tflie) ! Rotatorienzunahme
   zooint = rott * 1000. / grot
   if (zooint < 0.0) then
      zooint_old = zooint
      delzoo = zooint - zooind_s
      zooint = zooind_s / (zooind_s + abs(delzoo)) * zooind_s
      call print_clipping("konsum", "zooint", zooint_old, zooint, "Ind/l")
   endif

   ! --------------------------------------------------------------------------
   ! output
   ! --------------------------------------------------------------------------
   dzres1_s = rot  * (1. - exp(-rbar_s * tflie))
   abszo_s  = rott * (1. - exp(-morrot * tflie))
   dzres2_s = respar * ir_s

   zexki_s = ir_s * (1. - zass) * hconki
   zexgr_s = ir_s * (1. - zass) * hcongr
   zexbl_s = ir_s * (1. - zass) * hconbl

   algzok_s = min(aki_s * zaki, ir_s * hconki)
   algzog_s = min(agr_s * zagr, ir_s * hcongr)
   algzob_s = min(abl_s * zabl, ir_s * hconbl)

   rmuas_s = prodrot - morrot
   rakr_s  = morRot   ! ras(ior) * respaR
   rbar_s  = rbar_s
   
   if (chnf_s /= 0.0) then
      hnfza_s = (zhnf_s / chnf_s) * 24.
   else
      hnfza_s = 0.0
   endif
   
   ! --------------------------------------------------------------------------
   ! update return values
   ! --------------------------------------------------------------------------
   zooind_s = zooint

end subroutine zooplankton
