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
!> Berechnung der Verteilung von gelöster zu gesamter Schwermetallkonzentration
!!
!! Berechnung der Schwermetallkonzentrationen
!! Cd, Zn, Cu, Ni, As, Pb, Cr, Fe, Hg, Mn, U
!! @author Volker Kirchesch
!! @date 21.07.2019
!! file: Verteilungskoeff.f90 zurück: \ref lnk_schwermetalle
subroutine verteilungskoeff(hcSS,hcph  &
                            ,VTKoeff_Zn,VTKoeff_Cu,VTKoeff_Cad,VTKoeff_Ni,VTKoeff_As,VTKoeff_Pb           &
                            ,VTKoeff_Cr,VTKoeff_Fe,VTKoeff_Hg ,VTKoeff_Mn,VTKoeff_U                       &
                            ,iformVert, kontroll)
   
   use aparam
   
   implicit none
   
   real                   :: hcSS, hcph
   real                   :: VTKoeff_Zn, VTKoeff_Cad, VTKoeff_Cu, VTKoeff_Ni, VTKoeff_As, VTKoeff_Pb
   real                   :: VTKoeff_Cr, VTKoeff_Fe, VTKoeff_Hg, VTKoeff_Mn, VTKoeff_U
   integer                :: iformVert
   logical, intent(in)    :: kontroll  !< debugging
   
   ! Konstanten und Exponenten zur Berechnung der Verteilungsfunktion
   if (iformVert == 1) then
      ! --- DWA-Modell ---
      
      ! Berechnung der Verteilungskoeffizienten VTKoeff_Zn, VTKoeff_Cu, VTKoeff_Cad, VTKoeff_Ni
      ! VTKoff [l/g]
      VTKoeff_Zn = (c1Zn/hcSS**e1Zn) + (c2Zn/hcSS**e2Zn)*((hcph/9.)**(c3Zn/hcSS**e3Zn))          &
                   + ((c4Zn/hcSS**e4Zn)+c5Zn)*(((hcph-4.)/5.)**e5Zn-((hcph-4.)/5.)**(e5Zn-1))
      VTKoeff_Cu = (c1Cu/hcSS**e1Cu) + (c2Cu/hcSS**e2Cu)*((hcph/9.)**(c3Cu/hcSS**e3Cu))          &
                   + ((c4Cu/hcSS**e4Cu)+c5Cu)*(((hcph-4.)/5.)**e5Cu-((hcph-4.)/5.)**(e5Cu-1))
      VTKoeff_Cad = (c1Cad/hcSS**e1Cad) + (c2Cad/hcSS**e2Cad)*((hcph/9.)**(c3Cad/hcSS**e3Cad))   &
                    + ((c4Cad/hcSS**e4Cad)+c5Cad)*(((hcph-4.)/5.)**e5Cad-((hcph-4.)/5.)**(e5Cad-1))
      VTKoeff_Ni = (c1Ni/hcSS**e1Ni) + (c2Ni/hcSS**e2Ni)*((hcph/9.)**(c3Ni/hcSS**e3Ni))          &
                   + ((c4Ni/hcSS**e4Ni)+c5Ni)*(((hcph-4.)/5.)**e5Ni-((hcph-4.)/5.)**(e5Ni-1))
      VTKoeff_As = (c1As/hcSS**e1As) + (c2As/hcSS**e2As)*((hcph/9.)**(c3As/hcSS**e3As))          &
                   + ((c4As/hcSS**e4As)+c5As)*(((hcph-4.)/5.)**e5As-((hcph-4.)/5.)**(e5As-1))
      
      VTKoeff_Pb = (c1Pb/hcSS**e1Pb) + (c2Pb/hcSS**e2Pb)*((hcph/9.)**(c3Pb/hcSS**e3Pb))          &
                   + ((c4Pb/hcSS**e4Pb)+c5Pb)*(((hcph-4.)/5.)**e5Pb-((hcph-4.)/5.)**(e5Pb-1))
      VTKoeff_Cr = (c1Cr/hcSS**e1Cr) + (c2Cr/hcSS**e2Cr)*((hcph/9.)**(c3Cr/hcSS**e3Cr))          &
                   + ((c4Cr/hcSS**e4Cr)+c5Cr)*(((hcph-4.)/5.)**e5Cr-((hcph-4.)/5.)**(e5Cr-1))
      VTKoeff_Fe = (c1Fe/hcSS**e1Fe) + (c2Fe/hcSS**e2Fe)*((hcph/9.)**(c3Fe/hcSS**e3Fe))          &
                   + ((c4Fe/hcSS**e4Fe)+c5Fe)*(((hcph-4.)/5.)**e5Fe-((hcph-4.)/5.)**(e5Fe-1))
      
      VTKoeff_Hg = (c1Hg/hcSS**e1Hg) + (c2Hg/hcSS**e2Hg)*((hcph/9.)**(c3Hg/hcSS**e3Hg))          &
                   + ((c4Hg/hcSS**e4Hg)+c5Hg)*(((hcph-4.)/5.)**e5Hg-((hcph-4.)/5.)**(e5Hg-1))
      VTKoeff_Mn = (c1Mn/hcSS**e1Mn) + (c2Mn/hcSS**e2Mn)*((hcph/9.)**(c3Mn/hcSS**e3Mn))          &
                   + ((c4Mn/hcSS**e4Mn)+c5Mn)*(((hcph-4.)/5.)**e5Mn-((hcph-4.)/5.)**(e5Mn-1))
      VTKoeff_U = (c1U/hcSS**e1U) + (c2U/hcSS**e2U)*((hcph/9.)**(c3U/hcSS**e3U))                 &
                  + ((c4U/hcSS**e4U)+c5U)*(((hcph-4.)/5.)**e5U-((hcph-4.)/5.)**(e5U-1))
   else
      ! --- Deltares 2010 ---
      VTKoeff_Zn  = VTkoeffDe_Zn
      VTKoeff_Cu  = VTkoeffDe_Cu
      VTKoeff_Cad = VTkoeffDe_Cad
      VTKoeff_Ni  = VTkoeffDe_Ni
      VTKoeff_As  = VTkoeffDe_As
      VTKoeff_Pb  = VTkoeffDe_Pb
      VTKoeff_Cr  = VTkoeffDe_Cr
      VTKoeff_Fe  = VTkoeffDe_Fe
      VTKoeff_Hg  = VTkoeffDe_Hg
      VTKoeff_Mn  = VTkoeffDe_Mn
      VTKoeff_U   = VTkoeffDe_U
   endif
end subroutine verteilungskoeff
