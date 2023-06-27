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
subroutine verteilungskoeff(hcss, hcph, vtkoeff_zn, vtkoeff_cu, vtkoeff_cad, &
                            vtkoeff_ni, vtkoeff_as, vtkoeff_pb, vtkoeff_cr,  &
                            vtkoeff_fe, vtkoeff_hg, vtkoeff_mn, vtkoeff_u,   &
                            iformvert, control)
   
   use module_aparam
   
   implicit none
   
   ! --- dummy arguments ---
   real,    intent(in)  :: hcss
   real,    intent(in)  :: hcph
   real,    intent(out) :: vtkoeff_zn
   real,    intent(out) :: vtkoeff_cad
   real,    intent(out) :: vtkoeff_cu
   real,    intent(out) :: vtkoeff_ni
   real,    intent(out) :: vtkoeff_as
   real,    intent(out) :: vtkoeff_pb
   real,    intent(out) :: vtkoeff_cr
   real,    intent(out) :: vtkoeff_fe
   real,    intent(out) :: vtkoeff_hg
   real,    intent(out) :: vtkoeff_mn
   real,    intent(out) :: vtkoeff_u
   integer, intent(in)  :: iformvert
   logical, intent(in)  :: control  !< debugging
   
   ! Konstanten und Exponenten zur Berechnung der Verteilungsfunktion
   if (iformvert == 1) then
      ! --- DWA-Modell ---
      
      ! Berechnung der Verteilungskoeffizienten VTKoeff_Zn, VTKoeff_Cu, VTKoeff_Cad, VTKoeff_Ni
      ! VTKoff [l/g]
      vtkoeff_zn = (c1zn/hcss**e1zn) + (c2zn/hcss**e2zn)*((hcph/9.)**(c3zn/hcss**e3zn))          &
                 + ((c4zn/hcss**e4zn)+c5zn)*(((hcph-4.)/5.)**e5zn-((hcph-4.)/5.)**(e5zn-1))
      vtkoeff_cu = (c1cu/hcss**e1cu) + (c2cu/hcss**e2cu)*((hcph/9.)**(c3cu/hcss**e3cu))          &
                 + ((c4cu/hcss**e4cu)+c5cu)*(((hcph-4.)/5.)**e5cu-((hcph-4.)/5.)**(e5cu-1))
      vtkoeff_cad = (c1cad/hcss**e1cad) + (c2cad/hcss**e2cad)*((hcph/9.)**(c3cad/hcss**e3cad))   &
                  + ((c4cad/hcss**e4cad)+c5cad)*(((hcph-4.)/5.)**e5cad-((hcph-4.)/5.)**(e5cad-1))
      vtkoeff_ni = (c1ni/hcss**e1ni) + (c2ni/hcss**e2ni)*((hcph/9.)**(c3ni/hcss**e3ni))          &
                 + ((c4ni/hcss**e4ni)+c5ni)*(((hcph-4.)/5.)**e5ni-((hcph-4.)/5.)**(e5ni-1))
      vtkoeff_as = (c1as/hcss**e1as) + (c2as/hcss**e2as)*((hcph/9.)**(c3as/hcss**e3as))          &
                 + ((c4as/hcss**e4as)+c5as)*(((hcph-4.)/5.)**e5as-((hcph-4.)/5.)**(e5as-1))
      
      vtkoeff_pb = (c1pb/hcss**e1pb) + (c2pb/hcss**e2pb)*((hcph/9.)**(c3pb/hcss**e3pb))          &
                 + ((c4pb/hcss**e4pb)+c5pb)*(((hcph-4.)/5.)**e5pb-((hcph-4.)/5.)**(e5pb-1))
      vtkoeff_cr = (c1cr/hcss**e1cr) + (c2cr/hcss**e2cr)*((hcph/9.)**(c3cr/hcss**e3cr))          &
                 + ((c4cr/hcss**e4cr)+c5cr)*(((hcph-4.)/5.)**e5cr-((hcph-4.)/5.)**(e5cr-1))
      vtkoeff_fe = (c1fe/hcss**e1fe) + (c2fe/hcss**e2fe)*((hcph/9.)**(c3fe/hcss**e3fe))          &
                 + ((c4fe/hcss**e4fe)+c5fe)*(((hcph-4.)/5.)**e5fe-((hcph-4.)/5.)**(e5fe-1))
      
      vtkoeff_hg = (c1hg/hcss**e1hg) + (c2hg/hcss**e2hg)*((hcph/9.)**(c3hg/hcss**e3hg))          &
                 + ((c4hg/hcss**e4hg)+c5hg)*(((hcph-4.)/5.)**e5hg-((hcph-4.)/5.)**(e5hg-1))
      vtkoeff_mn = (c1mn/hcss**e1mn) + (c2mn/hcss**e2mn)*((hcph/9.)**(c3mn/hcss**e3mn))          &
                 + ((c4mn/hcss**e4mn)+c5mn)*(((hcph-4.)/5.)**e5mn-((hcph-4.)/5.)**(e5mn-1))
      vtkoeff_u = (c1u/hcss**e1u) + (c2u/hcss**e2u)*((hcph/9.)**(c3u/hcss**e3u))                 &
                + ((c4u/hcss**e4u)+c5u)*(((hcph-4.)/5.)**e5u-((hcph-4.)/5.)**(e5u-1))
   else
      ! --- deltares 2010 ---
      vtkoeff_zn  = vtkoeffde_zn
      vtkoeff_cu  = vtkoeffde_cu
      vtkoeff_cad = vtkoeffde_cad
      vtkoeff_ni  = vtkoeffde_ni
      vtkoeff_as  = vtkoeffde_as
      vtkoeff_pb  = vtkoeffde_pb
      vtkoeff_cr  = vtkoeffde_cr
      vtkoeff_fe  = vtkoeffde_fe
      vtkoeff_hg  = vtkoeffde_hg
      vtkoeff_mn  = vtkoeffde_mn
      vtkoeff_u   = vtkoeffde_u
   endif
end subroutine verteilungskoeff
