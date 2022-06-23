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
!> \page hnf heterotrophe Nanoflagellaten (HNF)
!!
!! <h2>Herkunft</h2>
!!
!! UNTERPROGRAMM ZUR BERECHNUNG DES Einflusses der heterotrophen \n
!! Nanoflagelaten AUF DEN STOFF-UND SAUERSTOFFHAUSHALT EINES\n
!! Fliessgewässers\n
!! AUTOR :      VOLKER KIRCHESCH \n
!! STAND :      15.06.2001 \n
!! VARIABLENLISTE:   \n
!! irmax  - max. Filtrierate in mueg Chl-a/(100 Ind*d) \n
!!
!! <h2>Teilprozesse</h2>
!! Die HNF Biomasse gemessen an ihrem Kohlenstoffanteil [μg C * l-1]\n
!! \n
!! wächst an infolge der Aufnahme von Bakterien und \n
!! nimmt ab infolge Mortalität, Respiration und Exkretion  \n
!! \n
!! ausserdem werden die heterotrophe Nanoflagellaten \n
!! vom Zooplankton ( Rotatorien, konsum() ) und \n
!! von den benthischen Filtrierern (Dreissena Muscheln dreissen() ) gefressen. \n
!! \n
!! <h2>Dokumentation und Veröffentlichungen</h2>
!! Kapitel <a href="./pdf/HNF_QSimDoku.pdf" target="_blank">8.Heterotrophe Nanoflagellaten</a> Ausschnitt aus:\n
!! "QSim - Das Gewässergütemodell der Bundesanstalt für Gewässerkunde" (QSimDoku_Kap7bis8bearbeiten.docx)\n
!! Änderungsstand 20. Juni 2018\n\n
!! <a href="./pdf/2002_Bergfeld_Dissertation_RhMoSa.pdf" target="_blank">Dissertation Tanja Bergfeld</a>\n
!!
!! <h2>Schnittstellenbeschreibung / IT-Realisierung</h2>
!!      subroutine hnf(\ref chnf,\ref bvhnf,\ref bac,\ref tempw,\ref vo2,\ref tflie                     &\n
!!     &,\ref echnf,\ref ebvhnf,\ref flag,\ref elen,\ref ior,\ref anze,\ref qeinl,\ref vabfl                      &\n
!!     &,\ref jiein,\ref drhnf,\ref zhnf,\ref hnfbac,\ref ro2hnf,\ref bsbhnf,\ref hnfmua,\ref uphnfe,\ref backse       &\n
!!     &,\ref hnfrea,\ref hnfupa,\ref hnfmoa,\ref hnfexa,\ref fkm,\ref mstr,\ref itags,\ref monats,\ref uhrz           & \n
!!     &,\ref kontroll ,\ref iglob ) !!wy  \n
!!
!! \n
!! Quelle hnf_huelle.f95; zurück zu: \ref lnk_ueberblick
!> SUBROUTINE hnf_huelle() wird beschrieben in: \ref hnf \n
!! Quelle hnf_huelle.f95
subroutine hnf_huelle(i)
   use modell
   use QSimDatenfelder
   use aparam
   implicit none
   integer :: i
   iglob = (i+meinrang*part)
   if (kontroll)print*,'hnf_huelle ... start',iglob
   CHNF(1:2) = planktonic_variable_p(48+(i-1)*number_plankt_vari) ! C-Masse der heterotrophen Nanoflagelaten
   if (CHNF(1) <= 0.0) CHNF(1:2) = 0.0 ! CHNF=-1 meint keine HNF
   BVHNF(1:2) = planktonic_variable_p(49+(i-1)*number_plankt_vari) ! Biovolumen der HNF
   if (BVHNF(1) <= 0.0) BVHNF(1:2) = 0.0 ! BVHNF=-1 meint keine HNF
   BAC(1:2) = planktonic_variable_p(42+(i-1)*number_plankt_vari) ! Masse der in heterotrophen Bakterien gespeicherten C-Verbindungen
   tempw(1:2) = planktonic_variable_p( 1+(i-1)*number_plankt_vari) ! Wassertemperatur
   vo2(1:2) = planktonic_variable_p( 2+(i-1)*number_plankt_vari) ! Sauerstoffgehalt tiefengemittelt
   tflie = real(deltat)/86400 ! Umwandlung des Zeitschritts von integer sekunden (QSim3D) in real Tage (QSim)
   eCHNF(1) = 0.0    ! Einleitungswert (keine Einleitungen in QSim3D)
   eBVHNF(1) = 0.0   ! Einleitungswert (keine Einleitungen in QSim3D)
   flag(1:2) = 0       ! keine Einleitungen
   elen(1:2) = 1       ! Elementlänge (nicht verwendet)
   ior = 1             ! Laufindex
   anze = 1            ! Anzahl der Profile im aktuellen Strang
   qeinl(1:2) = 0.0  ! kein Abfluss Einleitung
   vabfl(1:2) = 2.5  ! wird nur bei Einleitungen verwendet, die sind aber ausgeschaltet.
   jiein(1:2) = 0      ! null Punkte Einleitungen
   drHNF(1:2) = transfer_quantity_p(96+(i-1)*number_trans_quant) ! Dreissena-Muscheln fressen HNF
   zHNF(1:2) = transfer_quantity_p(74+(i-1)*number_trans_quant) ! zooplankton frisst HNF
   HNFBAC(1:2) = transfer_quantity_p(11+(i-1)*number_trans_quant) ! Verlust der Bakterien durch HNF-Grazing
   rO2HNF(1:2) = transfer_quantity_p(44+(i-1)*number_trans_quant) ! Sauerstoffverbrauch durch Respiration HNF
   BSBHNF(1:2) = transfer_quantity_p(10+(i-1)*number_trans_quant) ! Absterben und Exkretion Heterotropher Naloflagelaten
   ! APARAM.txt: upHNFe, BACkse
   ! Ausgabe: HNFmua,HNFrea,HNFupa,HNFmoa,HNFexa
   fkm = 0    ! (nicht verwendet)
   mstr = 1    ! (nicht verwendet) Strangzähler | nur ein Profil in einem Strang
   itags = 0  ! (nicht verwendet)
   monats = 0  ! (nicht verwendet)
   uhrz = 0  ! (nicht verwendet)
   !kontroll - ob kontrollausgabe, iglob -globale Knoten/Elementnummer der Berechnungsstützstelle
   !----------------------------------------------------------------------------------
   call HNF(CHNF,BVHNF,BAC,TEMPW,VO2,TFLIE                     &
            ,echnf,eBVHNF,flag,elen,ior,anze,qeinl,vabfl                      &
            ,jiein,drHNF,zHNF,HNFBAC,rO2HNF,BSBHNF,HNFmua,upHNF,BACks       &
            ,HNFrea,HNFupa,HNFmoa,HNFexa,fkm,mstr,itags,monats,uhrz           &
            ,kontroll ,iglob )
   !----------------------------------------------------------------------------------
   if (kontroll)print*,'hnf_huelle: CHNF,BVHNF = ',CHNF(1),BVHNF(1)
   planktonic_variable_p(48+(i-1)*number_plankt_vari) = CHNF(1) ! C-Masse der heterotrophen Nanoflagelaten
   planktonic_variable_p(49+(i-1)*number_plankt_vari) = BVHNF(1)! Biovolumen der HNF
   transfer_quantity_p(96+(i-1)*number_trans_quant) = drHNF(1)  ! Dreissena-Muscheln fressen HNF
   transfer_quantity_p(74+(i-1)*number_trans_quant) = zHNF(1)   ! Zooplankton frisst HNF
   transfer_quantity_p(11+(i-1)*number_trans_quant) = HNFBAC(1) ! Verlust der Bakterien durch HNF-Grazing
   transfer_quantity_p(44+(i-1)*number_trans_quant) = rO2HNF(1) ! Sauerstoffverbrauch durch Respiration HNF
   transfer_quantity_p(10+(i-1)*number_trans_quant) = BSBHNF(1) ! Absterben und Exkretion Heterotropher Naloflagelaten
   if (kontroll)print*,'hnf_huelle: HNFmua,HNFrea,HNFupa,HNFmoa,HNFexa = ',HNFmua(1),HNFrea(1),HNFupa(1),HNFmoa(1),HNFexa(1)
   return
end subroutine hnf_huelle
