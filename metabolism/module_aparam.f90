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
module aparam
   implicit none
   save
   
   !> global parameter from APARAM.txt
   
   ! Grünalgen
   real, protected    :: agchl, aggmax, IKge, agksn, agksp
   real, protected    :: agremi, frmuge, bsbgr, csbgr, Qmx_NG
   real, protected    :: Qmx_PG, Qmn_NG, Qmn_PG, upmxNG, upmxPG
   real, protected    :: opgrmi, opgrma, asgre, ToptG, kTemp_gr
   ! Kieselalgen
   real, protected    :: akchl, akgmax, IKke, akksn, akksp
   real, protected    :: akkssi, akremi, frmuke, bsbki, csbki
   real, protected    :: Qmx_NK, Qmx_PK, Qmx_SK, Qmn_NK, Qmn_PK
   real, protected    :: Qmn_SK, upmxNK, upmxPK, upmxSK, opkimi
   real, protected    :: opkima, askie, ToptK, kTemp_Ki
   ! Blaualgen
   real, protected    :: abchl, abgmax, IKbe, abksn, abksp, abremi, frmube,    &
                         bsbbl, csbbl, Qmx_NB, Qmx_PB, Qmn_NB, Qmn_PB, upmxNB, &
                         upmxPB, opblmi, opblma, asble, ToptB, kTemp_bl
   integer, protected :: ifix
   ! Rotatorien
   real, protected    :: IRMAX, FOPTR, GROT, ZRESG, ZAKI, ZAGR, ZABL
   ! irmaxe,FopIRe,GRote,zresge,zakie,zagre,zable
   ! Nitrosomonas
   real, protected    :: YNMAX1, STKS1, ANITR1, BNMX1, BNKS1
   ! ynmx1e, stks1e, anitrie, bnmx1e, bnks1e
   ! Nitrobacter
   real, protected    :: YNMAX2, STKS2, ANITR2, BNMX2, BNKS2
   ! ynmx2e, stks2e, anitri2e, bnmx2e, bnks2e
   ! Sediment-Flux
   real, protected    :: KNH4, KapN3, fPOC1, fPOC2, SorpCap, Klang, KdNh3
   ! KNH4e, KapN3e, fPOC1e, fPOC2e, SorpCape, Klange, KdNh3e
   ! Kohlenstoff
   real, protected    :: HyP1, hymxD, KsD1, KsD2, KsM, upBAC, YBAC, rsGBAC
   ! hyPe, hymxDe, KsD1e, KsD2e, KsMe, upBACe, YBACe, rsGBACe
   ! Muscheln
   real, protected    :: FoptD
   ! FoptDe
   ! HNF
   real, protected    :: upHNF,  BACks
   !real,  protected :: upHNFe, BACkse 
   ! Wasser
   real, protected    :: alamda
   !Hygiene
   real, protected    :: ratecd, etacd, rateci, xnuec, ratecg, ratecs
   ! ratecde, etacde, ratecie, xnuece, ratecge, ratecse
   ! Schwermetalle
   real, protected    :: c1Pb, e1Pb, c2Pb, e2Pb, c3Pb, e3Pb, c4Pb, e4Pb, c5Pb, e5Pb, VTKoeffDe_Pb
   real, protected    :: c1Cad, e1Cad, c2Cad, e2Cad, c3Cad, e3Cad, c4Cad, e4Cad, c5Cad, e5Cad, VTKoeffDe_Cad
   real, protected    :: c1Cr, e1Cr, c2Cr, e2Cr, c3Cr, e3Cr, c4Cr, e4Cr, c5Cr, e5Cr, VTKoeffDe_Cr
   real, protected    :: c1Fe, e1Fe, c2Fe, e2Fe, c3Fe, e3Fe, c4Fe, e4Fe, c5Fe, e5Fe, VTKoeffDe_Fe
   real, protected    :: c1Cu, e1Cu, c2Cu, e2Cu, c3Cu, e3Cu, c4Cu, e4Cu, c5Cu, e5Cu, VTKoeffDe_Cu
   real, protected    :: c1Mn, e1Mn, c2Mn, e2Mn, c3Mn, e3Mn, c4Mn, e4Mn, c5Mn, e5Mn, VTKoeffDe_Mn
   real, protected    :: c1Ni, e1Ni, c2Ni, e2Ni, c3Ni, e3Ni, c4Ni, e4Ni, c5Ni, e5Ni, VTKoeffDe_Ni
   real, protected    :: c1Hg, e1Hg, c2Hg, e2Hg, c3Hg, e3Hg, c4Hg, e4Hg, c5Hg, e5Hg, VTKoeffDe_Hg
   real, protected    :: c1U, e1U, c2U, e2U, c3U, e3U, c4U, e4U, c5U, e5U, VTKoeffDe_U
   real, protected    :: c1Zn, e1Zn, c2Zn, e2Zn, c3Zn, e3Zn, c4Zn, e4Zn, c5Zn, e5Zn, VTKoeffDe_Zn
   real, protected    :: c1As, e1As, c2As, e2As, c3As, e3As, c4As, e4As, c5As, e5As, VTKoeffDe_As
   
   
   ! Hardcoded
   real, parameter    :: Caki = 0.48   !< C-Anteil Kieselalgen [mgC/mgTG]
   real, parameter    :: Cabl = 0.48   !< C-Anteil Blaualgen [mgC/mgTG]
   real, parameter    :: Cagr = 0.48   !< C-Anteil Grünalgen [mgC/mgTG]
   
   real, parameter    :: CZoo = 0.45   !< C-Anteil Rotatorien [mgC/mgTG]
   real, parameter    :: PZoo = 0.01   !< P-Anteil Rotatorien [mgP/mgTG]
   real, parameter    :: NZoo = 0.11   !< N-Anteil Rotatorien [mgN/mgTG]
   
   
   public :: aparam_lesen, AParamParam
   
contains
   
   !> <h1>SUBROUTINE aparam_lesen()</h1>
   !! Einlesen der Konstanten   \n \n
   !! Beschreibung siehe: \ref lnk_globale_parameter \n
   !! Quelle: module_aparam.f90
subroutine aparam_lesen(cpfad,iwsim,icoli,ieros,ischwer, ifehl)
   
   character(255), intent(in) :: cpfad
   integer, intent(in)        :: iwsim, icoli, ieros, ischwer
   integer, intent(out)       :: ifehl
   
   integer                    :: io_error, summ_err
   character(500)             :: dateiname
   real                       :: dummy
   
   namelist /ALGAE/  &
      AGCHL, AGGMAX, IKge, AGKSN, AGKSP, AGREMI, frmuge, BSBGR, CSBGR, QMX_NG, &
      QMX_PG, QMN_NG, QMN_PG, UPMXNG, UPMXPG, OPGRMI, OPGRMA, ASGRE, TOPTG,    &
      KTEMP_GR, AKCHL, AKGMAX, IKke, AKKSN, AKKSP, AKKSSI, AKREMI, frmuke,     &
      BSBKI, CSBKI, QMX_NK, QMX_PK, QMX_SK, QMN_NK, QMN_PK, QMN_SK, UPMXNK,    &
      UPMXPK, UPMXSK, OPKIMI, OPKIMA, ASKIE, TOPTK, KTEMP_Ki, ABCHL, ABGMAX,   &
      IKbe, ABKSN, ABKSP, ABREMI, frmube, BSBBL, CSBBL, QMX_NB, QMX_PB, QMN_NB,&
      QMN_PB, UPMXNB, UPMXPB, OPBLMI, OPBLMA, ASBLE, TOPTB, KTEMP_Bl, ifix
   
   namelist /Rotatorien/ IRMAX, FOPTR, GROT, ZRESG, ZAKI, ZAGR, ZABL
   
   namelist /Nitrosomonas/ YNMAX1, STKS1, ANITR1, BNMX1, BNKS1
   
   namelist /Nitrobacter/ YNMAX2, STKS2, ANITR2, BNMX2, BNKS2
   
   namelist /Kohlenstoff/ HyP1, hymxD, KsD1, KsD2, KsM, upBAC, YBAC, rsGBAC
   
   namelist /Muscheln/ FoptD
   
   namelist /HNF/ upHNF, BACks
   
   namelist /Wasser/ ALAMDA
   
   namelist /Sediment/ KNH4, KapN3, fPOC1, fPOC2, SorpCap, Klang, KdNh3
   namelist /Hygiene/ ratecd, etacd, rateci, xnuec, ratecg, ratecs
   
   namelist /Schwermetalle/                                                       &
      c1Pb, e1Pb, c2Pb, e2Pb, c3Pb, e3Pb, c4Pb, e4Pb, c5Pb, e5Pb, VTKoeffDe_Pb,   &
      c1Cad, e1Cad, c2Cad, e2Cad, c3Cad, e3Cad, c4Cad, e4Cad, c5Cad, e5Cad, VTKoeffDe_Cad,&
      c1Cr, e1Cr, c2Cr, e2Cr, c3Cr, e3Cr, c4Cr, e4Cr, c5Cr, e5Cr, VTKoeffDe_Cr,   &
      c1Fe, e1Fe, c2Fe, e2Fe, c3Fe, e3Fe, c4Fe, e4Fe, c5Fe, e5Fe, VTKoeffDe_Fe,   &
      c1Cu, e1Cu, c2Cu, e2Cu, c3Cu, e3Cu, c4Cu, e4Cu, c5Cu, e5Cu, VTKoeffDe_Cu,   &
      c1Mn, e1Mn, c2Mn, e2Mn, c3Mn, e3Mn, c4Mn, e4Mn, c5Mn, e5Mn, VTKoeffDe_Mn,   &
      c1Ni, e1Ni, c2Ni, e2Ni, c3Ni, e3Ni, c4Ni, e4Ni, c5Ni, e5Ni, VTKoeffDe_Ni,   &
      c1Hg, e1Hg, c2Hg, e2Hg, c3Hg, e3Hg, c4Hg, e4Hg, c5Hg, e5Hg, VTKoeffDe_Hg,   &
      c1U, e1U, c2U, e2U, c3U, e3U, c4U, e4U, c5U, e5U, VTKoeffDe_U,              &
      c1Zn, e1Zn, c2Zn, e2Zn, c3Zn, e3Zn, c4Zn, e4Zn, c5Zn, e5Zn, VTKoeffDe_Zn,   &
      c1As, e1As, c2As, e2As, c3As, e3As, c4As, e4As, c5As, e5As, VTKoeffDe_As
   
   
   ! write example
   dateiname = trim(adjustl(cpfad)) // 'APARAM_example.nml'
   open (Unit=55 , file = dateiname, action = 'write', iostat = io_error)
   if (io_error == 0) then 
      write(55, nml = ALGAE)
      write(55, nml = Rotatorien)
      write(55, nml = Nitrosomonas)
      write(55, nml = Nitrobacter)
      write(55, nml = Kohlenstoff)
      write(55, nml = Muscheln)
      write(55, nml = HNF)
      write(55, nml = Wasser)
      write(55, nml = Sediment)
      write(55, nml = Hygiene)
      write(55, nml = Schwermetalle)
   else
      print*, 'open_error APARAM_example.nml'
   endif
   close (55)
   summ_err = 0
   ifehl = 0
   
   
   ! APARAM.nml
   ! write(dateiname,'(2A)')trim(adjustl(cpfad)),'APARAM.nml'
   ! open ( unit =55 , file = dateiname, status ='old', action ='read', iostat = io_error )
   ! if(io_error.eq.0)then ! APARAM.nml exists
   if (.false.) then 
      rewind (55)
      
      read(55,nml = ALGAE,iostat = io_error)
      if (io_error /= 0) print*, 'reading namelist ALGAE went wrong'
      
      read(55,nml = Rotatorien,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Rotatorien went wrong'
      print*,'Rotatorien: = IRMAX,FOPTR,GROT,ZRESG',IRMAX,FOPTR,GROT,ZRESG
      
      read(55,nml = Nitrosomonas,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Nitrosomonas went wrong'
      print*,'Nitrosomonas: YNMAX1,STKS1,ANITR1,BNMX1,BNKS1 = ',YNMAX1,STKS1,ANITR1,BNMX1,BNKS1
      
      read(55,nml = Nitrobacter,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Nitrobacter went wrong'
      
      read(55,nml = Kohlenstoff,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Kohlenstoff went wrong'
      
      read(55,nml = Muscheln,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Muscheln went wrong'
      
      read(55,nml = HNF,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist HNF went wrong'
      
      read(55,nml = Wasser,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Wasser went wrong'
      
      read(55,nml = Sediment,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Sediment went wrong'
      
      read(55,nml = Hygiene,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Hygiene went wrong'
      
      read(55,nml = Schwermetalle,iostat = io_error)
      if (io_error /= 0) print*,'reading namelist Schwermetalle went wrong'
      
      close (55)
      
   else ! APARAM.nml does not exist
      print*,'APARAM.nml option not yet operational'
      summ_err = 0
      
      ! APARAM.txt
      dateiname = trim(adjustl(cpfad)) // 'APARAM.txt'
      open(Unit=55, file = dateiname, status = 'old', action = 'read ', iostat = io_error )
      if (io_error /= 0) then
         print*,'open_error APARAM.txt file propably not existing'
         stop 55
      endif
      
      rewind (55)
      ! line 1
      read(55,*,iostat = io_error)agchl,aggmax,IKge,agksn,agksp         ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)agremi,frmuge,bsbgr,csbgr,Qmx_NG      ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)Qmx_PG,Qmn_NG,Qmn_PG,upmxNG,upmxPG    ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)opgrmi,opgrma,asgre,ToptG,kTemp_Gr    ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)akchl,akgmax,IKke,akksn,akksp         ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)akkssi,akremi,frmuke,bsbki,csbki      ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)Qmx_NK,Qmx_PK,Qmx_SK,Qmn_NK,Qmn_PK    ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)Qmn_SK,upmxNK,upmxPK,upmxSK,opkimi    ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)opkima,askie,ToptK,kTemp_Ki,abchl     ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)abgmax,IKbe,abksn,abksp,abremi        ; if (io_error /= 0)summ_err = summ_err+1
      ! line 11
      read(55,*,iostat = io_error)frmube,bsbbl,csbbl,Qmx_NB,Qmx_PB      ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)Qmn_NB,Qmn_PB,upmxNB,upmxPB,opblmi    ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)opblma,asble,ToptB,kTemp_Bl,ifix      ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)IRMAX,FOPTR,GROT,ZRESG,ZAKI           ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)ZAGR,ZABL,YNMAX1,STKS1,ANITR1         ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)BNMX1,BNKS1,YNMAX2,STKS2,ANITR2       ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)BNMX2,BNKS2,KNH4,KapN3,HyP1           ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)hymxD,KsD1,KsD2,KsM,upBAC             ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)YBAC,rsGBAC,FoptD,upHNF,BACks         ; if (io_error /= 0)summ_err = summ_err+1
      read(55,*,iostat = io_error)alamda,fPOC1,fPOC2,SorpCap,Klang      ; if (io_error /= 0)summ_err = summ_err+1
      ! line 21
      read(55,*,iostat = io_error)KdNh3,ratecd,etacd,rateci,xnuec       ; if (io_error /= 0)summ_err = summ_err+1
      
      if (ischwer == 0) then
         read(55,*,iostat = io_error)ratecg,ratecs                      ; if (io_error /= 0)summ_err = summ_err+1
      else
         read(55,*,iostat = io_error)ratecg,ratecs,dummy,c1Pb,e1Pb         ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c2Pb,e2Pb,c3Pb,e3Pb,c4Pb              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e4Pb,c5Pb,e5Pb,VTKoeffDe_Pb,c1Cad     ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e1Cad,c2Cad,e2Cad,c3Cad,e3Cad         ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c4Cad,e4Cad,c5Cad,e5Cad,VTKoeffDe_Cad ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c1Cr,e1Cr,c2Cr,e2Cr,c3Cr              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e3Cr,c4Cr,e4Cr,c5Cr,e5Cr              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)VTKoeffDe_Cr,c1Fe,e1Fe,c2Fe,e2Fe      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c3Fe,e3Fe,c4Fe,e4Fe,c5Fe              ; if (io_error /= 0)summ_err = summ_err+1
         ! line 31
         read(55,*,iostat = io_error)e5Fe,VTKoeffDe_Fe,c1Cu,e1Cu,c2Cu      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e2Cu,c3Cu,e3Cu,c4Cu,e4Cu              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c5Cu,e5Cu,VTKoeffDe_Cu,c1Mn,e1Mn      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c2Mn,e2Mn,c3Mn,e3Mn,c4Mn              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e4Mn,c5Mn,e5Mn,VTKoeffDe_Mn,c1Ni      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e1Ni,c2Ni,e2Ni,c3Ni,e3Ni              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c4Ni,e4Ni,c5Ni,e5Ni,VTKoeffDe_Ni      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c1Hg,e1Hg,c2Hg,e2Hg,c3Hg              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e3Hg,c4Hg,e4Hg,c5Hg,e5Hg              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)VTKoeffDe_Hg,c1U,e1U,c2U,e2U          ; if (io_error /= 0)summ_err = summ_err+1
         !line 41
         read(55,*,iostat = io_error)c3U,e3U,c4U,e4U,c5U                   ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e5U,VTKoeffDe_U,c1Zn,e1Zn,c2Zn        ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e2Zn,c3Zn,e3Zn,c4Zn,e4Zn              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c5Zn,e5Zn,VTKoeffDe_Zn,c1As,e1As      ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)c2As,e2As,c3As,e3As,c4As              ; if (io_error /= 0)summ_err = summ_err+1
         read(55,*,iostat = io_error)e4As,c5As,e5As,VTKoeffDe_As           ; if (io_error /= 0)summ_err = summ_err+1
      endif
      close (55)
   endif 
   
   ! check
   if (summ_err > 0) then
      print*,'during reading APARAM.txt ',summ_err,' errors occured'
      ifehl = 3
      return
   endif
   if (iwsim == 2 .and. icoli == 0)return
   
   if (icoli /= 1) then
      if (IKge < 0.0 .or. IKke < 0.0 .or. KNH4 < 0.0 .or. KapN3 < 0.0 .or. HyP1 < 0.0 .or. ToptG < 0.0 .or. Klang < 0.0 .or. ifix < 0) then
         ifehl = 3
         return
      endif
      if (kTemp_Gr < 0.0 .or. kTemp_Ki < 0.0 .or. kTemp_Bl < 0.0) then
         ifehl = 3
         return
      endif
      if (iwsim == 3 .and. IRMAX < -1 .or. FOPTR < -1.) then
         ifehl = 3
         return
      endif
      if (csbki < 1.) then
         ifehl = 27
         return
      endif
   else 
      if (ratecd < 0.0 .or. etacd < 0.0 .or. rateci < 0.0 .or. xnuec < 0.0 .or. ratecg < 0.0 .or. ratecs < 0.0) then
         ifehl = 26
         return
      endif
   endif 
   
   return
end subroutine aparam_lesen



!> Writes file `AParamParam.xml`
!!
!! aus Datei module_aparam.f95 ; zurück zu \ref lnk_modellerstellung
subroutine AParamParam(cpfad1)
   character (len = 255)       :: cpfad1
   character (len = 275)       :: pfadstring
   character (len = 8)         :: versionstext
  

  call version_string(versionstext)
   
   pfadstring =  trim(adjustl(cpfad1)) // 'AParamParam.xml'
   open(unit=200, file=pfadstring, encoding='UTF-8')
   
   write(200, '(A)')'<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
   write(200, '(3A)')'<GerrisParam FileType="AParam" QsimVersion="', versionstext, '">'
   write(200, '(A)') '<ParamSetDef Ident="AParam" Text="Allg. Parameter" Null="-1" Help="Allgemeine Simulations-Parameter" Scope="Modell" IsGrouped="0">'
   write(200, '(A)') '  <Parameter Ident="AGCHL" Text="Kohlenstoff/Chlorophyll Grünalgen (dunkeladaptiert) bei 20°C" Unit="mgC/mgChla" Format="F4.1" Null="-1" Help="" Default="12.4" Min="0" Max="99.9" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="AGGMAX" Text="Max. Wachstumsrate d. Grünalgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.2" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="IKge" Text="Lichtsättigung für kohlenstoffspez. Photosynthese der Grünalgen bei 20°C" Unit="µE/(m2*s)" Format="F6.2" Null="-1" Help="" Default="58.6" Min="0" Max="999.99" Gruppe="Grünalgen" Kategorie="Licht" />' 
   write(200, '(A)') '  <Parameter Ident="AGKSN" Text="Halbsättigung Grünalgen N" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="AGKSP" Text="Halbsättigung Grünalgen P" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="AGREMI" Text="Grundrespiration d. Grünalgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.11" Min="0.05" Max="9.999" Gruppe="Grünalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="frmuge" Text="Anteil der vom Wachstum abhängigigen Respiration (Grünalgen)" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.067" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BSBGR" Text="C-BSB5-Erhöhung Grünalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.54" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="CSBGR" Text="CSB-Erhöhung Grünalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Grünalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="QMX_NG" Text="max. N-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.11" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMX_PG" Text="max. P-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.012" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="QMN_NG" Text="min. N-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.02" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMN_PG" Text="min. P-Gehalt der Grünalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0016" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="UPMXNG" Text="max. N-Aufnahmerate der Grünalgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.18" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="UPMXPG" Text="max. P-Aufnahmerate der Grünalgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.69" Min="0" Max="9.999" Gruppe="Grünalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="OPGRMI" Text="RQ respiratorischer Quotient für Grünalgen" Unit="molC/mol O2" Format="F4.2" Null="-1" Help="" Default="0.67" Min="0" Max="9.99" Gruppe="Grünalgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="OPGRMA" Text="PQ photosynthetischer Quotient für Grünalgen" Unit="molO2/molC" Format="F4.2" Null="-1" Help="" Default="1.9" Min="0" Max="9.99" Gruppe="Grünalgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="ASGRE" Text="Sediment Grünalgen" Unit="0-1" Format="F5.2" Null="-1" Help="" Default="0.5" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="TOPTG" Text="optimal Temperatur für Grünalgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="" Default="30.2" Min="0" Max="99.99" Gruppe="Grünalgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="KTEMP_GR" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="" Default="0.0041" Min="0" Max="9.99999" Gruppe="Grünalgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="AKCHL" Text="Kohlenstoff/Chlorophyll Kieselalgen (dunkeladaptiert) bei 20°C" Unit="mgC/mgChla" Format="F4.1" Null="-1" Help="" Default="13.3" Min="0" Max="99.9" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="AKGMAX" Text="Max. Wachstumsate d. Kieselalgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.6" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="IKke" Text="Lichtsättigung für kohlenstoffspz.Photosynthese der Kieselalgen bei 20°C" Unit="µE/(m2*s)" Format="F6.2" Null="-1" Help="" Default="43.9" Min="0" Max="999.99" Gruppe="Kieselalgen" Kategorie="Licht" />'
   write(200, '(A)') '  <Parameter Ident="AKKSN" Text="N-Halbsättigung Kieselalgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="AKKSP" Text="P-Halbsättigung Kieselalgen" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="AKKSSI" Text="Si-Halbsättigung Kieselalgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.08" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Si" />'
   write(200, '(A)') '  <Parameter Ident="AKREMI" Text="Grundrespiration d. Kieselalgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.085" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="frmuke" Text="Anteil der vom Wachstum abhängigigen Respiration für Kieselalgen" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.2" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BSBKI" Text="C-BSB5-Erhöhung Kieselalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.73" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="CSBKI" Text="CSB-Erhöhung Kieselalgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Kieselalgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="QMX_NK" Text="max. N-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.052" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMX_PK" Text="max. P-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.009" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="QMX_SK" Text="max. Si-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.18" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="Si" />'
   write(200, '(A)') '  <Parameter Ident="QMN_NK" Text="min. N-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.012" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMN_PK" Text="min. P-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0011" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="QMN_SK" Text="min. Si-Gehalt der Kieselalgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.18" Min="0" Max="9.99999" Gruppe="Kieselalgen" Kategorie="Si" />'
   write(200, '(A)') '  <Parameter Ident="UPMXNK" Text="max. N-Aufnahmerate der Kieselalgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.31" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="UPMXPK" Text="max. P-Aufnahmerate der Kieselalgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.62" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="UPMXSK" Text="max. Si-Aufnahmerate der Kieselalgen" Unit="mgSi/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="2.5" Min="0" Max="9.999" Gruppe="Kieselalgen" Kategorie="Si" />'
   write(200, '(A)') '  <Parameter Ident="OPKIMI" Text="RQ respiratorischer Quotient für Kieselalgen" Unit="mol C/mol O2" Format="F4.2" Null="-1" Help="" Default="0.57" Min="0" Max="9.99" Gruppe="Kieselalgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="OPKIMA" Text="PQ photosynthetischer Quotient für Kieselalgen" Unit="mol O2/mol C" Format="F4.2" Null="-1" Help="" Default="2.1" Min="0" Max="9.99" Gruppe="Kieselalgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="ASKIE" Text="Sediment Kieselalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Sedimentierbarer Anteil für Kieselalgen" Default="0.5" Min="0" Max="99.99" Gruppe="Kieselalgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="TOPTK" Text="optimal Temperatur für Kieselalgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="" Default="20.3" Min="20.00" Max="28.00" Gruppe="Kieselalgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="KTEMP_Ki" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="" Default="0.0065" Min="0.002" Max="0.009" Gruppe="Kieselalgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="ABCHL" Text="Kohlenstoff/Chlorophyll Blaualgen" Unit="mgC/mgChla" Format="F5.1" Null="-1" Help="" Default="68." Min="0" Max="999.9" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="ABGMAX" Text="Max. Wachstumsrate d. Blaualgen bei 20°C" Unit="1/d" Format="F5.2" Null="-1" Help="" Default="1.2" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="IKbe" Text="Lichtsättigung für kohlenstoffspez.Photosynthese der Blaualgen bei 20°C" Unit="µE/m2*s)" Format="F6.2" Null="-1" Help="" Default="99.1" Min="0" Max="999.99" Gruppe="Blaualgen" Kategorie="Licht" />'
   write(200, '(A)') '  <Parameter Ident="ABKSN" Text="N-Halbsättigung Blaualgen" Unit="mg/l" Format="F5.3" Null="-1" Help="" Default="0.007" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="ABKSP" Text="P-Halbsättigung Blaualgen" Unit="mg/l" Format="F6.4" Null="-1" Help="" Default="0.023" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="ABREMI" Text="Grundrespiration d. Blaualgen bei 20°C" Unit="1/d" Format="F5.3" Null="-1" Help="" Default="0.085" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="frmube" Text="Anteil der vom Wachstum abhängigigen Respiration (Blaulalgen)" Unit=" -" Format="F5.2" Null="-1" Help="" Default="0.2" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BSBBL" Text="C-BSB5-Erhöhung Blaualgen" Unit="mg/mgC" Format="F6.4" Null="-1" Help="" Default="0.44" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="CSBBL" Text="CSB-Erhöhung Blaualgen" Unit="mg/µmgC" Format="F6.4" Null="-1" Help="" Default="3.1" Min="0" Max="9.9999" Gruppe="Blaualgen" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="QMX_NB" Text="max. N-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.088" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMX_PB" Text="max. P-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.007" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="QMN_NB" Text="min. N-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.026" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="QMN_PB" Text="min. P-Gehalt der Blaualgenzelle" Unit="mg/mgBio" Format="F7.5" Null="-1" Help="" Default="0.0009" Min="0" Max="9.99999" Gruppe="Blaualgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="UPMXNB" Text="max. N-Aufnahmerate der Blaualgen" Unit="mgN/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.31" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="UPMXPB" Text="max. P-Aufnahmerate der Blaualgen" Unit="mgP/(mgBio*d)" Format="F5.3" Null="-1" Help="" Default="0.62" Min="0" Max="9.999" Gruppe="Blaualgen" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="OPBLMI" Text="RQ respiratorischer Quotient für Blaualgen" Unit="mol C/mol O2" Format="F4.2" Null="-1" Help="" Default="0.68" Min="0" Max="9.99" Gruppe="Blaualgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="OPBLMA" Text="PQ photosynthetischer Quotient für Blaualgen" Unit="mol O2/mol C" Format="F4.2" Null="-1" Help="" Default="1.85" Min="0" Max="9.99" Gruppe="Blaualgen" Kategorie="O" />'
   write(200, '(A)') '  <Parameter Ident="ASBLE" Text="Sediment Blaualgen" Unit="0-1" Format="F5.2" Null="-1" Help="" Default="0" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="TOPTB" Text="optimal Temperatur für Blaualgenwachstum" Unit="°C" Format="F5.2" Null="-1" Help="Fadenbildend: 23.7; Kolonienbildend: 31.8" Default="26" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="KTEMP_Bl" Text="empirische Konstante KT(µ) für Temperaturabhängigkeit (Exponent)" Unit="1/°C" Format="F7.5" Null="-1" Help="µ = µmax*exp(-kT(µ)*(T-Topt)^2), Fadenbildend: 0.0069; Kolonienbildend: 0.0081" Default="0.0081" Min="0" Max="99.99" Gruppe="Blaualgen" Kategorie="Temperatur" />'
   write(200, '(A)') '  <Parameter Ident="ifix" Text="Luftstickstofffixierer (0/1)" Unit="" Format="I2" Null="-1" Help="Luftstickstofffixierer(0:Nein/1:Ja)" Default="0" Min="0" Max="1" Gruppe="Blaualgen" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="IRMAX" Text="max. Gewichtsspez. Algenaufnahmerate d. Rotatorien" Unit="µgC-2/3*d-1" Format="F5.2" Null="-1" Help="Max. Ingestionsrate für Rotatorien" Default="2.9" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="FOPTR" Text="Halbsättigungskonstante für Futteraufnahme d. Rotatorien" Unit="mg/l" Format="F5.2" Null="-1" Help="Optimale Futterkonzentration für Rotatorienwachstum" Default="0.80" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="GROT" Text="Gewicht Rotatorie" Unit="µg" Format="F5.2" Null="-1" Help="Gewicht einer Rotatorie" Default="0.3" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="ZRESG" Text="Grundrespiration Rotatorien" Unit="1/d" Format="F5.3" Null="-1" Help="Grundrespiration der Rotatorien" Default="0.09" Min="0" Max="9.999" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="ZAKI" Text="Filtrierbarkeit Kieselalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Kieselalgen durch Rotatorien" Default="0.6" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="ZAGR" Text="Filtrierbarkeit Grünalgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Grünalgen durch Rotatorien" Default="0.8" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="ZABL" Text="Filtrierbarkeit Blaualgen" Unit="0-1" Format="F5.2" Null="-1" Help="Filtrierbarkeit der Blaualgen durch Rotatorien" Default="0.1" Min="0" Max="99.99" Gruppe="Rotatorien" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="YNMAX1" Text="Max. Wachstum Nitrosomonas" Unit="1/d" Format="F4.2" Null="-1" Help="Max. Wachstumsrate der Nitrosomonas" Default="0.58" Min="0" Max="9.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="STKS1" Text="Halbsättigung Nitrosomonas" Unit="mgNH4-N/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante für Nitrosomonas" Default="0.49" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="ANITR1" Text="Absterberate Nitrosomonas" Unit="1/d" Format="F4.2" Null="-1" Help="Absterberate für Nitrosomonas" Default="0.09" Min="0" Max="9.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BNMX1" Text="Max. Umsatz Nitrosomonas" Unit="gNH4-N/(m²*l)" Format="F5.2" Null="-1" Help="Max. Umsatzrate sessiler Nitrosomonas" Default="2.4" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BNKS1" Text="Halbsätt. sessiler Nitrosomonas" Unit="mg/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante der sessilen Nitrosomonas" Default="3.7" Min="0" Max="99.99" Gruppe="Nitrosomonas" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="YNMAX2" Text="Max. Wachstum Nitrobacter" Unit="1/d" Format="F4.2" Null="-1" Help="Max. Wachstumsrate der Nitrobacter" Default="0.33" Min="0" Max="9.99" Gruppe="Nitrobacter" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="STKS2" Text="Halbsättigung Nitrobacter" Unit="mgNO2-N/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante für Nitrobacter" Default="0.35" Min="0" Max="99.99" Gruppe="Nitrobacter" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="ANITR2" Text="Absterberate Nitrobacter" Unit="1/d" Format="F4.2" Null="-1" Help="Absterberate für Nitrobacter" Default="0.11" Min="0" Max="9.99" Gruppe="Nitrobacter" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BNMX2" Text="Max. Umsatz Nitrobacter" Unit="gNO2-N/(m2*l)" Format="F5.2" Null="-1" Help="Max. Umsatzrate sessiler Nitrobacter" Default="4.9" Min="0" Max="99.99" Gruppe="Nitrobacter" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="BNKS2" Text="Halbsätt. sessiler Nitrobacter" Unit="mg/l" Format="F5.2" Null="-1" Help="Halbsättigungskonstante der sessilen Nitrobacter" Default="1.2" Min="0" Max="99.99" Gruppe="Nitrobacter" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="KNH4" Text="NH4-Umsatzgeschw. im Sediment" Unit="m/d" Format="F5.2" Null="-1" Help="NH4-Umsatzgeschw. im Sediment" Default="0.31" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="KapN3" Text="Denitrifikationsgeschw. im Sediment" Unit="m/d" Format="F5.2" Null="-1" Help="Denitrifikationsgeschw.im Sediment" Default="0.81" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="HyP1" Text="Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen " Unit="d-1" Format="F6.3" Null="-1" Help="Hydrolyserate für die leicht abbaubaren partikulären organischen C-Verbindungen " Default="0.12" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="hymxD" Text="Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen " Unit="d-1" Format="F6.3" Null="-1" Help="Hydrolyserate für die leicht abbaubaren gelösten organischen C-Verbindungen " Default="18" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="KsD1" Text="Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonstante für die Hydrolyse der leicht abbaubaren gelöster organischer C-Verbindungen" Default="0.25" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="KsD2" Text="Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonstante für die Hydrolyse der schwer abbaubaren gelöster organischer C-Verbindungen" Default="2.5" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="KsM" Text="Halbsättigungskonst. für den Abbau monomerer C-Verbindungen" Unit="mgC/l" Format="F6.3" Null="-1" Help="Halbsättigungskonst. für den Abbau monomerer C-Verbindungen" Default="0.1" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="upBAC" Text="max. Aufnahmerate monomerer C-Verbindungen d. Bakterien" Unit="d-1" Format="F6.3" Null="-1" Help="max. Aufnahmerate monomerer C-Verbindungen d. Bakterien bei 25°C (Topt)" Default="4.8" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="YBAC" Text="Ertragskoeffizient für Bakterienbiomasse" Unit=" -" Format="F6.3" Null="-1" Help="Ertragskoeffizient für Bakterienbiomasse" Default="0.25" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="rsGBAC" Text="Grundrespiration het. Bakterien" Unit="d-1" Format="F6.3" Null="-1" Help="Grundrespiration het. Bakterien" Default="0.03" Min="0" Max="99.999" Gruppe="Bakterien" Kategorie="Wachstum" />'
   write(200, '(A)') '  <Parameter Ident="FoptD" Text="Opt. Futterkonz. Dreissena" Unit="mgC/l" Format="F5.2" Null="-1" Help="Optimale Futterkonzentration für Dreissena-Wachstum" Default="1.2" Min="0" Max="99.99" Gruppe="Muscheln" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="upHNF" Text="max. Aufnahmerate der HNF" Unit="d-1" Format="F5.2" Null="-1" Help="Aufnahmerate heterotropher Nanoflagelaten" Default="1.61" Min="0" Max="99.99" Gruppe="HNF" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="BACks" Text="Halbsättigungsk. für BaK.-Aufnahme durch HNF" Unit="mgC/l" Format="F6.4" Null="-1" Help="Halbsättigungsk. für BaK.-Aufnahme durch HNF" Default="0.0143" Min="0" Max="9.9999" Gruppe="HNF" Kategorie="Grazing" />'
   write(200, '(A)') '  <Parameter Ident="ALAMDA" Text="Absorptionskoeff. für Gelbstoffe bei 440 nm" Unit="-" Format="F5.3" Null="-1" Help="Absorptionskoeff. für Gelbstoffe bei 440 nm" Default="0.75" Min="0" Max="9.999" Gruppe="Wasser" Kategorie="Licht" />'
   write(200, '(A)') '  <Parameter Ident="fPOC1" Text="leichtabbaubarer Anteil d. Sedimentkohlenstoffs" Unit=" - " Format="F5.2" Null="-1" Help="leichtabbaubarer Anteil d. Sedimentkohlenstoffs" Default="0.65" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="fPOC2" Text="schwerabbaubarer Anteil d. Sedimentkohlenstoffs" Unit=" - " Format="F5.2" Null="-1" Help="schwerabbaubarer Anteil d. Sedimentkohlenstoffs" Default="0.15" Min="0" Max="99.99" Gruppe="Sediment" Kategorie="C Biomasse" />'
   write(200, '(A)') '  <Parameter Ident="SorpCap" Text="SorptionsKapazität für Phosphor" Unit="mgP/gTG" Format="F6.2" Null="-99.99" Help="SorptionsKapazität für Phosphor" Default="28" Min="0.0" Max="99.99" Gruppe="Sediment" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="Klang" Text="Langmuirkoeffizient für Phosphorsorption" Unit="l/mgP" Format="F6.3" Null="-1" Help="Langmuirkoeffizient für Phosphorsorption" Default="0.7" Min="0" Max="99.999" Gruppe="Sediment" Kategorie="P" />'
   write(200, '(A)') '  <Parameter Ident="KdNh3" Text="Partitionskoeffizient für Ammonium" Unit="l/kg" Format="F5.2" Null="-9.99" Help="-1.-> Wert wird berechnet" Default="10." Min="-1" Max="99.99" Gruppe="Sediment" Kategorie="N" />'
   write(200, '(A)') '  <Parameter Ident="ratecd" Text="Grundmortalitätsrate coliformer Bakterien bei 20°C" Unit="1/d" Format="F6.3" Null="-1." Help="Grundmortalitätsrate coliformer Bakterien bei 20°C" Default="0.34" Min="0.0" Max="10." Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="etacd" Text="Temperaturkoeffizient" Unit="-" Format="F5.2" Null="-1." Help="Temperaturkoeffizient" Default="1.1" Min="1." Max="3." Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="rateci" Text="Inaktivierungskoeffizient im Licht" Unit="m2*MJ-1" Format="F5.2" Null="-1." Help="Inaktivierungskoeffizient im Licht" Default="1." Min="0.0" Max="99.99" Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="xnuec" Text="dimensionsloser Parameter" Unit="-" Format="F6.2" Null="-1." Help="dimensionsloser Parameter zur Beschreibung der Inaktiv. im Licht" Default="1.5" Min="1.0" Max="999.99" Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="ratecg" Text="Verlustrate durch Grazing" Unit="d-1" Format="F5.3" Null="-.1" Help="Coliforme Verlustrate durch Grazing" Default="0.0" Min="0.0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="ratecs" Text="Verlustrate durch Sedimentation" Unit="d-1" Format="F5.3" Null="-.1" Help="Coliforme Verlustrate durch Sedimentation" Default="0.0" Min="0" Max="9.999" Gruppe="Hygiene" Kategorie="Coliform" />'
   write(200, '(A)') '  <Parameter Ident="dummy" Text="bewahrt nur das Zeilenformat" Unit="d-1" Format="F5.3" Null="-.1" Help="Eingabe ohne Auswirkung" Default="0.0" Min="0" Max="9.999" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c1Pb" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="49090.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="e1Pb" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.586" Min="0.001" Max="9.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c2Pb" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="12556.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="e2Pb" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.641" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c3Pb" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="e3Pb" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-0.023" Min="-9.999" Max="99.999" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c4Pb" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-251483.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="e4Pb" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.835" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c5Pb" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="e5Pb" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.8" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Pb" Text="Verteilungskoeffizient für Blei" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="640." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Blei" />'
   write(200, '(A)') '  <Parameter Ident="c1Cad" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="49090.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="e1Cad" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.586" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="c2Cad" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="12556.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="e2Cad" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.641" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="c3Cad" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="e3Cad" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-0.023" Min="-9.999" Max="99.999" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="c4Cad" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-251483.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="e4Cad" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.835" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="c5Cad" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="e5Cad" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.8" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Cad" Text="Verteilungskoeffizient für Cadmium" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="130." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Cadmium" />'
   write(200, '(A)') '  <Parameter Ident="c1Cr" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="49090.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="e1Cr" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.586" Min="0.001" Max="9." Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="c2Cr" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="12556.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="e2Cr" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.641" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="c3Cr" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="e3Cr" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-0.023" Min="-99.999" Max="99.999" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="c4Cr" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-251483.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="e4Cr" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.835" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="c5Cr" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="e5Cr" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.8" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Cr" Text="Verteilungskoeffizient für Chrom" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="290." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Chrom" />'
   write(200, '(A)') '  <Parameter Ident="c1Fe" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="144.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="e1Fe" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.038" Min="0.001" Max="9.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="c2Fe" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17769.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="e2Fe" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.673" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="c3Fe" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="42." Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="e3Fe" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.056" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="c4Fe" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.308" Min="0.001" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="e4Fe" Text="4. Exponent" Unit="-" Format="F6.3" Null="-9.999" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-1." Min="-9" Max="99.999" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="c5Fe" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-101." Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="e5Fe" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.5" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Fe" Text="Verteilungskoeffizient für Eisen" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="246." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Eisen" />'
   write(200, '(A)') '  <Parameter Ident="c1Cu" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="45.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="e1Cu" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.496" Min="0.001" Max="9.999" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="c2Cu" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="2541.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="e2Cu" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.807" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="c3Cu" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="13." Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="e3Cu" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.172" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="c4Cu" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-1660" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="e4Cu" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.459" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="c5Cu" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="e5Cu" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.2" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Cu" Text="Verteilungskoeffizient für Kupfer" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="50." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Kupfer" />'
   write(200, '(A)') '  <Parameter Ident="c1Mn" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="144.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="e1Mn" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.038" Min="0.001" Max="9.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="c2Mn" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17769.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="e2Mn" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.673" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="c3Mn" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="42." Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="e3Mn" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.056" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="c4Mn" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.308" Min="0.001" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="e4Mn" Text="4. Exponent" Unit="-" Format="F6.3" Null="-9.999" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-1." Min="-9.999" Max="99.999" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="c5Mn" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-101." Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="e5Mn" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.5" Min="0.1" Max="99.99" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Mn" Text="Verteilungskoeffizient für Mangan" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="165." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Mangan" />'
   write(200, '(A)') '  <Parameter Ident="c1Ni" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="21.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="e1Ni" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.548" Min="0.001" Max="9.999" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="c2Ni" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1666.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="e2Ni" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.872" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="c3Ni" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="63.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="e3Ni" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.205" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="c4Ni" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-294.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="e4Ni" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.810" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="c5Ni" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="e5Ni" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="4.2" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Ni" Text="Verteilungskoeffizient für Nickel" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="9." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Nickel" />'
   write(200, '(A)') '  <Parameter Ident="c1Hg" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="49090.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="e1Hg" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.586" Min="0.001" Max="9." Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="c2Hg" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="12556.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="e2Hg" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.641" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="c3Hg" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="e3Hg" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-0.023" Min="-9.999" Max="99.999" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="c4Hg" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-251483.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="e4Hg" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.835" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="c5Hg" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="e5Hg" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.8" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Hg" Text="Verteilungskoeffizient für Quecksilber" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="170." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Quecksilber" />'
   write(200, '(A)') '  <Parameter Ident="c1U" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="21.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="e1U" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.548" Min="0.001" Max="9.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="c2U" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1666.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="e2U" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.872" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="c3U" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="63.0" Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="e3U" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.205" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="c4U" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-294.0" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="e4U" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.810" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="c5U" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="e5U" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="4.2" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_U" Text="Verteilungskoeffizient für Uran" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="12." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Uran" />'
   write(200, '(A)') '  <Parameter Ident="c1Zn" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="144.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="e1Zn" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="1.038" Min="0.001" Max="6" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="c2Zn" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="17769.0" Min="0.1" Max="99999" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="e2Zn" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.673" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="c3Zn" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="42." Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="e3Zn" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.056" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="c4Zn" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.308" Min="0.001" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="e4Zn" Text="4. Exponent" Unit="-" Format="F6.3" Null="-9.999" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-1." Min="-9." Max="99.999" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="c5Zn" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-101." Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="e5Zn" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.5" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_Zn" Text="Verteilungskoeffizient für Zink" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="110." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Zink" />'
   write(200, '(A)') '  <Parameter Ident="c1As" Text="1. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="45.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="e1As" Text="1. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.496" Min="0.001" Max="9.999" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="c2As" Text="2. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="2541.0" Min="0.1" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="e2As" Text="2. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.807" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="c3As" Text="3. Koeffizient" Unit="-" Format="F5.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="13." Min="0.1" Max="999.9" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="e3As" Text="3. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.172" Min="0.001" Max="99.99" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="c4As" Text="4. Koeffizient" Unit="-" Format="F11.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="-1660" Min="-999999.999" Max="9999999.999" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="e4As" Text="4. Exponent" Unit="-" Format="F6.3" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.459" Min="0.001" Max="99.999" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="c5As" Text="5. Koeffizient" Unit="-" Format="F7.1" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="0.0" Min="-9999.9" Max="99999.9" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="e5As" Text="5. Exponent" Unit="-" Format="F5.2" Null="-1" Help="Berechnung des Verteilungskoeffizient nach ATV" Default="3.2" Min="0.01" Max="99.99" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '  <Parameter Ident="VTKoeffDe_As" Text="Verteilungskoeffizient für Arsen" Unit="l/g" Format="F6.1" Null="-1" Help="Verteilungskoeffizient nach Deltares" Default="282." Min="0.1" Max="9999.9" Gruppe="Schwermetalle" Kategorie="Arsen" />'
   write(200, '(A)') '</ParamSetDef>'
   write(200, '(A)') '</GerrisParam>'
  
  CLOSE(200)
      RETURN

 END subroutine AParamParam
end module aparam
