!> \page N_Isotope N_Isotope
!! teilt Stickstoff in 14N und 15N auf .... 
!! zurück zu \ref Stickstoff; Code: module_isotope.f95\n\n
!
!> Das module isotope teilt Stickstoff in 14N und 15N auf .... 
!! zurück zu \ref  N_Isotope
      module isotope
      implicit none
      save

      integer :: irgendeine_zahl

      PUBLIC :: isotop_no3konsum
      CONTAINS
!----+-----+----
!> Verteilung der Isotope, wenn Algen Nitrat NO3-N konsumieren.
!! zurück zu module_isotope\n\n
      subroutine isotop_no3konsum(f,kontroll)
         implicit none
         real  :: c_source, delta_source, R_source
         real  :: c_sink, delta_sink, R_sink
         real  :: f, alpha, eps, dc, R_standard
         real  :: d_logie, d_logik, d0, c0
         logical  :: kontroll
         kontroll=.true.

         return !! ### erstmal nix

         open(unit=86, file='d15N.1000.10.01a' )
         write(86,*)'#f,c_source,R_source,delta_source,d_logie,c_sink,R_sink,delta_sink,d_logik'

         c0=1.0
         dc=0.001
         eps=10.0
         alpha=1+(eps/1000.0)
         c_source=c0
         c_sink=0.10
         R_standard=0.0036765
         R_source=R_standard
         R_sink=R_standard
         d0=1000.0*((R_source-R_standard)/R_standard)
         d_logik=0.0
         do while (c_source.gt.0.0)
            f=dc/c_source
            R_source=R_source*(((1.0+alpha*R_source)-alpha*f*(1.0+R_source)) &
     &                        /((1.0+alpha*R_source)-      f*(1.0+R_source)))
            R_sink=( ((R_sink*c_sink)/(1.0+R_sink)) + ((alpha*dc*R_source)/(1.0+alpha*R_source)) )    &
     &            /( (       (c_sink)/(1.0+R_sink)) + (      (dc)         /(1.0+alpha*R_source)) )
            delta_source=1000.0*((R_source-R_standard)/R_standard)
            delta_sink  =1000.0*((R_sink  -R_standard)/R_standard)
            d_logie=d0+eps*log(1.0-((c0-c_source)/c0))
            if(c0.gt.c_source)  &
     &         d_logik=( eps*(1.0-((c0-c_source)/c0))*log(1.0-((c0-c_source)/c0)) ) / ((c0-c_source)/c0)
            write(86,*)f,c_source,R_source,delta_source,d_logie,c_sink,R_sink,delta_sink,d_logik
            c_source=c_source-dc
            c_sink=c_sink+dc
         end do ! while

         !! ###stop !! ### Zwischenstop Testlauf
         return
      END subroutine isotop_no3konsum
!----+-----+----
      end module isotope

