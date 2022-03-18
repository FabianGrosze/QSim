
!> <h1> module allodim </h1>
!! array dimenions \n  Felddimensionierungs-Parameter  \n
!! from Datei qsim.f09 ; back zu \ref Main
      module allodim
      implicit none
      save

!> \anchor azstrs Stranganzahl 
    integer :: azStrs
!> \anchor ialloc1 Einleiter pro strang
    integer , parameter :: ialloc1 = 100  !!!##### do not change !!!!
!> \anchor ialloc2 Querprofile im Strang 
    integer , parameter :: ialloc2=1000  !!!##### do not change !!!!
!> \anchor ialloc3 Abschnitte im strang?
    integer , parameter :: ialloc3 = 20  !!!##### do not change !!!!
!> \anchor ialloc4 Sedimentschichten ???
    integer , parameter :: ialloc4 = 250  !!!##### do not change !!!!
!> \anchor ialloc5 Tiefenschichtenanzahl
    integer , parameter :: ialloc5 = 50  !!!##### do not change !!!!

      end module allodim