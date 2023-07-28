module module_datetime

   use, intrinsic :: iso_fortran_env, only: int64, real32, real64, &
                                            stderr => error_unit
  
   implicit none
   
   private

   public :: datetime, timedelta, clock
   public :: date2num
   public :: datetime_range
   public :: days_in_month
   public :: days_in_year
   public :: is_leapyear
   public :: num2date
   public :: machine_timezone
   public :: epochdatetime
   public :: local_time
   public :: gmtime
   public :: datetime_constructor
   
   real(real64), parameter :: zero = 0._real64, one = 1._real64

   ! constant multipliers to transform a number of some time unit to another
   real(real64), parameter :: d2h = 24._real64       ! day -> hour
   real(real64), parameter :: h2d = one / d2h        ! hour -> day
   real(real64), parameter :: d2m = d2h * 60._real64 ! day -> minute
   real(real64), parameter :: m2d = one / d2m        ! minute -> day
   real(real64), parameter :: m2h = one / 60         ! minute -> hour
   real(real64), parameter :: s2d = m2d / 60         ! second -> day
   real(real64), parameter :: d2s = 86400._real64    ! day -> second
   real(real64), parameter :: h2s = 3600._real64     ! hour -> second
   real(real64), parameter :: s2h = one / h2s        ! second -> hour
   real(real64), parameter :: m2s = 60._real64       ! minute -> second
   real(real64), parameter :: s2m = one / m2s        ! second -> minute

   integer, parameter :: maxstrlen = 99 ! maximum string length for strftime

   type :: datetime
      
      integer      :: year = 1        ! year [1-huge(year)]
      integer      :: month = 1       ! month in year [1-12]
      integer      :: day = 1         ! day in month [1-31]
      integer      :: hour = 0        ! hour in day [0-23]
      integer      :: minute = 0      ! minute in hour [0-59]
      integer      :: second = 0      ! second in minute [0-59]
      integer      :: millisecond = 0 ! milliseconds in second [0-999]
      real(real64) :: tz = 0          ! timezone offset from utc [hours]

   contains

      ! getter functions
      procedure, pass(self), public :: get_year
      procedure, pass(self), public :: get_month
      procedure, pass(self), public :: get_day
      procedure, pass(self), public :: get_hour
      procedure, pass(self), public :: get_minute
      procedure, pass(self), public :: get_second
      procedure, pass(self), public :: get_millisecond
      procedure, pass(self), public :: get_tz

      ! public methods
      procedure, pass(self), public :: isoformat
      procedure, pass(self), public :: is_valid
      procedure, nopass,     public :: now
      procedure, pass(self), public :: seconds_since_epoch
      procedure, pass(self), public :: tz_offset
      procedure, pass(self), public :: isoweekday
      procedure, pass(self), public :: isoweekday_long
      procedure, pass(self), public :: isoweekday_short
      procedure, pass(self), public :: utc
      procedure, pass(self), public :: weekday
      procedure, pass(self), public :: weekday_long
      procedure, pass(self), public :: weekday_short
      procedure, pass(self), public :: yearday

      ! private methods
      procedure, pass(self), private :: add_milliseconds
      procedure, pass(self), private :: add_seconds
      procedure, pass(self), private :: add_minutes
      procedure, pass(self), private :: add_hours
      procedure, pass(self), private :: add_days

      ! operator overloading procedures
      procedure, pass(d0), private :: datetime_plus_timedelta
      procedure, pass(d0), private :: timedelta_plus_datetime
      procedure, pass(d0), private :: datetime_minus_datetime
      procedure, pass(d0), private :: datetime_minus_timedelta
      procedure, pass(d0), private :: datetime_eq
      procedure, pass(d0), private :: datetime_neq
      procedure, pass(d0), private :: datetime_gt
      procedure, pass(d0), private :: datetime_ge
      procedure, pass(d0), private :: datetime_lt
      procedure, pass(d0), private :: datetime_le

      generic :: operator(+)  => datetime_plus_timedelta, timedelta_plus_datetime
      generic :: operator(-)  => datetime_minus_datetime, datetime_minus_timedelta
      generic :: operator(==) => datetime_eq
      generic :: operator(/=) => datetime_neq
      generic :: operator(>)  => datetime_gt
      generic :: operator(>=) => datetime_ge
      generic :: operator(<)  => datetime_lt
      generic :: operator(<=) => datetime_le
  
   end type datetime

   interface datetime
      module procedure :: datetime_constructor
   endinterface datetime

   type :: timedelta
      private

      integer :: days = 0
      integer :: hours = 0
      integer :: minutes = 0
      integer :: seconds = 0
      integer :: milliseconds = 0

   contains

      procedure, pass(self), public :: get_days
      procedure, pass(self), public :: get_hours
      procedure, pass(self), public :: get_minutes
      procedure, pass(self), public :: get_seconds
      procedure, pass(self), public :: get_milliseconds
   
      procedure, public  :: total_seconds
   
      procedure, private :: timedelta_plus_timedelta
      procedure, private :: timedelta_minus_timedelta
      procedure, private :: unary_minus_timedelta
      procedure, private :: timedelta_eq
      procedure, private :: timedelta_neq
      procedure, private :: timedelta_gt
      procedure, private :: timedelta_ge
      procedure, private :: timedelta_lt
      procedure, private :: timedelta_le
   
      generic :: operator(+)  => timedelta_plus_timedelta
      generic :: operator(-)  => timedelta_minus_timedelta, unary_minus_timedelta
      generic :: operator(==) => timedelta_eq
      generic :: operator(/=) => timedelta_neq
      generic :: operator(>)  => timedelta_gt
      generic :: operator(>=) => timedelta_ge
      generic :: operator(<)  => timedelta_lt
      generic :: operator(<=) => timedelta_le

   end type timedelta

   interface timedelta
      module procedure :: timedelta_constructor
   endinterface timedelta

   type :: clock
      type(datetime)  :: start_time
      type(datetime)  :: stop_time
      type(datetime)  :: current_time
      type(timedelta) :: tickinterval
      logical         :: alarm   = .false.
      logical         :: started = .false.
      logical         :: stopped = .false.
   contains
      procedure :: reset
      procedure :: tick
   end type clock

contains


   pure elemental subroutine reset(self)
      ! resets the clock to its start time.
      class(clock), intent(inout) :: self
    
      self % current_time = self % start_time
      self % started = .false.
      self % stopped = .false.
   end subroutine reset


  pure elemental subroutine tick(self)
    ! increments the currenttime of the clock instance by one tickinterval.
    class(clock), intent(inout) :: self
    
      if (self % stopped) return
      if (.not. self % started) then
         self % started = .true.
         self % current_time = self % start_time
      endif
      self % current_time = self % current_time + self % tickinterval
      if (self % current_time >= self % stop_time) self % stopped = .true.
  end subroutine tick


   pure elemental type(datetime) function datetime_constructor(    &
                        year, month, day, hour, minute, second, millisecond, tz)
    
      ! constructor function for the `datetime` class.
      integer,      intent(in), optional :: year, month, day, hour, minute, second, millisecond
      real(real64), intent(in), optional :: tz ! timezone offset in hours

      datetime_constructor % year = 1
      if (present(year)) datetime_constructor % year = year

      datetime_constructor % month = 1
      if (present(month)) datetime_constructor % month = month

      datetime_constructor % day = 1
      if (present(day)) datetime_constructor % day = day

      datetime_constructor % hour = 0
      if (present(hour)) datetime_constructor % hour = hour

      datetime_constructor % minute = 0
      if (present(minute)) datetime_constructor % minute = minute

      datetime_constructor % second = 0
      if (present(second)) datetime_constructor % second = second

      datetime_constructor % millisecond = 0
      if (present(millisecond)) datetime_constructor % millisecond = millisecond

      datetime_constructor % tz = 0
      if (present(tz)) datetime_constructor % tz = tz

   end function datetime_constructor


   pure elemental integer function get_year(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_year = self % year
   end function get_year


   pure elemental integer function get_month(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_month = self % month
   end function get_month


   pure elemental integer function get_day(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_day = self % day
   end function get_day


   pure elemental integer function get_hour(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_hour = self % hour
   end function get_hour


   pure elemental integer function get_minute(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_minute = self % minute
   end function get_minute


   pure elemental integer function get_second(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_second = self % second
   end function get_second


   pure elemental integer function get_millisecond(self)
      ! returns the year component
      class(datetime), intent(in) :: self
      get_millisecond = self % millisecond
   end function get_millisecond


   pure elemental real(real64) function get_tz(self)
      ! returns the timezone offset component
      class(datetime), intent(in) :: self
      get_tz = self % tz
   end function get_tz


   pure elemental subroutine add_milliseconds(self, ms)
      ! adds an integer number of milliseconds to self. called by `datetime`
      ! addition (`+`) and subtraction (`-`) operators.
      class(datetime), intent(inout) :: self
      integer,         intent(in)    :: ms
    
      self % millisecond = self % millisecond + ms
      do
         if (self % millisecond >= 1000) then
            call self % add_seconds(self % millisecond / 1000)
            self % millisecond = mod(self % millisecond, 1000)
         else if (self % millisecond < 0) then
            call self % add_seconds(self % millisecond / 1000 - 1)
            self % millisecond = mod(self % millisecond, 1000) + 1000
         else
            exit
         endif
      enddo
   end subroutine add_milliseconds


   pure elemental subroutine add_seconds(self, s)
      ! adds an integer number of seconds to self. called by `datetime`
      ! addition (`+`) and subtraction (`-`) operators.
      class(datetime), intent(inout) :: self
      integer,         intent(in)    :: s
      
      self % second = self % second + s
      do
         if (self % second >= 60) then
            call self % add_minutes(self % second / 60)
            self % second = mod(self % second, 60)
         else if (self % second < 0) then
            call self % add_minutes(self % second / 60 - 1)
            self % second = mod(self % second, 60) + 60
         else
            exit
         endif
      enddo
      
  end subroutine add_seconds


  pure elemental subroutine add_minutes(self,m)
    ! adds an integer number of minutes to self. called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(inout) :: self
    integer, intent(in) :: m
    self % minute = self % minute + m
    do
      if (self % minute >= 60) then
        call self % add_hours(self % minute / 60)
        self % minute = mod(self % minute, 60)
      else if (self % minute < 0) then
        call self % add_hours(self % minute / 60 - 1)
        self % minute = mod(self % minute, 60) + 60
      else
        exit
      endif
    enddo
   end subroutine add_minutes


   pure elemental subroutine add_hours(self,h)
      ! adds an integer number of hours to self. called by `datetime`
      ! addition (`+`) and subtraction (`-`) operators.
      class(datetime), intent(inout) :: self
      integer, intent(in) :: h
    
      self % hour = self % hour + h
      do
         if (self % hour >= 24) then
            call self % add_days(self % hour / 24)
            self % hour = mod(self % hour, 24)
         else if (self % hour < 0) then
            call self % add_days(self % hour / 24 - 1)
            self % hour = mod(self % hour, 24) + 24
         else
            exit
         endif
       enddo
  end subroutine add_hours


   pure elemental subroutine add_days(self, d)
      ! adds an integer number of dayss to self. called by `datetime`
      ! addition (`+`) and subtraction (`-`) operators.
      class(datetime), intent(inout) :: self
      integer,         intent(in)    :: d
      integer :: days_in_current_month
    
      self % day = self % day + d
      do
         days_in_current_month = days_in_month(self % month, self % year)
         if (self % day > days_in_current_month) then
            self % day = self % day - days_in_current_month
            self % month = self % month+1
            
            if (self % month > 12) then
               self % year = self % year + self % month/12
               self % month = mod(self % month, 12)
            endif
      
         else if (self % day < 1) then
            self % month = self % month-1
            if (self % month < 1) then
               self % year = self % year + self % month / 12 - 1
               self % month = 12 + mod(self % month, 12)
            endif
            self % day = self % day + days_in_month(self % month, self % year)
         else
            exit
         endif
      enddo
   end subroutine add_days


   pure elemental character(23) function isoformat(self,sep)
      ! returns character string with time in iso 8601 format.
      class(datetime), intent(in)           :: self
      character,       intent(in), optional :: sep
      character :: separator

      separator = 't'
      if (present(sep)) separator = sep

      isoformat = int2str(self % year,       4)//'-'//      &
                  int2str(self % month,      2)//'-'//      &
                  int2str(self % day,        2)//separator//&
                  int2str(self % hour,       2)//':'//      &
                  int2str(self % minute,     2)//':'//      &
                  int2str(self % second,     2)//'.'//      &
                  int2str(self % millisecond,3)

   end function isoformat


   pure elemental logical function is_valid(self)
      ! checks whether the `datetime` instance has valid component values.
      ! returns `.true.` if the `datetime` instance is valid, and `.false.`
      ! otherwise.
      class(datetime), intent(in) :: self

      ! assume valid
      is_valid = .true.

      if (self % year < 1) then
         is_valid = .false.
         return
      endif

      if (self % month < 1 .or. self % month > 12) then
         is_valid = .false.
         return
      endif

      if (self % day < 1 .or. &
         self % day > days_in_month(self % month,self % year)) then
         is_valid = .false.
         return
      endif

      if (self % hour < 0 .or. self % hour > 23) then
         is_valid = .false.
         return
      endif

      if (self % minute < 0 .or. self % minute > 59) then
         is_valid = .false.
         return
      endif

      if (self % second < 0 .or. self % second > 59) then
         is_valid = .false.
         return
      endif

      if (self % millisecond < 0 .or. self % millisecond > 999) then
         is_valid = .false.
         return
      endif

   end function is_valid


   type(datetime) function now()
      ! returns a `datetime` instance with current time.
      character(5) :: zone
      integer, dimension(8) :: values
      integer               :: hour, minute

      ! obtain local machine time zone information
      call date_and_time(zone=zone, values=values)

      read(zone(1:3), '(i3)') hour
      read(zone(4:5), '(i2)') minute

      now = datetime(year = values(1), month = values(2), day = values(3),     &
                     hour = values(5), minute = values(6), second = values(7), &
                     millisecond = values(8))

      now % tz = hour + minute * m2h

   end function now


   pure elemental integer function weekday(self)
      ! returns the day of the week calculated using zeller's congruence.
      ! returned value is an integer scalar in the range [0-6], such that:
      !
      ! 0: sunday
      ! 1: monday
      ! 2: tuesday
      ! 3: wednesday
      ! 4: thursday
      ! 5: friday
      ! 6: saturday
      class(datetime), intent(in) :: self
      integer :: year, month, j, k

      year  = self % year
      month = self % month

      if (month <= 2) then
         month = month + 12
         year  = year - 1
      endif

      j = year / 100
      k = mod(year, 100)

      weekday = mod(self % day + ((month + 1) * 26) / 10 + k + k / 4 + j / 4 + 5 * j, 7) -1

      if (weekday < 0) weekday = 6

   end function weekday


   pure elemental integer function isoweekday(self)
      ! returns the day of the week per iso 8601 returned from weekday().
      ! returned value is an integer scalar in the range [1-7].
      class(datetime), intent(in) :: self
      
      isoweekday = self % weekday()
      if (isoweekday == 0) isoweekday = 7
   end function isoweekday


   pure elemental character(9) function weekday_long(self)
      ! returns the full name of the day of the week.
      class(datetime), intent(in) :: self
      
      character(9), parameter :: days(*) = ['sunday   ', 'monday   ',    &
                                            'tuesday  ', 'wednesday',    &
                                            'thursday ', 'friday   ', 'saturday ']
      weekday_long = days(self % weekday() + 1)
   end function weekday_long


   pure elemental character(9) function isoweekday_long(self)
      ! returns the full name of the day of the week for iso 8601
      ! ordered weekdays.
      class(datetime), intent(in) :: self
      
      character(9), parameter :: days(7) = ['monday   ', 'tuesday  ',   &
                                            'wednesday', 'thursday ',    &
                                            'friday   ', 'saturday ', 'sunday   ']
      isoweekday_long = days(self % isoweekday())
   end function isoweekday_long


   pure elemental character(3) function weekday_short(self)
      ! returns the short (3-letter) name of the day of the week.
      class(datetime), intent(in) :: self
      character(3), parameter :: days(7) = ['sun', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat']
    
      weekday_short = days(self % weekday() + 1)
   end function weekday_short


   pure elemental character(3) function isoweekday_short(self)
      ! returns the short (3-letter) name of the day of the week
      ! based on iso 8601 ordering.
      class(datetime), intent(in) :: self
      character(3), parameter :: days(7) = ['mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun']
      isoweekday_short = days(self % isoweekday())
   end function isoweekday_short


   integer(int64) function seconds_since_epoch(self)
      ! returns an integer number of seconds since the unix epoch (1 jan 1970).
      ! since windows does not have strftime('%s'), we implement this using
      ! datetime itself.
      class(datetime), intent(in) :: self
    
      type(timedelta) :: delta
      type(datetime)  :: this_time, unix_time
      integer         :: sign, hours, minutes, tzsec

      this_time = datetime(self%year, self%month, self%day, &
                           self%hour, self%minute, self%second)
      unix_time = datetime(1970, 1, 1, 0, 0, 0)
      delta = this_time - unix_time
      seconds_since_epoch = delta%total_seconds()

      if (self % tz == 0_real64) return

      ! affect timezone information
      if (self % tz < 0_real64) then
         sign = -1
      else
         sign =  1
      endif

      hours = int(abs(self % tz))
      minutes = nint((abs(self % tz) - hours) * 60)

      if (minutes == 60) then
         minutes = 0
         hours = hours + 1
      endif

      tzsec = sign * (hours * h2s + minutes)
      seconds_since_epoch = seconds_since_epoch - tzsec

   end function seconds_since_epoch


   pure elemental character(5) function tz_offset(self)
      ! returns a character string with timezone offset in hours from utc,
      ! in format +/-[hh][mm].
      class(datetime), intent(in) :: self
      integer :: hours,minutes

      if (self % tz < 0) then
         tz_offset(1:1) = '-'
      else
         tz_offset(1:1) = '+'
      endif

      hours = int(abs(self % tz))
      minutes = nint((abs(self % tz) - hours) * 60)

      if (minutes == 60) then
         minutes = 0
         hours = hours + 1
      endif

      write(tz_offset(2:5), '(2i2.2)') hours, minutes

   end function tz_offset


   pure elemental type(datetime) function utc(self)
      ! returns the `datetime` instance at coordinated universal time (utc).
      class(datetime), intent(in) :: self
      integer :: hours, minutes, sgn
    
      hours = int(abs(self % tz))
      minutes = nint((abs(self % tz) - hours) * 60)
      sgn = int(sign(one, self % tz))
      utc = self - timedelta(hours=sgn * hours, minutes=sgn * minutes)
      utc % tz = 0
  end function utc


   pure elemental integer function yearday(self)
      ! returns the integer day of the year (ordinal date).
      class(datetime), intent(in) :: self
      integer :: month
      
      yearday = 0
      do month = 1, self % month-1
         yearday = yearday + days_in_month(month, self % year)
      enddo
      yearday = yearday + self % day
   end function yearday


   pure elemental function datetime_plus_timedelta(d0,t) result(d)
      ! adds a `timedelta` instance to a `datetime` instance, and returns a
      ! new `datetime` instance. overloads the operator `+`.
      class(datetime), intent(in) :: d0
      class(timedelta), intent(in) :: t
      type(datetime) :: d

      integer :: milliseconds, seconds, minutes, hours, days

      d = datetime(year        = d0 % get_year(),        &
                   month       = d0 % get_month(),       &
                   day         = d0 % get_day(),         &
                   hour        = d0 % get_hour(),        &
                   minute      = d0 % get_minute(),      &
                   second      = d0 % get_second(),      &
                   millisecond = d0 % get_millisecond(), &
                   tz          = d0 % get_tz())

      milliseconds = t % get_milliseconds()
      seconds = t % get_seconds()
      minutes = t % get_minutes()
      hours = t % get_hours()
      days = t % get_days()
      
      if (milliseconds /= 0) call d % add_milliseconds(milliseconds)
      if (seconds /= 0) call d % add_seconds(seconds)
      if (minutes /= 0) call d % add_minutes(minutes)
      if (hours /= 0) call d % add_hours(hours)
      if (days /= 0) call d % add_days(days)

   end function datetime_plus_timedelta


   pure elemental function timedelta_plus_datetime(t,d0) result(d)
      ! adds a `timedelta` instance to a `datetime` instance, and returns a
      ! new `datetime` instance. overloads the operator `+`.
      class(timedelta), intent(in) :: t
      class(datetime),  intent(in) :: d0
      type(datetime) :: d
      
      d = d0 + t
   end function timedelta_plus_datetime


   pure elemental function datetime_minus_timedelta(d0,t) result(d)
      ! subtracts a `timedelta` instance from a `datetime` instance and
      ! returns a new `datetime` instance. overloads the operator `-`.
      class(datetime), intent(in) :: d0
      class(timedelta), intent(in) :: t
      type(datetime) :: d
      d = d0 + (-t)
   end function datetime_minus_timedelta


   pure elemental function datetime_minus_datetime(d0,d1) result(t)
      ! subtracts a `datetime` instance from another `datetime` instance,
      ! and returns a `timedelta` instance. overloads the operator `-`.
      class(datetime), intent(in) :: d0, d1
      
      type(timedelta) :: t
      real(real64)    :: daysdiff
      integer         :: days, hours, minutes, seconds, milliseconds
      integer         :: sign_

      daysdiff = date2num(d0) - date2num(d1)

      if (daysdiff < 0) then
         sign_ = -1
         daysdiff = abs(daysdiff)
      else
         sign_ = 1
      endif

      days         = int(daysdiff)
      hours        = int((daysdiff - days) * d2h)
      minutes      = int((daysdiff - days - hours * h2d) * d2m)
      seconds      = int((daysdiff - days - hours * h2d - minutes * m2d) * d2s)
      milliseconds = nint((daysdiff - days - hours * h2d - minutes * m2d   &
                           - seconds * s2d) * d2s * 1e3_real64)

      t = timedelta(sign_*days,sign_*hours, sign_*minutes, sign_*seconds, sign_*milliseconds)

   end function datetime_minus_datetime


   pure elemental logical function datetime_gt(d0,d1) result(res)
      ! `datetime` comparison operator that returns `.true.` if `d0` is
      ! greater than `d1` and `.false.` otherwise. overloads the
      ! operator `>`.
      class(datetime), intent(in) :: d0, d1
      type(datetime) :: d0_utc, d1_utc

      ! convert to utc before making comparison
      d0_utc = d0 % utc()
      d1_utc = d1 % utc()

      ! compare years
      if (d0_utc % year > d1_utc % year) then
         res = .true.
      else if (d0_utc % year < d1_utc % year) then
         res = .false.
      else

         ! compare months
         if (d0_utc % month > d1_utc % month) then
            res = .true.
         else if (d0_utc % month < d1_utc % month) then
            res = .false.
         else

            ! compare days
            if (d0_utc % day > d1_utc % day) then
               res = .true.
            else if (d0_utc % day < d1_utc % day) then
               res = .false.
            else

               ! compare hours
               if (d0_utc % hour > d1_utc % hour) then
                  res = .true.
               else if (d0_utc % hour < d1_utc % hour) then
                  res = .false.
               else
                  
                  ! compare minutes
                  if (d0_utc % minute > d1_utc % minute) then
                     res = .true.
                  else if (d0_utc % minute < d1_utc % minute) then
                     res = .false.
                  else
                  
                     ! compare seconds
                     if (d0_utc % second > d1_utc % second) then
                        res = .true.
                     else if (d0_utc % second < d1_utc % second) then
                        res = .false.
                     else
                     
                        ! compare milliseconds
                        if (d0_utc % millisecond > d1_utc % millisecond) then
                           res = .true.
                        else
                           res = .false.
                        endif

                     endif
                  endif
               endif
            endif
         endif
      endif
   
   end function datetime_gt


   pure elemental logical function datetime_lt(d0,d1) result(res)
      ! `datetime` comparison operator that returns `.true.` if `d0` is
      ! less than `d1` and `.false.` otherwise. overloads the operator `<`.
      class(datetime), intent(in) :: d0, d1
      res = d1 > d0
   end function datetime_lt


   pure elemental logical function datetime_eq(d0,d1) result(res)
      ! `datetime` comparison operator that returns `.true.` if `d0` is
      ! equal to `d1` and `.false.` otherwise. overloads the operator `==`.
      class(datetime), intent(in) :: d0, d1
      type(datetime) :: d0_utc, d1_utc

      ! convert to utc before making comparison
      d0_utc = d0 % utc()
      d1_utc = d1 % utc()

      res = d0_utc % year        == d1_utc % year   .and. &
            d0_utc % month       == d1_utc % month  .and. &
            d0_utc % day         == d1_utc % day    .and. &
            d0_utc % hour        == d1_utc % hour   .and. &
            d0_utc % minute      == d1_utc % minute .and. &
            d0_utc % second      == d1_utc % second .and. &
            d0_utc % millisecond == d1_utc % millisecond

   end function datetime_eq

   
   pure elemental logical function datetime_neq(d0,d1) result(res)
      ! `datetime` comparison operator that eturns `.true.` if `d0` is
      ! not equal to `d1` and `.false.` otherwise. overloads the operator `/=`.
      class(datetime), intent(in) :: d0, d1
      res = .not. d0 == d1
   end function datetime_neq


   pure elemental logical function datetime_ge(d0,d1) result(res)
      ! `datetime` comparison operator. returns `.true.` if `d0` is greater
      ! than or equal to `d1` and `.false.` otherwise. overloads the
      ! operator `>=`.
      class(datetime), intent(in) :: d0, d1
      res = d0 > d1 .or. d0 == d1
   end function datetime_ge


   pure elemental logical function datetime_le(d0,d1) result(res)
      ! `datetime` comparison operator. returns `.true.` if `d0` is less
      ! than or equal to `d1`, and `.false.` otherwise. overloads the
      ! operator `<=`.
      class(datetime), intent(in) :: d0, d1
      res = d1 > d0 .or. d0 == d1
   end function datetime_le


   pure elemental logical function is_leapyear(year)
      ! returns `.true.` if year is leap year and `.false.` otherwise.
      integer, intent(in) :: year
      is_leapyear = (mod(year,4) == 0 .and. .not. mod(year,100) == 0)&
                     .or. (mod(year,400) == 0)
  end function is_leapyear


   pure function datetime_range(d0, d1, t)
      ! given start and end `datetime` instances `d0` and `d1` and time
      ! increment as `timedelta` instance `t`, returns an array of
      ! `datetime` instances. the number of elements is the number of whole
      ! time increments contained between datetimes `d0` and `d1`.
      type(datetime),  intent(in) :: d0, d1
      type(timedelta), intent(in) :: t
      
      real(real64)                :: datenum0, datenum1, eps, increment
      type(datetime), allocatable :: datetime_range(:)
      integer                     :: n, nm
      
      eps = 1e-10_real64
      datenum0 = date2num(d0)
      datenum1 = date2num(d1)
      increment = t % total_seconds() * s2d
      nm = floor((datenum1 - datenum0 + eps) / increment) + 1
      allocate(datetime_range(nm))
      do n = 1, nm
         datetime_range(n) = num2date(datenum0 + (n - 1) * increment)
      enddo
   end function datetime_range


   pure elemental integer function days_in_month(month,year)
      ! given integer month and year, returns an integer number
      ! of days in that particular month.
      integer, intent(in) :: month, year

      integer, parameter :: days(*) = [31, 28, 31, 30, 31, 30, &
                                       31, 31, 30, 31, 30, 31]

      if (month < 1 .or. month > 12) then
         ! should raise an error and abort here, however we want to keep
         ! the pure and elemental attributes. make sure this function is
         ! called with the month argument in range.
         days_in_month = 0
         return
      endif

      if (month == 2 .and. is_leapyear(year)) then
         days_in_month = 29
      else
         days_in_month = days(month)
      endif

   end function days_in_month


   pure elemental integer function days_in_year(year)
      ! returns the number of days in year.
      integer, intent(in) :: year
      
      if (is_leapyear(year)) then
         days_in_year = 366
      else
         days_in_year = 365
      endif
  end function days_in_year


   pure elemental real(real64) function date2num(d)
      ! given a datetime instance d, returns number of days since
      ! `0001-01-01 00:00:00`, taking into account the timezone offset.
      type(datetime), intent(in) :: d
      
      type(datetime) :: d_utc
      integer :: year

      ! convert to utc first
      d_utc = d % utc()

      ! d_utc % year must be positive:
      if (d_utc % year < 1) then
         date2num = 0
         return
      endif

      date2num = 0
      do year = 1,d_utc % year-1
         date2num = date2num + days_in_year(year)
      enddo

      date2num = date2num          &
               + d_utc % yearday() &
               + d_utc % hour*h2d  &
               + d_utc % minute*m2d&
               + (d_utc % second+1e-3_real64*d_utc % millisecond)*s2d

   end function date2num


   pure elemental type(datetime) function num2date(num)
      ! given number of days since `0001-01-01 00:00:00`, returns a
      ! correspoding `datetime` instance.
      real(real64), intent(in) :: num
      integer      :: year, month, day, hour, minute, second, millisecond
      real(real64) :: days, totseconds

      ! num must be positive
      if (num < 0) then
         num2date = datetime(1)
         return
      endif

      days = num

      year = 1
      do
         if (int(days) <= days_in_year(year))exit
         days = days-days_in_year(year)
         year = year+1
      enddo

      month = 1
      do
         if (int(days) <= days_in_month(month,year))exit
         days = days-days_in_month(month,year)
         month = month+1
      enddo

      day         = int(days)
      totseconds  = (days - day) * d2s
      hour        = int(totseconds * s2h)
      minute      = int((totseconds - hour * h2s) * s2m)
      second      = int(totseconds - hour * h2s - minute * m2s)
      millisecond = nint((totseconds - int(totseconds)) * 1e3_real64)

      num2date = datetime(year,month,day,hour,minute,second,millisecond,tz=zero)

      ! handle a special case caused by floating-point arithmethic:
      if (num2date % millisecond == 1000) then
         num2date % millisecond = 0
         call num2date % add_seconds(1)
      endif

      if (num2date % second == 60) then
         num2date % second = 0
         call num2date % add_minutes(1)
      endif
      
      if (num2date % minute == 60) then
         num2date % minute = 0
         call num2date % add_hours(1)
      endif
      
      if (num2date % hour == 24) then
         num2date % hour = 0
         call num2date % add_days(1)
      endif

   end function num2date


   real(real64) function machine_timezone()
      ! return a real value instance of local machine's timezone.
      character(len=5) :: zone
      integer :: values(8)
      integer :: hour, minute

      ! obtain local machine time zone information
      call date_and_time(zone=zone, values=values)
      read(zone(1:3), '(i3)') hour
      read(zone(4:5), '(i2)') minute

      if (hour < 0) then
         machine_timezone = real(hour, kind=real64) - real(minute, kind=real64) * m2h
      else
         machine_timezone = real(hour, kind=real64) + real(minute, kind=real64) * m2h
      endif
   end function machine_timezone


   pure elemental type(datetime) function epochdatetime()
         epochdatetime = datetime(1970,1,1,0,0,0,0,tz=zero)
   end function epochdatetime


   pure elemental type(datetime) function local_time(epoch, tz)
      ! returns a `datetime` instance from epoch.
      ! tz can be obtained from `machine_timezone`
      integer(int64),intent(in) :: epoch
      real(real64),  intent(in) :: tz !! local machine time zone information
    
      type(datetime)  :: datetime_from_epoch
      type(timedelta) :: td
      integer         :: day, sec
      integer(int64)  :: localseconds

      datetime_from_epoch = epochdatetime()

      localseconds = nint(tz * h2s) + epoch
      
      ! suppress overflow
      day = floor(localseconds/d2s, kind=real32)
      sec = localseconds - day * d2s
      td = timedelta(days=day, seconds=sec)
      datetime_from_epoch % tz = tz
      local_time = datetime_from_epoch + td
   end function local_time


   pure elemental type(datetime) function gmtime(epoch)
      ! returns a `datetime` instance from epoch.
      integer(int64),intent(in) :: epoch
   
      type(datetime)  :: datetime_from_epoch
      type(timedelta) :: td
      integer         :: day, sec
    
      datetime_from_epoch = epochdatetime()
    
      ! suppress overflow
      day = floor(epoch/d2s, kind=real32)
      sec = epoch - day * d2s
      td = timedelta(days=day, seconds=sec)
      gmtime = datetime_from_epoch + td
   end function gmtime

   pure function int2str(i, length)
      ! converts an integer `i` into a character string of requested length,
      ! pre-pending zeros if necessary.
      integer, intent(in) :: i, length
      character(length) :: int2str
      character(2) :: string
      
      write(string, '(i2)') length
      write(int2str, '(i' // string // '.' // string //')') i
   end function int2str


   pure elemental type(timedelta) function timedelta_constructor( &
               days, hours, minutes, seconds, milliseconds)
    
      ! constructor function for the `timedelta` class.
      integer, intent(in), optional :: days, hours, minutes, seconds, milliseconds
      
      timedelta_constructor % days = 0
      if (present(days)) timedelta_constructor % days = days
      
      timedelta_constructor % hours = 0
      if (present(hours)) timedelta_constructor % hours = hours
      
      timedelta_constructor % minutes = 0
      if (present(minutes)) timedelta_constructor % minutes = minutes
      
      timedelta_constructor % seconds = 0
      if (present(seconds)) timedelta_constructor % seconds = seconds
      
      timedelta_constructor % milliseconds = 0
      if (present(milliseconds)) timedelta_constructor % milliseconds = milliseconds

   end function timedelta_constructor


   pure elemental integer function get_days(self)
      ! returns the number of days.
      class(timedelta), intent(in) :: self
      get_days = self % days
   end function get_days


   pure elemental integer function get_hours(self)
      ! returns the number of hours.
      class(timedelta), intent(in) :: self
      get_hours = self % hours
   end function get_hours


   pure elemental integer function get_minutes(self)
      ! returns the number of minutes.
      class(timedelta), intent(in) :: self
      get_minutes = self % minutes
   end function get_minutes


   pure elemental integer function get_seconds(self)
      ! returns the number of seconds.
      class(timedelta), intent(in) :: self
      get_seconds = self % seconds
   end function get_seconds


   pure elemental integer function get_milliseconds(self)
      ! returns the number of milliseconds.
      class(timedelta), intent(in) :: self
      get_milliseconds = self % milliseconds
   end function get_milliseconds


   pure elemental real(real64) function total_seconds(self)
      ! returns a total number of seconds contained in a `timedelta`
      ! instance.
      class(timedelta), intent(in) :: self
      total_seconds = self % days*86400._real64       &
                    + self % hours*3600._real64       &
                    + self % minutes*60._real64       &
                    + self % seconds                  &
                    + self % milliseconds*1e-3_real64
   end function total_seconds


   pure elemental type(timedelta) function timedelta_plus_timedelta(t0,t1) result(t)
      ! adds two `timedelta` instances together and returns a `timedelta`
      ! instance. overloads the operator `+`.
      class(timedelta), intent(in) :: t0, t1
      t = timedelta(days         = t0 % days         + t1 % days,    &
                    hours        = t0 % hours        + t1 % hours,   &
                    minutes      = t0 % minutes      + t1 % minutes, &
                    seconds      = t0 % seconds      + t1 % seconds, &
                    milliseconds = t0 % milliseconds + t1 % milliseconds)
   end function timedelta_plus_timedelta


   pure elemental type(timedelta) function timedelta_minus_timedelta(t0,t1) result(t)
      ! subtracts a `timedelta` instance from another. returns a
      ! `timedelta` instance. overloads the operator `-`.
      class(timedelta), intent(in) :: t0, t1
      t = t0 + (-t1)
   end function timedelta_minus_timedelta


   pure elemental type(timedelta) function unary_minus_timedelta(t0) result(t)
      ! takes a negative of a `timedelta` instance. overloads the operator `-`.
      class(timedelta), intent(in) :: t0
      t % days         = -t0 % days
      t % hours        = -t0 % hours
      t % minutes      = -t0 % minutes
      t % seconds      = -t0 % seconds
      t % milliseconds = -t0 % milliseconds
   end function unary_minus_timedelta


   pure elemental logical function timedelta_eq(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if `td0`
      ! is equal to `td1` and `.false.` otherwise. overloads the operator `==`.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() == td1 % total_seconds()
   end function timedelta_eq


   pure elemental logical function timedelta_neq(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if `td0`
      ! is not equal to `td1` and `.false.` otherwise. overloads the
      ! operator `/=`.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() /= td1 % total_seconds()
   end function timedelta_neq


   pure elemental logical function timedelta_gt(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if
      ! `td0` is greater than `td1` and `.false.` otherwise. overloads the
      ! operator `>`.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() > td1 % total_seconds()
   end function timedelta_gt


   pure elemental logical function timedelta_ge(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if `td0`
      ! is greater than or equal to `td1` and `.false.` otherwise.
      ! overloads the operator >=.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() >= td1 % total_seconds()
   end function timedelta_ge


   pure elemental logical function timedelta_lt(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if `td0`
      ! is less than `td1` and `.false.` otherwise. overloads the operator `<`.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() < td1 % total_seconds()
   end function timedelta_lt


   pure elemental logical function timedelta_le(td0,td1) result(res)
      ! `timedelta` object comparison operator. returns `.true.` if `td0`
      ! is less than or equal to `td1` and `.false.` otherwise. overloads
      ! the operator `<=`.
      class(timedelta), intent(in) :: td0, td1
      res = td0 % total_seconds() <= td1 % total_seconds()
   end function timedelta_le

end module module_datetime
