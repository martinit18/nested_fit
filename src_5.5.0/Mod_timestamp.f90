module MOD_TIMESTAMP

contains
  function timestamp() result(str)
    implicit none
    character(len=15)                     :: str
    character(len=8)                      :: dt
    character(len=10)                     :: tm

    ! Get current time
    call date_and_time(DATE=dt, TIME=tm)  

    str = dt//'_'//tm(1:6)  ! tm(7:10) are milliseconds and decimal point

  end function timestamp
end module MOD_TIMESTAMP

