program test_time
    use time_tools
    implicit none

    character(len=8) :: t

    call add_hours_ymdh('51050600', 1, t)
    print *, '51050600 + 1h = ', t

    call add_hours_ymdh('51050600', 6, t)
    print *, '51050600 + 6h = ', t

    call add_hours_ymdh('51053123', 1, t)
    print *, '51053123 + 1h = ', t

    call add_hours_ymdh('99123123', 1, t)
    print *, '99123123 + 1h = ', t

    call add_hours_ymdh('00022923', 1, t)
    print *, '00022923 + 1h = ', t

end program test_time
