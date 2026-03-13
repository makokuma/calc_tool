module time_tools
    implicit none
contains

    logical function leap_year(year)
        integer, intent(in) :: year

        leap_year = .false.

        if (mod(year,400) == 0) then
            leap_year = .true.
        elseif (mod(year,100) == 0) then
            leap_year = .false.
        elseif (mod(year,4) == 0) then
            leap_year = .true.
        endif

    end function leap_year

    integer function days_in_month(year,month)
        integer, intent(in) :: year, month

        select case (month)
        case (1, 3, 5, 7, 8, 10, 12)
            days_in_month = 31
        case (4, 6, 9, 11)
            days_in_month = 30
        case (2)
            if (leap_year(year)) then
                days_in_month = 29
            else
                days_in_month = 28
            endif

        case default
            days_in_month = -1
        end select
    end function days_in_month

    !separate ymdh only bst data
    subroutine parse_ymdh(ymdh, year, month, day, hour)
        character(len=*), intent(in) :: ymdh
        integer, intent(out) :: year, month, day, hour
        integer :: yy

        if (len_trim(ymdh) < 8 ) then
            print *, 'parse_ymdh error: invalid ymdh = [', trim(ymdh), ']'
            stop
        endif

        read(ymdh(1:2), *) yy
        read(ymdh(3:4), *) month
        read(ymdh(5:6), *) day
        read(ymdh(7:8), *) hour

        !for bst track data
        if (yy >= 51) then
            year = 1900 + yy
        else
            year = 2000 + yy
        endif
    end subroutine parse_ymdh

    subroutine make_ymdh(year, month, day, hour, ymdh)
        integer, intent(in) :: year, month, day, hour
        character(len=8), intent(out) :: ymdh

        write(ymdh, '(I2.2,I2.2,I2.2,I2.2)') mod(year,100), month, day, hour
    end subroutine make_ymdh

    subroutine add_hours_ymdh(ymdh_in, dh, ymdh_out)
        character(len=*), intent(in) :: ymdh_in
        integer, intent(in) :: dh
        character(len=8), intent(out) :: ymdh_out

        integer :: year, month, day, hour
        integer :: nday

        call parse_ymdh(trim(ymdh_in), year, month, day, hour)
        
        !dh is add hour 
        hour = hour + dh

        !positive
        !over 24h 
        do while (hour >= 24)
            hour = hour - 24
            day = day + 1

            nday = days_in_month(year, month)
            !day over end of the month
            if (day > nday) then
                day = 1
                month = month + 1
                if (month > 12) then
                    month = 1
                    year = year + 1
                endif
            endif
        enddo

        !negative
        do while (hour < 0)
            hour = hour + 24
            day = day -1

            if (day < 1) then
                month = month - 1
                if (month < 1) then
                    month = 12
                    year = year - 1
                endif
                day = days_in_month(year, month)
            endif
        enddo

        call make_ymdh(year, month, day, hour, ymdh_out)
    end subroutine add_hours_ymdh

    integer function hours_from_origin(year, month, day, hour)
        integer, intent(in) :: year, month, day, hour
        integer :: y, m

        hours_from_origin = 0

        do y = 1, year - 1
            if (leap_year(y)) then
                hours_from_origin = hours_from_origin + 366 * 24
            else
                hours_from_origin = hours_from_origin + 365 * 24
            end if
        end do

        do m = 1, month - 1
            hours_from_origin = hours_from_origin + days_in_month(year, m) * 24
        end do

        hours_from_origin = hours_from_origin + (day - 1) * 24 + hour
    end function hours_from_origin

    subroutine diff_hours_ymdh(ymdh1, ymdh2, dh)
        character(len=*), intent(in) :: ymdh1, ymdh2
        integer, intent(out) :: dh

        integer :: y1, m1, d1, h1
        integer :: y2, m2, d2, h2
        integer :: t1, t2

        call parse_ymdh(trim(ymdh1), y1, m1, d1, h1)
        call parse_ymdh(trim(ymdh2), y2, m2, d2, h2)

        t1 = hours_from_origin(y1, m1, d1, h1)
        t2 = hours_from_origin(y2, m2, d2, h2)

        dh = t2 - t1
    end subroutine diff_hours_ymdh

end module time_tools



