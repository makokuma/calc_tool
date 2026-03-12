program search_hrid_test
    use inout
    use search
    implicit none

    integer, parameter :: NMAX = 1000
    integer :: ls_data, ios, nfields, nt, i
    integer :: year_case
    character(len=STRLEN), allocatable :: fields(:)
    character(len=STRLEN) :: target_hrid
    character(len=8) :: hra_time(NMAX)
    real :: hra_lat(NMAX), hra_lon(NMAX)
    character(len=32) :: arg_year

    call get_command_argument(1, arg_year)
    if (len_trim(arg_year) == 0) then
        stop 'need YEAR argument'
    end if
    read(arg_year, *) year_case

    call open_csv('fort.100', ls_data, ios)
    if (ios /= 0) stop 'open error'

    do
        call read_csv_row(ls_data, fields, nfields, ios)
        if (ios /= 0) exit
        if (nfields < 3) cycle

        ! fort.100 の dtst 年でフィルタ
        if (to_int(fields(3)(1:4), -999) /= year_case) cycle

        target_hrid = trim(fields(2))

        print *, '=========================='
        print *, 'target_hrid = ', trim(target_hrid)
        print *, 'year        = ', year_case
        print *, '=========================='

        call search_hrid('fort.400', trim(target_hrid), nt, hra_time, hra_lat, hra_lon, ios)
        if (ios /= 0) then
            print *, 'search error'
            cycle
        end if

        print *, 'nt = ', nt
        do i = 1, nt
            print *, trim(hra_time(i)), hra_lat(i), hra_lon(i)
        end do
    end do

    close(ls_data)
end program search_hrid_test
