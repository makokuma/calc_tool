program check_CE1_main
    use inout
    use search
    use calc_CE1
    implicit none

    integer, parameter :: NMAX = 2000
    integer :: ls_data, ios, nfields, nt, i
    integer :: ce1_flag
    integer :: year_case
    real :: min_dist, best_tylat, best_tylon
    integer :: best_pres

    character(len=STRLEN), allocatable :: fields(:)
    character(len=STRLEN) :: target_hrid
    character(len=STRLEN) :: dtst
    character(len=32) :: arg_year
    character(len=8) :: hra_time(NMAX)
    real :: hra_lat(NMAX), hra_lon(NMAX)
    character(len=8) :: best_time

    call get_command_argument(1, arg_year)
    if (len_trim(arg_year) == 0) stop 'need YEAR argument'
    read(arg_year, *) year_case

    call open_csv('fort.100', ls_data, ios)
    if (ios /= 0) stop 'open error: fort.100'

    ! header skip
    call read_csv_row(ls_data, fields, nfields, ios)
    if (ios /= 0) stop 'read header error'

    do
        call read_csv_row(ls_data, fields, nfields, ios)
        if (ios /= 0) exit
        if (nfields < 4) cycle

        dtst = trim(fields(3))
        if (len_trim(dtst) < 4) cycle

        ! year filter
        if (to_int(dtst(1:4), -999) /= year_case) cycle

        target_hrid = trim(fields(2))

        call search_hrid('fort.400', trim(target_hrid), nt, hra_time, hra_lat, hra_lon, ios)
        if (ios /= 0) then
            print *, 'search error: ', trim(target_hrid)
            cycle
        end if

        if (nt <= 0) then
            print *, 'no hra steps: ', trim(target_hrid)
            cycle
        end if

        ! safety check: hra_time year must match year_case
        do i = 1, nt
            if (to_int('20'//hra_time(i)(1:2), -999) /= year_case) then
                print *, 'skip year mismatch in fort.400: ', trim(target_hrid), trim(hra_time(i))
                nt = 0
                exit
            end if
        end do
        if (nt <= 0) cycle

        call judge_CE1('bst_1h_new.txt', nt, hra_time, hra_lat, hra_lon, &
                       ce1_flag, min_dist, best_time, best_tylat, best_tylon, best_pres)

        ! safety check: best_time year must match year_case
        if (ce1_flag /= -1) then
            if (to_int('20'//best_time(1:2), -999) /= year_case) then
                print *, 'skip bst year mismatch: ', trim(target_hrid), trim(best_time)
                cycle
            end if
        end if

        print *, '========================================'
        print *, 'HRID      = ', trim(target_hrid)
        print *, 'nt        = ', nt
        print *, 'CE1       = ', ce1_flag
        print *, 'min_dist  = ', min_dist
        print *, 'best_time = ', trim(best_time)
        print *, 'ty_latlon = ', best_tylat, best_tylon
        print *, 'best_pres = ', best_pres
    end do

    close(ls_data)
end program check_CE1_main
