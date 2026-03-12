program check_CE2_main
    use inout
    use search
    use calc_CE1
    use calc_CE2
    implicit none

    integer, parameter :: NMAX = 2000
    integer :: ls_data, ios, nfields, nt, i
    integer :: ce1_flag, ce2_flag
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
    logical :: found_stats
    real :: r1max, r3max, rtmax, racc, r1mean, r3mean, rtmean
    real :: CE1_r3max

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
        
        !search r3max
        call search_hrid_stats('fort.200', trim(target_hrid), found_stats, &
                       r1max, r3max, rtmax, racc, r1mean, r3mean, rtmean, ios)

        if (.not. found_stats) then
            print *, 'stats not found: ', trim(target_hrid)
            cycle
        end if

        !judge CE1
        call judge_CE1_case('bst_1h_new.txt', nt, hra_time, hra_lat, hra_lon, r3max, &
               ce1_flag, min_dist, best_time, best_tylat, best_tylon, best_pres)


        print *, '========================================'
        print *, 'CE1 judge 1=yes 0=no', trim(target_hrid), ce1_flag
        print *, '========================================'
        
        !judge CE2
        call judge_CE2_case('bst_1h_new.txt', nt, hra_time, hra_lat, hra_lon, rtmax, &
               ce2_flag, min_dist, best_time, best_tylat, best_tylon, best_pres)


        print *, '========================================'
        print *, 'CE2 judge 1=yes 0=no ', trim(target_hrid), ce2_flag
        print *, '========================================'

    end do

    close(ls_data)

end program check_CE2_main
