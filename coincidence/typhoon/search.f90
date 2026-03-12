module search
    use inout, only: open_csv, read_csv_row, to_int, field_to_real, STRLEN, find_column
    implicit none

contains

    subroutine search_hrid(hra_data_name, target_hrid, nt, hra_time, hra_lat, hra_lon,ios)
        character(len=*), intent(in) :: hra_data_name
        character(len=*), intent(in) :: target_hrid
        integer, intent(out) :: nt
        character(len=8), intent(out) :: hra_time(:)
        real, intent(out) :: hra_lat(:), hra_lon(:)
        integer, intent(out) :: ios

        integer :: hra_data, nfields
        integer :: year, month, day, hour
        character(len=STRLEN), allocatable :: fields(:)
        character(len=STRLEN) :: hrid
        character(len=8) :: ymdh

        nt = 0

        !open hra info file
        call open_csv(hra_data_name, hra_data, ios)
        if (ios /= 0) stop 'open error'

        do
            call read_csv_row(hra_data, fields, nfields, ios)
            if (ios /= 0) exit
            if (nfields < 7) cycle

            !read hrid
            hrid = trim(fields(5))
            if (trim(hrid) /= trim(target_hrid)) cycle

            year = to_int(fields(1), -999)
            month = to_int(fields(2), -999)
            day = to_int(fields(3), -999)
            hour = to_int(fields(4), -999)

            write(ymdh, '(I2.2,I2.2,I2.2,I2.2)') mod(year,100), month, day, hour

            nt = nt + 1
            if (nt > size(hra_time)) then
                print *, 'search_hrid error: array size too small'
                ios = 999
                exit
            endif

            hra_time(nt) = ymdh
            hra_lat(nt) = field_to_real(fields, nfields, 6, -999.0)
            hra_lon(nt) =  field_to_real(fields, nfields, 7, -999.0)

        enddo

        close(hra_data)

        if(ios /= 999) ios = 0
    end subroutine search_hrid

    subroutine search_hrid_stats(stats_file, target_hrid, found, &
                                 r1max, r3max, rtmax, racc, r1mean, r3mean, rtmean, ios)
        character(len=*), intent(in) :: stats_file
        character(len=*), intent(in) :: target_hrid
        logical, intent(out) :: found
        real, intent(out) :: r1max, r3max, rtmax, racc, r1mean, r3mean, rtmean
        integer, intent(out) :: ios

        integer :: unit, nheader, nfields
        integer :: col_hrid, col_r1max, col_r3max, col_rtmax
        integer :: col_racc, col_r1mean, col_r3mean, col_rtmean
        character(len=STRLEN), allocatable :: header(:), fields(:)

        found = .false.
        r1max  = -999.0
        r3max  = -999.0
        rtmax  = -999.0
        racc   = -999.0
        r1mean = -999.0
        r3mean = -999.0
        rtmean = -999.0
        ios = 0

        call open_csv(stats_file, unit, ios)
        if (ios /= 0) return

        ! header
        call read_csv_row(unit, header, nheader, ios)
        if (ios /= 0) then
            close(unit)
            return
        end if

        col_hrid   = find_column(header, nheader, 'hrid')
        col_r1max  = find_column(header, nheader, 'r1max')
        col_r3max  = find_column(header, nheader, 'r3max')
        col_rtmax  = find_column(header, nheader, 'rtmax')
        col_racc   = find_column(header, nheader, 'racc')
        col_r1mean = find_column(header, nheader, 'r1mean')
        col_r3mean = find_column(header, nheader, 'r3mean')
        col_rtmean = find_column(header, nheader, 'rtmean')

        if (col_hrid < 1) then
            ios = 999
            close(unit)
            return
        end if

        do
            call read_csv_row(unit, fields, nfields, ios)
            if (ios /= 0) exit

            if (trim(fields(col_hrid)) /= trim(target_hrid)) cycle

            found = .true.

            if (col_r1max  > 0) r1max  = field_to_real(fields, nfields, col_r1max,  -999.0)
            if (col_r3max  > 0) r3max  = field_to_real(fields, nfields, col_r3max,  -999.0)
            if (col_rtmax  > 0) rtmax  = field_to_real(fields, nfields, col_rtmax,  -999.0)
            if (col_racc   > 0) racc   = field_to_real(fields, nfields, col_racc,   -999.0)
            if (col_r1mean > 0) r1mean = field_to_real(fields, nfields, col_r1mean, -999.0)
            if (col_r3mean > 0) r3mean = field_to_real(fields, nfields, col_r3mean, -999.0)
            if (col_rtmean > 0) rtmean = field_to_real(fields, nfields, col_rtmean, -999.0)

            exit
        end do

        close(unit)
        ios = 0
    end subroutine search_hrid_stats

end module search

