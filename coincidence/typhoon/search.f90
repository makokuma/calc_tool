module search
    use inout, only: open_csv, read_csv_row, to_int, field_to_real, STRLEN
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

end module search

