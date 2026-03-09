program main
    use inout, only: open_csv, read_csv_row, find_column, get_field, STRLEN
    implicit none

    integer :: unit, ios
    integer :: col_hrid, col_lat, col_lon
    integer :: nheader, nfields, i
    character(len=STRLEN), allocatable :: header(:), fields(:)

    call open_csv('fort.100', unit, ios)
    if (ios /= 0) stop 'open error'

    call read_csv_row(unit, header, nheader, ios)
    if (ios /= 0) stop 'header read error'

    print *, 'nheader = ', nheader
    do i = 1, nheader
        print *, 'header(', i, ') = [', trim(header(i)), ']'
    end do

    col_hrid = find_column(header, nheader, 'hrid')
    col_lat  = find_column(header, nheader, 'lat')
    col_lon  = find_column(header, nheader, 'lon')

    print *, 'col_hrid = ', col_hrid
    print *, 'col_lat  = ', col_lat
    print *, 'col_lon  = ', col_lon

    if (col_hrid < 1 .or. col_lat < 1 .or. col_lon < 1) stop 'column not found'

    do
        call read_csv_row(unit, fields, nfields, ios)
        if (ios /= 0) exit

        print *, 'hrid = [', trim(get_field(fields, nfields, col_hrid)), ']'
        print *, 'lat  = [', trim(get_field(fields, nfields, col_lat)), ']'
        print *, 'lon  = [', trim(get_field(fields, nfields, col_lon)), ']'
    end do

    close(unit)
end program main
