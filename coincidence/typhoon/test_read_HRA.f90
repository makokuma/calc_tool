program read_test_HRA
    use inout
    implicit none
    integer :: hra_data, ios, nfields
    integer :: year, month, day, hour
    real :: lat, lon
    character(len=STRLEN), allocatable :: fields(:)
    character(len=20) :: hrid
    character(len=8) :: ymdh

    !read csv
    call open_csv('fort.400', hra_data, ios)
    if (ios /= 0) stop 'open error'

    do
        call read_csv_row(hra_data, fields, nfields, ios)
        if (ios /= 0) exit
        if (nfields < 7) cycle
        
        !read elements
        year = to_int(fields(1), -999)
        month = to_int(fields(2), -999)
        day = to_int(fields(3), -999)
        hour = to_int(fields(4), -999)
        hrid = trim(fields(5))
        lat = field_to_real(fields, nfields, 6, -999.0)
        lon = field_to_real(fields, nfields, 7, -999.0)

        write(ymdh, '(I2.2,I2.2,I2.2,I2.2)') mod(year,100), month, day, hour

        !print *, trim(hrid), trim(ymdh), lat, lon
        write(6, '(A,x,A,x,f6.2,x,f6.2)') trim(hrid), trim(ymdh), lat, lon
    enddo

    close(hra_data)

end program read_test_HRA




