module inout
    implicit none
    integer, parameter :: STRLEN = 256
contains

    subroutine open_csv(csvfile_name, csv_unit, ios)
        character(len=*), intent(in) :: csvfile_name
        integer, intent(out) :: csv_unit
        integer, intent(out) :: ios

        open(newunit=csv_unit, file=trim(csvfile_name), &
             status='old', action='read', iostat=ios)
    end subroutine open_csv


    subroutine read_csv_row(csv_unit, fields, nfields, ios)
        integer, intent(in) :: csv_unit
        character(len=STRLEN), allocatable, intent(out) :: fields(:)
        integer, intent(out) :: nfields
        integer, intent(out) :: ios

        character(len=10000) :: line

        read(csv_unit, '(A)', iostat=ios) line
        if (ios /= 0) then
            nfields = 0
            return
        end if

        call split_csv_line(trim(line), fields, nfields)
    end subroutine read_csv_row


    subroutine split_csv_line(line, fields, nfields)
        character(len=*), intent(in) :: line
        character(len=STRLEN), allocatable, intent(out) :: fields(:)
        integer, intent(out) :: nfields

        integer :: i, start, lenline, k
        integer, allocatable :: pos(:)

        lenline = len_trim(line)

        nfields = 1
        do i = 1, lenline
            if (line(i:i) == ',') nfields = nfields + 1
        end do

        allocate(fields(nfields))
        fields = ''

        if (nfields > 1) then
            allocate(pos(nfields-1))
            k = 0
            do i = 1, lenline
                if (line(i:i) == ',') then
                    k = k + 1
                    pos(k) = i
                end if
            end do
        end if

        start = 1
        do i = 1, nfields-1
            if (pos(i) >= start) then
                fields(i) = adjustl(line(start:pos(i)-1))
            else
                fields(i) = ''
            end if
            start = pos(i) + 1
        end do

        if (start <= lenline) then
            fields(nfields) = adjustl(line(start:lenline))
        else
            fields(nfields) = ''
        end if

        if (allocated(pos)) deallocate(pos)
    end subroutine split_csv_line


    integer function find_column(header, nheader, name)
        character(len=*), intent(in) :: header(:)
        integer, intent(in) :: nheader
        character(len=*), intent(in) :: name
        integer :: i

        find_column = -1
        do i = 1, nheader
            if (trim(adjustl(header(i))) == trim(adjustl(name))) then
                find_column = i
                return
            end if
        end do
    end function find_column


    function get_field(fields, nfields, idx) result(val)
        character(len=*), intent(in) :: fields(:)
        integer, intent(in) :: nfields, idx
        character(len=STRLEN) :: val

        val = ''
        if (idx < 1 .or. idx > nfields) return
        val = trim(fields(idx))
    end function get_field

    subroutine open_txt(filename, unit, ios)
        character(len=*), intent(in)  :: filename
        integer, intent(out) :: unit
        integer, intent(out) :: ios

        open(newunit=unit, file=trim(filename), status='old', action='read', iostat=ios)
    end subroutine open_txt

    subroutine read_line(unit, line, ios)
        integer, intent(in) :: unit
        character(len=*), intent(out) :: line
        integer, intent(out) :: ios

        read(unit, '(A)', iostat=ios) line
    end subroutine read_line

    function to_int(str, default) result(val)
        character(len=*), intent(in) :: str
        integer, intent(in), optional :: default
        integer :: val, ios

        read(str, *, iostat=ios) val
        if (ios /= 0) then
            if (present(default)) then
                val = default
            else
                val = -999
            end if
        end if
    end function to_int


end module inout
