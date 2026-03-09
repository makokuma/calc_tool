module TD_bst
    use inout, only: open_txt, read_line, to_int, STRLEN
    implicit none

    type :: bst_header_type
        character(len=5)  :: indicator = ''
        character(len=4)  :: intl_id = ''
        integer           :: nline = 0
        character(len=4)  :: tc_id = ''
        character(len=4)  :: intl_id_rep = ''
        integer           :: last_flag = -999
        integer           :: last_diff_hour = -999
        character(len=20) :: name = ''
        character(len=8)  :: revision_date = ''
    end type bst_header_type

    type :: bst_record_type
        character(len=8) :: ymdh = ''
        character(len=3) :: indicator = ''
        integer          :: grade = -999
        real             :: lat = -999.0
        real             :: lon = -999.0
        integer          :: pres = -999
        integer          :: wind = -999
        integer          :: dir50 = -999
        integer          :: long50 = -999
        integer          :: short50 = -999
        integer          :: dir30 = -999
        integer          :: long30 = -999
        integer          :: short30 = -999
        character(len=1) :: landfall = ''
    end type bst_record_type

contains

    function safe_slice(line, i1, i2) result(out)
        character(len=*), intent(in) :: line
        integer, intent(in) :: i1, i2
        character(len=i2-i1+1) :: out
        integer :: n

        out = ' '
        n = len(line)

        if (i1 > n) return
        out(1:min(i2,n)-i1+1) = line(i1:min(i2,n))
    end function safe_slice
    
    !read bst track format data
    subroutine read_bst_header(line, h)
        character(len=*), intent(in) :: line
        type(bst_header_type), intent(out) :: h
        
        h%indicator      = trim(adjustl(safe_slice(line, 1, 5)))
        h%intl_id        = trim(adjustl(safe_slice(line, 7, 10)))
        h%nline          = to_int(safe_slice(line, 13, 15), 0)
        h%tc_id          = trim(adjustl(safe_slice(line, 17, 20)))
        h%intl_id_rep    = trim(adjustl(safe_slice(line, 22, 25)))
        h%last_flag      = to_int(safe_slice(line, 27, 27), -999)
        h%last_diff_hour = to_int(safe_slice(line, 29, 29), -999)
        h%name           = trim(adjustl(safe_slice(line, 31, 50)))
        h%revision_date  = trim(adjustl(safe_slice(line, 69, 76)))

    end subroutine read_bst_header

    subroutine read_bst_data(line, r)
        character(len=*), intent(in) :: line
        type(bst_record_type), intent(out) :: r

        r%ymdh      = trim(adjustl(safe_slice(line, 1, 8)))
        r%indicator = trim(adjustl(safe_slice(line, 10, 12)))
        r%grade     = to_int(safe_slice(line, 14, 14), -999)
        r%lat       = 0.1 * real(to_int(safe_slice(line, 16, 18), -999))
        r%lon       = 0.1 * real(to_int(safe_slice(line, 20, 23), -999))
        r%pres      = to_int(safe_slice(line, 25, 28), -999)

        ! optional
        r%wind      = to_int(safe_slice(line, 30, 32), -999)
        r%dir50     = to_int(safe_slice(line, 34, 34), -999)
        r%long50    = to_int(safe_slice(line, 35, 38), -999)
        r%short50   = to_int(safe_slice(line, 40, 43), -999)
        r%dir30     = to_int(safe_slice(line, 45, 45), -999)
        r%long30    = to_int(safe_slice(line, 46, 49), -999)
        r%short30   = to_int(safe_slice(line, 51, 54), -999)
        r%landfall  = safe_slice(line, 64, 64)
    end subroutine read_bst_data

    subroutine intp_1h()



    end subroutine intp_1h

end module TD_bst
