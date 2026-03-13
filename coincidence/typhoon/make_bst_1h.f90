program make_bst_1h
    use inout
    use TD_bst
    use time_tools, only: add_hours_ymdh, diff_hours_ymdh
    implicit none

    integer :: bst_data, data_out, ios, i, dt, dh
    character(len=256) :: line, saved_header
    logical :: has_saved_header
    character(len=8) :: ymdh_out
    type(bst_header_type) :: h
    type(bst_record_type) :: r0, r1, rint

    has_saved_header = .false.
    saved_header = ''

    call open_txt('fort.300', bst_data, ios) !data_name, record_number, ios
    if (ios /= 0) stop 'open error: fort.300 bst_data'

    open(newunit=data_out, file='bst_1h.txt', status='replace', action='write', iostat = ios)
    if (ios /= 0) stop 'open error: bst_1h.txt'

    !search header
    do
        if (has_saved_header) then
            line = saved_header
            has_saved_header = .false.
        else

            do
                call read_line(bst_data, line, ios)
                if (ios /= 0) exit
                if (len_trim(line) == 0) cycle

                if (safe_slice(line,1,5) == '66666') exit

            enddo

        endif

        if (ios /= 0) exit
        if (safe_slice(line,1,5) /= '66666') cycle

        !read header
        call read_bst_header(line, h)

        !output header
        write(data_out, '(A)') trim(line)

        !data only 0 or 1
!        if (h%nline <= 0) then
!            write(data_out,'(A)') ''
!            cycle
!        end if

        !read data sec
        call read_line(bst_data, line, ios)
        if (ios /= 0) exit

        if (safe_slice(line,1,5) == '66666') then
            saved_header = line
            has_saved_header = .true.
            write(data_out, '(A)') ''
            cycle
        endif

        if (.not. is_bst_data_line(line)) then
            print *, 'skip invalid data line = [', trim(line), ']'
            cycle
        end if

        call read_bst_data(line, r0)

!        if (h%nline == 1) then
!            write(data_out,'(A,1X,F8.3,1X,F9.3,1X,I5)') trim(r0%ymdh), r0%lat, r0%lon, r0%pres
!            write(data_out,'(A)') ''
!            cycle
!        endif

        !interp
        do i = 2, h%nline
            call read_line(bst_data, line, ios)
            if (ios /= 0) exit

            if (safe_slice(line,1,5) == '66666') then
                saved_header = line
                has_saved_header = .true.
                exit
            end if

            if (.not. is_bst_data_line(line)) then
                print *, 'skip invalid data line = [', trim(line), ']'
                cycle
            end if

            call read_bst_data(line, r1)
            
            !avoid loop bug 3h int
            call diff_hours_ymdh(r0%ymdh, r1%ymdh, dh)

            if (dh <= 0) cycle

            do dt = 0, dh-1

!            do dt = 0, 5
                call interp_bst_1h(r0, r1, dt, dh, rint)
                call add_hours_ymdh(r0%ymdh, dt, ymdh_out)
                rint%ymdh = ymdh_out

                write(data_out,'(A,1X,F8.3,1X,F9.3,1X,I5)') trim(rint%ymdh), rint%lat, rint%lon, rint%pres
            enddo

            r0 = r1

        enddo

        !last 6h
        write(data_out,'(A,1X,F8.3,1X,F9.3,1X,I5)') trim(r0%ymdh), r0%lat, r0%lon, r0%pres

        !insert brank space
        write(data_out, '(A)') ''
    enddo

    close(bst_data)
    close(data_out)

    print *, 'output: bst_1h.txt'
end program make_bst_1h


