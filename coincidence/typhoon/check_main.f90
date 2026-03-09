program main
    use inout 
    use TD_bst
    implicit none

    integer :: unit, ios, i
    character(len=256) :: line
    type(bst_header_type) :: h
    type(bst_record_type) :: r

    call open_txt('fort.300', unit, ios)
    if (ios /= 0) stop 'open error: fort.300'

    do
        call read_line(unit, line, ios)
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle

        if (safe_slice(line,1,5) /= '66666') then
            print *, 'unexpected non-header line = [', trim(line), ']'
            cycle
        endif

        ! 1台風分のヘッダ
        call read_bst_header(line, h)

        print *, '======================================='
        if (len_trim(h%name) == 0) then
            print *, 'storm name = [UNKNOWN]'
        else
            print *, 'storm name = [', trim(h%name), ']'
        endif
        print *, 'intl_id    = [', trim(h%intl_id), ']'
        print *, 'tc_id      = [', trim(h%tc_id), ']'
        print *, 'nline      = ', h%nline
        print *, 'revision   = [', trim(h%revision_date), ']'
        
        ! 続く nline 行がデータ
        do i = 1, h%nline
            call read_line(unit, line, ios)
            if (ios /= 0) exit

            call read_bst_data(line, r)

            print *, 'time = ', trim(r%ymdh), &
                     ' grade = ', r%grade, &
                     ' lat = ', r%lat, &
                     ' lon = ', r%lon, &
                     ' p = ', r%pres
        end do
    end do

    close(unit)
end program main
