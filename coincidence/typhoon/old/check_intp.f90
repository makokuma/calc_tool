program main
    use inout
    use TD_bst
    use time_tools, only: add_hours_ymdh
    implicit none

    integer :: unit, ios, i, dt
    character(len=256) :: line
    character(len=8) :: ymdh_out
    type(bst_header_type) :: h
    type(bst_record_type) :: r0, r1, rint

    call open_txt('fort.300', unit, ios)
    if (ios /= 0) stop 'open error: fort.300'

    !----------------------------------
    ! 最初の台風ヘッダを探す
    !----------------------------------
    do
        call read_line(unit, line, ios)
        if (ios /= 0) stop 'header not found'
        if (len_trim(line) == 0) cycle
        if (safe_slice(line,1,5) == '66666') exit
    end do

    call read_bst_header(line, h)

    print *, '======================================='
    print *, 'HEADER CHECK'
    print *, 'storm name = [', trim(h%name), ']'
    print *, 'intl_id    = [', trim(h%intl_id), ']'
    print *, 'tc_id      = [', trim(h%tc_id), ']'
    print *, 'nline      = ', h%nline
    print *, 'revision   = [', trim(h%revision_date), ']'
    print *, '======================================='

    if (h%nline < 2) then
        print *, 'Not enough records for interpolation.'
        close(unit)
        stop
    end if

    !----------------------------------
    ! 最初のデータを読む
    !----------------------------------
    call read_line(unit, line, ios)
    if (ios /= 0) stop 'read error: first bst record'
    call read_bst_data(line, r0)

    print *, 'FIRST ORIGINAL RECORD'
    print *, 'time = ', trim(r0%ymdh), &
             ' grade = ', r0%grade, &
             ' lat = ', r0%lat, &
             ' lon = ', r0%lon, &
             ' p = ', r0%pres

    !----------------------------------
    ! 2行目以降との間を補間
    !----------------------------------
    do i = 2, h%nline
        call read_line(unit, line, ios)
        if (ios /= 0) exit

        call read_bst_data(line, r1)

        print *, '---------------------------------------'
        print *, 'ORIGINAL PAIR'
        print *, 'r0: ', trim(r0%ymdh), r0%lat, r0%lon, r0%pres
        print *, 'r1: ', trim(r1%ymdh), r1%lat, r1%lon, r1%pres

        print *, 'INTERPOLATED 1-HOURLY RECORDS'
        do dt = 0, 6
            call interp_bst_1h(r0, r1, dt, rint)
            call add_hours_ymdh(r0%ymdh, dt, ymdh_out)
            rint%ymdh = ymdh_out

            print *, 'dt = ', dt, &
                     ' time = ', trim(rint%ymdh), &
                     ' lat = ', rint%lat, &
                     ' lon = ', rint%lon, &
                     ' p = ', rint%pres
        end do

        ! 次の区間へ
        r0 = r1

        ! テスト用: 最初の2区間だけ見たいならここで止める
        if (i >= 3) exit
    end do

    close(unit)

end program main
