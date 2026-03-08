    module inout
        implicit none
    contains

        subroutine readcsv(csvfile_name, sel_col_name)
            !read csvfile (.dat)
            character(len=*), intent(in):: csvfile_name
            character(len=*), intent(in):: sel_col_name
            integer :: csv_unit, ios
            character(len=10000) :: line
            character(len=:), allocatable :: header(:), sel_col(:), fields(:)
 

            !read csv
            open(newunit=csv_unit, file=trim(csvfile_name))
            if (ios /= 0) stop 'open error'

            !read header
            read(csv_unit, '(A)', iostat=ios) line
            if (ios /= 0) then
                print *, 'header read error'
                stop
            endif

            call split_csv_line(trim(line), header)

            !header information
            sel_col = find_column(header, select_col_name)

            !read data
            nline = 0
            do
                read(csv_unit, '(A)', iostat=ios) line
                if (ios /= 0) exit

                call split_csv_line(trim(line), fields)
                
                if (sel_col > 0) then
                    if (trim(fields(sel_col)) /= trim(select_value)) cycle
                end if
!                nline = nline + 1

                 
!            do
!              read(csv_unit, '(A)', iostat=ios) line
!              if (ios /= 0) exit
!              print *, trim(line)
!            enddo

            enddo

            close(csv_unit)
        end subroutine readcsv

        subroutine split_csv_line(line, fields)
            character(len=*), intent(in) :: line
            character(len=:), allocatable, intent(out) :: fields(:)

            integer :: i, n, start, lenline, maxlen
            integer, allocatable :: pos(:)
            integer :: k

            lenline = len_trim(line)

            n = 1
            do i = 1, lenline
              if (line(i:i) == ',') n = n + 1
            enddo

            allocate(pos(n-1))
            k = 0
            do i = 1, lenline
              if (line(i:i) == ',') then
                  k = k + 1
                  pos(k) = i
              endif
            enddo

            maxlen = 1
            start = 1
            do i = 1, n-1
              maxlen = max(maxlen, pos(i) - start)
              start = pos(i) + 1
            enddo
            maxlen = max(maxlen, lenline - start + 1)

            allocate(character(len=maxlen) :: fields(n))
            fields = ''

            start = 1
            do i = 1, n-1
              if (pos(i) >= start) then
                fields(i) = adjustl(line(start:pos(i)-1))
              else
                  fields(i) = ''
              endif
              start = pos(i) + 1
            enddo

            if (start <= lenline) then
              fields(n) = adjustl(line(start:lenline))
            else
              fields(n) = ''
            endif
        end subroutine split_csv_line

        integer function find_column(header, name)
          character(len=*), intent(in) :: header(:)
          character(len=*), intent(in) :: name
          integer :: i

          find_column = -1
          do i = 1, size(header)
            if (trim(header(i)) == trim(name)) then
                find_column = i
                return
            endif
          enddo
        end function find_column
    
    end module inout
