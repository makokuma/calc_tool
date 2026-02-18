        program main
        use readdata, only: read_namelist
        implicit none
        character(len=512) :: nmlfile_name


        !get file informaition
        print *, "enter nml file name"
        read(*,'(A)') nmlfile_name
        call read_namelist(nmlfile_name)


        end program main
