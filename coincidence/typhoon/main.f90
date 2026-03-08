    program main
        use inout, only: readcsv
        implicit none

        !read csv
        !HRA_info file
        call readcsv('fort.100')

        end program main

