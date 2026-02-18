      module readdata
        implicit none
      contains

        subroutine read_namelist(nmlfile_name)
          !read *.nml file and show output
          character(len=*), intent(in) :: nmlfile_name
          integer :: nvars
          character(len=10) :: start_date, end_date,ymdh
          character(len=200) :: input_dir, file_prefix, file_suffix,&
                                &datfile,output_dir
          character(len=20) :: varname, vars_all(20)
          character(len=20), allocatable :: vars(:)
          character(len=50) :: input_meta
          character(len=4) :: YYYY 
          integer:: dt_hours, nmlfile, i, z_start,z_end,log_level
          logical:: usevar(20),do_time_mean, do_interp

          namelist /io/ input_dir, input_meta, output_dir, file_prefix,&
                     &file_suffix
          namelist /select/ vars_all,nvars, z_start, z_end, start_date,&
                     end_date,&
                     &dt_hours
          namelist /process/ do_time_mean, do_interp, log_level
        
        !open *.nml file get nmlfile_name from main program
!        file = trim(nmlfile_name)
          open(newunit=nmlfile, file=trim(nmlfile_name))

          !read namelist
          read(nmlfile, nml=io)
          read(nmlfile, nml=select)

!          print *, "input_dir=[", trim(input_dir), "]"
!          print *, "file_prefix=[", trim(file_prefix), "]"
!          print *, "file_suffix=[", trim(file_suffix), "]"

!          print *, "nvars(nml)=", nvars
!          do i=1,20; print *, i, "[", trim(vars_all(i)), "]"; end do

        !use datetime??? --> use other subroutine
        ymdh = start_date 

        !for small test
          YYYY = start_date(1:4)
          print *, YYYY

          !select vars
!          do i=1,20
!              usevar(i) = (len_trim(vars_all(i)) > 0)
!          end do
!          nvars = count(usevar)
!          allocate(vars(nvars))
!          vars = pack(vars_all, usevar)

          do i=1,nvars
            varname = vars_all(i)
!            print *, "varname=[", trim(varname), "]"
            !set path to data file
            datfile = trim(input_dir)//"/"//trim(YYYY)//"/"&
                    &//trim(varname)//&
                    &"/"//trim(file_prefix)//trim(varname)//&
                    &"_"//trim(ymdh)//&
                    &trim(file_suffix)

!            print *, varname
            print '(A)', trim(datfile)
!            print '(A)', "datfile=["//trim(datfile)//"]"

          end do

        end subroutine read_namelist


        subroutine get_datainfo(metafile_name)
          character(len=*), intent(in) :: metafile_name
          integer :: metafile
          
          ! get datainfo from meta file
          open(newunit=metafile, file=trim(metafile_name))

          !use read_meta
          

        end subroutine get_datainfo

        subroutine read_meta
        !subroutine for reading meta format file

        end subroutine read_meta

      end module readdata

