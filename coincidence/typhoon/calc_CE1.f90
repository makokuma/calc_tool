module calc_CE1
    use calc_distance, only: find_min_bst1h_distance_at_time
    implicit none
    private
    public :: calc_min_ty_for_hra, judge_CE1

contains

    subroutine calc_min_ty_for_hra(bstfile, nt, hra_time, hra_lat, hra_lon, &
                                   found_any, min_dist, best_time, best_tylat, best_tylon, best_pres)
        character(len=*), intent(in) :: bstfile
        integer, intent(in) :: nt
        character(len=*), intent(in) :: hra_time(nt)
        real, intent(in) :: hra_lat(nt), hra_lon(nt)

        logical, intent(out) :: found_any
        real, intent(out) :: min_dist
        character(len=*), intent(out) :: best_time
        real, intent(out) :: best_tylat, best_tylon
        integer, intent(out) :: best_pres

        integer :: i
        logical :: found
        real :: dist, ty_lat, ty_lon
        integer :: ty_pres

        found_any = .false.
        min_dist = 1.0e30
        best_time = ''
        best_tylat = -999.0
        best_tylon = -999.0
        best_pres = -999

        do i = 1, nt
            call find_min_bst1h_distance_at_time(bstfile, hra_time(i), hra_lat(i), hra_lon(i), &
                                                 found, dist, ty_lat, ty_lon, ty_pres)
            if (.not. found) cycle

            if (dist < min_dist) then
                found_any = .true.
                min_dist = dist
                best_time = hra_time(i)
                best_tylat = ty_lat
                best_tylon = ty_lon
                best_pres = ty_pres
            end if
        end do
    end subroutine calc_min_ty_for_hra


    subroutine judge_CE1(bstfile, nt, hra_time, hra_lat, hra_lon, &
                         ce1_flag, min_dist, best_time, best_tylat, best_tylon, best_pres)
        character(len=*), intent(in) :: bstfile
        integer, intent(in) :: nt
        character(len=*), intent(in) :: hra_time(nt)
        real, intent(in) :: hra_lat(nt), hra_lon(nt)

        integer, intent(out) :: ce1_flag
        real, intent(out) :: min_dist
        character(len=*), intent(out) :: best_time
        real, intent(out) :: best_tylat, best_tylon
        integer, intent(out) :: best_pres

        logical :: found_any

        call calc_min_ty_for_hra(bstfile, nt, hra_time, hra_lat, hra_lon, &
                                 found_any, min_dist, best_time, best_tylat, best_tylon, best_pres)

        if (.not. found_any) then
            ce1_flag = -1
            return
        end if

        !500km min
        if (min_dist <= 500.0) then
            ce1_flag = 1
        else
            ce1_flag = 0
        end if
    end subroutine judge_CE1

end module calc_CE1
