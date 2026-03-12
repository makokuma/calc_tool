module calc_CE2
    use calc_distance, only: find_min_bst1h_distance_at_time
    use calc_CE1, only: calc_min_ty_for_hra
    implicit none
    private 
    public :: judge_CE2_case
contains

    subroutine judge_CE2_case(bstfile, nt, hra_time, hra_lat, hra_lon, rtmax, &
                         ce2_flag, min_dist, best_time, best_tylat, best_tylon, best_pres)
        character(len=*), intent(in) :: bstfile
        integer, intent(in) :: nt
        character(len=*), intent(in) :: hra_time(nt)
        real, intent(in) :: hra_lat(nt), hra_lon(nt)
        real, intent(in) :: rtmax

        integer, intent(out) :: ce2_flag
        real, intent(out) :: min_dist
        character(len=*), intent(out) :: best_time
        real, intent(out) :: best_tylat, best_tylon
        integer, intent(out) :: best_pres

        logical :: found_any
        logical :: cond_dist, cond_rt

        call calc_min_ty_for_hra(bstfile, nt, hra_time, hra_lat, hra_lon, &
                                 found_any, min_dist, best_time, best_tylat, best_tylon, best_pres)

        if (.not. found_any) then
            ce2_flag = -1
            return
        end if

        ! HK2022 CE2 condition
        !distance within 500km
        cond_dist = (min_dist <= 500.0)
        !rtmax below 300mm
        cond_rt   = (rtmax < 200.0)

        if (cond_dist .and. cond_rt ) then
            ce2_flag = 1
        else
            ce2_flag = 0
        end if
    end subroutine judge_CE2_case

end module calc_CE2
    
