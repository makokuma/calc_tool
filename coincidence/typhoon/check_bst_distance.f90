program check_bst_distance
    use calc_distance
    implicit none

    logical :: found
    real :: min_dist, ty_lat, ty_lon
    integer :: ty_pres

    call find_min_bst1h_distance_at_time('bst_1h_new.txt', '51021915', 31.95, 147.50, &
                                         found, min_dist, ty_lat, ty_lon, ty_pres)

    if (.not. found) then
        print *, 'not found'
        stop
    end if

    print *, 'best ty lat/lon/p = ', ty_lat, ty_lon, ty_pres
    print *, 'min_dist = ', min_dist, ' km'
end program check_bst_distance
