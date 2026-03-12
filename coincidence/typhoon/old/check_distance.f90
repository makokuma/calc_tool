program check_distance
    use calc_distance
    implicit none

    real :: dist, dx, dy

    dist = calc_dist_km(31.95, 147.50, 32.00, 147.56)
    print *, 'dist = ', dist, ' km'

    call calc_dxdy_km(31.95, 147.50, 32.00, 147.56, dx, dy)
    print *, 'dx = ', dx, ' km'
    print *, 'dy = ', dy, ' km'
end program check_distance
