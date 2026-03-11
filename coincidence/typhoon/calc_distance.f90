module calc_distance
    implicit none
    private
    public :: calc_dist_km, calc_dxdy_km, wrap_lon_diff, find_bst1h_at_time, find_min_bst1h_distance_at_time
contains

    real function wrap_lon_diff(lon1, lon2)
        ! longitude difference adjusted to shortest path [-180, 180]
        real, intent(in) :: lon1, lon2
        real :: dlon

        dlon = lon2 - lon1

        if (dlon > 180.0) then
            dlon = dlon - 360.0
        elseif (dlon < -180.0) then
            dlon = dlon + 360.0
        end if

        wrap_lon_diff = dlon
    end function wrap_lon_diff


    subroutine calc_dxdy_km(lat1, lon1, lat2, lon2, dx, dy)
        ! approximate zonal/meridional distance [km]
        ! dx: east-west component
        ! dy: north-south component
        real, intent(in) :: lat1, lon1, lat2, lon2
        real, intent(out) :: dx, dy

        real :: dlat, dlon, mean_lat
        real, parameter :: pi = 3.14159265358979323846
        real, parameter :: re = 6371.0
        real, parameter :: deg2rad = pi / 180.0

        dlat = lat2 - lat1
        dlon = wrap_lon_diff(lon1, lon2)
        mean_lat = 0.5 * (lat1 + lat2)

        dy = re * deg2rad * dlat
        dx = re * cos(mean_lat * deg2rad) * deg2rad * dlon
    end subroutine calc_dxdy_km


    real function calc_dist_km(lat1, lon1, lat2, lon2)
        ! approximate distance [km]
        real, intent(in) :: lat1, lon1, lat2, lon2
        real :: dx, dy

        call calc_dxdy_km(lat1, lon1, lat2, lon2, dx, dy)
        calc_dist_km = sqrt(dx*dx + dy*dy)
    end function calc_dist_km

    subroutine find_bst1h_at_time(bstfile, target_time, found, lat, lon, pres)
        use inout, only: open_txt, read_line
        use TD_bst
        character(len=*), intent(in) :: bstfile
        character(len=*), intent(in) :: target_time
        logical, intent(out) :: found
        real, intent(out) :: lat, lon
        integer, intent(out) :: pres

        integer :: unit, ios
        character(len=256) :: line
        type(bst1h_record_type) :: rec

        found = .false.
        lat = -999.0
        lon = -999.0
        pres = -999

        call open_txt(bstfile, unit, ios)
        if (ios /= 0) return

        do
            call read_line(unit, line, ios)
            if (ios /= 0) exit

            call parse_bst1h_line(line, rec, ios)
            if (ios /= 0) cycle
            if (rec%is_header) cycle

            if (trim(rec%ymdh) == trim(target_time)) then
                found = .true.
                lat = rec%lat
                lon = rec%lon
                pres = rec%pres
                exit
            end if
        end do

        close(unit)
    end subroutine find_bst1h_at_time

    subroutine find_min_bst1h_distance_at_time(bstfile, target_time, hra_lat, hra_lon, &
                                           found, min_dist, best_lat, best_lon, best_pres)
        use inout, only: open_txt, read_line
        use TD_bst, only: bst1h_record_type, parse_bst1h_line
        character(len=*), intent(in) :: bstfile
        character(len=*), intent(in) :: target_time
        real, intent(in) :: hra_lat, hra_lon

        logical, intent(out) :: found
        real, intent(out) :: min_dist
        real, intent(out) :: best_lat, best_lon
        integer, intent(out) :: best_pres

        integer :: unit, ios
        character(len=256) :: line
        type(bst1h_record_type) :: rec
        real :: dist

        found = .false.
        min_dist = 1.0e30
        best_lat = -999.0
        best_lon = -999.0
        best_pres = -999

        call open_txt(bstfile, unit, ios)
        if (ios /= 0) return

        do
            call read_line(unit, line, ios)
            if (ios /= 0) exit

            call parse_bst1h_line(line, rec, ios)
            if (ios /= 0) cycle
            if (rec%is_header) cycle

            if (trim(rec%ymdh) /= trim(target_time)) cycle

            dist = calc_dist_km(hra_lat, hra_lon, rec%lat, rec%lon)

            if (dist < min_dist) then
                found = .true.
                min_dist = dist
                best_lat = rec%lat
                best_lon = rec%lon
                best_pres = rec%pres
            end if
        end do

        close(unit)
    end subroutine find_min_bst1h_distance_at_time
end module calc_distance
