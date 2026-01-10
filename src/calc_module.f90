        module calc_PV
          implicit none
          integer, parameter :: nx=721, ny=577, nz=17, omega=7.2d5
          real :: lat(:),lon(:),theta_x(:,:,:),theta_y(:,:,:),&
                    & theta_z(:,:,:),f(:,:,:),lat(1,:,1),grad(:,:,:),&
                    & PV(:,:,:)
          lev(:) = (/1000, 975, 950, 925, 900, 850, 800, &
                    & 700, 600, 500, 400, 300, 250,&
                    & 200, 150, 100, 70/)
          lev(:) = lev(:)*100

          !for calc PV
          do i = 1, nx, 1
            do j = 1, ny, 1
                do k = 1, nz, 1

                  !calc absolute vorticity
                  f = 2*omega*sin(lat(j))
                  vorticity = calc_vorticity(u(i,j,k),v(i,j,k)) + f

                  !calc theta grad
                  theta_x = calc_theta(temp)(i+1,j,k)-&
                            & calc_theta(temp)(i,j,k)
                  theta_y = calc_theta(temp)(i,j+1,k)-&
                            & calc_theta(temp)(i,j,k)
                  theta_z = calc_theta(temp)(i,j,k+1)-&
                            & calc_theta(temp)(i,j,k)

                  grad = theta_x**2 + theta_y**2 + theta_z**2

                  !calc PV
                  PV = vorticity * grad/rho
                  
                enddo
            enddo
          enddo


          contains

            real function calc_theta(temp)
              implicit none
              integer :: lev
              integer, parameter :: p0=1000*100
              real :: temp(nx, ny, nz)
              real, parameter :: gamma=287/1004
              do i = 1, nx, 1
                do j = 1, ny, 1
                    do k = 1, nz, 1
                      theta = temp * (p0 / lev)**gamma
                    enddo
                enddo
              enddo

              return
              
            endfunction calc_theta theta

            real function calc_vorticity(u,v)
              implicit none
              integer ::  
              real :: u(nx, ny, nz), v(nx, ny, nz),diff_u(:,:,:),&
                        & diff_v(:,:,:),vorticity(:,:,:)

              do i = 1, nx, 1
                do j = 1, ny, 1
                    do k = 1, nz, 1
                        diff_u = u(i+1,j,k) - u(i,j,k)
                        diff_v = v(i,j+1,k) - v(i,j,k)
                        vorticity = diff_u - diff_v
                    enddo
                enddo
              enddo
              return

            endfunction calc_vorticity vorticity

            real function calc_rho(temp)
              implicit none
              integer, parameter :: Rd=287
              do i = 1,nx, 1
                do j = 1, ny, 1
                  do k = 1, nz, 1
                    rho = lev(k)/Rd*temp(:,:,:)
                  enddo
                enddo
              enddo

              return
            endfunction rho

                    
            



