program exerA
    implicit none

    real(8), parameter :: G_Ms = 4.0d0 * (3.14159265358979323846d0 ** 2.0d0)
    real(8) :: M1, m2, x0, x, xi, y0, y, yi, t, v0x
    real(8) :: r, v0y, deltat
    integer :: unidade1_out, unidade2_out

    read(*,*) r
    read(*,*) v0y
    read(*,*) deltat

    x0 = r
    y0 = 0.0d0
    v0x = 0.0d0
    t = 0.0d0
    do
        yi = y0 + v0y * deltat * t
        xi = x0 + v0x * deltat * t
        r = sqrt(xi**2 + yi**2)

        y = 2.0d0 * yi - y0 - (G_Ms * yi / r**3.0d0) * (deltat*t)**2.0d0
        x = 2.0d0 * xi - x0 - (G_Ms * xi / r**3.0d0) * (deltat*t)**2.0d0

        y0 = yi
        yi = y
        
        print*, t, x, y

        t = t + 1.0d0
    end do


    unidade1_out = 10
    open(unit=unidade1_out, file="trajA1_out.dat", status="replace")

end program