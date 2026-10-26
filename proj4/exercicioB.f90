program exercicioB
    implicit none
    ! tem que considerar o raio da bola na trave(infinitesimal)

    real(8), parameter :: g = 9.81d0, v0 = 360.0d0, deltat = 0.01d0, mb = 1.0d0
    real(8), parameter :: beta1 = 0.0d0, beta2 = 0.0005d0, beta3 = 0.001d0
    real(8) :: x, vx, y, vy, z, vz, vi, t, w, F, theta, phi, gamma_mb
    integer :: unidade_out1, unidade_out2, unidade_out3

    unidade_out1 = 10
    unidade_out2 = 11
    unidade_out3 = 12
    open(unit=unidade_out1, file="trajetoriaB1.dat", status="replace")
    open(unit=unidade_out2, file="trajetoriaB2.dat", status="replace")
    open(unit=unidade_out3, file="trajetoriaB3.dat", status="replace")

    theta = 65.0d0 * 3.14159265358979323846d0 / 180.0d0
    phi = 10.0d0 * 3.14159265358979323846d0 / 180.0d0

    w =  v0 * 0.1d0 / 2.0d0 * 0.13d0 * 0.13d0

    x = 0.0d0
    y = 0.0d0
    z = 0.0d0

    vx = abs(v0) * sin(theta) * cos(phi)
    vy = abs(v0) * sin(theta) * sin(phi)
    vz = abs(v0) * cos(theta)
    vi = sqrt(vx * vx + vy * vy + vz * vz)

    gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))

    t = 0.0d0
    do

        x = x + vx * deltat * t
        y = y + vy * deltat * t
        z = z + vz * deltat * t

        write(unidade_out1, *) x, y

        if ((x > 40.0d0) .or. (z < 0.0d0))then
            exit
        end if

        vx = vx - (gamma_mb * vi * vx + beta1 * w * vy) * deltat * t
        vy = vy - (gamma_mb * vi * vy + beta1 * w * vx) * deltat * t
        vz = vz - (g + gamma_mb * vi * vz) * deltat * t

        vi = sqrt(vx * vx + vy * vy + vz * vz)
        gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))

        t = t + 1.0d0

    end do

    x = 0.0d0
    y = 0.0d0
    z = 0.0d0

    vx = abs(v0) * sin(theta) * cos(phi)
    vy = abs(v0) * sin(theta) * sin(phi)
    vz = abs(v0) * cos(theta)
    vi = sqrt(vx * vx + vy * vy + vz * vz)

    gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))

    t = 0.0d0
    do

        x = x + vx * deltat * t
        y = y + vy * deltat * t
        z = z + vz * deltat * t

        write(unidade_out2, *) x, y

        if ((x > 40.0d0) .or. (z < 0.0d0))then
            exit
        end if

        vx = vx - (gamma_mb * vi * vx + beta2 * w * vy) * deltat * t
        vy = vy - (gamma_mb * vi * vy + beta2 * w * vx) * deltat * t
        vz = vz - (g + gamma_mb * vi * vz) * deltat * t

        vi = sqrt(vx * vx + vy * vy + vz * vz)
        gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))


        t = t + 1.0d0

    end do

    x = 0.0d0
    y = 0.0d0
    z = 0.0d0

    vx = abs(v0) * sin(theta) * cos(phi)
    vy = abs(v0) * sin(theta) * sin(phi)
    vz = abs(v0) * cos(theta)
    vi = sqrt(vx * vx + vy * vy + vz * vz)

    gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))

    t = 0.0d0
    do

        x = x + vx * deltat * t
        y = y + vy * deltat * t
        z = z + vz * deltat * t

        write(unidade_out3, *) x, y

        if ((x > 40.0d0) .or. (z < 0.0d0))then
            exit
        end if

        vx = vx - (gamma_mb * vi * vx + beta3 * w * vy) * deltat * t
        vy = vy - (gamma_mb * vi * vy + beta3 * w * vx) * deltat * t
        vz = vz - (g + gamma_mb * vi * vz) * deltat * t

        vi = sqrt(vx * vx + vy * vy + vz * vz)
        gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((vi-35.0d0)/5.0d0))

        t = t + 1.0d0

    end do

    if ((4.13d0 < y) .and. (z < 2.37d0) .and. (y < 9.87d0)) then
        print *, "sim"
    else
        print *, "nao"
    end if
    print *, x, y, z

    close(unidade_out1)
    close(unidade_out2)
    close(unidade_out3)

end program exercicioB