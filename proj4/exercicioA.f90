program exercicioA
    implicit none
    ! tem que considerar o raio da bola na trave(infinitesimal)

    real(8), parameter :: g = 9.81, v0 = 360, deltat = 0.01, mb = 1.0
    real(8) :: beta_mb, theta, phi, F, w, gamma_mb
    real(8) :: x, vx, y, vy, z, vz, vi, t
    integer :: unidade_out

    unidade_out = 10
    open(unit=unidade_out, file="chute_out.dat", status="replace")

    read(*,*) beta_mb
    read(*,*) theta
    read(*,*) phi

    theta = theta * 3.14159265358979323846d0 / 180.0d0
    phi = phi * 3.14159265358979323846d0 / 180.0d0

    F = mb * v0 / deltat

    w = F * 0.1d0 * deltat / 2.0d0 * mb * 0.13d0 * 0.13d0

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

        write(unidade_out, *) x, y, t

        if ((x > 40.0d0) .or. (z < 0.0d0))then
            exit
        end if

        vx = vx - (gamma_mb * vi * vx + beta_mb * w * vy) * deltat * t
        vy = vy - (gamma_mb * vi * vy + beta_mb * w * vx) * deltat * t
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

    close(unidade_out)

end program exercicioA