program exercicioA
    implicit none
    ! tem que considerar o raio da bola na trave(infinitesimal)

    real(8), parameter :: g = 9.8d0, v0 = 100.0d0 / 3.6d0, deltat = 0.01d0, mb = 1.0d0
    real(8) :: beta_mb, theta, phi, w, gamma_mb
    real(8) :: x, vx, vxi, y, vy, vyi, z, vz, vzi, v, t
    integer :: unidade_out
    character(16) :: nome_saida

    read(*,*) beta_mb
    read(*,*) theta
    read(*,*) phi
    read(*,*) nome_saida

    unidade_out = 10
    open(unit=unidade_out, file=nome_saida, status="replace")

    theta = theta * 3.14159265358979323846d0 / 180.0d0
    phi = phi * 3.14159265358979323846d0 / 180.0d0

    w =  3.0d0 * v0 * 0.1d0  / (2.0d0 * 0.13d0 * 0.13d0)

    x = 0.0d0
    y = 0.0d0
    z = 0.0d0

    vx = abs(v0) * sin(theta) * cos(phi)
    vy = abs(v0) * sin(theta) * sin(phi)
    vz = abs(v0) * cos(theta)

    t = 0.0d0
    do

        write(unidade_out, *) x, y, t

        if ((x > 40.0d0) .or. (z < 0.0d0))then
            exit
        end if

        v = sqrt(vx * vx + vy * vy + vz * vz)
        gamma_mb = 0.0039d0 + 0.0058d0 / (1.0d0 + exp((v-35.0d0)/5.0d0))

        x = x + vx * deltat
        y = y + vy * deltat
        z = z + vz * deltat

        vxi = vx - (gamma_mb * v * vx + beta_mb * w * vy) * deltat
        vyi = vy - (gamma_mb * v * vy - beta_mb * w * vx) * deltat
        vzi = vz - (g + gamma_mb * v * vz) * deltat

        t = t + deltat
        vx = vxi
        vy = vyi
        vz = vzi

    end do

    if ((z < 2.37d0) .and. (z > 0.13d0) .and. (y < 9.87d0) .and. (4.13d0 < y)) then
        write(*,'(A)') "sim"
    else
        write(*,'(A)') "nao"
    end if

    close(unidade_out)

end program exercicioA