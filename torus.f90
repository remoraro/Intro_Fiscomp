program torus

    implicit none
    integer :: r1
    integer :: r2
    real :: r_int
    real :: r_ext
    real :: area
    real :: volume
    real, parameter :: pi = 3.1415927


    print *, 'enter the r1 (intern) of the torus: '
    read(*,*) r1
    print *, 'enter the r2 (outer) of the torus: '
    read(*,*) r2
    r_int = (r2 - r1) / 2.0
    r_ext = (r2 + r1) / 2.0
    area = 4.0 * pi * pi * r_int * r_ext
    volume = 2.0 * pi * pi * r_int * r_int * r_ext

    print *, 'the area is: ', area
    print *, 'the volume is: ', volume

end program torus