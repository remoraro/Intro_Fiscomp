program exercicio1
    implicit none

    integer :: i, num_h
    integer :: unidade_in, unidade_out

    real(8) :: f_linha_frente, f_linha_tras, f_linha_sim_3, f_linha_sim_5
    real(8) :: f_linhalinha_5, f_linhalinhalinha_5
    real(8) :: h, fmenos2, fmenos1, f0, f1, f2
    real(8) :: exata1, exata2, exata3
    real(8) :: erro_frente, erro_tras, erro_sim3, erro_sim5
    real(8) :: erro_linhalinha_sim5, erro_linhalinhalinha_sim5
    real(8) :: menor_erro_frente, menor_erro_tras, menor_erro_sim3
    real(8) :: menor_erro_sim5, menor2_erro_sim5, menor3_erro_sim5
    real(8) :: h_frente, h_tras, h_sim3, h_sim5, h2_sim5, h3_sim5
    real(8), allocatable :: hs(:)
    real(8), parameter :: x = 3.0d0 ** (-1)


    exata1 = exp(4.0*x) * (4.0*cos(x/2.0) - 0.5*sin(x/2.0))
    exata2 = exp(4.0*x) * (15.75*cos(x/2.0) - 4.0*sin(x/2.0))
    exata3 = exp(4.0*x) * (61.0*cos(x/2.0) - 23.875*sin(x/2.0))
    !print *, exata1
    !print *, exp(4.0d0*x) * (4.0d0*cos(x/2.0d0) - 0.5d0*sin(x/2.0d0))
    !print *, x
    !print *, 1/3d0


    unidade_in = 10
    unidade_out = 11

    open(unit=unidade_in, file="tabela1_in.dat", status="old")
    open(unit=unidade_out, file="tabela1_out.dat", status="replace")
    read(unidade_in,*) num_h
    allocate(hs(num_h))
    read(unidade_in,*) (hs(i), i=1,num_h)


    write(unidade_out,'(A)') "h | derivada simétrica 3p | derivada pra trás 2p |& 
    & derivada pra frente 2p | derivada simétrica 5p | derivada &
    & segunda simétrica 5p | derivada terceira anti-simétrica 5p"

    menor_erro_frente = 100000000
    menor_erro_tras   = 100000000
    menor_erro_sim3   = 100000000
    menor_erro_sim5   = 100000000
    menor2_erro_sim5  = 100000000
    menor3_erro_sim5  = 100000000

    do i = 1, num_h

        h = hs(i)
        fmenos2 = exp(4.0*(x - 2.0*h)) * cos((x - 2.0*h)/2.0)
        fmenos1 = exp(4.0*(x - h)) * cos((x - h)/2.0)
        f0 = exp(4.0*x) * cos(x/2.0)
        f1 = exp(4.0*(x + h)) * cos((x + h)/2.0)
        f2 = exp(4.0*(x + 2.0*h)) * cos((x + 2.0*h)/2.0)

        f_linha_frente = (f1 - f0) / h
        f_linha_tras = (f0 - fmenos1) / h
        f_linha_sim_3 = (f1 - fmenos1) / (2.0*h)
        f_linha_sim_5 = (-f2 + 8.0*f1 - 8.0*fmenos1 + fmenos2) / (12.0*h)
        f_linhalinha_5 = (-f2 + 16.0*f1 - 30.0*f0 + 16.0*fmenos1 - fmenos2) / (12.0*h**2)
        f_linhalinhalinha_5 = (f2 - 2.0*f1 + 2.0*fmenos1 - fmenos2) / (2.0*h**3)


        erro_frente = f_linha_frente - exata1
        erro_tras   = f_linha_tras - exata1
        erro_sim3   = f_linha_sim_3 - exata1
        erro_sim5   = f_linha_sim_5 - exata1
        erro_linhalinha_sim5 = f_linhalinha_5 - exata2
        erro_linhalinhalinha_sim5 = f_linhalinhalinha_5 - exata3

        write(unidade_out,'(7ES25.17)') h, erro_sim3, erro_tras, erro_frente, &
        & erro_sim5, erro_linhalinha_sim5, erro_linhalinhalinha_sim5

        if (abs(erro_frente) < abs(menor_erro_frente)) then
            menor_erro_frente = erro_frente
            h_frente = h
        end if
        if (abs(erro_tras) < abs(menor_erro_tras)) then
            menor_erro_tras = erro_tras
            h_tras = h
        end if
        if (abs(erro_sim3) < abs(menor_erro_sim3)) then
            menor_erro_sim3 = erro_sim3
            h_sim3 = h
        end if
        if (abs(erro_sim5) < abs(menor_erro_sim5)) then
            menor_erro_sim5 = erro_sim5
            h_sim5 = h
        end if
        if (abs(erro_linhalinha_sim5) < abs(menor2_erro_sim5)) then
            menor2_erro_sim5 = erro_linhalinha_sim5
            h2_sim5 = h
        end if
        if (abs(erro_linhalinhalinha_sim5) < abs(menor3_erro_sim5)) then
            menor3_erro_sim5 = erro_linhalinhalinha_sim5
            h3_sim5 = h
        end if

    end do

    close(unidade_in)
    close(unidade_out)

    print *, "A justificativa da melhor escolha de h para cada caso é o menor erro possível"
    print *, "                                        Melhores valores de h     Menor erro"
    print *, "Derivada pra frente 2p:              ", h_frente, menor_erro_frente
    print *, "Derivada pra trás 2p:               ", h_tras, menor_erro_tras
    print *, "Derivada simétrica 3p:              ", h_sim3, menor_erro_sim3
    print *, "Derivada simétrica 5p:              ", h_sim5, menor_erro_sim5
    print *, "Derivada  segunda simétrica 5p:     ", h2_sim5, menor2_erro_sim5
    print *, "Derivada terceira anti-simétrica 5p:", h3_sim5, menor3_erro_sim5

end program exercicio1