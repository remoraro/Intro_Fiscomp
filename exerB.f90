program exerB
    implicit none

    integer :: n
    real(4) :: n1, ep_a1
    real(8) :: n2, ep_a2

    real(4) :: ep1, cosseno1, precisao1, vetor1(4)
    real(8) :: ep2, cosseno2, precisao2, vetor2(4)

    vetor1 = [0.1, 0.2, 0.3, 0.4]
    vetor2 = [0.1_8, 0.2_8, 0.3_8, 0.4_8]

    print *, "x      precisao simples     precisao dupla"

    do n = 1, 4

        cosseno1 = 1.0
        n1 = 2.0
        ep1 = -(vetor1(n)**2) / 2.0

        do while (cosseno1 /= cosseno1 + ep1)
            ep_a1 = ep1

            cosseno1 = cosseno1 + ep1 
                      
            n1 = n1 + 2.0
            ep1 = -ep1 * (vetor1(n)**2) / (n1 * (n1 - 1.0))

        end do

        precisao1 = abs(ep_a1 / cosseno1)

        cosseno2 = 1.0_8
        n2 = 2.0_8
        ep2 = -(vetor2(n)**2) / 2.0_8 

        do while (cosseno2 /= cosseno2 + ep2)
            ep_a2 = ep2 
            
            cosseno2 = cosseno2 + ep2 
            
            n2 = n2 + 2.0_8
            ep2 = -ep2 * (vetor2(n)**2) / (n2 * (n2 - 1.0_8))
        end do

        precisao2 = abs(ep_a2 / cosseno2)
        print '(F3.1, 2ES20.8)', vetor1(n), precisao1, precisao2

    end do

    print *, "Penso que a funcao cosseno deve ser aproximada por series devido&
    & a sua velocidade em calcular resultados extremamente precisos para x pequenos.&
    & Isso não significa que séries de Taylor sempre sejam o melhor caminho, &
    &especialmente para x maiores, que podem causar problemas com a fatorial e precisão"
end program exerB