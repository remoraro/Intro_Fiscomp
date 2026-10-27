set terminal pdfcairo enhanced color size 10,8
set output "graf.pdf"

set title "Trajetória da bola"
set grid
set key top left
set xlabel "X (metros)"
set xrange [0:40]
set ylabel "Y (metros)"

plot "trajetoriaB1.dat" using 1:2 with lines lw 2 title "Simulação B1: {/Symbol b}_0/m_b = 0", \
     "trajetoriaB2.dat" using 1:2 with lines lw 2 title "Simulação B2: {/Symbol b}_0/m_b = 5 x 10^{-4}", \
     "trajetoriaB3.dat" using 1:2 with lines lw 2 title "Simulação B3: {/Symbol b}_0/m_b = 10^{-3}"
