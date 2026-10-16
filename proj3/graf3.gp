set terminal pdfcairo enhanced color size 10,8
set output "graf3.pdf"

set multiplot layout 2,1 title "Euler e Euler-Cromer"
set grid
set xlabel "Tempo (segundos)"

set ylabel "Theta (radianos)"
set xrange [0:10]

plot (10*pi/180.0)*cos(sqrt(9.8)*x) with lines lw 2 title "Solucao exata", \
     "exer3A_out.dat" using 1:2 with lines title "Euler", \
     "exer3B_out.dat" using 1:2 with lines title "Euler-Cromer"

set ylabel "Energia (J)"

plot "energia3A.dat" using 1:2 with lines title "Euler", \
     "energia3B.dat" using 1:2 with lines title "Euler-Cromer"

unset multiplot
set output
