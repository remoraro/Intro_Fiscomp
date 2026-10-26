set terminal pdfcairo enhanced color size 10,8
set output "graf.pdf"

set multiplot layout 2,1 title "Trajetória da bola"
set grid
set xlabel "X (metros)"
set ylabel "Y (metros)"


plot "trajetoriaB1.dat" using 1:2 with lines title "Trajetória B1", \
     "trajetoriaB2.dat" using 1:2 with lines title "Trajetória B2", \
     "trajetoriaB3.dat" using 1:2 with lines title "Trajetória B3"


unset multiplot
set output
