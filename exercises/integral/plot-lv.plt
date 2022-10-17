# plot.plt
set term png
set output "x-and-y-lotka-volterra.png"
set title "Lotka-volterra"
set grid
set xlabel "t"
set ylabel "x"
plot "int3-lv.txt" using 1:2 with lines title "x(t)", "int3-lv.txt" using 1:3 with lines title "y(t)"
