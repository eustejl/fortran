# plot.plt
set term png
set output "x-vs-t-euler.png"
set title "Euler method"
set grid
set xlabel "t"
set ylabel "x"
plot "int3-euler.txt" using 1:2 with lines title "Euler", "int3-euler.txt" using 1:3 with lines title "expected (x=e^t)"

set term png
set output "x-vs-t-midpoint.png"
set title "Midpoint Euler method"
set grid
set xlabel "t"
set ylabel "x"
plot "int3-midpoint.txt" using 1:2 with lines title "Midpoint", "int3-midpoint.txt" using 1:3 with lines title "expected (x=e^t)"
