set term png
set output "cdf.png"
set title "CDF"
set grid
plot "cumul.dat" using 1:2 with lines title("cdf")

set term png
set output "hist.png"
set title "Histogram"
set grid
plot "histogram.dat" using 1:2 with boxes title("hist")
