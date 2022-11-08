# plot_cdf.plt
set term png
set output "cdf.png"
set title "CDF"
set grid
plot "pdf1-cdf.txt" using 1:2 with lines title("cdf")

set term png
set output "hist.png"
set title "Histogram"
set grid
plot "pdf1-hist.txt" using 1:2 with boxes title("hist"),"pdf1-kde.txt" using 1:2 with lines title("kde")
#plot "pdf1-kde.txt" using 1:2 with lines title("kde")
