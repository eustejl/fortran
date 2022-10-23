# plot_cdf.plt
set term png
set output "cdf.png"
set title "CDF"
set grid
plot "pdf1-cdf.txt" using 1:2 with dots title("cdf")
