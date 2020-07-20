set terminal pngcairo size 1200,780
set output "NZL_forecast.png"
set datafile separator ","
set xdata time
set timefmt '%Y-%m-%d'
set format x "%d-%m"
set grid
set key off
set xlabel "Date"
set ylabel "Cases, cumulative"
set yrange [0:       1200]
set xrange ["2020-02-23":"2020-10-01"]
set arrow from "2020-04-09",0 to "2020-04-09",  956 nohead lw 3 lc rgb 'dark-turquoise'
set label "New Zealand" at "2020-02-25",       1080 left
set label "Model parameters: a1: 3.000E-1 b1: 1.125E+0 b2: 1.350E+0 k1: 6.250E-1 k2: 5.125E+0" at "2020-02-25",       1020 left
set label "Cumulative cases:  1126. Bend at 2020-04-09" at "2020-02-25",        960 left
set label "(C) XR Pharmaceuticals Ltd. xph.co.nz, 2020" at "2020-02-25",         -64 left
plot 'NZL_forecast.csv' every ::59::170 using 1:4 with points pt 7 lw 3 lc rgb 'brown',\
'NZL_forecast.csv' every ::59 using 1:9 with lines dt 2 lw 2 lc rgb 'blue',\
'NZL_forecast.csv' every ::162::170 using 1:4 with points pt 7 lc rgb 'red'
