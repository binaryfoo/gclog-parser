#!/usr/bin/env gnuplot

# output of ToTsv
input = 'example.tsv'
set datafile separator '\t'

set terminal svg enhanced mouse standalone size 1280,960
set output 'example.svg'

# to output png instead
#set terminal pngcairo size 2280,960
#set output 'example.png'

set xdata time
set timefmt '%Y-%m-%d %H:%M:%S'
#set format x '%Y-%m-%d %H:%M:%S'
set format x '%Y-%m-%d'
set lmargin 10
set multiplot layout 2,1

set title 'Pause time (s)'
plot for [i=2:2] input using 1:i w lines lt i t column(i)

set title 'Heap sizes'
set format y '%.1t %c'
plot for [i=3:5] input using 1:i w lines lt i t column(i)
