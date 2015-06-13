set macro

#####  Color Palette by Color Scheme Designer
#####  Palette URL: http://colorschemedesigner.com/#3K40zsOsOK-K-

   blue_000 = "#A9BDE6" # = rgb(169,189,230)
   blue_025 = "#7297E6" # = rgb(114,151,230)
   blue_050 = "#1D4599" # = rgb(29,69,153)
   blue_075 = "#2F3F60" # = rgb(47,63,96)
   blue_100 = "#031A49" # = rgb(3,26,73)

   green_000 = "#A6EBB5" # = rgb(166,235,181)
   green_025 = "#67EB84" # = rgb(103,235,132)
   green_050 = "#11AD34" # = rgb(17,173,52)
   green_075 = "#2F6C3D" # = rgb(47,108,61)
   green_100 = "#025214" # = rgb(2,82,20)

   red_000 = "#F9B7B0" # = rgb(249,183,176)
   red_025 = "#F97A6D" # = rgb(249,122,109)
   red_050 = "#E62B17" # = rgb(230,43,23)
   red_075 = "#8F463F" # = rgb(143,70,63)
   red_100 = "#6D0D03" # = rgb(109,13,3)

   brown_000 = "#F9E0B0" # = rgb(249,224,176)
   brown_025 = "#F9C96D" # = rgb(249,201,109)
   brown_050 = "#E69F17" # = rgb(230,159,23)
   brown_075 = "#8F743F" # = rgb(143,116,63)
   brown_100 = "#6D4903" # = rgb(109,73,3)

   grid_color = "#d5e0c9"
   text_color = "#6a6a6a"

   my_font = "SVBasic Manual, 12"
   my_font_file = "~/local/share/fonts/defaults/LiberationMono-Regular.ttf"
   my_export_sz = "1024,768"

   my_line_width = "2"
   my_axis_width = "1.5"
   my_ps = "1.2"
   my_font_size = "14"

# must convert font fo svg and ps
#set term svg  size @my_export_sz fname my_font fsize my_font_size enhanced dynamic rounded
# set term png  size @my_export_sz large font my_font
# set term jpeg size @my_export_sz large font my_font
#set term wxt enhanced font my_font

set style data linespoints
set style function lines
set pointsize my_ps

set style line 1  linecolor rgbcolor blue_025  linewidth @my_line_width pt 7
set style line 2  linecolor rgbcolor green_025 linewidth @my_line_width pt 5
set style line 3  linecolor rgbcolor red_025   linewidth @my_line_width pt 9
set style line 4  linecolor rgbcolor brown_025 linewidth @my_line_width pt 13
set style line 5  linecolor rgbcolor blue_050  linewidth @my_line_width pt 11
set style line 6  linecolor rgbcolor green_050 linewidth @my_line_width pt 7
set style line 7  linecolor rgbcolor red_050   linewidth @my_line_width pt 5
set style line 8  linecolor rgbcolor brown_050 linewidth @my_line_width pt 9
set style line 9  linecolor rgbcolor blue_075  linewidth @my_line_width pt 13
set style line 10 linecolor rgbcolor green_075 linewidth @my_line_width pt 11
set style line 11 linecolor rgbcolor red_075   linewidth @my_line_width pt 7
set style line 12 linecolor rgbcolor brown_075 linewidth @my_line_width pt 5
set style line 13 linecolor rgbcolor blue_100  linewidth @my_line_width pt 9
set style line 14 linecolor rgbcolor green_100 linewidth @my_line_width pt 13
set style line 15 linecolor rgbcolor red_100   linewidth @my_line_width pt 11
set style line 16 linecolor rgbcolor brown_100 linewidth @my_line_width pt 7
set style line 17 linecolor rgbcolor "#224499" linewidth @my_line_width pt 5

#set style line 1  linecolor rgbcolor "#a0bae9" linewidth @my_line_width pt 7
#set style line 2  linecolor rgbcolor "#ff7f7f" linewidth @my_line_width pt 5
#set style line 3  linecolor rgbcolor "#80c65a" linewidth @my_line_width pt 9
#set style line 4  linecolor rgbcolor "#ffcc7f" linewidth @my_line_width pt 13
#set style line 5  linecolor rgbcolor "#dedc06" linewidth @my_line_width pt 11
#set style line 6  linecolor rgbcolor "#7711ff" linewidth @my_line_width
#set style line 7  linecolor rgbcolor "#ff0000" linewidth @my_line_width
#set style line 8  linecolor rgbcolor "#008000" linewidth @my_line_width
#set style line 9  linecolor rgbcolor "#ff9900" linewidth @my_line_width
#set style line 10 linecolor rgbcolor "#aa9900" linewidth @my_line_width
#set style line 11 linecolor rgbcolor "#990066" linewidth @my_line_width
#set style line 12 linecolor rgbcolor "#990000" linewidth @my_line_width
#set style line 13 linecolor rgbcolor "#003971" linewidth @my_line_width
#set style line 14 linecolor rgbcolor "#76a4fb" linewidth @my_line_width
#set style line 15 linecolor rgbcolor "#d5e0c9" linewidth @my_line_width
#set style line 16 linecolor rgbcolor "#e5ecf9" linewidth @my_line_width
#set style line 17 linecolor rgbcolor "#224499" linewidth @my_line_width

## plot 1,2,3,4,5,6,7,8,9
set style increment user
set style arrow 1 filled

## used for bar chart borders
set style fill solid 0.5

# Grey background
#set object 1 rectangle from screen 0, screen 0 to screen 1, screen 1 behind fc  rgbcolor "#cccccc"

set size noratio
set samples 300

set xtics textcolor rgb text_color font my_font
set ytics textcolor rgb text_color font my_font
set label textcolor rgb text_color font my_font

set border 31 lw @my_axis_width lc rgb text_color

set grid lc rgb grid_color

set key outside
set key center bottom
