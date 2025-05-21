library(DiagrammeRsvg)
library(rsvg)

g <- grViz("
digraph flowchart {
  graph [layout = dot, rankdir = TB, bgcolor = transparent, splines = line]
  
  node [shape = box, style = filled, fontname = Helvetica, style = \"rounded,filled\", color = 'grey']
  edge [arrowhead = none, color = black, penwidth = 1.2]

  CCData       [label = 'Carbonate Chemistry Data', fillcolor = 'lightblue']
  Ocean        [label = 'Oceanographic Data', fillcolor = 'lightblue']
  Merged1      [label = 'Merged Data', fillcolor = 'lightyellow']
  ESPER        [label = 'ESPER', fillcolor = 'lightyellow']
  CO2SYS       [label = 'CO2SYS Derived Data', fillcolor = 'lightyellow']
  Merged2      [label = 'Merged Data', fillcolor = 'lightyellow']
  Bio          [label = 'Biovolume data', fillcolor = 'lightblue']
  GAM          [label = 'GAMs', fillcolor = 'lightpink']
  Station      [label = 'Station by Station Models', fillcolor = 'lightpink']
  Hierach      [label = 'Hierachecal Models', fillcolor = 'lightpink']
  
  CCData -> Merged1
  Ocean -> Merged1
  Merged1 -> ESPER
  Merged1 -> CO2SYS
  Bio -> Merged2
  CO2SYS -> Merged2
  Merged2 -> GAM
  CO2SYS -> Station
  CO2SYS -> Hierach
  
}
")

rsvg_png(charToRaw(export_svg(g)), "images/flowchart.png", width = 2400, height = 1800)

