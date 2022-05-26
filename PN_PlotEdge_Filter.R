PN_PlotEdge_Filter <- function(p, edge.from, edge.to, edge.color){
  
  # edge.from/to/color
  
  pEdge <- p +
    geom_conn_bundle(data = get_con(from = edge.from, to = edge.to, col = edge.color),
                     aes(colour = col),
                     width = 0.6, tension = 0.6, alpha = .7) +
    scale_edge_colour_manual(values = rbMixPalette2,name = "Entry_ID",drop=F)+
    guides(colour = guide_legend(order = 1, override.aes = list(size = 7)), 
           size = guide_legend(order = 2, nrow = 1, byrow = T),
           edge_color = guide_legend(order = 2, override.aes = list(edge_alpha = 1, edge_width = 3)))+
   theme(
      aspect.ratio = 1,
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      rect = element_rect(fill = "transparent"),
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.key = element_rect(fill = "transparent", colour = NA), # get rid of key legend fill, and of the surrounding
      legend.title=element_text(size=14), 
      legend.text=element_text(size=12)
    )  
  
  
  return(pEdge)
  
}