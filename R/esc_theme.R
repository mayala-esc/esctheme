
#' Create a ggplot2 theme consistent with the ESC style guide.
#' 
#' @import ggplot2
#' @import extrafont
#' 
#' @param scale Adjust the scale of the chart. Default is 0.8
#' 
#' @export

esc_theme <- function(scale = 0.8, rm_x_leg = FALSE, rm_y_leg = FALSE){
  
  loadfonts(device="win", quiet = TRUE)
  
  colorlist <- list(lt_gray = "#D9D9D9",
                    gray = "#D7D7D7",
                    dk_gray = "#595959",
                    lt_black = "#262626",
                    txt_gray = "#75787B")
  
  if (rm_x_leg == TRUE) {
    x_leg <- element_blank()
  }else{x_leg <- element_text(margin=unit(c(0,0,0,.15)*scale, "cm", colorlist$txt_gray), size = 20*scale)}
  
  if (rm_y_leg == TRUE) {
    y_leg <- element_blank()
  }else{y_leg <- element_text(margin=unit(c(0,0,0,.15)*scale, "cm", colorlist$txt_gray), size = 20*scale)}
  
  
  theme_minimal() %+replace%
    
    theme(
      text = element_text(
        face = "plain",
        colour = colorlist$txt_gray,
        family = "Arial"
      ),
    line = element_line(
        colour = colorlist$lt_gray, 
        linewidth = 0.5#1*scale
                        ),
    rect = element_rect(
        fill = NA,
        linetype = 0,
        colour = "white"
        ),#"NA"
    
    axis.title.y.left = y_leg,
    axis.title.y.right = element_text(angle=0,vjust=1.08,hjust=0,margin=unit(c(0,0,0,-1.5)*scale, "cm", colorlist$lt_black), size=18*scale),
    
    axis.title.x = x_leg,
    
    axis.ticks.x = element_line(size=1*scale, colour= colorlist$dk_gray), #D2D2D2
    axis.ticks.length = unit(-0.15*scale, "cm"),
    
    axis.text = element_text(angle = 0, colour = colorlist$txt_gray,size=16*scale), #495057
    axis.text.x = element_text(margin=unit(c(0.35,0.35,0,0.5)*scale, "cm")),
    axis.text.y = element_text(margin=unit(c(0.5,0.35,0.5,0.5)*scale, "cm")),
    axis.text.y.right= element_text(margin=unit(c(0.5,0.5,0.5,0.35)*scale, "cm")),
    
    
    axis.line.x = element_line(size=1*scale, colour= colorlist$dk_gray), #"#495057"
    axis.line.y = element_blank(),#element_line(size=1*scale, colour= "#495057"),
    
    legend.key = element_rect(colour=NA, fill=NA),
    legend.margin = margin(0,0,0,0),
    legend.text = element_text(size = 18*scale, colour = colorlist$dk_grey),
    legend.title = element_blank(),
    legend.background = element_rect(),
    legend.spacing.x = unit(0,'cm'),
    legend.spacing.y = unit(0,'cm'),
    
    plot.title = element_text(
      margin=unit(c(0.2,0,0.15,0)*scale,"cm"),
      size=24*scale, 
      hjust=0.0, 
      face = 'bold', 
      colour = "#7ba0be"
      ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.subtitle = element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0)*scale,"cm"),size=18*scale),
    plot.caption = element_text(hjust=0.0,size=14*scale,margin=unit(c(0.25,0,0.15,0)*scale,"cm")),
    
    legend.justification.top = "left",
    legend.position = "top",
    legend.location = "plot",
    legend.direction = "horizontal",
    legend.box = "horizontal", # "vertical",
    
    panel.background = element_rect(
      fill = NA, #'white', 
      color = NA #'#808080'
      ),
    #panel.grid = element_line(colour = NULL),
    panel.grid.major.y = element_line(colour = colorlist$lt_gray), #D2D2D2
    panel.grid.major.x = element_blank(),#element_line(color = '#D7D7D7'), #added
    panel.grid.minor = element_blank(),#element_line(color = '#D7D7D7'),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    
    strip.background = element_blank(),
    strip.placement = "inside",
    panel.spacing = unit(1, "lines"),
    strip.text = element_text(size = 14*scale,
                              margin=unit(c(0.15,0,0.5,0)*scale,"cm"),
                              colour = colorlist$txt_gray)
    )
}


