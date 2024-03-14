library(tidyverse)
library(ggthemes)
library(extrafont)

esc_theme_sf <- function(scale = 0.6){
  
  colorlist <- list(lt_gray = "#D9D9D9",
                    gray = "#BFBFBF",
                    dk_gray = "#595959",
                    lt_black = "#262626")
  
  theme_minimal() %+replace%
    
    theme(
      text = element_text(
        face = "plain",
        colour = colorlist$dk_gray
      ),
      line = element_blank(),
      rect = element_rect(
        fill = NA,
        linetype = 1,
        colour = "white",
        linewidth = 0.01),
      
      axis.ticks.x = element_blank(),
      
      axis.text = element_blank(),
      
      legend.key = element_rect(colour=NA, fill=NA),
      legend.key.size = unit(1*scale, 'cm'),
      legend.key.width= unit(1*scale, 'cm'),
      legend.margin = margin(0,0,0,0),
      legend.text = element_text(size = 14*scale),
      legend.title = element_text(margin=unit(c(0.2,0,0.15,0)*scale,"cm"),size=16*scale, hjust=0.0, face = 'bold'),
      legend.background = element_rect(fill = "white"),
      legend.spacing.x = unit(0.5,'cm'),
      legend.spacing.y = unit(0.5,'cm'),
      
      plot.title = element_text(margin=unit(c(0.2,0,0.15,0)*scale,"cm"),size=24*scale, hjust=0.0, face = 'bold', colour = "#7ba0be"),
      plot.title.position = "plot",
      plot.subtitle = element_text(hjust=0.0,margin=unit(c(0.15,0,0.5,0)*scale,"cm"),size=18*scale),
      plot.caption = element_text(hjust=0.0,size=18*scale,margin=unit(c(0.25,0,0.15,0)*scale,"cm")),
      legend.justification.top = "left",
      legend.position = "inside",
      legend.position.inside = c(0.9, 0.88),
      legend.location = "plot",
      legend.direction = "vertical",
      legend.box = "vertical",
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "lines"),
      panel.border = element_rect(colour = colorlist$gray, fill=NA, linewidth=0.01),
      
      strip.background = element_rect()
    )
}


