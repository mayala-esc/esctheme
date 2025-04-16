
#' Save a ESC styled plot with sensible defaults
#' 
#' @importFrom stats lag
#' 
#' @param n Name given to chart
#' @param path File path given to save chart
#' @param device "png" or "svg" file type
#' @param width The width size. Default to 14.5
#' @param height The height size. Default to 6
#' @param ... Optional arguments to ggsave.
#' 
#' @return Saved png file
#' 
#' @export

esc_save <- function(n, path = NULL, device = "png", width = 14.5, height = 6,
                     ...){
  
  if(!is.character(n)){stop('Graph name needs to be in character format')}
  if(device!="png"){ttl <- paste0(n,'.svg')
  ggplot2::ggsave(ttl, width = width, height = height, path = path, device = "svg", bg = NULL)} #10.03
  
  else{ttl <- paste0(n,'.png')
  ggplot2::ggsave(ttl, width = width, height = height, path = path, device = "png", bg = "white")}
}


