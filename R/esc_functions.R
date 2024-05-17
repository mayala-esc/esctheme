
#' generic functions used in the package  ####
#' 
#' @importFrom stats lag
#' 
#' @param n Name given to chart
#' 
#' @return Saved png file
#' 
#' @export

esc_save <- function(n, path = NULL, device = "png"){
  if(!is.character(n)){stop('Graph name needs to be in character format')}
  if(device!="png"){ttl <- paste0(n,'.svg')
  ggplot2::ggsave(ttl, width = 12.03, height = 6.67, path = path, device = "svg", bg = "white")} #10.03
  
  else{ttl <- paste0(n,'.png')
  ggplot2::ggsave(ttl, width = 12.03, height =  6.67, path = path, device = "png", bg = "white")}
}


#' Trail sum
#' 
#' @export
#' 

trail_sum <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% dplyr::mutate(value = dplyr::case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right")*p)) #multiply by n period
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}

#' Trail average
#' 
#' @export
#' 

trail_avg <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% dplyr::mutate(value = dplyr::case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right")))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}


gr <- function(x,p){
  (x/lag(x,p)-1)*100}



#' Year average growth
#' 
#' @export
#' 


y_avg_growth <- function(x,p=4,ivars=unique(x$variable)) {
  #INPUTS
  #x:   Data frame in long format
  #ivars: variables in the data frame to be transformed (defaults to all)
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      dplyr::mutate(value = dplyr::case_when(!is.na(value)~caTools::runmean(value,p,endrule="NA",align="right"))) %>%
      dplyr::mutate(value = gr(value,p)) #applies growth cal after the rolling average
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}

#' Growth rate
#' 
#' @export
#' 

growth <- function(x,p,ivars=unique(x$variable)) {
  
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      dplyr::mutate(value = gr(value,p))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)} 

#' Cumulative average growth rate
#' 
#' @export
#' 


cagr <- function(x,p=4,ivars=unique(x$variable)) {
  
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      dplyr::mutate(value = dplyr::case_when(!is.na(value)~runmean(value,4,endrule="NA",align="right"))) %>%
      dplyr::mutate(value = ((value/lag(value,p*4))^(1/p)-1)*100) #applies the cagr after the rolling average
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

