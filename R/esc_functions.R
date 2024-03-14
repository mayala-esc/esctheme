### ESC functions ####

esc_save <- function(n, path = NULL, device = "png"){
  if(!is.character(n)){stop('Graph name needs to be in character format')}
  if(device!="png"){ttl <- paste0(n,'.svg')
  ggsave(ttl, w = 12.03, h = 6.67, path = path, device = "svg", bg = "white")} #10.03
  
  else{ttl <- paste0(n,'.png')
  ggsave(ttl, w = 12.03, h = 6.67, path = path, device = "png", bg = "white")}
}


trail_sum <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% mutate(value = case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right")*p)) #multiply by n period
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}

trail_avg <- function(x,p=4,ivars=unique(x$variable)){
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>% mutate(value = case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right")))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}

gr <- function(x,p){
  (x/lag(x,p)-1)*100}

y_avg_growth <- function(x,p=4,ivars=unique(x$variable)) {
  #INPUTS
  #x:   Data frame in long format
  #ivars: variables in the data frame to be transformed (defaults to all)
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value = case_when(!is.na(value)~runmean(value,p,endrule="NA",align="right"))) %>%
      mutate(value = gr(value,p)) #applies growth cal after the rolling average
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}

growth <- function(x,p,ivars=unique(x$variable)) {
  gr <- function(x,p){
    (x/lag(x,p)-1)*100}
  
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value = gr(value,p))
    x[which(x$variable==ivars[i]),] <- b}
  return(x)} 


cagr <- function(x,p=4,ivars=unique(x$variable)) {
  for(i in 1:length(ivars)){
    b <- x[which(x$variable==ivars[i]),] 
    b <- b %>%
      mutate(value = case_when(!is.na(value)~runmean(value,4,endrule="NA",align="right"))) %>%
      mutate(value = ((value/lag(value,p*4))^(1/p)-1)*100) #applies the cagr after the rolling average
    x[which(x$variable==ivars[i]),] <- b}
  return(x)}


get_png <- function(filename) {
  grid::rasterGrob(png::readPNG(filename), interpolate = TRUE)
}

