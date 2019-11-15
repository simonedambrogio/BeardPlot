#density_groups
density_groups <- function(data, x, group, beard_fullness, distance, outliers){
  
  source('/home/simone/Dropbox/UNIVERSITÀ/Statistica/R/Funzioni/df_lapply.R')
  
  if( missing(group) ){
    data$group <- 'a'
  } else { names(data)[which(group == data[1, ])] <- 'group'  }
  
  names(data)[which(x == data[1, ])] <- 'x'
  
  d <- df_lapply( data %>% select(group) %>% unique() %>% pull() %>% as.character(), function(lev){
    
    if(outliers == FALSE){
      #create a x without outliers
      data <- df_lapply( data %>% pull(group) %>% unique(), function(lev){ 
        limits <- data[data$group==lev, ] %>% pull(x) %>% IQR.outliers()
        data[data$group==lev, ] %>% filter(data[data$group==lev, ] %>% pull(x) > limits[1] &
                                             data[data$group==lev, ] %>% pull(x) < limits[2])
      })
      
      dens <- data[data$group==lev, ] %>% pull(x) %>% density(n=beard_fullness, from = min(.), to = max(.))
      ddf <- data.frame(X = dens$x, density = dens$y, group = lev)
    } else {
      dens <- data[data$group==lev, ] %>% pull(x) %>% density(n=beard_fullness, from = min(.), to = max(.))
      ddf <- data.frame(X = dens$x, density = dens$y, group = lev)
    }
    
  })
  
  xmax <- max(d$density)
  d[d$group==unique(d$group)[1], 'xmin'] <- 0
  
  if( length(unique(d$group)) > 1 ){
  for (lev in 2:length(unique(d$group))) {
    levels <- unique(d$group)
    d[d$group==levels[lev], 'density'] <- d[d$group==levels[lev], 'density'] + max(d[d$group==levels[lev-1], 'density'])+ xmax*as.numeric(paste0('1.', distance))
    d[d$group==levels[lev], 'xmin'] <- max(d[d$group==levels[lev-1], 'density']) + xmax*as.numeric(paste0('1.', distance))
    d[d$group==levels[lev], ]
  }}
  
  names(d) <- c('y', 'xmax', 'group', 'xmin')
    
  return(
    d
  )
} 

#density_groups(data = dat, x = x, group = group, beard_fullness=40, distance=1, outliers=T)
#density_groups(data = mpg, x = cty, group = drv, beard_fullness=40, distance=1, outliers=F)

#outliers
IQR.outliers <- function(x) {
  if(any(is.na(x)))
    stop("x is missing values")
  if(!is.numeric(x))
    stop("x is not numeric")
  
  Q3<-quantile(x,0.75)
  Q1<-quantile(x,0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(1.5*IQR))
  right<- (Q3+(1.5*IQR))
  
  return(c(left, right))
}

#dataset for boxplot
boxplot_position <- function(ddf, data, group){
  
  names(data)[which(group == data[1, ])] <- 'group'
  
  d <- sapply(split(ddf$xmin, ddf$group), unique)
  for(i in 1:length(d)){
    data[data$group==names(d)[i], 'xmin'] <- d[names(d)[i]]
  }
  return(data)
}