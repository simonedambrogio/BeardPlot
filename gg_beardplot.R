gg_beardplot <- function(data, x, group, beard_fullness=40, outliers=FALSE, distance=1, width_boxplot=1, smile=FALSE){
  
  library(dplyr)
  library(ggplot2)
  source('/home/simone/Scrivania/BeardPlot/utilities.r')
  
  ddf <- density_groups(data = data, x = x, group = group, beard_fullness=beard_fullness, distance=distance, outliers=outliers)
  ddf_bp <- boxplot_position(ddf, data=data, group=group)
  names(data)[which(x == data[1, ])] <- 'x'
  
  if(missing(group)){
    
    if(outliers==TRUE){
    box <- geom_boxplot(data = data, 
                        aes(y = x),
                        width = .2 * max(ddf$xmax) * width_boxplot,
                        position = position_nudge( x = abs(unique(ddf$xmin)) ) )
    }else{
      box <- geom_boxplot(data = data, 
                          aes(y = x),
                          width = .2 * max(ddf$xmax) * width_boxplot,
                          position = position_nudge( x = abs(unique(ddf$xmin)) ),
                          outlier.shape = NA)
      }
    
    
    seg <- geom_segment(data = ddf,
                        aes(x=-xmax, xend=-xmin, y=y, yend=y), 
                        size = 1)
    
    if (smile == TRUE){
      smile <- geom_path( data = ddf,aes(x=-xmax, y=y))
    } else { smile <- theme_bw() }
    
  } else {
    
    if(outliers==TRUE){
      box <- geom_boxplot(data = ddf_bp, 
                          aes(y = x, color=group, x=-xmin),
                          width = .2 * max(ddf$xmax) * width_boxplot)
    } else {
      box <- geom_boxplot(data = ddf_bp, 
                          aes(y = x, color=group, x=-xmin ),
                          width = .2 * max(ddf$xmax) * width_boxplot,
                          outlier.shape = NA)
    }
      
      seg <- geom_segment(data = ddf,
                          aes(x=-xmax, xend=-xmin, y=y, yend=y, color = group),
                          size = 1)
      
      if (smile == TRUE){
        smile <- geom_path( data = ddf,aes(x=-xmax, y=y, color = group))
      } else { smile <- theme_bw() }
  }
    
 p <- ggplot()+
      seg +
      smile +
      box +
      theme_bw() +
      coord_flip()
      
    
  return(p)
}

gg_beardplot(data = dat, x=x, group=group, smile = T, outliers = T)
gg_beardplot(data = mpg, x=cty, group=class ,  outliers = F)

ggplot(data = mpg, aes(cty, x=0) )+
  geom_boxplot( )
