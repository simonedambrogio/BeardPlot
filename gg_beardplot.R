gg_beardplot <- function(data, x, group, beard_fullness=40, outliers=FALSE, distance=1, width_boxplot=1, smile=FALSE, color){
  
  library(dplyr)
  library(ggplot2)
  source('/home/simone/Scrivania/BeardPlot/utilities.r')
  
  
  ddf <- density_groups(data = data, x = x, group = group, beard_fullness=beard_fullness, distance=distance, outliers=outliers)
  names(data)[apply(data[which(!is.na(x)),]==x[which(!is.na(x))], 2, all)] <- 'x'
  
  if(missing(group)){
    
    if(missing(color)){ color='black' } else {color=color}
    if(outliers==TRUE){
    box <- geom_boxplot(data = data, 
                        aes(y = x),
                        color=color,
                        width = .2 * max(ddf$xmax) * width_boxplot,
                        position = position_nudge( x = abs(unique(ddf$xmin)) ) )
    }else{
      box <- geom_boxplot(data = data, 
                          aes(y = x),
                          color=color,
                          width = .2 * max(ddf$xmax) * width_boxplot,
                          position = position_nudge( x = abs(unique(ddf$xmin)) ),
                          outlier.shape = NA)
      }
    
    
    seg <- geom_segment(data = ddf,
                        color=color,
                        aes(x=-xmax, xend=-xmin, y=y, yend=y), 
                        size = 1)
    
    if (smile == TRUE){
      smile <- geom_path( data = ddf, aes(x=-xmax, y=y), color=color)
    } else { smile <- theme_bw() }
    
  } else {
    
    
    
    ddf_bp <- boxplot_position(ddf=ddf, data=data, group=group, x=x)
    
    if(outliers==TRUE){
      box <- geom_boxplot(data = ddf_bp, 
                          aes(y = x, color=as.factor(group), x=-xmin),
                          width = .1 * max(ddf$xmax) * width_boxplot)
    } else {
      box <- geom_boxplot(data = ddf_bp, 
                          aes(y = x, color=as.factor(group), x=-xmin ),
                          width = .1 * max(ddf$xmax) * width_boxplot,
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

#gg_beardplot(data = mpg, x=cty, color = 'red')
#gg_beardplot(data = mpg, x=cty, smile = T)
#gg_beardplot(data = mpg, x=cty, smile = T, outliers = T)

#gg_beardplot(data = mpg, x=cty, group=drv, smile = T, outliers = T)
#gg_beardplot(data = mpg, x=cty, group=cyl, outliers = T, smile = F, width_boxplot = 0.5, beard_fullness = 30, distance = 1 )

#gg_beardplot(data = datasets::airquality, x=Ozone, group = Month, outliers = T, smile = F, width_boxplot = 0.05, beard_fullness = 300, distance = 5 )
