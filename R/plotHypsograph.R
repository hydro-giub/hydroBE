plotHypsograph <- function(x,ylab='m.a.s.l.',xlab='area km2',col='blue',unit.conv=1e6,...) {

  if(class(x) != 'RasterLayer') {stop("Invalid argument: 'class(x)' must be 'RasterLayer'")}
  z.vals <- sort(raster::values(x),na.last=NA)
  x.vals <- (1:length(z.vals))*raster::res(x)[1]*raster::res(x)[2]/unit.conv
  plot(x=rev(x.vals),y=z.vals,type='l',xlab=xlab,ylab=ylab,col=col,...)
  invisible()
  
}