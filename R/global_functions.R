
global_ksmooth <- function(x, y){
  
  ksmooth <- ksmooth(x, y, 
                     n.points = length(x), 
                     kernel = "box",  
                     bandwidth = 2
  )
  
  return(ksmooth$y)
}
