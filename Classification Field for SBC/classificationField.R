# Encoding: UTF-8

# Arguments:
#   method           -  function-classifier
#   xl               -  data set
#   ...              -  'method' arguments except data set and object to be classified
#   colors, cex, pch -  'plot'/'points' options
#   expand           -  [data set plot size] + expand = [classification map size]
#
classificationMap <- function(method, xl, ..., 
                              colors, 
                              cex = 1, 
                              pch = 1, 
                              expand = 0) {
    
    expand + 0.2 -> expand
    
    rangeX <- seq(xl[[1]][which.min(xl[[1]])] - expand, 
                  xl[[1]][which.max(xl[[1]])] + expand, 
                  0.1)
    rangeY <- seq(xl[[2]][which.min(xl[[2]])] - expand, 
                  xl[[2]][which.max(xl[[2]])] + expand, 
                  0.1)
    
    length(rangeX) * length(rangeY) -> pointsAmount
    map <- matrix(NA, pointsAmount, 2)
    
    i <- 1
    for (y in rangeY) {
        for (x in rangeX) {
            map[i, ] = c(x, y)
            
            i + 1 -> i
        }
    }
    
    classes <- array(dim = pointsAmount)
    for (i in 1:pointsAmount) {
        method(xl = xl, z = map[i, ], ...) -> classes[i]
        cat('\rComputed ', i, '/', pointsAmount, ' points', sep = '')
    }
    
    points(map, col = colors[classes], bg = colors[classes], cex = cex, pch = pch)
}
