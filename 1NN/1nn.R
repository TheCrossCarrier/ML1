# Encoding: UTF-8

euclideanDistance <- function(p, q) sqrt(sum((p - q) ^ 2))

NN <- function(xl, z, metricFunction = euclideanDistance) {
    
    # Если передано несколько точек в массиве
    if (is.matrix(z)) {
        dim(z)[1] -> zSize
        result <- array(dim = zSize)
        for (i in 1:zSize)
            result[i] <- NN(xl, c(z[i, 1], z[i, 2]))
        
        return(result)
    }
    
    distances <- c()
    for (i in 1:dim(xl)[1]) 
        distances[i] <- euclideanDistance(c(xl[i, 1], xl[i, 2]), z)
    
    return(xl[which.min(distances), 3])
}

# Рассматриваем лепестки ирисов
iris[, 3:5] -> petals

petals.minX <- petals$Petal.Length[which.min(petals$Petal.Length)]
petals.maxX <- petals$Petal.Length[which.max(petals$Petal.Length)]

petals.minY <- petals$Petal.Width[which.min(petals$Petal.Width)]
petals.maxY <- petals$Petal.Width[which.max(petals$Petal.Width)]

plotColors <- c('setosa' = 'mediumspringgreen', 
                'versicolor' = 'gold', 
                'virginica' = 'purple')

plotPadding <- 0.3

plot(NA, 
     xlim = c(petals.minX - plotPadding, petals.maxX + plotPadding), 
     ylim = c(petals.minY - plotPadding, petals.maxY + plotPadding), 
     main = list('1NN на примере лепестков ирисов Фишера', font = 1, col = 'slategray'), 
     xlab = list('Длина', font = 3), 
     ylab = list('Ширина', font = 3), 
     asp = 1)

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
  
  points(map,  col = colors[classes], cex = cex, pch = pch)
}

classificationMap(NN, petals, 
                  colors = plotColors, 
                  cex = 0.4, 
                  expand = plotPadding)

points(petals$Petal.Length, petals$Petal.Width, 
       pch = 19, 
       col = plotColors[petals$Species])

legend('topleft', 
       names(plotColors), 
       fill = plotColors, 
       border = 0, 
       inset = .07)

# Интересующие параметры лепестков для классификации
rbind(c(5.2, 1.6), 
      c(4.4, 1.7), 
      c(2.3, 1)) -> z

# Добавление точек на график
points(z[, 1], z[, 2], 
       pch = 22, 
       col = 'red', 
       bg = plotColors[NN(petals, z)], 
       cex = 1.3)
