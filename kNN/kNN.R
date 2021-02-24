# Encoding: UTF-8

euclideanDistance <- function(p, q) sqrt(sum((p - q) ^ 2))

kNN <- function(xl, z, k, metricFunction = euclideanDistance) {
    # Если передано несколько точек в массиве
    if (is.matrix(z)) {
        dim(z)[1] -> zSize
        result <- array(dim = zSize)
        for (i in 1:zSize)
            result[i] <- kNN(xl, c(z[i, 1], z[i, 2]), k)
        
        return(result)
    }
    
    # Индексы k ближайших элементов
    kNearestIndexes <- function(k, x) {
        result <- c()
        for (i in 1:k) {
            which.min(x) -> min
            result[i] <- min
            x[min] <- NA
        }
        
        return(result)
    }
    
    # Мода
    mode <- function(x) {
        #return(unique(x)[which.max(tabulate(match(x, unique(x))))])
        
        unique(x) -> classes
        
        match(x, classes) -> numericalX
        
        tabulate(numericalX) -> countedX
        
        which.max(countedX) -> maxClassID
        
        return(classes[maxClassID])
    }
    
    distances <- c()
    for (i in 1:dim(xl)[1]) 
        distances[i] <- metricFunction(c(xl[i, 1], xl[i, 2]), z)
    
    xl[kNearestIndexes(k, distances), 3] -> kNearestClasses
    
    return(mode(kNearestClasses))
}

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

# Рассматриваем лепестки ирисов
iris[, 3:5] -> petals

k <- 7

# Размеры выборки
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
     main = list(paste('kNN на примере лепестков ирисов Фишера [ k =', k ,']'), 
                 font = 1, col = 'slategray'), 
     xlab = list('Длина', font = 3), 
     ylab = list('Ширина', font = 3), 
     asp = 1)
    
classificationMap(kNN, petals, k, 
                  colors = plotColors, 
                  cex = 0.4, 
                  expand = plotPadding)

points(petals$Petal.Length, petals$Petal.Width, 
       pch = 19, 
       col = plotColors[petals$Species], 
       bg = plotColors[petals$Species], 
       cex = 1)

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
       bg = plotColors[kNN(petals, z, k)], 
       cex = 1.3)
