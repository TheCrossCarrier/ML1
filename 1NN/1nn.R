# Encoding: UTF-8

euclideanDistance <- function(p, q) sqrt(sum((p - q) ^ 2))

NN <- function(xl, z, metricFunction = euclideanDistance) {
    distances <- c()
    for (i in 1:dim(xl)[1]) 
        distances[i] <- euclideanDistance(c(xl[i, 1], xl[i, 2]), z)
    
    return(xl[which.min(distances), 3])
}

# Рассматриваем лепестки ирисов
iris[, 3:5] -> petals

plotColors <- c('setosa' = 'mediumspringgreen', 
                'versicolor' = 'gold', 
                'virginica' = 'purple')

plot(petals$Petal.Length, petals$Petal.Width, 
     main = list('1NN на примере лепестков ирисов Фишера', font = 1, col = 'slategray'), 
     xlab = list('Длина', font = 3), 
     ylab = list('Ширина', font = 3), 
     pch = 19, 
     col = plotColors[petals$Species])

legend('topleft', 
       names(plotColors), 
       fill = plotColors,
       border = 0, 
       inset = .07)

# Интересующие параметры лепестков для классификации
rbind(c(5, 1), 
      c(4.5, 2.2),
      c(2.2, 1)) -> z

# Добавление точек на график
for (i in 1:dim(z)[1])
    points(z[i, 1], 
           z[i, 2], 
           pch = 22, 
           bg = plotColors[NN(petals, z[i, ])])
