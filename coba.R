data("trees")
head(trees)

getFromRData <- function(name, x, y) {
    d <- data(name)
    return(data.frame(x = d[x], y = d[y]))
}

getFromRData("trees", "Height", "Girth")

get.data <- switch("rock",
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars,
           "mpg" = mpg,
           "mtcars" = mtcars,
           "diamonds" = diamonds)
get.data      
