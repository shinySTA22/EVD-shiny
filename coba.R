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

model.reg <- lm(mpg ~ hp, mtcars)
res <- residuals(model.reg)
stdres <- studres(model.reg)
pred <- predict(model.reg, mtcars)


        if (input$type.res == "Raw"){
            y <- res
        } else {
            y <- stdres
        }

        if (input$plot.res == "Galat vs X") {
            x <- data()$x
        } else {
            x <- pred
        }

        data.frame(x = x, y = y)