library(broom)
data <- iris
head(data)

reg <- lm(Sepal.Length~Sepal.Width, data = data)
summary(reg)

glance(reg)
