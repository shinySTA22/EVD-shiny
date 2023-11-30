# NOMOR 1

# --- 1A ---
# pembangkitan bilangan acak untuk model
# residual berdistribusi normal
# banyaknya amatan

library(sp)
n <- 1000
set.seed(1)
epsilon <- rnorm(n, mean = 0, sd = 1)
epsilon

# NOMOR 2
# bangkitkan peubah penjelas Xi yang saling bebas
# bnx: banyaknya nilai x yang ingin dimasukkan dalam regresi
bnx <- 1
set.seed(1)
X <- replicate(bnx, rnorm(n, mean = 0, sd = 1.5))
head(X)
X

# bangkitkan peubah Y dengan menggunakan model regresi
betas <- matrix(c(10, 1, 5, 7), nrow = 4, ncol = 1)
Xgab <- cbind(1, X)

Y <- Xgab %*% betas + epsilon

dataRegresi <- data.frame(Y, X)
colnames(dataRegresi) <- c("Y", "X1")
head(dataRegresi)
dataRegresi

# pengecekan dengan model regresi
modelRegresi <- lm(Y ~ ., data = dataRegresi)
summary(modelRegresi)

# NOMOR 3

# --- 3A ---
# bangkitkan peubah penjelas Xi yang saling bebas
# bnx: banyaknya nilai x yang ingin dimasukkan dalam regresi
bnx <- 2 
set.seed(1)
X <- replicate(bnx, rnorm(n, mean = 0, sd = 1.5))
head(X)
X

# bangkitkan peubah Y dengan menggunakan model regresi
betas <- matrix(c(10, 2, 5, 7), nrow = 4, ncol = 1)
Xgab <- cbind(1, X)

Y <- Xgab %*% betas + epsilon

dataRegresi <- data.frame(Y, X)
colnames(dataRegresi) <- c("Y", "X1", "X2")
head(dataRegresi)
dataRegresi

# pengecekan dengan model regresi
modelRegresi <- lm(Y ~ ., data = dataRegresi)
summary(modelRegresi)

# --- 3B ---
# input b0, b1
b0 <- 1
b1 <- 1
b0hat <- NULL  # untuk menyimpan hasil dugaan
b1hat <- NULL
for (i in 1:100) {
  eps <- rnorm(10)
  X <- runif(10, 5, 10)
  Y <- b0 + b1 * X + eps
  obj <- lm(Y ~ X)
  obj
  b0hat <- c(b0hat, obj$coefficients[1])
  b1hat <- c(b1hat, obj$coefficients[2])
}

hasil <- matrix(c(mean(b0hat), sd(b0hat), mean(b1hat), sd(b1hat)), nrow = 2, ncol = 2)
rownames(hasil) <- c(" mean ", "sd")
colnames(hasil) <- c("b0", "b1")
hasil

#  misalkan x1, x2 ditentukan sebesar 0.75
library(MASS)
b0 <- 1
b1 <- 1
b2 <- 1
b0hat <- NULL
b1hat <- NULL
b2hat <- NULL
Sigma <- matrix(c(1, 0.75, 0.75, 1), nrow = 2, ncol = 2)
mu <- c(1, 1)
for (i in 1:100) {
  eps <- rnorm(10)
  X <- mvrnorm(10, mu, Sigma)
  Y <- b0 + b1 * X[, 1] * b2 * X[, 2] + eps
  obj <- lm(Y ~ X)
  b0hat <- c(b0hat, obj$coefficients[1])
  b1hat <- c(b1hat, obj$coefficients[2])
  b2hat <- c(b2hat, obj$coefficients[3])
}
hasil <- matrix(c(mean(b0hat), sd(b0hat), mean(b1hat), sd(b1hat), mean(b2hat), sd(b2hat)), 
                nrow = 2, ncol = 3)
rownames(hasil) <- c(" mean ", "sd")
colnames(hasil) <- c("b0", "b1", "b2")
hasil