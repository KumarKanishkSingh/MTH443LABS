library(ggplot2)
library(rgl)

data <- read.csv('./eco_dev_data.csv')

View(data)
num.data <- data[2:dim(data)[2]]

samp.cov <- cov(num.data)
svd.res <- svd(samp.cov)

# Checking eigenvector and eigenvalue
# (samp.cov %*% svd.res$u[,1]) - (svd.res$d[1] * svd.res$u[,1])

# Checking orthogonality
# t(svd.res$u) %*% svd.res$u
# svd.res$u %*% t(svd.res$u)

# Checking with in-built function
# our.pc <- data.matrix(num.data) %*% svd.res$u
# cal.pc <- prcomp(num.data, center=FALSE, scale=FALSE)
# 
# diff <- ((-1.0) * cal.pc$x) - our.pc
# for (i in 1:dim(diff)[2] ){
#     print(max(diff[, i]))
# }

data.pc <- data.matrix(num.data) %*% svd.res$u

var.prop <- svd.res$d /  sum(svd.res$d)

pairs(data.pc, pch = 16, cex=0.4)

plot(data.pc[,1], data.pc[,2], pch=16)
text(data.pc[,1], data.pc[,2]+0.3, labels=data$Country)

# Scree Plot
bar.x <- barplot(var.prop, names.arg=1:dim(data.pc)[2], space=0.1, main="Scree Plot")
lines(bar.x, var.prop, lwd=2)
points(bar.x, var.prop, pch=16)

# 3D plot
plot3d(data.pc[,1], data.pc[,2], data.pc[,3],
       type = 's', 
       radius = .1,
       col = 'green'
)
text3d(data.pc[,1], data.pc[,2]+0.8, data.pc[,3]+0.8, data$Country)

# pairs(num.data, pch = 16, cex=0.4)
