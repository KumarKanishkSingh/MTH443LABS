library(ggplot2)
library(rgl)

data <- read.csv('./PS_bank_fin_ratio.csv')

View(data)
num.data <- data[2:dim(data)[2]]

mean.data <- apply(num.data, 2, mean)
sd.data <- apply(num.data, 2, sd)
st.data <- t( apply(num.data, 1, function(x) ( (x - mean.data)/sd.data ) ) )

cov(st.data)
apply(st.data, 2, mean)

samp.cov <- cov(st.data)
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

data.pc <- data.matrix(st.data) %*% svd.res$u

var.prop <- svd.res$d /  sum(svd.res$d)

pairs(data.pc, pch = 16, cex=0.4)

plot(data.pc[,1], data.pc[,2], pch=16)
text(data.pc[,1], data.pc[,2]+0.3) # , labels=data$Banks)

for(i in seq(1, dim(data.pc)[1], by=4))
{
    lines(data.pc[i:(i+3),1], data.pc[i:(i+3),2], lwd=1)
}
# Scree Plot
bar.x <- barplot(var.prop, names.arg=1:dim(data.pc)[2], space=0.1, main="Scree Plot")
lines(bar.x, var.prop, lwd=2)
points(bar.x, var.prop, pch=16)

# 3D plot
plot3d(data.pc[,1], data.pc[,2], data.pc[,3],
       type = 's', 
       radius = .06,
       col = 'green'
)
text3d(data.pc[,1], data.pc[,2]+0.2, data.pc[,3]+0.2, data$Banks)
    # svd.res$u
# par(mfrow=c(1,1))
# plot(st.data[,6],st.data[,8])
# text(st.data[,6],st.data[,8]+0.1,data$Country)
# plot(st.data[,6],st.data[,3])
# text(st.data[,6],st.data[,3]+0.1,data$Country)
# plot(st.data[,3],st.data[,8])
# text(st.data[,3],st.data[,8]+0.1,data$Country)

# pairs(num.data, pch = 16, cex=0.4)

