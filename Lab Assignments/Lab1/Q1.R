library(aplpack)

data1<-read.csv("eco_dev_data.csv")

data1
class(data1[2,5])

data1[,2:12]  <- lapply(data1[,2:12], as.numeric)



faces(data1[,2:12],labels=data1[,1])


