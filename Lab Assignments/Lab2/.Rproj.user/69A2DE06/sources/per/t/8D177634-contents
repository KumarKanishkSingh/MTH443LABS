data<-read.csv("eco_dev_data.csv")
data2<-data[-1, 2:ncol(data)]

df <- data.frame(sapply(data2, function(x) as.numeric(as.character(x))))

data_pca <- prcomp(df, center = TRUE, scale = TRUE)
data_pca_var <- data_pca$sdev^2

data_pca_ve <- data_pca_var / sum(data_pca_var)
data_pca_ve


biplot(data_pca, scale = 0, cex = 0.6)
