library(rtf)
require(reshape2)
data <- read.csv('C:/Users/jufei/Documents/Datas/Normalized expression.csv', header=F, row.names=1, stringsAsFactors = FALSE)
colnames <- data[1,]
data <- data[-1,]
data1 <- apply(data, 2, function(x){ifelse(x<=0.1, data1<-'-',ifelse(x>0.1 & x<= 0.4, data1<-'low', ifelse(x> 0.4 & x<= 0.6, data1<-'med', data1<-'hi')))})
names(data1) <- c(colnames)
write.csv(data1,file="C:/Users/jufei/Documents/Datas/DefiningClusters.csv",quote=F)
