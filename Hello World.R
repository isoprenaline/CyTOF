library(xtable)
df <- read.csv('C:/Users/jufei/Documents/Datas/DefiningClusters.csv', header=T, row.names=1, stringsAsFactors = FALSE)
df1<- read.csv('C:/Users/jufei/Documents/Datas/DefiningClusters.csv', header=F, row.names=1, stringsAsFactors = FALSE)
colnames <- names(df)
colnames1 <- df1[1,]
fun <- function(x){paste0("\textsuperscript{", df[,x], "}")}
re <- matrix(nrow = nrow(df), ncol = ncol(df))
for (i in 1:ncol(df)) {
  re[,i] <- paste0(colnames1[i],fun(i))
}
names(re) <- c(colnames1)

xtable(re,digits=3,caption="Head of Iris Data")


