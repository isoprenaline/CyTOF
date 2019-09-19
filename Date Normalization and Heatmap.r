#本脚本能够读取Marker表达量csv文件中的数据，去除不需要的Parameter，将需要分析的Parameter名转换成蛋白Marker名，例如：将“Bi209Di<209Bi_MHC_II>”转换为“MHC-II”
#再将需要分析的Marker表达量数据都进行Normalization，并生成热图
#注意：需要手动将自己要分析的蛋白marker名称放在一个csv文件中作为KeyWords，用来从原始数据中抽取数据
#KeyWord必须能够与原始Parameter名称中的一部分能够完全匹配，例如：“Bi209Di<209Bi_MHC_II>”对应的Keyword是“MHC_II”而不是“MHC-II”，后面再将“_”替换成“-”即可
#某些特殊的marker名称可以发生多重匹配，比如“CD4”能够匹配“CD4”，“CD45”与“CD44”，需要对“CD4”进行“标记”，一般是保留“CD4”后面的一个字符，例如：“Sm147Di<147Sm_CD4>”对应的KeyWord是“CD4>”,后面再将“>”去掉即可

library(ggplot2)
require(reshape2)
require(scales)
	#读取marker表达量文件
	data <- read.csv('C:/Users/jufei/Documents/total CD3_Rphenograph_cluster_median_data.csv', header=F, row.names=1, stringsAsFactors = FALSE)
	#读取蛋白marker列表作为匹配关键词
	keywords <- read.csv('C:/Users/jufei/Documents/markers.csv', header=F, stringsAsFactors = FALSE)
	
			#marker定位
			site <- apply(keywords, 2, grep, data)
			
			#抽取需要分析的marker数据
			data.markersWithoutColname <- data[,site]
			
			#将"_"替换为"-",并去除其他不需要的符号
			CloName <- gsub("_", "-", keywords)
			CloName <- gsub(">", "", CloName)
			
			#将正式表头与marker表达量组合成正式表格
			names(data.markersWithoutColname) <- c(CloName)
			data.markers <- data.markersWithoutColname
			data.markers <- data.markers[-1,]
			
			#导出成CSV文件
			write.csv(data.markers,file="C:/Users/jufei/Documents/Datas/ClusterMarkerDatas.csv",quote=F)
			
			#读取marker表达文件
			data1 <- read.csv('C:/Users/jufei/Documents/Datas/ClusterMarkerDatas.csv', header=T, row.names=1, stringsAsFactors = FALSE) 
			
			#表头读取可能会把“-”变成“.”，用正式表头替代读取后的表头
			names(data1) <- c(CloName)
	
			#矩阵转置
			data <- t(data1[])
			
			#逐行进行数据Normalization
			normalized.data <- apply(data, 2, rescale)
			
			#导出成CSV文件
			write.csv(normalized.data,file="C:/Users/jufei/Documents/Datas/Normalized expression.csv",quote=F)
			
			#DefiningClusters
			data.defined <- apply(normalized.data, 2, function(x){ifelse(x<=0.1, data1<-'-',ifelse(x>0.1 & x<= 0.4, data1<-'low', ifelse(x> 0.4 & x<= 0.6, data1<-'med', data1<-'hi')))})
			write.csv(data.defined,file="C:/Users/jufei/Documents/Datas/DefiningClusters.csv",quote=F)
			
			normalized.data1 <- melt(normalized.data)
			
			#ggplot生成热图
			heatmap <- ggplot(normalized.data1, aes(x=Var1, y=Var2))  + coord_fixed() #设置XY轴，Cell形状设定为正方形
			heatmap <- heatmap + geom_tile(aes(fill = value), colour = "white") #设置填充数据与Cell周围白线
			heatmap <- heatmap + scale_fill_gradientn(colours=c("black", "#5c266e", "#b63459", "#f17223", "yellow"), values=rescale(c(0, 0.2, 0.4, 0.6, 0.8, 1)), guide="colorbar") #设置颜色范围
			heatmap <- heatmap + theme_bw() + theme(panel.grid.major = element_blank()) + theme(legend.key=element_blank()) + theme(axis.text.x=element_text(angle=45,hjust=1, vjust=1, size=15)) #设置x轴字体
			heatmap <- heatmap + theme(axis.text.y=element_text(size=15)) #设置y轴字体
			heatmap <- heatmap + xlab("Clusters") + ylab("Markers") #设置标题
			
			#导出热图
			ggsave(heatmap, filename="C:/Users/jufei/Documents/Datas/heatmap1.pdf", width=30, height=45, units=c("cm"),colormodel="srgb")