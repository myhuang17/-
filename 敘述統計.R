library(corrplot)
library(ggplot2)
library(dplyr)
library(GGally)
library(ggpubr)
library(lattice)
#讀取資料
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\winequality-red.csv")
sum(is.na(data))#遺失值
datasum = summary(data)#取得各項資訊
write.csv(datasum, "C:\\Users\\user\\Desktop\\多變量報告\\data\\敘述統計1.csv")
#1. 常態分佈檢定
W = NULL
pvalue = NULL
for (i in 1:length(data)) {
  W[i] = shapiro.test(data[,i])$statistic#12個變數都不是常態分佈
  pvalue[i] = shapiro.test(data[,i])$p.value#12個變數都不是常態分佈
}
dist = cbind(W,pvalue)
#2. 相關係數檢定
data.corr = cor(data)
corrplot.mixed(data.corr, upper = "square")#關係矩陣

#變數分布圖
hist(x=data$quality, main = NULL)
ggqqplot(data, x ="fixed.acidity")
ggqqplot(data, x = "volatile.acidity")
x1 = densityplot(data$fixed.acidity)
x2 = densityplot(data[,2])
densityplot(data, layout = c(3,4))

#變數分布圖GGPLOT
ggplot(data, aes(x=quality))+geom_bar(stat = "count",position = "dodge", fill = "darkred")+
  scale_x_continuous(breaks = seq(3,8,1))+
  ggtitle("Distribution of Red Wine Quality Ratings")+
  theme_classic()

#敘述大圖
wine_df1 = data
wine_df1$quality = as.factor(wine_df1$quality)
ggpairs(wine_df1, aes(colour = quality, alpha = 0.4))
ggpairs(data[,-12])