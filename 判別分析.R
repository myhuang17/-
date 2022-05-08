library(corrplot)#相關係數圖
library(MASS)#LDA模型
#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\cellphone.csv")
sum(is.na(data))#遺失值
summary(data)
data.corr = cor(data[-21])
corrplot(data.corr)
#判別分析
ldamodel = lda(price_range~., data = data)#”~.”代表將資料中除了花的種類以外的變項拿來建模
ldamodel
