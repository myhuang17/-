library(corrplot)#相關係數圖
#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\cellphone.csv")
sum(is.na(data))#遺失值
data.corr = cor(data[-21])
corrplot(data.corr)
#群集分析