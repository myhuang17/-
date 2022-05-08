library(corrplot)#相關係數圖
#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\cellphone.csv")
sum(is.na(data))#遺失值
data.corr = cor(data[-21])
corrplot(data.corr)
tdata = data[,-21]

#群集分析
#階層式分群
E.dist <- dist(tdata, method="euclidean") # 歐式距離
M.dist <- dist(tdata, method="manhattan") # 曼哈頓距離

par(mfrow=c(1,1)) # 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化
# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")
# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")
#可以在hclust()裡面調整參數method，選擇不同的方法：
hclust(E.dist, method="single")   #歐-最近法
hclust(E.dist, method="complete") #歐-最遠法
hclust(E.dist, method="average")  #歐-平均法
hclust(E.dist, method="centroid") #歐-中心法
hclust(E.dist, method="ward.D2")  #歐-華德法

plot(hclust(E.dist, method="single"))
abline(h=100, col="red")
