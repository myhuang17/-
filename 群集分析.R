library(corrplot)#相關係數圖
#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\winequality-red.csv")
sum(is.na(data))#遺失值
tdata = data[,-12]

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
en = hclust(E.dist, method="single")   #歐-最近法
ef = hclust(E.dist, method="complete") #歐-最遠法
ea = hclust(E.dist, method="average")  #歐-平均法
ec = hclust(E.dist, method="centroid") #歐-中心法
ew = hclust(E.dist, method="ward.D2")  #歐-華德法

mn = hclust(M.dist, method="single")   #曼-最近法
mf = hclust(M.dist, method="complete") #曼-最遠法
ma = hclust(M.dist, method="average")  #曼-平均法
mc = hclust(M.dist, method="centroid") #曼-中心法
mw = hclust(M.dist, method="ward.D2")  #曼-華德法

resulten = cutree(en, k=6)
table(resulten, data$quality)

resultmn = cutree(mn, k=6)
table(resultmn, data$quality)

plot(ef)
abline(h=18, col="red")
