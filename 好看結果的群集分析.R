#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\winequality-red.csv")
ans = data$quality
sum(is.na(data))#遺失值
tdata = data[,-12]

#群集分析
ca = function(odata, z){
  q4 = NULL
  distanceee = c("euclidean", "manhattan")
  way = c("single", "complete", "average", "centroid", "ward.D2")
  for(i in 1:2){
    for(j in way){
      R.dist = dist(odata, method=distanceee[i])
      q11 = hclust(R.dist, method=j)
      nres = cutree(q11, k=z)
      q4 = rbind(q4, nres)
      #table(resulten, ans)
    }
  }
  return(q4)
}
#暴力分解Q4
ans4 = ca(tdata,6)
table(ans4[1,], data$quality)
table(ans4[2,], data$quality)
table(ans4[3,], data$quality)
table(ans4[4,], data$quality)
table(ans4[5,], data$quality)
table(ans4[6,], data$quality)
table(ans4[7,], data$quality)
table(ans4[8,], data$quality)
table(ans4[9,], data$quality)
table(ans4[10,], data$quality)
#5-1以主成分分析的主成分做群集分析
pca = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\wq_pca.csv")
pca = pca[, 1:4]
ans51 = ca(pca,6)
table(ans51[1,], data$quality)
table(ans51[2,], data$quality)
table(ans51[3,], data$quality)
table(ans51[4,], data$quality)
table(ans51[5,], data$quality)
table(ans51[6,], data$quality)
table(ans51[7,], data$quality)
table(ans51[8,], data$quality)
table(ans51[9,], data$quality)
table(ans51[10,], data$quality)
#5-2依照主成分分析的重要變數做群集分析
data5.2 = cbind(tdata$fixed.acidity, tdata$citric.acid, tdata$density, tdata$pH)
ans52 = ca(data5.2,6)
table(ans52[1,], data$quality)
table(ans52[2,], data$quality)
table(ans52[3,], data$quality)
table(ans52[4,], data$quality)
table(ans52[5,], data$quality)
table(ans52[6,], data$quality)
table(ans52[7,], data$quality)
table(ans52[8,], data$quality)
table(ans52[9,], data$quality)
table(ans52[10,], data$quality)
#5-3減少群數做主成分分析
data5.31 = subset(data, quality == 5)
data5.32 = subset(data, quality == 6)
data5.3 = rbind(data5.31, data5.32)
ans5.3 = ca(data5.3, 2)
table(ans5.3[1,], data5.3$quality)
table(ans5.3[2,], data5.3$quality)
table(ans5.3[3,], data5.3$quality)
table(ans5.3[4,], data5.3$quality)
table(ans5.3[5,], data5.3$quality)
table(ans5.3[6,], data5.3$quality)
table(ans5.3[7,], data5.3$quality)
table(ans5.3[8,], data5.3$quality)
table(ans5.3[9,], data5.3$quality)
table(ans5.3[10,], data5.3$quality)
#=====================================================================================
