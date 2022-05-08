library(corrplot)#相關係數圖
#資料讀取與初步剖析
data = read.csv("C:\\Users\\user\\Desktop\\多變量報告\\data\\cellphone.csv")
sum(is.na(data))#遺失值
data.corr = cor(data[-21])
corrplot(data.corr)

#PCA+群集分析
#主成分分析PCA
pca = prcomp(formula = ~ battery_power+blue+clock_speed+dual_sim+fc+
               four_g+int_memory+m_dep+mobile_wt+n_cores+
               pc+px_height+px_width+ram+sc_h+
               sc_w+talk_time+three_g+touch_screen+wifi
             , data = data, scale = TRUE)#SCALE 正規化資料
plot(pca,
     type="line", #用直線連結每個點
     main="Price of Cellphone number") #主標題 5
abline(h=1, col="blue") #凱莎原則：用藍線標示出特徵值=1的地方 10

#從pca中取出標準差(pca$sdev)後再平方，計算variance(特徵值)
vars = (pca$sdev)^2
#計算每個主成分的解釋比例 = 各個主成分的特徵值/總特徵值
props = vars / sum(vars)

# 累加前n個主成分的解釋比例
cumulative.props = cumsum(props)
cumulative.props[5]
plot(cumulative.props)

top10.pca.eigenvector = pca$rotation[, 1:10]
pcaplot = function(i){
  dotchart(top10.pca.eigenvector[,i][order(top10.pca.eigenvector[,i],decreasing = FALSE)],
         main="Loading Plot for PC1")
}
pcaplot(2)
