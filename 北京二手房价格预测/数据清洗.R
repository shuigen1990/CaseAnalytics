setwd('F:/case/北京二手房/')
dat <- read.csv('二手房.csv', header=T)
head(dat)
str(dat)
dat[, c(2,3,6,7)] <- as.data.frame(apply(dat[, c(2,3,6,7)], 2, as.factor))
summary(dat) #数据概述可以看出，在考虑现实情况下很多字段存在异常值

## 数据预处理

apply(dat[,c(1,2,3,5)], 2, table)

### 统计各城区观测数
table(dat$CATE) #分布较均匀，石景山相对少些

### 卧室数分布
table(dat$bedrooms)
dat <- dat[!(dat$bedrooms %in% c(0,6,7,8,9)), ]  #卧室数0,6,7,8,9的观测很少，考虑删除

### 客厅数分布
table(dat$halls)
dat <- dat[!(dat$halls %in% c(4,9)), ] #客厅数4,9的观测很少，考虑删除

### 房屋面积分布
quantile(dat$AREA) #从数据可以看出，有异常值出现，不太符合现实情况
quantile(dat$AREA, probs = c(0.01,0.99)) #查看1%和99%的分位数
dat <- dat[dat$AREA>30 & dat$AREA<300, ]

### 楼层分布
table(dat$floor)
dat <- dat[dat$floor != 'basement ', ] #地下室的观测数少，我们可以做删除处理

### 是否临近地铁
prop.table(table(dat$subway)) #82.9%的观测临近地铁

### 是否为学区房
prop.table(table(dat$school)) #30.1%的观测是学区房

### 房价分布
quantile(dat$price) #有许多异常值
quantile(dat$price, probs = c(0.001,0.999)) #查看%1和99%的分位数
dat <- dat[dat$price>13500 & dat$price<150000, ]

### 北京的经纬度取值范围：
dat <- dat[dat$LONG>=115.5 & dat$LONG <= 117.5, ] #经度115.5~117.5
dat <- dat[dat$LAT>=39.5 & dat$LAT <= 41, ] #纬度39.5~41

### 保存清洗后的数据
write.csv(dat, file='mydata.csv',row.names = F)








