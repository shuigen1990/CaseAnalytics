dat <- read.csv('mydata.csv', header = T)
str(dat)
head(dat)
#将bedrooms,halls,subway和school字段转化为因子类型数据
dat[, c(2,3,6,7)] <- as.data.frame(apply(dat[, c(2,3,6,7)], 2, as.factor)) 
summary(dat)

# 描述性统计分析
## 在做分析之前，将CATE和floor字段的取值换成中文，以便作图输出美观
dict1 <- levels(dat$CATE)
dict2 <- c('朝阳','东城','丰台','海淀','石景山','西城')
dat$CATE <- as.factor(dict2[match(dat$CATE, dict1)])

dict3 <- levels(dat$floor)
dict4 <- c('高','低','中')
dat$floor <- as.factor(dict4[match(dat$floor, dict3)])

## 整个北京市场二手房房价分布图
dat$price <- dat$price/10000  #单位转化为万元
library(ggplot2)
ggplot(dat, aes(x=price)) + 
  geom_histogram(bins=13, fill='lightblue', color='black') + 
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), color='red')) +
  labs(x='单位面积房价(万元/平方米)', y='频数', title='2016年5月北京市二手房单价分布图') +
  scale_x_continuous(breaks=c(2:15))

dat[which.max(dat$price), ] #求出房价最高的观测值
dat[which.min(dat$price), ] #求出房价最低的观测值

## 六大城区对房价的分组箱线图
ggplot(dat, aes(x=CATE,y=price)) + 
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), color='red')) +
  labs(x='城区', y='单位面积房价(万元)', title='2016年5月北京市六大城区二手房单价分布图') +
  scale_x_discrete(limits=c('石景山','丰台','朝阳','东城','海淀','西城'))

## 卧室数对房价的分组箱线图
ggplot(dat, aes(x=bedrooms,y=price)) +
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), color='red')) +
  labs(x='卧室数', y='单位面积房价(万元)', title='卧室数对二手房单价的分布图') 

## 厅数对房价的分组箱线图
ggplot(dat, aes(x=halls,y=price)) +
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), color='red')) +
  labs(x='厅数', y='单位面积房价(万元)', title='厅数对二手房单价的分布图')

## 楼层对房价的分组箱线图
ggplot(dat, aes(x=floor,y=price)) +
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), color='red')) +
  labs(x='楼层', y='单位面积房价(万元)', title='楼层对二手房单价的分布图') + 
  scale_x_discrete(limits=c('低','中','高'))

## 是否临近地铁对房价的分组箱线图
ggplot(dat, aes(x=subway,y=price)) +
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), colour = 'red')) +
  labs(x='临近地铁', y='单位面积房价(万元)', title='临近地铁对二手房单价的分布图') +
  scale_x_discrete(labels = c('否', '是'))

## 是否学区房对房价的分组箱线图
ggplot(dat, aes(x=school,y=price)) +
  geom_boxplot(fill='lightblue') +
  theme(axis.text.x=element_text(size=rel(1.5)), axis.text.y=element_text(size=rel(1.5)),
        axis.title.x=element_text(size=rel(1.5)), axis.title.y=element_text(size=rel(1.5)),
        plot.title=element_text(size=rel(2), colour = 'red')) +
  labs(x='学区房', y='单位面积房价(万元)', title='学区房对二手房单价的分布图') +
  scale_x_discrete(labels = c('否', '是'))

# 线性回归分析预测二手房房价
modeldata <- dat[,1:8]
dummvar <- model.matrix(price~.,modeldata)
modeldata <- cbind(dummvar[,-1],modeldata$price)
colnames(modeldata) <- c('CATE_dc','CATE_ft','CATE_hd','CATE_sjs','CATE_sc','bedrooms2',
                         'bedrooms3','bedrooms4','bedrooms5','halls1','halls2','halls3',
                         'AREA','floor_high','floor_medium','subway','school','price')
modeldata <- as.data.frame(modeldata)
lm1.sol <- lm(price~., data=modeldata)
summary(lm1.sol)
par(mfrow=c(2,2))
plot(lm1.sol)


lm2.sol <- lm(log(price)~., data=modeldata)
summary(lm2.sol)
par(mfrow=c(2,2))
plot(lm2.sol)


lm1.step <- step(lm2.sol)
par(mfrow=c(2,2))
plot(lm1.step)

lm3.sol <- update(lm1.step, .~.+CATE_dc*school+CATE_ft*school+
                    CATE_hd*school+CATE_sjs*school+CATE_sc*school)
par(mfrow=c(2,2))
plot(lm3.sol)



lm2.step <- step(lm3.sol)
par(mfrow=c(2,2))
plot(lm2.step)



summary(lm2.step)
drop1(lm2.step)

lm.opt <- update(lm2.step,.~.-floor_medium,data=modeldata)
summary(lm.opt)
plot(lm.opt)


AIC(lm2.sol,lm1.step,lm3.sol,lm2.step,lm.opt)
BIC(lm2.sol,lm1.step,lm3.sol,lm2.step,lm.opt)

library(car)
vif(lm.opt)
vif(lm1.sol)
AIC(lm1.sol)













