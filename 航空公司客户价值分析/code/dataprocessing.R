##设置工作空间
setwd("/home/wenshuigen/code/case/航空公司客户价值分析/code/")
#数据读取
datafile=read.csv('../data/air_data.csv',he=T)

#丢弃票价为空的记录
library(dplyr)
delet_na <- datafile %>%
              filter(!(is.na(SUM_YR_1) | is.na(SUM_YR_2)))

#丢弃票价为0、平均折扣率不为0、总飞行公里数大于0的记录
deletdata <- delet_na %>%
               filter(!((SUM_YR_1 == 0 & delet_na$SUM_YR_2 == 0) *
                      (delet_na$avg_discount != 0) *
                      (delet_na$SEG_KM_SUM > 0)))

#根据LRFMC模型,选择与其相关的六个属性
cleanedfile=deletdata[,c('LOAD_TIME', 'FFP_DATE', 'LAST_TO_END', 'FLIGHT_COUNT', 'SEG_KM_SUM', 'avg_discount')]
write.csv(cleanedfile, file = '../data/cleanedfile.csv')
#数据变换之属性构造
zscoredata <- cleanedfile %>% 
                   transmute(L=as.numeric((as.Date(LOAD_TIME)-as.Date(FFP_DATE))/30),
                             R=LAST_TO_END,
                             f=FLIGHT_COUNT,
                             M=SEG_KM_SUM,
                             C=avg_discount)
#数据标准化
zscoreddata <- scale(zscoredata)
#数据写入
write.csv(zscoreddata, file = '../data/zscoreddata.csv', row.names = F)















