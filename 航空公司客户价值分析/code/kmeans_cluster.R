##设置工作空间
setwd("/home/wenshuigen/code/case/航空公司客户价值分析/code/")
#数据读取
inputfile <- read.csv('../data/zscoreddata.csv',he=T)

#聚类分析
result=kmeans(inputfile,5)

#结果输出
type=result$cluster
table(type)#查看类别分布
centervec=result$center

#客户价值分析可视化
library(ggplot2)
library(tidyr)
library(dplyr)
rownames(centervec) <- c('客户群1', '客户群2', '客户群3', '客户群4', '客户群5')
centervec <- as.data.frame(centervec)
centervec$客户群 <- rownames(centervec)
centervec <- centervec[, c(6,1:5)]
data <- centervec %>% gather(LRFMC, 聚类中心值, -客户群)

ggplot(data, aes(x=客户群, y=聚类中心值, fill=LRFMC))　+
    geom_bar(position = 'dodge', stat = 'identity') +
    theme(axis.text.x = element_text(size = rel(1.5)), axis.text.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)), axis.title.y = element_text(size = rel(1.5)),
          legend.title = element_text(size = rel(1.5)), legend.text = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(2))) +
    ggtitle('聚类结果可视化分析') 
    
