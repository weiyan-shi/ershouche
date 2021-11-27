
data=read.csv(file=file.choose(),header = T)
data=data[,-1]#去除第一列数据编号
names(data)#变量
head(data)

#############################################################
#############################################################
#数据描述：

library(ggplot2)
#因变量:保值率(密度核函数图)
ggplot(data,aes(x=hedge.ratio))+geom_density(fill="red",alpha=.5)+
labs(title="保值率核密度函数图",x="保值率", y="密度") +
  theme(plot.title = element_text(hjust = 0.5))


#自变量：上牌时间(密度核函数图)
ggplot(data, aes(x=time, fill=location))+geom_density(alpha=.3)+
  labs(title="上牌时间核密度函数图",x="上牌时间", y="密度") +
  theme(plot.title = element_text(hjust = 0.5))
#二手车3-6年内卖出居多。


#自变量：排量(箱线图和小提琴图)
ggplot(data, aes(x=group2, y=hedge.ratio)) + geom_boxplot()+
  geom_violin(scale="count",fill="lightblue",alpha=.3)+
  labs(title="不同车的保值率",x="排量", y="保值率") +
  theme(plot.title = element_text(hjust = 0.5))


#自变量：品牌属地(箱线图和小提琴图)
ggplot(data, aes(x=location, y=hedge.ratio)) + geom_boxplot()+
  geom_violin(scale="count",fill="lightblue",alpha=.3)+
  stat_summary(fun.y = "mean",geom="point",shape=23,size=3,fill="white")+
  labs(title="不同车的保值率",x="品牌属地", y="保值率") +
  theme(plot.title = element_text(hjust = 0.5))


#自变量：款式(箱线图和小提琴图)
ggplot(data, aes(x=style, y=hedge.ratio)) + geom_boxplot()+
  geom_violin(scale="count",fill="lightblue",alpha=.3)+
  stat_summary(fun.y = "mean",geom="point",shape=23,size=3,fill="white")+
  labs(title="不同车的保值率",x="款式", y="保值率") +
  theme(plot.title = element_text(hjust = 0.5))


#自变量：手动/自动(箱线图和小提琴图)
ggplot(data, aes(x=gear, y=hedge.ratio)) + geom_boxplot()+
  geom_violin(scale="count",fill="lightblue",alpha=.3)+
  labs(title="不同车的保值率",x="手动/自动", y="保值率") +
  theme(plot.title = element_text(hjust = 0.5))


#自变量：行驶里程(箱线图和小提琴图)
ggplot(data, aes(x=group1, y=hedge.ratio)) + geom_boxplot()+
  geom_violin(scale="count",fill="lightblue",alpha=.3)+
  labs(title="不同车的保值率",x="里程", y="保值率") +
  theme(plot.title = element_text(hjust = 0.5))


#################################################################
#################################################################
head(data)
data=data[,-c(7,8,10)]#删除现价、原价、里程分组
#线性建模
data.lm=lm(hedge.ratio~.,data)
summary(data.lm)
anova(data.lm)

#模型诊断
#多重共线性
library(car)
vif(data.lm,digits = 3)

#绘制残差图检验模型基本假定
par(mfrow=c(1,2));qqnorm(data.lm$res);qqline(data.lm$res)
plot(data.lm$res~data.lm$fit,main="Residuals vs Fiited");abline(h=0,lty=2)

#异常点检验
#car程序包中outlierTest()函数求最大标准化残差绝对值Bonferronni调整后的p值检验异常点存在
outlierTest(data.lm)

#数据集帽子值分布
#需要点击图上偏离较大的点，然后按Esc键退出编辑！！！
par(mfrow=c(1,1))
hat.plot=function(data.lm){
  p = length(coefficients(data.lm))
  n = length(fitted(data.lm))
  plot(hatvalues(data.lm), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(data.lm), names(hatvalues(data.lm)))
}
hat.plot(data.lm)

#cook距离图
cutoff = 4/(nrow(data) - length(data.lm$coefficients) - 2)
plot(data.lm, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

#删除异常点重新建模
data=data[-c(26,101,111,149,218,497,737,1502),]
data.lm=lm(hedge.ratio~.,data)
summary(data.lm)
anova(data.lm)

#加入交互项建模
data.lm2=lm(hedge.ratio~Vehicle.brand+location+style+gear+time*kilometres+group2,data)
summary(data.lm2)
anova(data.lm2)

#模型诊断:
#绘制残差图检验模型基本假定
par(mfrow=c(1,2));qqnorm(data.lm2$res);qqline(data.lm2$res)
plot(data.lm2$res~data.lm2$fit,main="Residuals vs Fiited");abline(h=0,lty=2)
#cook距离图
par(mfrow=c(1,1))
cutoff = 4/(nrow(data) - length(data.lm2$coefficients) - 2)
plot(data.lm2, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

#交叉验证评价模型泛化能力
#CV函数随机把数据的下标分成Z份以做交叉验证时用
CV=function(n,Z=10,seed=888){
  z=rep(1:Z,ceiling(n/Z))[1:n]
  set.seed(seed);z=sample(z,n)
  mm=list();for(i in 1:Z) mm[[i]]=(1:n)[z==i]
  return(mm)}
D=7;Z=10;n=nrow(data);mm=CV(n,Z)#用CV函数把数据下标随机分成10份，mm存储了10个下标集，D说明因变量是第7个变量

library(e1071)
library(rpart)
library(randomForest)
MSE=rep(0,Z)#建立一个向量存储结果
set.seed(1111);for(i in 1:Z)#对每一组训练集和测试集做1次，共Z=10次
m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
# a=lm(hedge.ratio~Vehicle.brand+location+style+gear+time+kilometres+group2,data)#线性回归，这里[-m]为训练集下标集合
# a=svm(hedge.ratio~Vehicle.brand+location+style+gear+time+kilometres+group2,data,type='eps-regression')# svm
# a=rpart(hedge.ratio~Vehicle.brand+location+style+gear+time+kilometres+group2,data) # 回归树
a=randomForest(hedge.ratio~Vehicle.brand+location+style+gear+time+kilometres+group2,data)
summary(a)
res= (data[D]-list(predict(a,data)))^2#求测试集的NMSE
mse=sqrt(sum(res))/length(data)# mean(MSE)#测试集的NMSE





#加入交互项模型的交叉验证
MSE=rep(0,Z)
set.seed(1111);for(i in 1:Z)
{m=mm[[i]];M=mean((data[m,D]-mean(data[m,D]))^2)
# a=lm(hedge.ratio~Vehicle.brand+location+style+gear+time*kilometres+group2,data[-m,])
MSE[i]=mean((data[m,D]-predict(a,data[m,]))^2)/M}
mean(MSE)


#变量相对重要性:
library(randomForest)
set.seed(1010)
a=randomForest(hedge.ratio~.,data)
names(a)
#变量相对重要性
par(mfrow=c(1,1))
for(i in 1){
  barplot(a$importance[,i],horiz = T,xlim=c(0,20),cex.axis =1,cex.names = .7)
  title(colnames(a$importance)[i])
}

#预测：
x_new=data[10,]
x_new$hedge.ratio #实际值
predict(data.lm,x_new)  #预测值


m=c(0.375,0.339,0.330,0.209)

barplot(m,main="不同回归方法的MSE柱状图",col=c("#ED1C24","#FF5000","#22B14C","#FFC90E"),names.arg=c("回归树","线性回归","svm","随机森林"),family='GB1')
