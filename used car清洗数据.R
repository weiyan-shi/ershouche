
dat1=read.csv(file=file.choose(),header = T,na.strings = c("NA",""))
dat0=dat1       #取dat0，方便后面与原数据比较。
names(dat0)     #查看各变量名。
#给变量重新命英文名:
names(dat0)=c("Vehicle.brand","brand","location","style","type","emission",
              "gear","model","time","kilometres","price","original.price",
              "hedge.ratio","group1","group2")
summary(dat0) #缺失值在变量type,emission,gear,model中。


#############################################################
#############################################################
#数据清洗：

#汽车品牌，品牌(Vehickle.brand & brand)：
table(dat0$Vehicle.brand)    #查看各汽车品牌卖出车的数量。
dat0=dat0[,-2]  #知道汽车品牌就知道了品牌的归属（例：宝马5、6系均属宝马）。
#汽车厂商按频数取排名前十的品牌，其余品牌替换为其他
a=sort(table(dat0$Vehicle.brand),decreasing = T)#品牌按频数降序排列
a[1:10]#显示排名前十的厂商
attach(dat0)
b=which(Vehicle.brand!="宝马5系"& Vehicle.brand!="大众迈腾"& Vehicle.brand!="福特福克斯"
      & Vehicle.brand!="大众速腾"& Vehicle.brand!="奥迪A6L"& Vehicle.brand!="大众高尔夫"
      & Vehicle.brand!="宝马3系"& Vehicle.brand!="奔驰C级"& Vehicle.brand!="大众途观"
      & Vehicle.brand!="福特蒙迪欧")
detach(dat0)
dat0[,1]=as.character(dat0[,1])#因子型转换为字符串类型
dat0[b,1]="其他"#其余品牌替换为其他
dat0[,1]=as.factor(dat0[,1])
summary(dat0$Vehicle.brand)

#品牌属地(location)：
table(dat0$location)

#款式(style)：
table(dat0$style)
levels(dat0$style)  #共12款式类型。

#车型,版型(type & model)：
table(dat0$type)
a=which(is.na(dat0$type)==T)
(p=length(a)/nrow(dat0))    #查看缺失比例
dat0=dat0[,-4]#根据车辆品牌和款式是可以确定车型的，并且缺失很多数据，故删除。
dat0=dat0[,-6]  #知道车辆品牌和款式就知道了品牌的版型，故删去。

#排量(emission)：
table(dat0$emission)
levels(dat0$emission)#共79种类型，T涡轮增压;L不带有涡轮增压;FSIs是燃油分层喷射;SIDI直喷涡轮增压。
summary(dat0)    #共有7个缺失值。
dat0=na.omit(dat0) #删除缺失值。

#手动/自动(gear)：
table(dat0$gear)
levels(dat0$gear)#实际上双离合和无极均属于自动。
a=which(dat0$gear=="双离合");b=which(dat0$gear=="无级")
dat0[,5]=as.character(dat0[,5])       #因子型转换为字符串类型。
dat0[a,5]="自动";dat0[b,5]="自动"     #把“双离合”和“无极”替换为“自动”。
dat0[,5]=as.factor(dat0[,5])
summary(dat0$gear)

#上牌时间(time)：
head(sort(dat0$time,decreasing=T)) #查看最近卖出二手车的时间。
#自编函数计算上牌时间到2016年底过了多少个月(w为数据集，D为上牌时间所在列)
Months=function(w,D){
enddate=as.Date("2016-12-31")
n=length(w[,D])
m=vector()
for (i in 1:n){
  date=dat0[i,D]
  startdate=as.Date(date)
  a=difftime(enddate, startdate, units="days")
  b=as.numeric(a);b=b/30
  m[i]=b}
return(m)}
M=Months(dat0,6)
dat0[,6]=M
summary(dat0$time)

#保值率(hedge.ratio):
summary(dat0$hedge.ratio)

#里程分组(group1)：
table(dat0$group1)

#排量分组(group2):
table(dat0$group2)
levels(dat0$group2)  #共5个分组。
dat0=dat0[,-4]  #由于排量分组的水平只有5个，而排量的水平有79个，用排量分组更简洁。
summary(dat0)

#查看删除的观测条数和比例：
diff=dim(dat1)[1]-dim(dat0)[1]
round(100*diff/dim(dat1)[1],2)
head(dat0)

#保存清洗过的数据集：
write.csv(dat0,file="mydata.csv")

