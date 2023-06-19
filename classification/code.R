#对正负样本划分训练集和测试集
neg_4mer<-read.csv('./classification/neg_4mer.csv',header = FALSE)
pos_4mer<-read.csv('./classification/pos_4mer.csv',header = FALSE)
#划分训练集测试集并保存成文件

# 设置随机数种子以确保可复现性
set.seed(42)

# 计算划分比例
train_ratio <- 0.8  # 训练集占总数据集的比例

# 计算划分后的样本数
neg_train_size <- round(nrow(neg_4mer) * train_ratio)
pos_train_size <- round(nrow(pos_4mer) * train_ratio)

# 随机抽样获取训练集和测试集的索引
neg_train_index <- sample(nrow(neg_4mer), neg_train_size)
pos_train_index <- sample(nrow(pos_4mer), pos_train_size)

# 根据索引划分数据集
neg_train <- neg_4mer[neg_train_index, ]
pos_train <- pos_4mer[pos_train_index, ]
neg_test <- neg_4mer[-neg_train_index, ]
pos_test <- pos_4mer[-pos_train_index, ]

# 将训练集和测试集保存为txt文件
write.table(neg_train, file = "./classification/neg_train.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(pos_train, file = "./classification/pos_train.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(neg_test, file = "./classification/neg_test.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(pos_test, file = "./classification/pos_test.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

#####划分训练集验证集
neg_train=read.table(file = "./classification/neg_train.txt")
pos_train=read.table(file = "./classification/pos_train.txt")
neg_test=read.table(file = "./classification/neg_test.txt")
pos_test=read.table(file = "./classification/pos_test.txt")
# 对独立测试集生成标签
test=rbind(neg_test,pos_test)
test[,ncol(test)+1]=as.factor(c(rep(0,nrow(neg_test)),rep(1,nrow(pos_test))))
#对所有的训练集生成标签
train=rbind(neg_train,pos_train)
train[,ncol(train)+1]=as.factor(c(rep(0,nrow(neg_train)),rep(1,nrow(pos_train))))
#对训练集进行十次重复的十折交叉验证寻参

training_index=split(sample(1:nrow(train)),rep((1:10),length=nrow(train)))#生成训练集十折交叉验证每折的样本号
save(training_index,file = "./train.index.Rdata") 

#划分好x和y训练集、验证集的十折整合列表
x<-train[,-ncol(train)]
y<-matrix(,nrow = nrow(train),ncol=1)
y[,1]<-train[,ncol(train)]
y<-as.factor(y)
x.train<-list()
x.vali<-list()
y.train<-list()
y.vali<-list()
y.vali.total<-c()

for(i in 1:10){
  x.vali[[i]]<-x[training_index[[i]],]
  x.train[[i]]<-x[setdiff(1:nrow(train),training_index[[i]]),]
  y.vali[[i]]<-y[training_index[[i]]]
  y.train[[i]]<-y[setdiff(1:nrow(train),training_index[[i]])]
  y.vali.total<-c(y.vali.total,y.vali[[i]])
}

###################knn
library(pROC)
library(kknn)
k.list=c(20,30,40,50)
# k.list=c(5)
knn.assess<-matrix(,5,4)
rownames(knn.assess)<-c("auc","acc","recall","precision","F1")
colnames(knn.assess)<-c("k=5","k=10","k=15","k=20")
for(j in k.list){
  cat("k=",j)
  y.vali.pred.knn.total<-c()
  y.vali.prob.knn.total<-c()
  for(i in 1:10){
      print(paste('fold:',i))
      data<-x.train[[i]]
      data$label<-y.train[[i]]
      knn.fit <- kknn(label~.,data,x.vali[[i]],k=j,distance = 1)
      y.vali.pred.knn<- knn.fit[[1]]
      y.vali.pred.knn.total<-c(y.vali.pred.knn.total,y.vali.pred.knn)
      y.vali.prob.knn<-knn.fit[[6]][,2]
      y.vali.prob.knn.total<-c(y.vali.prob.knn.total,y.vali.prob.knn)
    }
    roc.knn<-roc(as.numeric(y.vali.total),y.vali.prob.knn.total)
    auc.knn<-roc.knn$auc
    confusionMatrix<-table(y.vali.pred.knn.total,y.vali.total)
    TP.freq<-confusionMatrix[4]
    FP.freq<-confusionMatrix[2]
    FN.freq<-confusionMatrix[3]
    TN.freq<-confusionMatrix[1]
    ACC.knn=(TP.freq+TN.freq)/length(y.vali.total)
    recall.knn=TP.freq/(TP.freq+FN.freq)
    precision.knn=TP.freq/(TP.freq+FP.freq)
    F1.knn=2*TP.freq/(length(y.vali.total)+TP.freq-TN.freq)
    knn.assess[1,j/5-3]<-auc.knn
    knn.assess[2,j/5-3]<-ACC.knn
    knn.assess[3,j/5-3]<-recall.knn
    knn.assess[4,j/5-3]<-precision.knn
    knn.assess[5,j/5-3]<-F1.knn
}

###################SVM
library(pROC)
library(e1071)
dataset=train
colnames(dataset)[ncol(dataset)]='labels'
# 设置参数范围
gamma.list=c(0.00001,0.0005,0.005,0.01,0.1)
cost.list=c(0.001,0.01,0.1,1,10)

  
# 定义参数网格搜索
svm.assess<-matrix(,5,25)
rownames(svm.assess)<-c("auc","acc","recall","precision","F1")
counts=0
for(gamma in gamma.list){
  for(cost in cost.list){
  counts=counts+1
  print(paste('gamma=',gamma))
  print(paste('cost=',cost))
  y.vali.pred.svm.total<-c()
  y.vali.prob.svm.total<-c()
  for(i in 1:10){
      print(paste('fold:',i))
      data<-x.train[[i]]
      data$label<-y.train[[i]]
      svm.fit <- svm(label ~.,data=data, kernel = "radial", cost = cost, gamma = gamma,probability=TRUE)
      y.vali.pred.svm<-predict(svm.fit,newdata=as.matrix(x.vali[[i]]),probability=TRUE)
      y.vali.pred.label<-predict(svm.fit,newdata=as.matrix(x.vali[[i]]))
      prob.svm<-attributes(y.vali.pred.svm)$probabilities[,2]
      y.vali.prob.svm.total<-c(y.vali.prob.svm.total,prob.svm)
      y.vali.pred.svm.total<-c(y.vali.pred.svm.total,y.vali.pred.label)
    }
    roc.svm<-roc(as.numeric(y.vali.total),y.vali.prob.svm.total)
    auc.svm<-roc.svm$auc
    confusionMatrix<-table(y.vali.pred.svm.total,y.vali.total)
    TP.freq<-confusionMatrix[4]
    FP.freq<-confusionMatrix[2]
    FN.freq<-confusionMatrix[3]
    TN.freq<-confusionMatrix[1]
    ACC.svm=(TP.freq+TN.freq)/length(y.vali.total)
    recall.svm=TP.freq/(TP.freq+FN.freq)
    precision.svm=TP.freq/(TP.freq+FP.freq)
    F1.svm=2*TP.freq/(length(y.vali.total)+TP.freq-TN.freq)
    svm.assess[1,counts]<-auc.svm
    svm.assess[2,counts]<-ACC.svm
    svm.assess[3,counts]<-recall.svm
    svm.assess[4,counts]<-precision.svm
    svm.assess[5,counts]<-F1.svm
    print(svm.assess)
  }
}

########randomForest
library(pROC)
library(randomForest)
dataset=train
colnames(dataset)[ncol(dataset)]='labels'
# 设置参数范围
ntree.grid <- seq(50, 250, by = 50)
mtry.grid <- seq(2, ncol(train), by = 40)
  
# 定义参数网格搜索
randomForest.assess<-matrix(,5,35)
rownames(randomForest.assess)<-c("auc","acc","recall","precision","F1")
counts=0
for(ntree in ntree.grid){
  for(mtry in mtry.grid){
  counts=counts+1
  print(paste('ntree=',ntree))
  print(paste('mtry=',mtry))
y.vali.pred.randomForest.total<-c()
y.vali.prob.randomForest.total<-c()
  for(i in 1:10){
      print(paste('fold:',i))
      data<-x.train[[i]]
      data$label<-y.train[[i]]
      RF.fit <- randomForest(label~.,data=data,ntree=ntree,mtry=mtry,importance=TRUE,proximity=TRUE)
      y.vali.pred.rf<-predict(RF.fit,newdata=as.matrix(x.vali[[i]]),type='prob')
      y.vali.pred.label<-predict(RF.fit,newdata=as.matrix(x.vali[[i]]))
      prob.rf<-y.vali.pred.rf[,2]
      y.vali.prob.randomForest.total<-c(y.vali.prob.randomForest.total,prob.rf)
      y.vali.pred.randomForest.total<-c(y.vali.pred.randomForest.total,y.vali.pred.label)
    }
    roc.randomForest<-roc(as.numeric(y.vali.total),y.vali.prob.randomForest.total)
    auc.randomForest<-roc.randomForest$auc
    confusionMatrix<-table(y.vali.pred.randomForest.total,y.vali.total)
    TP.freq<-confusionMatrix[4]
    FP.freq<-confusionMatrix[2]
    FN.freq<-confusionMatrix[3]
    TN.freq<-confusionMatrix[1]
    ACC.randomForest=(TP.freq+TN.freq)/length(y.vali.total)
    recall.randomForest=TP.freq/(TP.freq+FN.freq)
    precision.randomForest=TP.freq/(TP.freq+FP.freq)
    F1.randomForest=2*TP.freq/(length(y.vali.total)+TP.freq-TN.freq)
    randomForest.assess[1,counts]<-auc.randomForest
    randomForest.assess[2,counts]<-ACC.randomForest
    randomForest.assess[3,counts]<-recall.randomForest
    randomForest.assess[4,counts]<-precision.randomForest
    randomForest.assess[5,counts]<-F1.randomForest
    print(randomForest.assess)
  }
}

############logistic
library(pROC)
dataset=train
colnames(dataset)[ncol(dataset)]='labels'
# 设置参数范围
y.vali.prob.logistic.total<-c()
# 定义参数网格搜索
logistic.assess<-matrix(,5,1)
rownames(logistic.assess)<-c("auc","acc","recall","precision","F1")
for(i in 1:10){
  print(paste('fold:',i))
  data<-x.train[[i]]
  data$label<-y.train[[i]]
  glm.fit <- glm(label~.,data=data,family=binomial,control=list(maxit=100))
  y.vali.pred.prob<-predict(glm.fit,newdata=x.vali[[i]], type="response")
  y.vali.prob.logistic.total<-c(y.vali.prob.logistic.total,y.vali.pred.prob)
}
roc.logistic<-roc(as.numeric(y.vali.total),y.vali.prob.logistic.total)
auc.logistic<-roc.logistic$auc
y.vali.pred.logistic.total=rep("0",length(y.vali.prob.logistic.total))
y.vali.pred.logistic.total[y.vali.prob.logistic.total>0.5]="1"
confusionMatrix<-table(y.vali.pred.logistic.total,y.vali.total)
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.logistic=(TP.freq+TN.freq)/length(y.vali.total)
recall.logistic=TP.freq/(TP.freq+FN.freq)
precision.logistic=TP.freq/(TP.freq+FP.freq)
F1.logistic=2*TP.freq/(length(y.vali.total)+TP.freq-TN.freq)
logistic.assess[1,1]<-auc.logistic
logistic.assess[2,1]<-ACC.logistic
logistic.assess[3,1]<-recall.logistic
logistic.assess[4,1]<-precision.logistic
logistic.assess[5,1]<-F1.logistic


#使用SVM最优参数进行测试集
data<-train[,-ncol(train)]
data$label<-train[,ncol(train)]
svm.fit <- svm(label ~.,data=data, kernel = "radial", cost = 10, gamma = 0.005,probability=TRUE)
y.test.pred.svm<-predict(svm.fit,newdata=as.matrix(test[,-ncol(test)]),probability=TRUE)
y.test.pred.label<-predict(svm.fit,newdata=as.matrix(test[,-ncol(test)]))
prob.svm<-attributes(y.test.pred.svm)$probabilities[,2]
roc.svm<-roc(as.numeric(test[,ncol(test)]),prob.svm)
auc.svm<-roc.svm$auc
confusionMatrix<-table(y.test.pred.label,test[,ncol(test)])
TP.freq<-confusionMatrix[4]
FP.freq<-confusionMatrix[2]
FN.freq<-confusionMatrix[3]
TN.freq<-confusionMatrix[1]
ACC.svm=(TP.freq+TN.freq)/length(test[,ncol(test)])
recall.svm=TP.freq/(TP.freq+FN.freq)
precision.svm=TP.freq/(TP.freq+FP.freq)
F1.svm=2*TP.freq/(length(test[,ncol(test)])+TP.freq-TN.freq)
print(auc.svm)
print(ACC.svm)
print(recall.svm)
print(precision.svm)
print(F1.svm)