y<-read.csv('./y.csv',header = TRUE)
x<-read.csv('./x-2k_sequence5mer.csv',header = FALSE)
no_n_index<-read.csv('./no_N_index.csv',header=FALSE)

#划分训练集测试集并保存成文件

# 设置随机数种子以确保可复现性
set.seed(42)

# 计算划分比例
train_ratio <- 0.8  # 训练集占总数据集的比例

# 计算划分后的样本数
train_size <- round(nrow(dataset) * train_ratio)

# 随机抽样获取训练集和测试集的索引
train_index <- sample(nrow(dataset), train_size)

# 根据索引划分数据集
train <- dataset[train_index, ]
test <- dataset[-train_index,]

# 将训练集和测试集保存为txt文件
write.table(train, file = "./train.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
write.table(test, file = "./test.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

#####划分训练集验证集
train=read.table(file = "./train.txt")
test=read.table(file = "./test.txt")

#对训练集进行十次重复的十折交叉验证寻参
training_index=split(sample(1:nrow(train)),rep((1:10),length=nrow(train)))#生成训练集十折交叉验证每折的样本号
save(training_index,file = "./train.index.Rdata") 

#划分好x和y训练集、验证集的十折整合列表
x<-train[,-ncol(train)]
y<-matrix(,nrow = nrow(train),ncol=1)
y[,1]<-train[,ncol(train)]
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


##############Lasso
#train上面十折交叉验证，然后对test进行预测测试
library(glmnet)
Lasso.cv<-cv.glmnet(as.matrix(train[,-ncol(train)]),as.numeric(train[,ncol(train)]))
bestlambda<-Lasso.cv$lambda.min
Lasso.model<-Lasso.cv$glmnet.fit
y.test.pred.Lasso<-predict(Lasso.model,newx=as.matrix(test[,-ncol(test)]),s=bestlambda)
PCC.test.cv=cor(y.test.pred.Lasso,test[,ncol(test)], method = "pearson")
print(PCC.test.cv)



#################ridge
#train上面十折交叉验证，然后对test进行预测测试
library(glmnet)
ridge.cv<-cv.glmnet(as.matrix(train[,-ncol(train)]),as.numeric(train[,ncol(train)]),alpha=0)
bestlambda<-ridge.cv$lambda.min
ridge.model<-ridge.cv$glmnet.fit
y.test.pred.ridge<-predict(ridge.model,newx=as.matrix(test[,-ncol(test)]),s=bestlambda)
PCC.test.cv=cor(y.test.pred.ridge,test[,ncol(test)], method = "pearson")
print(PCC.test.cv)


########################Random Forest
library(randomForest)
ntree.grid <- seq(50, 500, by = 50)
mtry.grid <- seq(2, ncol(train), by = 30)
# 定义参数网格搜索
randomForest.assess<-matrix(,1,35)
rownames(randomForest.assess)<-c("PCC")
counts=0
for(ntree in ntree.grid){
    for(mtry in mtry.grid){
        counts=counts+1
        print(paste('ntree=',ntree))
        print(paste('mtry=',mtry))
        y.vali.pred.randomForest.total<-c()
        for(i in 1:10)
        {
            print(paste('fold:',i))
            data<-x.train[[i]]
            data$label<-y.train[[i]]
            RF.fit <- randomForest(as.matrix(x.train[[i]]),as.numeric(y.train[[i]]),ntree=ntree,mtry=mtry)
            y.vali.pred.rf<-predict(RF.fit,x.vali[[i]])
            y.vali.pred.randomForest.total<-c(y.vali.pred.randomForest.total, y.vali.pred.rf)
        }
        randomForest.assess[1,counts]<-cor( y.vali.pred.randomForest.total,y.vali.total, method = "pearson")
        print(randomForest.assess)
    }
}


#用最优超参数在测试集上预测
randomForest.fit <- randomForest(as.matrix(train[,-ncol(train)]),as.numeric(train[,ncol(train)]),ntree=500,mtry=32)
y.test.pred.randomForest<-predict(randomForest.fit,as.matrix(test[,-ncol(test)]))
test_PCC=cor(y.test.pred.randomForest,test[,ncol(test)], method = "pearson")


#########################SVR
library(pROC)
library(e1071)
dataset=train
colnames(dataset)[ncol(dataset)]='labels'
# 设置参数范围
gamma.list=c(0.0001,0.001,0.01,0.1,1,10,100)
cost.list=c(0.001,0.01,0.1,1,10)

  
# 定义参数网格搜索
svr.assess<-matrix(,1,25)
rownames(svr.assess)<-c("PCC")
counts=0
for(gamma in gamma.list){
  for(cost in cost.list){
    counts=counts+1
    print(paste('gamma=',gamma))
    print(paste('cost=',cost))
    y.vali.pred.svr.total<-c()
    for(i in 1:10)
    {
        print(paste('fold:',i))
        data<-x.train[[i]]
        data$label<-y.train[[i]]
        svr.fit <- svm(as.matrix(x.train[[i]]),as.numeric(y.train[[i]]),gamma=gamma,cost=cost)
        y.vali.pred.svr<-predict(svr.fit,as.matrix(x.vali[[i]]))
        y.vali.pred.svr.total<-c(y.vali.pred.svr.total, y.vali.pred.svr)
    }
    svr.assess[1,counts]<-cor( y.vali.pred.svr.total,y.vali.total, method = "pearson")
    print(svr.assess)
  }
}

#测试集上测试
svr.fit <- svm(as.matrix(train[,-ncol(train)]),as.numeric(train[,ncol(train)]),cost=1,gamma=0.001)
y.test.pred.svr<-predict(svr.fit,as.matrix(test[,-ncol(test)]))
test_PCC=cor(y.test.pred.svr,test[,ncol(test)], method = "pearson")
