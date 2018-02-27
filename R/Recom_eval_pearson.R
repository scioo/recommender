library('recommenderlab')
library('reshape')
library('data.table')
library('bigmemory')
library('pryr')
library('biganalytics')
library('reshape')
library('SlopeOne')  
#library('SVDApproximation')
library('RPostgreSQL')
library('PRROC')
library(bigtabulate)
library('onehot')

library(doMC)


registerDoMC(cores = 3)



#print('########## Set work path ##########' )
#setwd("D:/Projetos/Scioo/PagueMenos/dataset")
#getwd()

print('########## Preambules ##########' )
mem_used()
gc(reset=TRUE)
rm(list=ls())

print('########## Reading matrix ##########' )
matriz<-read.csv('matriz_total2.csv',header=FALSE)
head(matriz)
#cliente<-read.csv('cliente.csv',header=FALSE)
#matriz <- matriz[-1,1:3]
#options(bigmemory.allow.dimnames=TRUE)
#matriz <- read.big.matrix("./matriz.csv",type ="integer", header = FALSE) 

#drv <- dbDriver("PostgreSQL")
#con<-dbConnect(drv, user='', password='', dbname='', host='')


#cliente_M<-as.matrix(as.data.frame(lapply(cliente, as.numeric)))

#cliente<-onehot(cliente,addNA=FALSE)
#print(head(cliente_M))
#cl <- kmeans(na.omit(cliente_M), 10)
#out <- cbind(na.omit(cliente), clusterNum = cl$cluster)
#var.names<-tolower(colnames(out))
#colnames(out)<-var.names
#print(head(out))

#dbWriteTable(con, name = "cliente_cluster", value =out, overwrite=TRUE, row.names = FALSE)
  
#write.table(cliente,file="clusters.csv",sep=",",eol="\r\n")




#colnames(matriz)<-c('V1','V2','V3')
#print(head(matriz))
#print(colnames(matriz))


print('########## Formating CPF ##########')
matriz$V1<-gsub( "-", "", as.character(matriz$V1))
matriz$V1<-gsub( "\\.", "", as.character(matriz$V1))
#matriz$V1 <- as.numeric(as.character(matriz$V1))
#matriz$V2 <- as.numeric(as.character(matriz$V2))
matriz$V3 <- as.numeric(as.character(matriz$V3))
#print(head(matriz))

print('########## Casting to a data table ##########')
matriz_old<-matriz
colnames(matriz_old) <- c("user_id", "item_id", "rating")
matriz<-data.table::dcast(matriz, V1 ~ V2, sum)
#matriz_m <- bigtable(matriz,c(1,2,3))
#matriz_m<-cast(matriz_m, V1 ~ V2, sum)
#print(head(matriz))
rownames(matriz) <- matriz[,1]


print('########## Seting customer and item matrixes ##########')
matriz_cpf<-matriz$V1
matriz_produtos<- matriz[,-1]
print(typeof(matriz_produtos))
#########################################################
###   UBCF Pearson based recommendations
###
###
#########################################################
print('########## Create and sort "ratings" array ##########')
ratings <- matriz_produtos

print("Criando matriz produtos")

#print(head(ratings))

#R<-as(data.matrix(ratings), "itemMatrix")


#ratings[is.na(ratings)] <- 0
m<-data.matrix(ratings)
s<-as(m, "sparseMatrix")
#head(s)

r<- new("realRatingMatrix", data =s) 

#r <- new("binaryRatingMatrix", data = R)

r1<-r[rowCounts(r)>5]

head(as(r1,"matrix"))

print('Normalizacao ')

e <- evaluationScheme(r1, method="split", train=0.8, given=-5)

model <- Recommender(getData(e, "train"), "UBCF")
prediction <- predict(model, r1, type="ratings")


result<-as(prediction, "matrix")

head(result)

write.table(result,file="ubcf_pearson_2.csv",sep=",",eol="\r\n")


model1 <- Recommender(getData(e, "train"), "IBCF")

prediction1 <- predict(model1, r1, type="ratings")

result1<-as(prediction1, "matrix")

head(result1)

write.table(result1,file="ibcf_pearson_2.csv",sep=",",eol="\r\n")

model2 <- Recommender(getData(e, "train"), "POPULAR")

prediction2 <- predict(model2, r1,type="ratings")

result2<-as(prediction2, "matrix")

head(result2)

write.table(result2,file="popular_pearson_2.csv",sep=",",eol="\r\n")




e<- evaluationScheme(r1, method="split", train=0.9, given=5,goodRating=2)
#e<-evaluationScheme(r1,method='cross-validation',k=5,given=10)



train<-getData(e, "train")

algorithms.bin<-list(POPULAR=list(name="POPULAR",param=NULL),
                     RAND=list(name="RANDOM"),
		     SVD=list(name="SVD"),
		     ALSE=list(name="ALS_explicit",para= list( normalize=NULL, lambda=0.1, n_factors=200,n_iterations=10, seed = 1234, verbose = TRUE)),	
		     ALSI= list(name="ALS_implicit",param = list(lambda=0.1, alpha = 0.5, n_factors=10,n_iterations=10, seed = 1234, verbose = TRUE)),
		     PEARSON= list(name="UBCF", param=list(method="Pearson")),
		     COSINE= list(name="UBCF", param=list(method="Cosine")),
		     JACCARD= list(name="UBCF", param=list(method="Jaccard")),
UBCF=list(name="UBCF"),IBCF=list(name="IBCF"),RERECOMMEND=list(name="RERECOMMEND")) 
results.bin<-evaluate(e,algorithms.bin,type = "topNList", n=c(1, 3, 5, 10, 15, 20,30,40,50,100,200,300,400))


print(avg(results.bin))


jpeg('ComparacacaoR.jpg')
plot(results.bin,legend="topleft",annotate=T)
dev.off()


recom <- HybridRecommender(
Recommender(train, method = "POPULAR"),
Recommender(train, method = "UBCF"),
Recommender(train, method = "IBCF"),
weights = c(.5, .4, .3)
)


p1 <- predict(recom, getData(e, "known"), type="ratings")
ac<-calcPredictionAccuracy(p1, getData(e, "unknown")),


print(ac)







