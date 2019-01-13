modelF <- readRDS(paste0(fileDir,fileModel))
market<-c()

nowa <- forexA[nrow(forexA)-1,]
pred = predict(modelF,newdata=forexC[,!(names(forexC) %in% c("varpip"))])
pred2 <- as.numeric(tail(pred,1))

obj <- round(nowa$close + (round(pred2,1)/precision),log10(precision))
syrh <- nowa$high
syrl <- nowa$low
syrc <- nowa$close

### Plot results
nowc <- head(forexC,nrow(forexC)-1)
predc <- head(pred,length(pred)-1)

nowc$mov<-"B"
nowc$mov[nowc$varpip>=0]<-"A"
movpred<-rep("B",length(predc))
movpred[predc>=0]<-"A"
cm<-confusionMatrix(nowc$mov,movpred)
accuracy<-as.numeric(cm$overall['Accuracy'])

mse <- rmse(nowc$varpip, predc)
r2 <- cor(nowc$varpip, predc)^2

textmse <- paste0("RMSE=",round(mse,2))
textr2 <- paste0("R2=",format(r2, scientific=TRUE))
textaccu <- paste0("AC=",round(accuracy,4))
subtitle <- paste(textmse,textr2,sep = "|")
subtitle <- paste(subtitle,textaccu,sep = "|")

accFX<-data.frame("model"=fileModel,"mse"=mse,"r2"=r2,"acc"=accuracy)
plot(nowc$varpip,type='l',main = fileModel, sub = subtitle
     , xlab = "points", ylab = "pips")
lines(predc,col="red")
abline(h=0,col="blue", lwd=1, lty=2)

#####################################
### Calculo de inidcadores ADX y ATR
#####################################

dataFX00 <- forexA
dataFX00$id <- c(1:nrow(dataFX00))
period <- 18

### Obtener el rango anterior
dataFX01 <- dataFX00
previous <- dataFX01[,c("id","high","low","close")]
previous['id'] <- previous['id'] + 1
colnames(previous) <- paste0("n",colnames(previous))
dataFX01<-merge(dataFX01,previous,by.x = "id", by.y = "nid")

### Realizar operaciones
dataFX01$rango1 <- (dataFX01$high - dataFX01$low)
dataFX01$rango2 <- (dataFX01$high - dataFX01$nclose)
dataFX01$rango3 <- (dataFX01$nclose - dataFX01$low)
dataFX01$rangoV <- apply(dataFX01[,c("rango1","rango2","rango3")],1,max)
dataFX01$posdm <- (dataFX01$high - dataFX01$nhigh)
dataFX01$negdm <- (dataFX01$nlow - dataFX01$low)
dataFX01$pdm <- ifelse(dataFX01$posdm>dataFX01$negdm,dataFX01$posdm,0)
dataFX01$ndm <- ifelse(dataFX01$negdm>dataFX01$posdm,dataFX01$negdm,0)
dataFX01$pdm <- ifelse(dataFX01$pdm>0,dataFX01$pdm,0)
dataFX01$ndm <- ifelse(dataFX01$ndm>0,dataFX01$ndm,0)
dataFX02 <- dataFX01[,c("id","rangoV","pdm","ndm")]

### Cálculo de ATR
now    <- dataFX02
count  <- 1
while (count <= (period-1)) {
  previous <- dataFX02[,c("id","rangoV")]
  previous['id'] <- previous['id'] + count
  colnames(previous) <- paste0("n",colnames(previous))
  now <- merge(now,previous,by.x = "id", by.y = "nid")
  now[paste0(count,"rangoV")] <- now$nrangoV
  now$nrangoV <- NULL
  count <- count + 1
  rm(previous)
}

now$atr<-rowMeans(now[,grepl( "rangoV" , colnames( now ) )])
dataFX03<-now[,c("id","atr")]
rm(count,now)

### Cálculo de ATX
now    <- dataFX02
count  <- 1
while (count <= period) {
  previous <- dataFX02[,c("id","pdm","ndm","rangoV")]
  previous['id'] <- previous['id'] + count
  colnames(previous) <- paste0("n",colnames(previous))
  now <- merge(now,previous,by.x = "id", by.y = "nid")
  now[paste0(count,"rangoV")] <- now$nrangoV
  now[paste0(count,"pdm")] <- now$npdm
  now[paste0(count,"ndm")] <- now$nndm
  now$nrangoV <- NULL
  now$npdm <- NULL
  now$nndm <- NULL
  count <- count + 1
  rm(previous)
}

now$atr<-rowMeans(now[,grepl( "rangoV" , colnames( now ) )])

now$trsmooth00 <- rowSums(now[,grepl( "rangoV" , colnames( now ) )])
now$trsmooth01 <- (now$trsmooth00 - now$rangoV)
now$trsmooth01 <- now$trsmooth01/period
now$trsmooth <- now$trsmooth00 - now$trsmooth01

now$pdmsmooth00 <- rowSums(now[,grepl( "pdm" , colnames( now ) )])
now$pdmsmooth01 <- (now$pdmsmooth00 - now$pdm)
now$pdmsmooth01 <- now$pdmsmooth01/period
now$pdmsmooth <- now$pdmsmooth00 - now$pdmsmooth01

now$ndmsmooth00 <- rowSums(now[,grepl( "ndm" , colnames( now ) )])
now$ndmsmooth01 <- (now$ndmsmooth00 - now$ndm)
now$ndmsmooth01 <- now$ndmsmooth01/period
now$ndmsmooth <- now$ndmsmooth00 - now$ndmsmooth01

now$pdi<-now$pdmsmooth/now$trsmooth * 100
now$ndi<-now$ndmsmooth/now$trsmooth * 100

now$divdx <-now$pdi+now$ndi
now$numdx <-abs(now$pdi-now$ndi)
now$dx <- now$numdx/now$divdx * 100

dataFX04 <- now[,c("id","dx")]
rm(now)

### Calcular el promedio de dx
now    <- dataFX04
count  <- 1
while (count <= (period-1)) {
  previous <- dataFX04[,c("id","dx")]
  previous['id'] <- previous['id'] + count
  colnames(previous) <- paste0("n",colnames(previous))
  now <- merge(now,previous,by.x = "id", by.y = "nid")
  now[paste0(count,"dx")] <- now$ndx
  now$ndx <- NULL
  count <- count + 1
  rm(previous)
}
now$adx <-rowMeans(now[,grepl( "dx" , colnames( now ) )])
dataFX05<-now[,c("id","adx")]
rm(count,now)

###output
dataFX<-merge(dataFX05,dataFX03,by="id")
dataFX<-merge(dataFX00,dataFX,by="id")
dataFX$id<-NULL
rm(dataFX00,dataFX01,dataFX02,dataFX03,
   dataFX04,dataFX05,period)

################################################
### Obtener la predicción del valor objetivo
################################################

dataFX$pred<-tail(pred,nrow(dataFX))
dataFX$syr <-(1-(dataFX$adx/100))*dataFX$atr
dataFX$obj <- dataFX$open + (dataFX$pred/precision)
dataFX$syrhigh <- dataFX$open + dataFX$syr
dataFX$syrlow <- dataFX$open - dataFX$syr

minvalue <- min(dataFX[,c("syrhigh","syrlow","high","low","obj","close")])
maxvalue <- max(dataFX[,c("syrhigh","syrlow","high","low","obj","close")])
labels<-paste0(day(dataFX$date),month(dataFX$date))
labels<-paste(hour(dataFX$date),labels,sep="-")

plot(dataFX$close,type="l", xaxt="n",
     ylim=c(minvalue, maxvalue),
     xlab="", ylab="Precio",
     main = divisa)
abline(v=seq(1,nrow(dataFX),1),lty=2,col="lightgray") 
axis(1, at = seq(1, nrow(dataFX)),labels = labels, las=2)
points(dataFX$syrhigh,col="red",pch=19)
points(dataFX$high,col="red",pch=21)
points(dataFX$syrlow,col="green",pch=19)
points(dataFX$low,col="green",pch=21)
points(dataFX$open,col="black",cex=0.8,pch=16)
lines(dataFX$obj,col="red")

################################################
### Save results
################################################

market$prec<-precision
market$spread<-spread
market$divisa<-divisa

rm(obj,pred,syrh,syrl,labels,maxvalue,minvalue)
rm(fileDir,fileModel,pred2,movpred,cm,accuracy,textaccu)
rm(nowa,spread,textmse,textr2,subtitle,mse,r2,nowc,predc)
rm(forexA,forexB,forexC,modelF,precision,title,divisa,syrc)