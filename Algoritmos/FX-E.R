#####################################
### Obtener data
#####################################
forex00 <- read.csv(file=paste0(fileDir,timeFrame),header = FALSE)
colnames(forex00) <- c("day","hour","open","high","low","close","volume")

### Selecting range of data
forex01<-forex00
forex01['date'] <- as_datetime(paste(gsub('\\.','-',forex01$day),forex01$hour),tz = 'Etc/GMT+5')
forex01<-forex01[forex01$date>=dateIni,]
forex01<-forex01[forex01$date<dateEnd,]
forex01<-forex01[order(forex01$date),]

### Selecting proportions
forexA <- forex01[,c("date","open","high","low","close")]

rm(forex00,forex01,timeFrame,dateIni,dateEnd)

#####################################
### Calculo del indicador ADX
#####################################

dataFX00 <- forexA
dataFX00$id <- c(1:nrow(dataFX00))
period <- historic

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
dataFX02 <- dataFX01[,c("id","rangoV")]

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

###output
dataFX<-merge(dataFX00,dataFX03,by="id")
dataFX$id<-NULL

rm(dataFX00,dataFX01,dataFX02,dataFX03,period,forexA)
rm(fileModel,historic,lag,rmColumns,title)
