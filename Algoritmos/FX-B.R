### Get data
forexB00 <- forexA

### Target data
forexB00['id'] <- c(1:nrow(forexB00))
datab <- forexB00[,c("id","close")]
datab['id'] <- datab['id'] - lag
colnames(datab)<-paste("next",colnames(datab),sep = "")
forexB01<-merge(forexB00,datab,by.x = "id", by.y = "nextid")
forexB01['varpip'] <- (forexB01$nextclose - forexB01$close) * precision
forexB01$nextclose<-NULL
rm(datab,lag)

### Historic Data
now <- forexB01
i <- 1
while (i <= historic) {
  previous <- forexB01[,c("id","high","low","close")]
  previous['id'] <- previous['id'] + i
  colnames(previous) <- paste0("n",colnames(previous))
  now<-merge(now,previous,by.x = "id", by.y = "nid")
  now[paste0(i,"h")] <- (now$close - now$nhigh) * precision
  now[paste0(i,"l")] <- (now$close - now$nlow) * precision
  now[paste0(i,"c")] <- (now$close - now$nclose) * precision
  now$nhigh <- NULL
  now$nlow <- NULL
  now$nclose <- NULL
  i <- i + 1
  rm(previous)
}
forexB02 <- now
rm(historic,i,now)

### Creating date
forexB03<-forexB02
forexB03['d07'] <- as.factor(wday(forexB03$date))
forexB03['h24'] <- as.factor(hour(forexB03$date))
forexB03$date<-NULL

### Creating candle and body
forexB04<-forexB03
forexB04['max'] <- apply(forexB04[c("open","close")],1,FUN = max)
forexB04['min'] <- apply(forexB04[c("open","close")],1,FUN = min)
forexB04['pips']   <- (forexB04$close - forexB04$open) * precision
forexB04['candle'] <- (forexB04$high - forexB04$low) * precision
forexB04['body']<-(abs(forexB04$pips)/forexB04$candle) * 100
forexB04$body[is.na(forexB04$body)]<-99.9999

forexB04['dhigh']<-(forexB04$high - forexB04$max) * precision
forexB04['ehigh']<-(forexB04$high - forexB04$min) * precision
forexB04['dlow']<-(forexB04$min - forexB04$low) * precision
forexB04['elow']<-(forexB04$max - forexB04$low) * precision

forexB04['0h']<-(forexB04$close - forexB04$high) * precision
forexB04['0l']<-(forexB04$close - forexB04$low) * precision

forexB04$max<-NULL
forexB04$min<-NULL
forexB04$open<-NULL
forexB04$high<-NULL
forexB04$low<-NULL
forexB04$close<-NULL
forexB04$id<-NULL

forexB<-forexB04
rm(forexB00,forexB01,forexB02,forexB03,forexB04)
