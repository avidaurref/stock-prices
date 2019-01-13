### Read data
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

