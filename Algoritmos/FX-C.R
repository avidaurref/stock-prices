forexC00 <- forexB

forexC <- forexC00[,(names(forexC00) %in% rmColumns)]
rm(forexC00,rmColumns)
