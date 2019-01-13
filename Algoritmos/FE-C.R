forexC00 <- forexB

rmColumns<-append(rmColumns,paste0(rmHigh,"h"))
rmColumns<-append(rmColumns,paste0(rmLow,"l"))
rmColumns<-append(rmColumns,paste0(rmClose,"c"))

### eliminar manualmente columnas
forexC01 <- forexC00[,!(names(forexC00) %in% rmColumns)]

### seleccionar automaticamente columnas
x <- data.matrix(forexC01[,!(names(forexC01) %in% c("varpip"))])
y <- forexC01$varpip  
flasso <- glmnet(x, y, alpha=1)
rm(x,y)

sl <- flasso$lambda[r]
coef <- predict(flasso,type = "coefficients",s=sl)[1:ncol(forexC01),]
coef<-abs(coef[coef!=0])
coef["(Intercept)"]<-NA
fselect <- names(na.omit(coef))
fselect <- append(fselect,"varpip")

### Output
forexC <- forexC01[,(names(forexC01) %in% fselect)]
rm(forexC00,forexC01,r,coef,flasso,sl,rmColumns)
rm(rmHigh,rmClose,rmLow)
