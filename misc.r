#R



#create plot with black backgroud

fm<-lm(atrans~ltrans,data=data)
par(bg='black',fg='white')
plot(data$ltrans,data$atrans,col.lab='white',col.axis='white')
abline(fm,col='white')

