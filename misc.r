#R



#create plot with black backgroud

fm<-lm(atrans~ltrans,data=data)
par(bg='black',fg='white')
plot(data$ltrans,data$atrans,col.lab='white',col.axis='white')
abline(fm,col='white')

#change libpath to install in root-access directory (make library available to all users)
#for lab linux machine example
.libPaths("/usr/lib/R/library")
install.packages()


