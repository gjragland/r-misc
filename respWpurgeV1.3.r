
#for (it in 1:14) {
for (it in 1:length(MeasureFile)){
  print(PurgeFile[it])
  print(MeasureFile[it])
  data <- read.table(PurgeFile[it], header=TRUE);
  
  tStartVec <- 0;
  count=1;
  for (i in 1:nrow(data)) {
	if (data[i,ncol(data)]!=-1) {
        tStartVec[count]=PurgeTime[it] + data[i,1]/60^2;
        count=count+1;
      }  
  }
  totalPpmVec<-rep(0,length(tStartVec))
  timeVec<-rep(MeasureTime[it],length(tStartVec))
  data <- read.table(MeasureFile[it], header=TRUE)
  data[,flowCol]<-data[,flowCol]*1e-3/60 #adjust flowrate as above
  markerInd<-which(data[,markerCol]!=-1,arr.ind=T)
  markerInd<-c(markerInd,nrow(data))
  start=markerInd[1]
  count=1
  vals=vector()
  for (i in markerInd[-1]) {
      if (flowMeter==T) {
          vals=data[start:i,ppmCol]*data[start:i,flowCol]
      } else {
          vals=data[start:i,ppmCoo]*flowrate
      }
      timeVec[count]<-timeVec[count]+start/(60^2)
      totalPpmVec[count]<-sum(vals)
      if ( (i-start) < 10 ) {totalPpmVec[count]<-NA} #if dummy marker
      count=count+1
      start=i
  }
  #correct for and remove references
  ##refInd<-which(Refs[it,]!=0)
  refInd<-as.numeric(Refs[it,Refs[it,]!=0])
  totalPpmVec<-totalPpmVec[-refInd]-mean(totalPpmVec[refInd])
  elapsedTime<-timeVec+ (DateM[it] - DateP[it])*24 - tStartVec
  elapsedTime<-elapsedTime[-refInd]
  ppmPerHour<-totalPpmVec/elapsedTime
  if (it==1) {respCurves<-ppmPerHour} else {respCurves<-cbind(respCurves,ppmPerHour)}
  
}
matplot(aperm(array(DateM)),t(respCurves),type='b',xlab='date',ylab='ulCO2/h')
#matplot(aperm(array(DateM[])),t(respCurves),type='b',xlab='date',ylab='ulCO2/h')
colnames(respCurves)<-DateM
write.table(respCurves,outfile, sep="\t",row.names=FALSE,quote=F);





}
