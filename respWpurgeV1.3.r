#R
#GJR 2-29-08
#modified 6/1/2017 to allow any recording interval, not just consecutive days (V1.3), plus overhauled most of the code
  #requires an additional field in the info file
#modified ~Fall 2011 to accept data with flowrate recorded
#modified 7-6-09 to reflect error in flowrate
#modified from matlab file 'resp_data_wPurge.m' on 7-7-09
# now flowrate set at 2.5e-3 (corrct) vs. the former 2.5e-5(incorrect)
 
#-----this version is modified to work with data sets where syringes are
    #purged on one day and measured the next. Two data files are required
    #per measurement date:
    #1. A 'dummy' exported expedata file with markers indicating when each syringe was purged
    #2. The actual measurement expedata file, taken the day following (this
    #is important because elapsed time is calculated based on 24hr
    #differentials)
 
 
#for analysis of serial closed-cell respirometry data
#data files must contain markers
#resp_data takes a series of exported ASCII files from Expedata (repiratory analysis
    #software) as input. Each file should contain a series of peaks, one for each
        #individual and one for each reference cell. For each file, the program:
    #1. integrates the respiration peaks between markers
        #(as recorded in expedata)
    #2. subtracts the value of the average reference cell
    #3. divides by the elapsed time
    #4. accounts for flowrate to convert to specfied units (fixed at ul
        #CO2/h
    #4. creates a vector of these calculated values
#finally, the program creates a figure plotting corrected respiration
#values for each individual vs. a specified time vector (e.g., julian date)
#and creates an output file in which the time reference appears in the
#first row, and subsequent rows are the time series for each sample
 
#program requires an input tab-delimited file containing one row per file
#to be analyzed including the following entries:
 
    #1. the filename of the purge (start) file
    #2. filename of the measurment file
    #3. Numeric date of purge (could be julian, could just be sequential numbers, i.e., day1 =1, day2 = 2, etc.)
    #4. Numeric date of measurement (see above)
    #5. time the PURGE file recording started, in military decimal format
      #(e.g., 1:15PM = 13.25)
    #6. time the MEASUREMENT file recording started, in military decimal format
      #(e.g., 1:15PM = 13.25)   
    #7. marker order of up to five reference cells (e.g., if the first
        #reference recording starts at the first marker then the value of
        #'ref1' should be specified as '1'.
 
#USE:
 
# respWpurge('filelist','outflile',flowMeter=TRUE)
    # where 'filelist' is the name of the tab-delimited input file and
    # 'outfile' is the name of the output file
    # if flowMeter=T, data must have four columns, w/ col#3 measuring flow rate
    # if flowMeter=F, data must have 3 columns, w/ col#2 measuring CO2 concentration
    # if flowMeter=F and flowrate different than 150mls/minute, specify flowrate as final input paramater
        
        
 
    
#note1: this program is designed to work with a recording in which the final
    #peak occurs after the final marker (i.e., there is no terminal marker)
#note2: Integration is based on one sample per second, so expedata files
    #should be exported using the 'seconds' time option
#note3: The order of samples MUST be the same in EVERY expedata file to be
    #analysed. Samples out of order must be corrected manually BEFORE
    #processing
#note4: If samples in the time series are either lost or no longer measured
    # (e.g., sample flies eclose) if you continue to insert dummy markers for
    # these samples with a recording duration of less than 10 seconds then
    # this program will automatically insert missing values for those
    # markers. If you do not insert dummy markers, the number of samples
    # across the time metric will be mismatched and you will get an error
    # message


#flowrate in ml/min

respWpurge <- function(infile,outfile,flowMeter=TRUE,flowrate=150) {

#rm(list=ls());
#Dir <- 'Q:/Greg/Metabolic_rate_fall_2008/GrantHaw_expedata/HawPostWinter';
 
#setwd(Dir);
ppmCol=2;
markerCol=3;
if (flowMeter==TRUE) {
  markerCol=4;
  flowCol=3;
}

#adjust flowrate to Liters per second X ml/min * 1e-3 L/ml * 60 min/H
# for 150 ml/min, yields 2.5e-3 L/S
flowrate<-flowrate*1e-3/60

inp <- scan(infile, list("","",0,0,0,0,0,0,0,0,0));



 
PurgeFile <- inp[[1]];
MeasureFile <- inp[[2]];
DateP <- inp[[3]]
DateM <- inp[[4]];
PurgeTime <- inp[[5]];
MeasureTime <- inp[[6]];


Refs <- data.frame(inp[[7]],inp[[8]],inp[[9]],inp[[10]],inp[[11]]);

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

