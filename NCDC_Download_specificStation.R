### NCDC_Download.R ############################
#
# Script to download data from NCDC server
# Origin: https://github.com/akkagawa/HITempMap/blob/master/redownload.R
# 
#
# Modified by yfhuang with Anna's accompany on 2016/12/16
# No need to manual format data while in the download data part
# 'read download data' is still testing
################################################
#clean the working space
rm(list = ls(all = TRUE))
ls(all = TRUE)

### Environment Setting --------
setwd("C:/Users/Yu-Fen Huang/GoogleDrive_yfhuang/R_Script/R_workplace/NCDC_Download")  # Insert working directory

surl<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt"
surl2<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
download.file(surl, destfile="readme.txt")
download.file(surl2, destfile="ghcnd-stations.txt")

### Download HI station daily data ==================

### looking for 'HI'-------
gstns<-readLines('ghcnd-stations.txt')
state<-substr(gstns, 39, 40)
gstns.hi<-gstns[grep(state, pattern='HI')]  # Enter pattern with the State abbr.
bugline<-grep(gstns.hi,pattern="#")       # The "#" in the "Name" is a trouble maker! 
debug<-gsub("#","%",gstns.hi[bugline])    # Replace '#' by '%'
gstns.hi[bugline[1:5]]<-debug[1:5]
write(file="temp.txt",gstns.hi)
gstns.hi<-read.fortran('temp.txt',
                    c("A11","1X","F8.4","1X","F9.4","1X","F6.1","1X",
                      "A2","1X","A30","1X","A3","1X","A3","1X","A5"))
#c write.table(gstns.hi,file='ghcnd-stationsHI.csv',col.names=F,row.names=F,quote=F)
#c write.csv(gstns.hi,file="ghcnd-stationsHI.csv",quote=F,row.names=F)

colnames(gstns.hi)<-c("ID","Lat","Lon","Ele","State","Name","GSN","HCN/CRN","WMO")

# Station ID
stationID<-gstns.hi$ID[grep(gstns.hi$Name, pattern='LYON')]


### Download files from server -------------
#! this step will take a while..
files<-paste(stationID, ".dly", sep="")
dir<-"ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/"
for (i in 1:1){                              #test  
#for(i in 1:length(files)){
  download.file(paste(dir,files[i], sep=""), destfile=paste(files[i]))
}

### Read download data, ".dly" ======================

### Read files and put into list "dat"---------
dat<-list()
#for (i in 1:length(files)){
for (i in 1:1){                              #test  
  dat[[i]]<-readLines(files[i])
}

#### Write down to rainfall records (PRCP) ----
rowPn<-list()
#  rowMaxn<-list()
#  rowMinn<-list()
filesn<-list()
datn<-list()
j<-1
#for (i in 1:length(files)){
for (i in 1:1){                              #test  

### Precip. download

  rowP<-grep(dat[[i]], pattern="PRCP")
  if (length(rowP!=0)){
    filesn[j]<-files[i]
    datn[[j]]<-dat[[i]]
    rowPn[[j]]<-rowP
    
### Temp download

#  rowMax<-grep(dat[[i]], pattern="TMAX")
#  rowMin<-grep(dat[[i]], pattern="TMIN")
#  if (length(rowMax!=0)&length(rowMin!=0)){
#    filesn[j]<-files[i]
#    datn[[j]]<-dat[[i]]
#    rowMaxn[[j]]<-rowMax
#    rowMinn[[j]]<-rowMin
    j<-j+1
  }
}

##### Parse files and coerce into columns
# from readme.txt:
# III. FORMAT OF DATA FILES (".dly" FILES)
#
# Each ".dly" file contains data for one station.  The name of the file
# corresponds to a station's identification code.  For example, "USC00026481.dly"
# contains the data for the station with the identification code USC00026481).
#
# Each record in a file contains one month of daily data.  The variables on each
# line include the following:
#
# ------------------------------
# Variable   Columns   Type
# ------------------------------
# ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
# .           .          .
# .           .          .
# .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------


vi<-22+8*0:30  #where to cut a day
vf<-vi+4       #VALUE  
mi<-vf+1       #MGLAG
qi<-mi+1       #QFLAG
si<-qi+1       #SFLAG

##### Start parsing rows and files

m<-1            # file number
#for (m in 1:length(filesn)){
for (m in 1:1){                              #test  
  
  datP<-datn[[m]][rowPn[[m]]]
#  datMax<-datn[[m]][rowMaxn[[m]]]
#  datMin<-datn[[m]][rowMinn[[m]]]
  
  station<-substr(datP,1,11)
  year<-substr(datP,12,15)
  month<-substr(datP,16,17)
  element<-substr(datP,18,21)  #Character
#  station<-substr(datMax,1,11)
#  year<-substr(datMax,12,15)
#  month<-substr(datMax,16,17)
#  element<-substr(datMax,18,21)  #Character
  
  days<-sprintf("%02d", 1:31)
  
  for (n in 1:31){
    value<-substr(datP,vi[n],vf[n])   #Integer
    mflag<-substr(datP,mi[n],mi[n])   #Character
    qflag<-substr(datP,qi[n],qi[n])   #Character
    sflag<-substr(datP,si[n],si[n])   #Character
#    value<-substr(datMax,vi[n],vf[n])   #Integer
#    mflag<-substr(datMax,mi[n],mi[n])   #Character
#    qflag<-substr(datMax,qi[n],qi[n])   #Character
#    sflag<-substr(datMax,si[n],si[n])   #Character
    day<-days[n]
    val<-cbind(station, year, month, day,
               element, value, mflag, qflag, sflag)
    if(n==1){vals<-val
    }else{vals<-rbind(vals, val)}
  }
  

  
#  stationN<-substr(datMin,1,11)
#  yearN<-substr(datMin,12,15)
#  monthN<-substr(datMin,16,17)
#  elementN<-substr(datMin,18,21)  #Character
  
#  for (p in 1:31){
#    valueN<-substr(datMin,vi[p],vf[p])   #Integer
#    mflagN<-substr(datMin,mi[p],mi[p])   #Character
#    qflagN<-substr(datMin,qi[p],qi[p])   #Character
#    sflagN<-substr(datMin,si[p],si[p])   #Character
#    dayN<-days[p]
#    valN<-cbind(stationN, yearN, monthN, dayN, 
#                elementN, valueN, mflagN, qflagN, sflagN)
#    if(p==1){valsN<-valN
#    }else{valsN<-rbind(valsN, valN)}
#  }
#  
  if(m==1){vals.df<-vals
#  valsN.df<-valsN
  }else{vals.df<-rbind(vals.df,vals)
#  valsN.df<-rbind(valsN.df,valsN)
  }
}

dim(vals.df)   
#dim(valsN.df) 

##### Clean up file

valP.df<-data.frame(vals.df)
rmP<-valP.df$value=='-9999'| valP.df$qflag!=' '

#valTmax.df<-data.frame(vals.df)
#rmMax<-valTmax.df$value=='-9999'| valTmax.df$qflag!=' '

#valTmin.df<-data.frame(valsN.df)
#rmMin<-valTmin.df$valueN=='-9999'| valTmin.df$qflagN!=' '

valP.cln<-valP.df[!rmP,]
#valTmax.cln<-valTmax.df[!rmMax,]
#valTmin.cln<-valTmin.df[!rmMin,]

names(valP.cln)<-names(valP.cln)
valP.cln$date<-paste(valP.cln$year, valP.cln$month, valP.cln$day, sep="")
#names(valTmin.cln)<-names(valTmax.cln)
#valTmax.cln$date<-paste(valTmax.cln$year, valTmax.cln$month, valTmax.cln$day, sep="")
#valTmin.cln$date<-paste(valTmin.cln$year, valTmin.cln$month, valTmin.cln$day, sep="")


dim(valP.cln)
#dim(valTmax.cln) # 2038300
#dim(valTmin.cln) # 2035529

ValP.sort<-with(valP.cln, valP.cln[order(valP.cln$station,valP.cln$date),])

# convert the precipitation to mm
m2ft<-3.28084
mm2tm<-10  #PRCP = Precipitation (tenths of mm)
ValP.sort$value<-as.character(ValP.sort$value)
ValP.sort$precipt<-as.numeric(ValP.sort$value)/mm2tm

# Organize data
ouput<-data.frame(Date=ValP.sort$date, Preciptation_mm=ValP.sort$precipt)
write.csv(ouput, "Lyon_daily_precip.csv", row.names=F)

#split into two? Excel max rows ~1million
#write.csv(valTmax.cln, "allNCDCdailyMax.csv", row.names=F)
#write.csv(valTmin.cln, "allNCDCdailyMin.csv", row.names=F)

