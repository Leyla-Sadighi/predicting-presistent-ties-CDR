########################################################################################################
###                   This code calculate memory for all dataset we have:21 dataset                  ###
#########################################################################################################
#loading libraries
options(max.print = 1000000)
library(gdata)
library(timeDate)
library(plyr)
library(dplyr)
library(lubridate)
library(base)
library(ConvCalendar)
library(anytime)
library(ggplot2)
library(scales)
library(igraph)
library(cluster)
library(NbClust)
library(NbClust)
library(deldir)
library(caret)
library(randomForest)
library(tictoc)
####################################################################
###                     All Function                             ###
####################################################################
#sorting data based on date & time 
Sorting<-function(a){
  a0<-order(a[[3]],a[[4]])
  v1<-a[[4]][a0];v2<-a[[3]][a0]
  v3<-a[[1]][a0];v4<-a[[9]][a0]
  v5<-a[[13]][a0];v6<-a[[2]][a0]
  v7<-a[[5]][a0];v8<-a[[6]][a0]
  data_frame_<-data.frame(calls=v3,called=v6,date=v2,time=v1,duration=v4,type=v5,lac=v7,cell=v8)
}
#convert Jalali Date to Gregorian
convert_Date<-function(data0){
  x1<-data0[[3]]
  w<-c()
  for(i in 1:length(x1)) {
    w[i]<-toString(x1[i])
  }
  ye<-substr(w,start = 1,stop = 4)
  mon<-substr(w,start = 6,stop = 7)
  da<-substr(w,start = 9,stop = 10)
  out0<-list(ye,mon,da)
  y0<-as.numeric(out0[[1]])
  m0<-as.numeric(out0[[2]])
  d0<-as.numeric(out0[[3]])
  p<-c()
  f<-c()
  persian <- OtherDate(day=d0[1], month=m0[1], year=y0[1],calendar="persian")
  gregorian<-as.Date( persian )
  for(k in 1:length(y0)){
    if(m0[k]==1||m0[k]==2||m0[k]==3||m0[k]==4||m0[k]==5||m0[k]==6){
      p[k]<-(365*(y0[k]-y0[1]))+(31*(m0[k]-m0[1]))+(d0[k]-d0[1])  
    }else if(m0[k]==12){
      p[k]<-((365*(y0[k]-y0[1]))+(29*(m0[k]-m0[1]))+(d0[k]-d0[1]))+17
    }
    else{
      p[k]<-((365*(y0[k]-y0[1]))+(30*(m0[k]-m0[1]))+(d0[k]-d0[1]))+6
    }
    f[k]<-as.character(as.Date(p[k],origin = gregorian))
  }
  out<-f
}
#seperate input & output calls & sms
I_O<-function(d2){
  io<-{}
  s1<-count(d2,d2[[2]])
  s2<-s1[[1]][which(s1[[2]]==max(s1[[2]]))]
  s3<-as.character(d2[[2]])
  s4<-substring(s3,1,1)
  s4<-data.frame(s4,z<-1)
  s4<-count(s4,s4[[1]])
  s5<-s4[[1]][which(s4[[2]]==max(s4[[2]]))]
  if(s5==9){
    d2[[1]]<-Same_Dig(d2[[1]])
  }else{d2[[1]]<-d2[[1]]}
  for(q1 in 1:length(d2[[2]])){
    if(!is.na(d2[[2]][q1])){
      if(d2[[2]][q1]==s2){io[q1]<-"input"}
      else{io[q1]<-"output"}
    }else{
      if(d2[[1]][q1]==s2){io[q1]<-"output"}
      else{io[q1]<-"input"}
    }
  }
  out<-io
}
#make same digit of calls & called for comparision values
Same_Dig<-function(o){
  o1<-as.character(o)
  o2<-substring(o1,1,1)
  for(o3 in 1:length(o1)){
    if(o2[o3]=="0"){o1[o3]=substring(o1[o3],2)}
  }
  out4<-o1
}
##############################################################
#This function seperate component of dataset
cal_component<-function(df_test){
  df_test_5<-list(); df_test_call<-list();df_test_call0<-list();len_test_all0<-c();df_test_all.<-list()
  df_test_call.<-list();df_test_sms<-list();df_test_sms0<-list();df_test_all<-list();len_test_all.<-c()
  df_test_sms.<-list();len_test_call<-c();len_test_call0<-c();len_test_all<-c()
  len_test_call.<-c();len_test_sms<-c();len_test_sms0<-c();len_test_sms.<-c()
  df_test[[2]]<-as.character(df_test[[2]])#as.character for comparision gave same type
  c0<-substring(df_test[[2]],1,1)
  #c0<-data.frame(c0,c2<-1)
  #c0<-count(c0,c0[[1]])
  c0<-as.data.frame(table(c0))
  c1<-c0[[1]][which(c0[[2]]==max(c0[[2]]))]
  #extraxt first number of called, if it is 0 then the calls & called gave same digit
  #& we dont need to use Same_Dig function else we should use this function
  if(c1==9){
    df_test[[1]]<-Same_Dig(df_test[[1]])
  }else{df_test[[1]]<-df_test[[1]]}
  df_test_1<- df_test[( df_test[[8]]=="input") ,]#input calls & SMSs of data frame
  df_test_2<- df_test[( df_test[[8]]=="output"),]#output calls & SMSs of data frame 
  df_test_3<-count(df_test_1,df_test_1[[1]])#count input calls & SMSs
  df_test_4<-count(df_test_2,df_test_2[[2]])#count output calls & SMSs
  #count column CALLS & seperate the numbers
  if(length(df_test_3[[1]])!=0){
    for(k. in 1:length(df_test_3[[1]])){
      v0<- df_test_3[[1]][k.]
      d1<-df_test[[1]]==v0
      d2<-df_test[[2]]==v0
      d3<-d1|d2
      df_test_5[[k.]]<-df_test[d3,]
      #
      len_test_all[[k.]]<-length( df_test_5[[k.]]$CALLS)
      #component of calls
      df_test_call[[k.]]<-df_test_5[[k.]][df_test_5[[k.]][[4]]=="voice"|df_test_5[[k.]][[4]]=="callforward",]
      len_test_call[[k.]]<-length( df_test_call[[k.]]$CALLS)
      #component of SMSs
      df_test_sms[[k.]]<-df_test_5[[k.]][df_test_5[[k.]][[4]]=="sms",]
      len_test_sms[[k.]]<-length( df_test_sms[[k.]]$CALLS)
    }
  }
  # if there is a number in called which is not in calls this part find them & seprate components
  if(length(df_test_4[[1]])!=0){
    for(k_ in 1:length(df_test_4[[1]])){
      v1<- df_test_4[[1]][[k_]]
      if(!(v1%in%df_test[[1]])){
        d5<-df_test[[2]]==v1
        l.<-k_+length(df_test_3[[1]])
        df_test_5[[l.]]<-df_test[d5,]
        #
        len_test_all[[l.]]<-length( df_test_5[[l.]][[1]])
        #
        df_test_call[[l.]]<-df_test_5[[l.]][df_test_5[[l.]][[4]]=="voice",]
        len_test_call[[l.]]<-length(df_test_call[[l.]][[1]])
        df_test_sms[[l.]]<-df_test_5[[l.]][df_test_5[[l.]][[4]]=="sms",]
        len_test_sms[[l.]]<-length(df_test_sms[[l.]][[1]]) 
      }
    }
  }
  # if there are null array in list this part remove them
  all<-length( len_test_all[which(sapply(  len_test_all,is.na))])
  ac<-length( len_test_call[which(sapply( len_test_call,is.na))])
  as<-length( len_test_sms[which(sapply( len_test_sms,is.na))]) 
  #
  if(all!=0){
    df_test_all<-df_test_5[-which(sapply(df_test_5,is.null))]
    len_test_all0<-len_test_all[-which(sapply(len_test_all,is.na))]
  }else{
    df_test_all<-df_test_5
    len_test_all0<-len_test_all
  }
  #
  if(ac!=0){
    df_test_call0<-df_test_call[-which(sapply(df_test_call,is.null))]
    len_test_call0<-len_test_call[-which(sapply(len_test_call,is.na))]
  }else{
    df_test_call0<-df_test_call
    len_test_call0<-len_test_call
  }
  if(as!=0){
    df_test_sms0<-df_test_sms[-which(sapply(df_test_sms,is.null))]
    len_test_sms0<-len_test_sms[-which(sapply(len_test_sms,is.na))]
  }else{
    df_test_sms0<-df_test_sms
    len_test_sms0<-len_test_sms
  }
  #delet length zero array
  #
  m.all<-len_test_all0!=0
  #
  mc<-len_test_call0!=0
  ms<-len_test_sms0!=0
  #
  df_test_all.<-df_test_all[m.all]
  len_test_all.<-len_test_all0[m.all]
  #
  df_test_call.<-df_test_call0[mc]
  len_test_call.<-len_test_call0[mc]
  df_test_sms.<-df_test_sms0[ms]
  len_test_sms.<-len_test_sms0[ms]
  ###############################################################
  #out2<-list(df_test_call.,len_test_call.,df_test_sms.,len_test_sms.)
  out2<-list(df_test_call.,len_test_call.,df_test_sms.,len_test_sms., df_test_all., len_test_all.)
}
#this function seperate input & output component for both call & sms
sep_IO_comp<-function(a){
  com_in_call<-list()
  len_in_call<-c()
  com_out_call<-list()
  len_out_call<-c()
  com_in_sms<-list()
  len_in_sms<-c()
  com_out_sms<-list()
  len_out_sms<-c()
  a1<-a[[1]]
  a2<-a[[2]]
  a3<-a[[3]]
  a4<-a[[4]]
  for(x. in 1:length(a2)){
    b1<-a1[[x.]]
    com_in_call[[x.]]<-b1[which(b1[[8]]=="input"),]
    len_in_call[x.]<-length( com_in_call[[x.]][[1]])
    com_out_call[[x.]]<-b1[which(b1[[8]]=="output"),]
    len_out_call[x.]<-length( com_out_call[[x.]][[1]])
  }
  for(x.. in 1:length(a4)){
    b2<-a3[[x..]]
    com_in_sms[[x..]]<-b2[which(b2[[8]]=="input"),]
    len_in_sms[x..]<-length( com_in_sms[[x..]][[1]])
    com_out_sms[[x..]]<-b2[which(b2[[8]]=="output"),]
    len_out_sms[x..]<-length( com_out_sms[[x..]][[1]]) 
  }
  g1<- len_in_call!=0
  g2<-len_out_call!=0
  g3<-len_in_sms!=0
  g4<-len_out_sms!=0
  com_in_call<-com_in_call[g1]
  len_in_call<- len_in_call[g1]
  com_out_call<-com_out_call[g2]
  len_out_call<-len_out_call[g2]
  com_in_sms<-com_in_sms[g3]
  len_in_sms<- len_in_sms[g3]
  com_out_sms<-com_out_sms[g4]
  len_out_sms<-len_out_sms[g4]
  
  oue0<-list(com_in_call,len_in_call,com_out_call,len_out_call,com_in_sms,len_in_sms,com_out_sms,len_out_sms)
}
#calculate memory
cal_memory<-function(a,b){
  y<-c()
  sum0<-c()
  mem_comp<-list()
  num_comp<-length(b)
  for(m. in 1:num_comp){
    comp<-a[[m.]][[3]]
    comp<-diff(as.integer(comp))
    nk<-length(comp)
    if(nk>3){
      for(m.. in 1:nk-1){
        mu1<-mean(comp[1:nk-1])
        mu2<-mean(comp[2:nk])
        sd1<-sd(comp[1:nk-1])
        sd2<-sd(comp[2:nk])
        y[m..]<-((((comp[m..]-mu1)*(comp[m..+1]-mu2))/(sd1*sd2)))
        sum0<-append(sum0,y[m..])
      }
    }
  }
  oo<-sum0
  sum0<-sum0[which(!is.na(sum0))]
  out_<-(1/num_comp)*(sum(sum0))
}
#This function calculate Local variation
LV<-function(df2_){
  y.<-c(); sum.<-c()
  r0__<-df2_[[3]];ta<-diff(as.integer(r0__));
  N<-length(ta)
  for(l.. in 1:N-1){
    y.[l..]<-(3*((ta[l..]-ta[l..+1])^2))/((ta[l..]+ta[l..+1])^2)
    sum.<-append(sum.,y.[l..])
  }
  out.<-(1/(N-1))*sum(sum.)
}
#calculate coefficient of variation
CV<-function(df1_){
  sum_ta<-c(); y_ta<-c()
  r0_<-df1_[[3]];to<-diff(as.integer(r0_));
  n.<-length(to); tm<-mean(to)
  for(u. in 1:n.){
    y_ta[u.]<-(to[u.]-tm)^2;
    sum_ta<-append(sum_ta,y_ta[u.])
  }
  output.<-(sqrt((1/(u.-1))*sum(sum_ta)))/tm
}
#calculate burstiness 
Burstiness<-function(df_){
  r0<-df_[[3]];n0<-length(r0)
  r1<-as.integer(r0)
  r2<-diff(r1)
  sd_r<-sd(r2)
  mean_r<-mean(r2)
  r<-sd_r/mean_r
  burst3<-(sqrt(n0+1)*r-sqrt(n0-1))/((sqrt(n0+1)-2)*r+sqrt(n0-1))
}
#This function plot time line of all call & SMS for each user
Time_lines<-function(dataset.,name,i1){
  d.c<-dataset.[which( dataset.[[4]]=="voice"|dataset.[[4]]=="callforward"),]
  d.ic<-d.c[which(d.c[,5]=="input"),]
  d.oc<-d.c[which(d.c[,5]=="output"),]
  t.c<-d.c[,3]
  y.c<-rep(1,length(t.c))
  t.ic<-d.ic[,3]
  y.ic<-rep(1,length(t.ic))
  t.oc<-d.oc[,3]
  y.oc<-rep(1,length(t.oc))
  d.s<-dataset.[which( dataset.[[4]]=="sms"),]
  d.is<-d.s[which(d.s[,5]=="input"),]
  d.os<-d.s[which(d.s[,5]=="output"),]
  t.s<-d.s[,3]
  y.s<-rep(1,length(t.s))
  t.is<-d.is[,3]
  y.is<-rep(1,length(t.is))
  t.os<-d.os[,3]
  y.os<-rep(1,length(t.os))
  par(mar=c(2,2,2,2))
  par(mfrow=c(4,1))
  if(length(t.c)!=0){
    h1<-plot(t.c,y.c,main=paste(" All calls for ",name[i],sep = ":"),type="h",ylim=c(0,1)) 
  }
  if(length(t.ic)!=0){
    h2<-plot(t.ic,y.ic,main=paste(" All input calls for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="red")
  }
  if(length(t.oc)!=0){
    h3<-plot(t.oc,y.oc,main=paste(" All output calls for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")
  }
  if(length(t.ic)!=0 & length(t.oc)!=0){
    plot(t.ic,y.ic,main="All calls for with 2 colors",type="h",ylim=c(0,1),col="red")
    h4<-points(t.oc,y.oc,main=paste("All input & output calls  with 2 colors",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")  
  }
  if(length(t.s)!=0){
    h5<-plot(t.s,y.s,main=paste(" All SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1))  
  }
  if(length(t.is)!=0){
    h6<-plot(t.is,y.is,main=paste(" All input SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="red")
  }
  if(length(t.os)!=0){
    h7<-plot(t.os,y.os,main=paste(" All output SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")  
  }
  if(length(t.is)!=0 & length(t.os)!=0){
    plot(t.is,y.is,main="All calls for with 2 colors",type="h",ylim=c(0,1),col="red")
    h8<-points(t.os,y.os,main=paste("All input & output SMSs  with 2 colors",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue") 
  }
  
}
#This function plot time line of on month with year a2 & month a1 for both call & SMS 
T.l_mounth<-function(dataset.,name,i,a1,a2){
  d.c<-dataset.[which( dataset.[[4]]=="voice"|dataset.[[4]]=="callforward"),]
  d.ic<-d.c[which(d.c[,5]=="input"),]
  d.oc<-d.c[which(d.c[,5]=="output"),]
  t.c<-d.c[,3]
  t.c<-t.c[which(year(t.c)==a1 & month(t.c)==a2)]
  y.c<-rep(1,length(t.c))
  t.ic<-d.ic[,3]
  t.ic<-t.ic[which(year(t.ic)==a1 & month(t.ic)==a2)]
  y.ic<-rep(1,length(t.ic))
  t.oc<-d.oc[,3]
  t.oc<-t.oc[which(year(t.oc)==a1 & month(t.oc)==a2)]
  y.oc<-rep(1,length(t.oc))
  d.s<-dataset.[which( dataset.[[4]]=="sms"),]
  d.is<-d.s[which(d.s[,5]=="input"),]
  d.os<-d.s[which(d.s[,5]=="output"),]
  t.s<-d.s[,3]
  t.s<-t.s[which(year(t.s)==a1 & month(t.s)==a2)]
  y.s<-rep(1,length(t.s))
  t.is<-d.is[,3]
  t.is<-t.is[which(year(t.is)==a1 & month(t.is)==a2)]
  y.is<-rep(1,length(t.is))
  t.os<-d.os[,3]
  t.os<-t.os[which(year(t.os)==a1 & month(t.os)==a2)]
  y.os<-rep(1,length(t.os))
  par(mar=c(2,2,2,2))
  par(mfrow=c(4,1))
  if(length(t.c)!=0){
    h1<-plot(t.c,y.c,main=paste(" All calls for ",name[i],sep = ":"),type="h",ylim=c(0,1)) 
  }
  if(length(t.ic)!=0){
    h2<-plot(t.ic,y.ic,main=paste(" All input calls for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="red")
  }
  if(length(t.oc)!=0){
    h3<-plot(t.oc,y.oc,main=paste(" All output calls for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")
  }
  if(length(t.ic)!=0 & length(t.oc)!=0){
    plot(t.ic,y.ic,main="All calls for with 2 colors",type="h",ylim=c(0,1),col="red")
    h4<-points(t.oc,y.oc,main=paste("All input & output calls  with 2 colors",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")  
  }
  if(length(t.s)!=0){
    h5<-plot(t.s,y.s,main=paste(" All SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1))  
  }
  if(length(t.is)!=0){
    h6<-plot(t.is,y.is,main=paste(" All input SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="red")
  }
  if(length(t.os)!=0){
    h7<-plot(t.os,y.os,main=paste(" All output SMSs for ",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue")  
  }
  if(length(t.is)!=0 & length(t.os)!=0){
    plot(t.is,y.is,main="All calls for with 2 colors",type="h",ylim=c(0,1),col="red")
    h8<-points(t.os,y.os,main=paste("All input & output SMSs  with 2 colors",name[i],sep = ":"),type="h",ylim=c(0,1),col="blue") 
  }
}
#Function to calculate location of each user when they have events
mobility<-function(q0,q1){
  longitude<-c()
  latitude<-c()
  l1<-dim(q0)[1]
  for (i. in 1:l1){
    if(!is.na(q0$LAC[[i.]]) & !is.na(q0$CELL[[i.]]) &q0$LAC[[i.]]!=65535 & q0$CELL[[i.]]!=65535){
      s0<-which((q1$cid==q0$CELL[i.] & q1$lac==q0$LAC[i.]),arr.ind = TRUE)
      if(length(q1$lng[s0])!=0 & length(q1$cid[s0])!=0){
        longitude[i.]<-q1$lng[s0]
        latitude[i.]<-q1$lat[s0]  
      }
    }
    else if(q0$LAC[[i.]]==65535 & q0$CELL[[i.]]==65535){
      longitude[i.]<--79.51327
      latitude[i.]<-9.005321
    }
    else{
      longitude[i.]<-NA
      latitude[i.]<-NA
    }
  }
  out<-cbind(q0,longitude,latitude)
  
}
#This function seperate data to 3 region:befor,middle, after
sep_region<-function(z1,df..,file.n,burst,cv.,lv.,iet1,iet2){
  data.new<-list();data.befor<-list();data.middle<-list();data.affter<-list()
  R<-c();r<-c();l.bf<-c();l.md<-c();l.af<-c();l.all<-c();
  #delet datasets which are out of our selected range
  metric<-!((as.character(df..[[3]])<=as.Date("2017-01-01"))|(as.character(df..[[2]])>=as.Date("2016-01-01")))
  r1<-as.Date("2016-01-13");r2<-as.Date("2017-01-13");
  #end of befor range
  r3<-as.Date(r1+(3*30));
  #end of middle range
  r4<-as.Date(r1+(9*30));
  #two last week of middle
  r4.<-as.Date(r4-14)
  #new dataset
  df.0<-df..[which(metric),];ind<-c(1:length(z1));index<-ind[metric];name.new<-file.n[metric];
  df.new<-z1[metric];s1<-df.0[[2]];s2<-df.0[[3]]
  burst.<-burst[metric];cv.<-cv.[metric];lv.<-lv.[metric];
  iet1.<-iet1[metric];iet2.<-iet2[metric];
  #our range
  mat<-as.character(as.Date(r1:r2,origin="1970-01-01"));
  for(j. in 1:length(df.new)){
    #seperate datetime in our range
    z1.<-df.new[[j.]];z1.t<-z1.[[9]]; z2.<-z1.t%in%mat 
    data.new[[j.]]<-z1.[z2.,];l.all[j.]<-length(data.new[[j.]][[1]]);
    #start & stop date in each dataset
    r.start<-as.Date(first(data.new[[j.]][[9]]));r.end<-as.Date(last(data.new[[j.]][[9]]))
    #befor dataset
    bf<-as.character(as.Date(r.start:r3,origin="1970-01-01"));bf.<-data.new[[j.]][[9]]%in%bf;
    data.befor[[j.]]<-data.new[[j.]][bf.,]; l.bf[j.]<-length(data.befor[[j.]][[1]])
    #middle dataset
    md<-as.character(as.Date(r3:r4,origin="1970-01-01"));md.<-data.new[[j.]][[9]]%in%md;
    data.middle[[j.]]<-data.new[[j.]][md.,];l.md[j.]<-length(data.middle[[j.]][[1]])
    #affter dataset
    af<-as.character(as.Date(r4:r.end,origin="1970-01-01"));af.<-data.new[[j.]][[9]]%in%af;
    data.affter[[j.]]<-data.new[[j.]][af.,];l.af[j.]<-length(data.affter[[j.]][[1]])
  }
  output1.<-data.frame(name.new,s1,s2,l.all,l.bf,l.md,l.af,iet1.,iet2.,burst.,lv.,cv.);
  colnames(output1.)<-c("name","start","stop","all","beffor","middle","affter","ave","sd","burst","LV","CV")
  output2.<-list(data.befor,data.middle,data.affter)
  out.all<-list(output1.,output2.)
}
tic()
####################################################################
#all_local<-read.table("locations.txt",sep=",",header=TRUE,fill=TRUE)
#######################################################################
###      Read all datasets & convert them to soficient format       ###
#######################################################################
#definition list & vector we need
f<-list();dataset<-list();dataset_final<-list();IO<-list();name<-list();all_component<-list();component<-list();cell<-list()
dataset_final1<-c();dataset_final2<-c();dataset_final3<-c();dataset_final4<-c();location<-list();countt<-list();fo<-list();loc<-list();le<-c();r_g<-c()
w_call<-list();w_call_input<-list();w_call_output<-list();w_sms<-list();w_sms_input<-list();w_sms_output<-list()
df_call<-list();df_call_input<-list();df_call_output<-list();df_sms<-list();df_sms_input<-list();df_sms_output<-list()
iet_call<-list();iet_call_in<-list();iet_call_out<-list();iet.c.ave<-c();iet.c.sd<-c();iet.c.skew<-c();iet.c.kurt<-c()
iet_sms<-list();iet_sms_in<-list();iet_sms_out<-list();iet.s.ave<-c();iet.s.sd<-c();iet.s.skew<-c();iet.s.kurt<-c()
burst.c<-c();burst.c.in<-c();burst.c.out<-c();burst.s<-c();burst.s.in<-c();burst.s.out<-c()
cv.c<-c();cv.c.in<-c();cv.c.out<-c();cv.s<-c();cv.s.in<-c();cv.s.out<-c();mem.c<-c();mem.c.in<-c();mem.c.out<-c();mem.s<-c();mem.s.in<-c();mem.s.out<-c()
lv.c<-c();lv.c.in<-c();lv.c.out<-c();lv.s<-c();lv.s.in<-c();lv.s.out<-c();n.c<-c();n.c.in<-c();n.c.out<-c();n.s<-c();n.s.in<-c();n.s.out<-c()
c.comp<-c();c.in.comp<-c();c.out.comp<-c();s.comp<-c();s.in.comp<-c();s.out.comp<-c()
my_data_call<-list();my_data_sms<-list();counter.call<-list();counter.call.in<-list();counter.call.out<-list()
counter.sms<-list();counter.sms.in<-list();counter.sms.out<-list();count.c<-list();max1.c<-c();max2.c<-c();len.c<-c()
count.c.in<-list();max1.c.in<-c();max2.c.in<-c();len.c.in<-c();count.c.out<-list();max1.c.out<-c();max2.c.out<-c();len.c.out<-c()
count.s<-list();max1.s<-c();max2.s<-c();len.s<-c();count.s.in<-list();max1.s.in<-c();max2.s.in<-c();len.s.in<-c();dataset_final4<-list()
count.s.out<-list();max1.s.out<-c();max2.s.out<-c();len.s.out<-c();rdu_gy.c<-c();rdu_gy.s<-c()
all.first<-c();call.first<-c();sms.first<-c();all.last<-c();call.last<-c();sms.last<-c()
burst.all<-c();cv.all<-c();lv.all<-c();iet.all.ave<-c();iet.all.sd<-c()
#######################################################################
#######################################################################
#name of datasets
for(i1 in 1:21){
  name[[i1]]<-paste0("R_",i1,".txt")
  f[[i1]]<-read.table(name[[i1]],sep=",",header=TRUE,fill=TRUE)
  colnames(f[[i1]][1])<-"calls"
}
all_data<-f;data_frame_all<-list()
########################################################################
###              Finall calculation from function                    ###
########################################################################
file_name<-c()
for(g. in 1:21){
  file_name[g.]<-paste0("R_",g.)
}

#main code
for(i in 1:length(all_data)) 
{
  print(i)
  #This part is uses for obtain a new dataset which have what we want to work on & 
  df2<-Sorting(all_data[[i]])
  data_frame_0<-df2[which(!is.na(df2[[5]])),]
  datem_g<-convert_Date(data_frame_0)
  date_time<-as.POSIXct(parse_date_time(paste(datem_g,data_frame_0[[4]]),orders = "ymd HMS"))
  data_frame_all[[i]]<-cbind(data_frame_0,date_time,datem_g)
  dataset[[i]]<-data_frame_all[[i]][!duplicated(data_frame_all[[i]][,"date_time"]),]
  #sorting again
  z0<-order(dataset[[i]]$date_time)
  dataset[[i]][[1]]<-dataset[[i]][[1]][z0];dataset[[i]][[2]]<-dataset[[i]][[2]][z0];dataset[[i]][[3]]<-dataset[[i]][[3]][z0]
  dataset[[i]][[4]]<-dataset[[i]][[4]][z0];dataset[[i]][[5]]<-dataset[[i]][[5]][z0];dataset[[i]][[6]]<-dataset[[i]][[6]][z0]
  dataset[[i]][[7]]<-dataset[[i]][[7]][z0]; dataset[[i]][[8]]<-dataset[[i]][[8]][z0];dataset[[i]][[9]]<-dataset[[i]][[9]][z0];
  dataset[[i]][[10]]<-dataset[[i]][[10]][z0];
  CALLS<-dataset[[i]][[1]];CALLED<-dataset[[i]][[2]];Date<-dataset[[i]][[3]];Time<-dataset[[i]][[4]];Duration<-dataset[[i]][[5]];
  TYPE<-dataset[[i]][[6]];LAC<-dataset[[i]][[7]];CELL<-dataset[[i]][[8]];DATE_TIME<-dataset[[i]][[9]];M_date<- dataset[[i]][[10]]
  dataset_final[[i]]<-data.frame(CALLS,CALLED, DATE_TIME,TYPE,Duration,Date,Time)
  IO[[i]]<-I_O(dataset_final[[i]])
  dataset_final1[[i]]<-cbind(dataset_final[[i]],IO[[i]],M_date)
  all.first[i]<-as.character(as.Date(first(dataset_final1[[i]]$M_date)))
  all.last[i]<-as.character(as.Date(last(dataset_final1[[i]]$M_date)))
  burst.all[i]<-Burstiness(dataset_final1[[i]])
  cv.all[i]<-CV(dataset_final1[[i]])
  lv.all[i]<-LV(dataset_final1[[i]])
  iet.all.ave[i]<-mean(diff(as.integer(dataset_final1[[i]][[3]])))
  iet.all.sd[i]<-sd(diff(as.integer(dataset_final1[[i]][[3]])))
  #Time_lines(dataset_final[[i]],file_name,i)
  #data frame of calls we have
  df_call[[i]]<-dataset_final1[[i]][which(dataset_final1[[i]][[4]]!="sms"),]
  call.first[i]<-as.character(as.Date(first(df_call[[i]]$M_date)))
  call.last[i]<-as.character(as.Date(last(df_call[[i]]$M_date)))
  #data frame of input calls
  df_call_input[[i]]<-df_call[[i]][which(df_call[[i]]$`IO[[i]]`=="input"),]
  #data frame of output calls
  df_call_output[[i]]<-df_call[[i]][which(df_call[[i]]$`IO[[i]]`=="output"),]
  #seperate date_time variable for all calls
  w_call[[i]]<-df_call[[i]][[3]]
  #number of all calls
  n.c[i]<-length(w_call[[i]])
  #seperate date_time variable for input calls
  w_call_input[[i]]<-df_call_input[[i]][[3]]
  #number of all input calls
  n.c.in[[i]]<-length(w_call_input[[i]])
  #seperate date_time variable for output calls
  w_call_output[[i]]<-df_call_output[[i]][[3]]
  #number of all output calls
  n.c.out[[i]]<-length(w_call_output[[i]])
  #calculate inter event times for all calls
  iet_call[[i]]<-diff(as.integer(w_call[[i]]))
  #count inter event times for all calls
  if(length(iet_call[[i]])!=0){
    u<-data.frame(iet_call[[i]],yu<-1)
    counter.call[[i]]<-count(u,iet_call[[i]])
  }
  #calculate inter event times for input calls
  iet_call_in[[i]]<-diff(as.integer(w_call_input[[i]]))
  #count inter event times for input calls
  if(length(iet_call_in[[i]])!=0){
    u.in<-data.frame(iet_call_in[[i]],yu.in<-1)
    counter.call.in[[i]]<-count(u.in,iet_call_in[[i]])
  }
  #calculate inter event times for output calls
  iet_call_out[[i]]<-diff(as.integer(w_call_output[[i]]))
  #count inter event times for output calls
  if(length(iet_call_out[[i]])!=0){
    u.out<-data.frame(iet_call_out[[i]],yu.out<-1)
    counter.call.out[[i]]<-count(u.out,iet_call_out[[i]]) 
  }
  # calculate cv for calls
  cv.c[i]<-CV(df_call[[i]])
  # calculate cv for input calls
  cv.c.in[i]<-CV(df_call_input[[i]])
  # calculate cv for output calls
  cv.c.out[i]<-CV(df_call_output[[i]])
  # calculate lv for calls
  lv.c[i]<-LV(df_call[[i]])
  # calculate lv for input calls
  lv.c.in[i]<-LV(df_call_input[[i]])
  # calculate lv for output calls
  lv.c.out[i]<-LV(df_call_output[[i]])
  # momen 1 for calls
  iet.c.ave[i]<-mean(iet_call[[i]])
  # momen 2 for calls
  iet.c.sd[i]<-sd(iet_call[[i]])
  # momen 3 for calls
  iet.c.skew[i]<-skewness(iet_call[[i]])
  # momen 4 for calls
  iet.c.kurt[i]<-kurtosis(iet_call[[i]])
  #calculate burstiness of all calls
  burst.c[i]<-Burstiness(df_call[[i]])
  #calculate burstiness of all input calls
  burst.c.in[i]<-Burstiness(df_call_input[[i]])
  #calculate burstiness of all input calls
  burst.c.out[i]<-Burstiness(df_call_output[[i]])
  #dataset of all SMSs
  df_sms[[i]]<-dataset_final1[[i]][which(dataset_final1[[i]][[4]]=="sms"),]
  sms.first[i]<-as.character(as.Date(first(df_sms[[i]]$M_date)))
  sms.last[i]<-as.character(as.Date(last(df_sms[[i]]$M_date)))
  #dataset of input SMSs
  df_sms_input[[i]]<-df_sms[[i]][which(df_sms[[i]]$`IO[[i]]`=="input"),]
  #dataset of output SMSs
  df_sms_output[[i]]<-df_sms[[i]][which(df_sms[[i]]$`IO[[i]]`=="output"),]
  #seperate date_time variable for all SMSs
  w_sms[[i]]<-df_sms[[i]][[3]]
  #number of all SMSs
  n.s[i]<-length(w_sms[[i]])
  #seperate date_time variable for input SMSs
  w_sms_input[[i]]<-df_sms_input[[i]][[3]]
  #number of input SMSs
  n.s.in[[i]]<-length(w_sms_input[[i]])
  #seperate date_time variable for output SMSs
  w_sms_output[[i]]<-df_sms_output[[i]][[3]]
  #number of output SMSs
  n.s.out[[i]]<-length(w_sms_output[[i]])
  #calculate inter event times for all SMSs
  iet_sms[[i]]<-diff(as.integer(w_sms[[i]]))
  #count inter event times for all SMSs
  if(length(iet_sms[[i]])!=0){
    v<-data.frame(iet_sms[[i]],yv<-1)
    counter.sms[[i]]<-count(v,iet_sms[[i]]) 
  }
  #calculate inter event times for input SMSs
  iet_sms_in[[i]]<-diff(as.integer(w_sms_input[[i]]))
  #count inter event times for input SMSs
  if(length(iet_sms_in[[i]])!=0){
    v.in<-data.frame(iet_sms_in[[i]],yv.in<-1)
    counter.sms.in[[i]]<-count(v.in,iet_sms_in[[i]])  
  }
  #calculate inter event times for output SMSs
  iet_sms_out[[i]]<-diff(as.integer(w_sms_output[[i]]))
  #count inter event times for output SMSs
  if(length(iet_sms_out[[i]])!=0){
    v.out<-data.frame(iet_sms_out[[i]],yv.out<-1)
    counter.sms.out[[i]]<-count(v.out,iet_sms_out[[i]])
  }
  # calculate cv for SMSs
  cv.s[i]<-CV(df_sms[[i]])
  # calculate cv for input SMSs
  cv.s.in[i]<-CV(df_sms_input[[i]])
  # calculate cv for output SMSs
  cv.s.out[i]<-CV(df_sms_output[[i]])
  # calculate lv for SMSs
  lv.s[i]<-LV(df_sms[[i]])
  # calculate lv for input SMSs
  lv.s.in[i]<-LV(df_sms_input[[i]])
  # calculate lv for output SMSs
  lv.s.out[i]<-LV(df_sms_output[[i]])
  # momen 1 for SMSs
  iet.s.ave[i]<-mean(iet_sms[[i]])
  # momen 2 for SMSs
  iet.s.sd[i]<-sd(iet_sms[[i]])
  # momen 3 for SMSs
  iet.s.skew[i]<-skewness(iet_sms[[i]])
  # momen 4 for SMSs
  iet.s.kurt[i]<-kurtosis(iet_sms[[i]])
  #############################################
  burst.s[i]<-Burstiness(df_sms[[i]])
  burst.s.in[i]<-Burstiness(df_sms_input[[i]])
  burst.s.out[i]<-Burstiness(df_sms_output[[i]])
  #calculate component of dataset
  component[[i]]<-cal_component(dataset_final1[[i]])
  #number of components for all calls & SMSs
  c.comp[i]<-length(component[[i]][[2]]); s.comp[i]<-length(component[[i]][[4]])
  #seperate all input & output components for calls & SMSs
  all_component[[i]]<-sep_IO_comp(component[[i]])
  #calculate number of all input & output components for calls 
  c.in.comp[i]<- length(all_component[[i]][[2]]);c.out.comp[i]<- all_component[[i]][[4]]
  #calculate all input & output components for SMSs
  s.in.comp[i]<- length(all_component[[i]][[6]]);s.out.comp[i]<-length(length(all_component[[i]][[8]]))
  #calculate memory of all calls
  mem.c[i]<-cal_memory(component[[i]][[1]],component[[i]][[2]])
  #calculate memory of all input calls
  if(length(all_component[[i]][[2]])!=0){mem.c.in[i]<-cal_memory(all_component[[i]][[1]],all_component[[i]][[2]])}else{
    mem.c.in[i]<-NA
  }
  #calculate memory of all output calls
  if(length(all_component[[i]][[4]])!=0){mem.c.out[i]<-cal_memory(all_component[[i]][[3]],all_component[[i]][[4]])}else{
    mem.c.out[i]<-NA
  }
  #calculate memory of all SMSs
  mem.s[i]<-cal_memory(component[[i]][[3]],component[[i]][[4]])
  #calculate memory of all input SMSs
  if(length(all_component[[i]][[6]])!=0){mem.s.in[i]<-cal_memory(all_component[[i]][[5]],all_component[[i]][[6]])}else{
    mem.s.in[i]<-NA
  }
  #calculate memory of all output SMSs
  if(length(all_component[[i]][[8]])!=0){mem.s.out[i]<-cal_memory(all_component[[i]][[7]],all_component[[i]][[8]])}else{
    mem.s.out[i]<-NA
  }
}
###########################################################
###########################################################
df.tie<-data.frame(matrix(NA,nrow = 1,ncol=13));
all_<-data.frame(file_name,all.first,all.last)
call<-data.frame(file_name,call.first,call.last)
sms<-data.frame(file_name,sms.first,sms.last)
###########################################################
sep_call<-sep_region(df_call,call,file_name,burst.c,cv.c,lv.c,iet.c.ave,iet.c.sd)
sep_sms<-sep_region(df_sms,sms,file_name,burst.s,cv.s,lv.s,iet.s.ave,iet.s.sd)
sep_all<-sep_region(dataset_final1,all_,file_name,burst.all,cv.all,lv.all,iet.all.ave,iet.all.sd)
bf.call<-sep_call[[2]][[1]];bf.all<-sep_all[[2]][[1]];bf.sms<-sep_sms[[2]][[1]]
mid.call<-sep_call[[2]][[2]];mid.all<-sep_all[[2]][[2]];mid.sms<-sep_sms[[2]][[2]]
aff.call<-sep_call[[2]][[3]];aff.all<-sep_all[[2]][[3]];aff.sms<-sep_sms[[2]][[3]]
###############################################################
out1.call<-sep_call[[1]];idx.call<-c(1:length(out1.call$name));r.call<-out1.call$middle!=0;idx1.call<-idx.call[r.call];bf.call1<-bf.call[idx1.call];mid.call1<-mid.call[idx1.call];
aff.call1<-aff.call[idx1.call]
out2.call<-sep_call[[1]][idx1.call,]
out1.sms<-sep_sms[[1]];idx.sms<-c(1:length(out1.sms$name));r.sms<-out1.sms$middle!=0;idx1.sms<-idx.sms[r.sms];bf.sms1<-bf.sms[idx1.sms];mid.sms1<-mid.sms[idx1.sms];aff.sms1<-aff.sms[idx1.sms]
out2.sms<-sep_sms[[1]][idx1.sms,]
out1.all<-sep_all[[1]];idx.all<-c(1:length(out1.all$name));r.all<-out1.all$middle!=0;idx1.all<-idx.all[r.all];bf.all1<-bf.all[idx1.all];mid.all1<-mid.all[idx1.all];aff.all1<-aff.all[idx1.all]
out2.all<-sep_all[[1]][idx1.all,]
toc()





