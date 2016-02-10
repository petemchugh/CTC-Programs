cwt.dat<-read.table(file.choose(),header=T)
cwt.dat1<-subset(cwt.dat,is.na(cwt.dat$mod.ER)==FALSE)
attach(cwt.dat1)
#library(gplots) #uncomment and install this package if you want to write as jpg, etc.
names(cwt.dat1)
levels(IndicatorAcronym)


#set writing directory to whatever you want
this_is_the_place<-file.path(choose.dir())
# Pete shortcut for directory "C:\\Users\\mchugpam\\Desktop\\RfigTest\\RevisedOcnER"

# function for adding CI bounds on model plots
CIcurves<-
  function(form=y~x,
           lty=2,lwd=1.5,col="black",
           newdata=data.frame(x=seq(from=min(x,na.rm=T)-0.05,to=max(x,na.rm=T)+0.05,by=0.01))){
    line.lm<-lm(form)
    x1<-newdata[,all.vars(form)[2]]
    hat<-predict(line.lm,newdata=newdata,interval="confidence")
    lines(spline(x1,hat[,"fit"]),lty=1,col=col,lwd=1.5)
    lines(spline(x1,hat[,"lwr"]),lty=lty,col=col)
    lines(spline(x1,hat[,"upr"]),lty=lty,col=col)
  }



#Create  acouple of data frames for storing/exporting summary stats characterizing differences
#Add variables to data frames and code below if additional summary stats are needed.
r<-length(levels(ModCWT))
fill<-rep(NA,r)
Total.out <- data.frame(Model.Stock=fill,CWT.Stock=fill,R2=fill,P.value=fill,
                     full.Bias=fill,full.Contrast=fill,five.Bias=fill,five.Contrast=fill,
                     n=fill, first.by=fill,last.by=fill,
                     cwt.ran=fill,mod.ran=fill,mod.cwt.rat=fill,
                     full.diff=fill,full.difflcb=fill,full.diffucb=fill,
                     five.diff=fill,five.difflcb=fill,five.diffucb=fill,
                     B.LOW=fill,B.MID=fill,B.HIGH=fill,cwt5.ran=fill,mod5.ran=fill,mod5.cwt5.rat=fill)
Ocn.out <- data.frame(Model.Stock=fill,CWT.Stock=fill,R2=fill,P.value=fill,
                        full.Bias=fill,full.Contrast=fill,five.Bias=fill,five.Contrast=fill,
                        n=fill, first.by=fill,last.by=fill,
                        cwt.ran=fill,mod.ran=fill,mod.cwt.rat=fill,
                        full.diff=fill,full.difflcb=fill,full.diffucb=fill,
                        five.diff=fill,five.difflcb=fill,five.diffucb=fill,
                        B.LOW=fill,B.MID=fill,B.HIGH=fill,cwt5.ran=fill,mod5.ran=fill,mod5.cwt5.rat=fill)



#side-by-side Ocean & Total ER scatterplots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window

#win.metafile(file = paste(this_is_the_place,"\\","Total&OceanER.emf",sep=""),height = 11, width = 8.5)
#jpeg(filename = paste(this_is_the_place,"\\","Total&OceanER.jpg",sep=""),
     #width = 8.5, height = 11, units = "in",res=800,quality = 75)

pdf(file = paste(this_is_the_place,"\\","Total&OceanER_Final.pdf",sep=""),height = 11, width = 8.5)
    pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
    par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
    for(i in 1:length(levels(ModCWT)))
    {
        #subset the dataset (total ER) for stock i
        stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
        name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
        by<-BroodYear[IndicatorAcronym==stk]
        x<-TM_TotalER[IndicatorAcronym==stk]
        y<-mod.ER[IndicatorAcronym==stk]
      
        #Stock details for total and ocean output files
        Total.out$Model.Stock[i]<-as.character(name)
        Total.out$CWT.Stock[i]<-as.character(stk)
        Total.out$n[i]<-length(x)
        Total.out$first.by[i]<-min(by)
        Total.out$last.by[i]<-max(by)
        Ocn.out$Model.Stock[i]<-as.character(name)
        Ocn.out$CWT.Stock[i]<-as.character(stk)
        Ocn.out$n[i]<-length(x)
        Ocn.out$first.by[i]<-min(by)
        Ocn.out$last.by[i]<-max(by)
        
        #Single title for columns of figures (keep empty 'cept for top one)  
        mainname<-""
        
        pagetest=0
        for(k in 1:length(pages))
        {
          if(pages[k]==i)
          {pagetest<-1}
        }
        
        if(pagetest==1)
        {mainname<-"TOTAL"}
        
        #************************************************************************
        #*********TOTAL ER Plot Portion of Loop**********************************
        #************************************************************************
            # calculate differences and some useful summary stats for figure and outfile
        diff<-(y-x)
        diffmean<-sprintf("%.3f",mean(diff))
        difftest<-t.test(diff) #paired t-test for H0: diff = 0
        tstat<-format(as.numeric(difftest[1]),digits=3) #t-statistic
        Pt<-format(as.numeric(difftest[3]),digits=3) # P-value
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CI on diff
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) # upper 95% CI on diff
        difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") 
        ranx<-round(max(x)-min(x),2) #CWT range
        rany<-round(max(y)-min(y),2) #Model range
        rat.ran<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
        ranx<-sprintf("%.2f",ranx)
        rany<-sprintf("%.2f",rany)
        CVtext<-paste("CWT range: ",ranx,", Model range: ",rany,sep="")
        CVrattext<-paste("Model-to-CWT range ratio: ",rat.ran,sep="")
        #Now put these in the output file for report
        Total.out$cwt.ran[i]<-ranx
        Total.out$mod.ran[i]<-rany
        Total.out$mod.cwt.rat[i]<-rat.ran
        Total.out$full.diff[i]<-diffmean
        Total.out$full.difflcb[i]<-lcb
        Total.out$full.diffucb[i]<-ucb
          
        #additional variables to isolate the last five brood years
        x1<-x[(length(x)-4):length(x)]
        y1<-y[(length(x)-4):length(x)]
        
    
        #Finally, let's make a graph    
        plot(x,y,ylim=c(0,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
             ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
        points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
        
        #fit regression and extract summary stats for figure and output file
        m1<-lm(y~x) #regression
        
        #)))))))))))))))))))(((((((((((((((((())))))))))))))))))
        #for bias assessment
        high.test<-mean(x)+sd(x)
        low.test<-mean(x)-sd(x)
        mid.test<-mean(x)
        High<-predict(m1,newdata=data.frame(x=c(high.test)),interval="confidence")
        Mid<-predict(m1,newdata=data.frame(x=c(mid.test)),interval="confidence")
        Low<-predict(m1,newdata=data.frame(x=c(low.test)),interval="confidence")
        if(High[3]<high.test&&High[1]<high.test)
          {Total.out$B.HIGH[i]<-"Negative"} else
        if(High[3]>high.test&&High[1]>high.test)
            {Total.out$B.HIGH[i]<-"Positive"} else
            {Total.out$B.HIGH[i]<-"No"}
        if(Low[3]<low.test&&Low[1]<low.test)
          {Total.out$B.LOW[i]<-"Negative"} else
        if(Low[3]>low.test&&Low[1]>low.test)
          {Total.out$B.LOW[i]<-"Positive"} else
          {Total.out$B.LOW[i]<-"No"}
        if(Mid[3]<mid.test&&Mid[1]<mid.test)
          {Total.out$B.MID[i]<-"Negative"} else
        if(Mid[3]>mid.test&&Mid[1]>mid.test)
          {Total.out$B.MID[i]<-"Positive"} else
          {Total.out$B.MID[i]<-"No"}
        
        Total.out$cwt5.ran[i]<-max(x1)-min(x1) #CWT range
        Total.out$mod5.ran[i]<-max(y1)-min(y1) #Model range
        Total.out$mod5.cwt5.rat[i]<-Total.out$mod5.ran[i]/Total.out$cwt5.ran[i] #Model range<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
        
        
        #)))))))))))))))))))(((((((((((((((((())))))))))))))))))
          
          
        
        #summary(m1)
        R2<-format(round(summary(m1)$r.squared,3),digits=2)
        P.val<-format(round(anova(m1)[1,5],3),digits=3)
        
        # More goods for outfile
        Total.out$R2[i]<-R2
        Total.out$P.value[i]<-anova(m1)[1,5]
        #same difference variables as above, but for most recent five broods
        diff<-(y1-x1)
        diffmean<-sprintf("%.3f",mean(diff))
        difftest<-t.test(diff)
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1]))
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2]))
        Total.out$five.diff[i]<-diffmean
        Total.out$five.difflcb[i]<-lcb
        Total.out$five.diffucb[i]<-ucb
        
        #for tidier display of fitted regression   (full series)  
        ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
        ddd1<-predict.lm(m1,ddd)
        #only display regression if significant
        if(P.val<0.05)
        {
          CIcurves(y~x)
          if(P.val<0.001)
          {P.val="P < 0.001"}
          else
          {P.val=paste("P =",P.val)}
        } else 
        {P.val=paste("P =",P.val)}
        
        #now make final text strings and add them to figures using 'legend'
        mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
        legend(x=-0.08,y=0.97,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.1,y=1.07,
               c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
               box.lty=0,box.col=0,bg="transparent",cex=1.2)
        legend(x=-0.08,y=0.90,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.08,y=0.83,CVtext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.08,y=0.76,CVrattext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        
          
        # add 1:1 line for reference and put a box around the plot
        abline(a=0,b=1,lty=2,lwd=1.25)
        box(lwd=2)
        #************************************************************************
        #************************************************************************
        #************************************************************************
        
        
        
        #************************************************************************
        #*********OCEAN ER Plot Portion of Loop**********************************
        #**********Just a repeat of Total w/ Ocn ERs only************************
        #************************************************************************  
        if(pagetest==1)
        {mainname<-"OCEAN"}  
        x<-TM_OcnER[IndicatorAcronym==stk]
        y<-mod.ocnER[IndicatorAcronym==stk]
        
        # calculate differences and some useful summary stats for figure and outfile
        diff<-(y-x)
        diffmean<-sprintf("%.3f",mean(diff))
        difftest<-t.test(diff) #paired t-test for H0: diff = 0
        tstat<-format(as.numeric(difftest[1]),digits=3) #t-statistic
        Pt<-format(as.numeric(difftest[3]),digits=3) # P-value
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CI on diff
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) # upper 95% CI on diff
        difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") 
        ranx<-round(max(x)-min(x),2) #CWT range
        rany<-round(max(y)-min(y),2) #Model range
        rat.ran<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
        ranx<-sprintf("%.2f",ranx)
        rany<-sprintf("%.2f",rany)
        CVtext<-paste("CWT range: ",ranx,", Model range: ",rany,sep="")
        CVrattext<-paste("Model-to-CWT range ratio: ",rat.ran,sep="")
        #Now put these in the output file for report
        Ocn.out$cwt.ran[i]<-ranx
        Ocn.out$mod.ran[i]<-rany
        Ocn.out$mod.cwt.rat[i]<-rat.ran
        Ocn.out$full.diff[i]<-diffmean
        Ocn.out$full.difflcb[i]<-lcb
        Ocn.out$full.diffucb[i]<-ucb
        
        #additional variables to isolate the last five brood years
        x1<-x[(length(x)-4):length(x)]
        y1<-y[(length(x)-4):length(x)]
        
        
        #Finally, let's make a graph    
        plot(x,y,ylim=c(0,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
             ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
        points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
        
        #fit regression and extract summary stats for figure and output file
        m1<-lm(y~x) #regression
        
        #)))))))))))))))))))(((((((((((((((((())))))))))))))))))
        #for bias assessment
        high.test<-mean(x)+sd(x)
        low.test<-mean(x)-sd(x)
        mid.test<-mean(x)
        High<-predict(m1,newdata=data.frame(x=c(high.test)),interval="confidence")
        Mid<-predict(m1,newdata=data.frame(x=c(mid.test)),interval="confidence")
        Low<-predict(m1,newdata=data.frame(x=c(low.test)),interval="confidence")
        if(High[3]<high.test&&High[1]<high.test)
        {Ocn.out$B.HIGH[i]<-"Negative"} else
          if(High[3]>high.test&&High[1]>high.test)
          {Ocn.out$B.HIGH[i]<-"Positive"} else
          {Ocn.out$B.HIGH[i]<-"No"}
        if(Low[3]<low.test&&Low[1]<low.test)
        {Ocn.out$B.LOW[i]<-"Negative"} else
          if(Low[3]>low.test&&Low[1]>low.test)
          {Ocn.out$B.LOW[i]<-"Positive"} else
          {Ocn.out$B.LOW[i]<-"No"}
        if(Mid[3]<mid.test&&Mid[1]<mid.test)
        {Ocn.out$B.MID[i]<-"Negative"} else
          if(Mid[3]>mid.test&&Mid[1]>mid.test)
          {Ocn.out$B.MID[i]<-"Positive"} else
          {Ocn.out$B.MID[i]<-"No"}
        
        Ocn.out$cwt5.ran[i]<-max(x1)-min(x1) #CWT range
        Ocn.out$mod5.ran[i]<-max(y1)-min(y1) #Model range
        Ocn.out$mod5.cwt5.rat[i]<-Ocn.out$mod5.ran[i]/Ocn.out$cwt5.ran[i] #Model range<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
        
        
        #)))))))))))))))))))(((((((((((((((((())))))))))))))))))
        
        
        
        
        #summary(m1)
        R2<-format(round(summary(m1)$r.squared,3),digits=2)
        P.val<-format(round(anova(m1)[1,5],3),digits=3)
        
        # More goods for outfile
        Ocn.out$R2[i]<-R2
        Ocn.out$P.value[i]<-anova(m1)[1,5]
        #same difference variables as above, but for most recent five broods
        diff<-(y1-x1)
        diffmean<-sprintf("%.3f",mean(diff))
        difftest<-t.test(diff)
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1]))
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2]))
        Ocn.out$five.diff[i]<-diffmean
        Ocn.out$five.difflcb[i]<-lcb
        Ocn.out$five.diffucb[i]<-ucb
        
        #for tidier display of fitted regression   (full series)  
        ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
        ddd1<-predict.lm(m1,ddd)
        #only display regression if significant
        if(P.val<0.05)
          {
            CIcurves(y~x)
            if(P.val<0.001)
            {P.val="P < 0.001"}
            else
            {P.val=paste("P =",P.val)}
          } else 
            {P.val=paste("P =",P.val)}
        
        #now make final text strings and add them to figures using 'legend'
        mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
        legend(x=-0.08,y=0.97,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.1,y=1.07,
               c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
               box.lty=0,box.col=0,bg="transparent",cex=1.2)
        legend(x=-0.08,y=0.90,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.08,y=0.83,CVtext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        legend(x=-0.08,y=0.76,CVrattext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
        
        # add 1:1 line for reference and put a box around the plot
        abline(a=0,b=1,lty=2,lwd=1.25)
        box(lwd=2)
        #************************************************************************
        #************************************************************************
        #************************************************************************
      
        #Now add a single x-axis and y-axis for each page     
        mtext("Chinook Model ER",cex=1.2,side = 2,outer=TRUE,font=1)
        mtext("Coded Wire Tag ER",cex=1.2,side = 1,outer=TRUE,font=1)
    }

#write output files to spreadsheet
write.csv(x=Ocn.out, file = paste(this_is_the_place,"\\","OceanOut.csv",sep=""))
write.csv(x=Total.out, file = paste(this_is_the_place,"\\","TotalOut.csv",sep=""))

dev.off() #done!
#************************************************************************************
#************************************************************************************
#************************************************************************************







#Deviation By Year Plots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window

pdf(file = paste(this_is_the_place,"\\","Differences.pdf",sep=""),height = 11, width = 8.5)
    pages<-seq(1,48,4)
    par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
    for(i in 1:length(levels(ModCWT)))
    {
        #subset the dataset (total ER) for stock i
        stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
        name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
        x<-BroodYear[IndicatorAcronym==stk]
        y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
        
        #Single title for columns of figures (keep empty 'cept for top one)  
        mainname<-""
        
        pagetest=0
        for(k in 1:length(pages))
        {
          if(pages[k]==i)
          {pagetest<-1}
        }
        
        if(pagetest==1)
        {mainname<-"TOTAL"}
        
        #************************************************************************
        #*********TOTAL ER Diff Series First*************************************
        #************************************************************************
        #Compute differences for y axis
        diff<-y
        diffmean<-sprintf("%.3f",mean(diff)) #mean difference
        difftest<-t.test(diff) #paired t-test of H0: diff = 0
        tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
        Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
        difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
        
        #plot the series
        plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
             ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
        lines(x,y)
        points(x,y,pch=21,cex=1.5,col="black",bg="gray")

        #Add stock details and summary stats on difference to figure
        legend(x=1974.6,y=-.5,
               c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
               box.lty=0,box.col=0,cex=1.2)
        legend(x=1975.5,y=-0.7,difftext,box.lty=0,box.col=0,cex=0.9)
        
        #Add horizontal 0 reference line and a box
        abline(h=0,lty=2,lwd=1.25)
        box(lwd=2)
        
        #************************************************************************
        #*********Ocean ER Diff Series First*************************************
        #************************************************************************
        if(pagetest==1)
        {mainname<-"OCEAN"}  
        x<-BroodYear[IndicatorAcronym==stk]
        y<-mod.ocnER[IndicatorAcronym==stk]-TM_OcnER[IndicatorAcronym==stk]
        
        #Compute differences for y axis
        diff<-y
        diffmean<-sprintf("%.3f",mean(diff)) #mean difference
        difftest<-t.test(diff) #paired t-test of H0: diff = 0
        tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
        Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
        lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
        ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
        difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
        
        #plot the series
        plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
             ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
        lines(x,y)
        points(x,y,pch=21,cex=1.5,col="black",bg="gray")
        
        #Add stock details and summary stats on difference to figure
        legend(x=1974.6,y=-.5,
               c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
               box.lty=0,box.col=0,cex=1.2)
        legend(x=1975.5,y=-0.7,difftext,box.lty=0,box.col=0,cex=0.9)
        
        #Add horizontal 0 reference line and a box
        abline(h=0,lty=2,lwd=1.25)
        box(lwd=2)
        
        #finally, add single x- and y-axes to page
        mtext("Model ER - CWT ER",cex=1.2,side = 2,outer=TRUE,font=1)
        mtext("Brood Year",cex=1.2,side = 1,outer=TRUE,font=1)
    }
dev.off()
#************************************************************************************
#************************************************************************************
#************************************************************************************





#Deviation By Year Plots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window

pdf(file = paste(this_is_the_place,"\\","Differences.pdf",sep=""),height = 11, width = 8.5)
pages<-seq(1,48,4)
par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
for(i in 1:length(levels(ModCWT)))
{
  #subset the dataset (total ER) for stock i
  stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
  name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
  x<-BroodYear[IndicatorAcronym==stk]
  y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
  
  #Single title for columns of figures (keep empty 'cept for top one)  
  mainname<-""
  
  pagetest=0
  for(k in 1:length(pages))
  {
    if(pages[k]==i)
    {pagetest<-1}
  }
  
  if(pagetest==1)
  {mainname<-"TOTAL"}
  
  #************************************************************************
  #*********TOTAL ER Diff Series First*************************************
  #************************************************************************
  #Compute differences for y axis
  diff<-y
  diffmean<-sprintf("%.3f",mean(diff)) #mean difference
  difftest<-t.test(diff) #paired t-test of H0: diff = 0
  tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
  Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
  lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
  ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
  difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
  
  #plot the series
  plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
       ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
  lines(x,y)
  points(x,y,pch=21,cex=1.5,col="black",bg="gray")
  
  #Add stock details and summary stats on difference to figure
  legend(x=1974.6,y=-.5,
         c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
         box.lty=0,box.col=0,cex=1.2)
  legend(x=1975.5,y=-0.7,difftext,box.lty=0,box.col=0,cex=0.9)
  
  #Add horizontal 0 reference line and a box
  abline(h=0,lty=2,lwd=1.25)
  box(lwd=2)
  
  #************************************************************************
  #*********Ocean ER Diff Series First*************************************
  #************************************************************************
  if(pagetest==1)
  {mainname<-"OCEAN"}  
  x<-BroodYear[IndicatorAcronym==stk]
  y<-mod.ocnER[IndicatorAcronym==stk]-TM_OcnER[IndicatorAcronym==stk]
  
  #Compute differences for y axis
  diff<-y
  diffmean<-sprintf("%.3f",mean(diff)) #mean difference
  difftest<-t.test(diff) #paired t-test of H0: diff = 0
  tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
  Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
  lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
  ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
  difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
  
  #plot the series
  plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
       ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
  lines(x,y)
  points(x,y,pch=21,cex=1.5,col="black",bg="gray")
  
  #Add stock details and summary stats on difference to figure
  legend(x=1974.6,y=-.5,
         c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
         box.lty=0,box.col=0,cex=1.2)
  legend(x=1975.5,y=-0.7,difftext,box.lty=0,box.col=0,cex=0.9)
  
  #Add horizontal 0 reference line and a box
  abline(h=0,lty=2,lwd=1.25)
  box(lwd=2)
  
  #finally, add single x- and y-axes to page
  mtext("Model ER - CWT ER",cex=1.2,side = 2,outer=TRUE,font=1)
  mtext("Brood Year",cex=1.2,side = 1,outer=TRUE,font=1)
}
dev.off()
#************************************************************************************
#************************************************************************************
#************************************************************************************












#bastard version for plotting
#side-by-side Ocean & Total ER scatterplots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window
i=1
fish<-1
tick<-1
while(fish<=round(length(levels(ModCWT))/4,0)) #Outer loop for figures
{
    
    #win.metafile(file = paste(this_is_the_place,"\\","Total&OceanER.emf",sep=""),height = 11, width = 8.5)
    jpeg(filename = paste(this_is_the_place,"\\",fish,"Total&OceanER.jpg",sep=""),
         width = 8.5, height = 11, units = "in",res=800,quality = 75)
    
    #pdf(file = paste(this_is_the_place,"\\","Total&OceanER.pdf",sep=""),height = 11, width = 8.5)
    pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
    par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
    
    Max<-tick+3
    if(Max>=length(levels(ModCWT)))
      {
        Max<-length(levels(ModCWT))
      }
    
    while(i<=Max)
    {
      #subset the dataset (total ER) for stock i
      stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
      name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
      by<-BroodYear[IndicatorAcronym==stk]
      x<-TM_TotalER[IndicatorAcronym==stk]
      y<-mod.ER[IndicatorAcronym==stk]
      
      #Stock details for total and ocean output files
      Total.out$Model.Stock[i]<-as.character(name)
      Total.out$CWT.Stock[i]<-as.character(stk)
      Total.out$n[i]<-length(x)
      Total.out$first.by[i]<-min(by)
      Total.out$last.by[i]<-max(by)
      Ocn.out$Model.Stock[i]<-as.character(name)
      Ocn.out$CWT.Stock[i]<-as.character(stk)
      Ocn.out$n[i]<-length(x)
      Ocn.out$first.by[i]<-min(by)
      Ocn.out$last.by[i]<-max(by)
      
      #Single title for columns of figures (keep empty 'cept for top one)  
      mainname<-""
      
      pagetest=0
      for(k in 1:length(pages))
      {
        if(pages[k]==i)
        {pagetest<-1}
      }
      
      if(pagetest==1)
      {mainname<-"TOTAL"}
      
      #************************************************************************
      #*********TOTAL ER Plot Portion of Loop**********************************
      #************************************************************************
      # calculate differences and some useful summary stats for figure and outfile
      diff<-(y-x)
      diffmean<-sprintf("%.3f",mean(diff))
      difftest<-t.test(diff) #paired t-test for H0: diff = 0
      tstat<-format(as.numeric(difftest[1]),digits=3) #t-statistic
      Pt<-format(as.numeric(difftest[3]),digits=3) # P-value
      lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CI on diff
      ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) # upper 95% CI on diff
      difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") 
      ranx<-round(max(x)-min(x),2) #CWT range
      rany<-round(max(y)-min(y),2) #Model range
      rat.ran<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
      ranx<-sprintf("%.2f",ranx)
      rany<-sprintf("%.2f",rany)
      CVtext<-paste("CWT range: ",ranx,", Model range: ",rany,sep="")
      CVrattext<-paste("Model-to-CWT range ratio: ",rat.ran,sep="")
      #Now put these in the output file for report
      Total.out$cwt.ran[i]<-ranx
      Total.out$mod.ran[i]<-rany
      Total.out$mod.cwt.rat[i]<-rat.ran
      Total.out$full.diff[i]<-diffmean
      Total.out$full.difflcb[i]<-lcb
      Total.out$full.diffucb[i]<-ucb
      
      #additional variables to isolate the last five brood years
      x1<-x[(length(x)-4):length(x)]
      y1<-y[(length(x)-4):length(x)]
      
      
      #Finally, let's make a graph    
      plot(x,y,ylim=c(0,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
           ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
      points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
      
      #fit regression and extract summary stats for figure and output file
      m1<-lm(y~x) #regression
      #summary(m1)
      R2<-format(round(summary(m1)$r.squared,3),digits=2)
      P.val<-format(round(anova(m1)[1,5],3),digits=3)
      
      # More goods for outfile
      Total.out$R2[i]<-R2
      Total.out$P.value[i]<-anova(m1)[1,5]
      #same difference variables as above, but for most recent five broods
      diff<-(y1-x1)
      diffmean<-sprintf("%.3f",mean(diff))
      difftest<-t.test(diff)
      lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1]))
      ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2]))
      Total.out$five.diff[i]<-diffmean
      Total.out$five.difflcb[i]<-lcb
      Total.out$five.diffucb[i]<-ucb
      
      #for tidier display of fitted regression   (full series)  
      ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
      ddd1<-predict.lm(m1,ddd)
      #only display regression if significant
      if(P.val<0.05)
      {
        CIcurves(y~x)
        if(P.val<0.001)
        {P.val="P < 0.001"}
        else
        {P.val=paste("P =",P.val)}
      } else 
      {P.val=paste("P =",P.val)}
      
      #now make final text strings and add them to figures using 'legend'
      mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
      legend(x=-0.08,y=0.97,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.1,y=1.07,
             c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
             box.lty=0,box.col=0,bg="transparent",cex=1.2)
      legend(x=-0.08,y=0.90,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.08,y=0.83,CVtext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.08,y=0.76,CVrattext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      
      
      # add 1:1 line for reference and put a box around the plot
      abline(a=0,b=1,lty=2,lwd=1.25)
      box(lwd=2)
      #************************************************************************
      #************************************************************************
      #************************************************************************
      
      
      
      #************************************************************************
      #*********OCEAN ER Plot Portion of Loop**********************************
      #**********Just a repeat of Total w/ Ocn ERs only************************
      #************************************************************************  
      if(pagetest==1)
      {mainname<-"OCEAN"}  
      x<-TM_OcnER[IndicatorAcronym==stk]
      y<-mod.ocnER[IndicatorAcronym==stk]
      
      # calculate differences and some useful summary stats for figure and outfile
      diff<-(y-x)
      diffmean<-sprintf("%.3f",mean(diff))
      difftest<-t.test(diff) #paired t-test for H0: diff = 0
      tstat<-format(as.numeric(difftest[1]),digits=3) #t-statistic
      Pt<-format(as.numeric(difftest[3]),digits=3) # P-value
      lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CI on diff
      ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) # upper 95% CI on diff
      difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") 
      ranx<-round(max(x)-min(x),2) #CWT range
      rany<-round(max(y)-min(y),2) #Model range
      rat.ran<-sprintf("%.2f",round(rany/ranx,2)) #Ratio of ranges
      ranx<-sprintf("%.2f",ranx)
      rany<-sprintf("%.2f",rany)
      CVtext<-paste("CWT range: ",ranx,", Model range: ",rany,sep="")
      CVrattext<-paste("Model-to-CWT range ratio: ",rat.ran,sep="")
      #Now put these in the output file for report
      Ocn.out$cwt.ran[i]<-ranx
      Ocn.out$mod.ran[i]<-rany
      Ocn.out$mod.cwt.rat[i]<-rat.ran
      Ocn.out$full.diff[i]<-diffmean
      Ocn.out$full.difflcb[i]<-lcb
      Ocn.out$full.diffucb[i]<-ucb
      
      #additional variables to isolate the last five brood years
      x1<-x[(length(x)-4):length(x)]
      y1<-y[(length(x)-4):length(x)]
      
      
      #Finally, let's make a graph    
      plot(x,y,ylim=c(0,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
           ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
      points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
      
      #fit regression and extract summary stats for figure and output file
      m1<-lm(y~x) #regression
      #summary(m1)
      R2<-format(round(summary(m1)$r.squared,3),digits=2)
      P.val<-format(round(anova(m1)[1,5],3),digits=3)
      
      # More goods for outfile
      Ocn.out$R2[i]<-R2
      Ocn.out$P.value[i]<-anova(m1)[1,5]
      #same difference variables as above, but for most recent five broods
      diff<-(y1-x1)
      diffmean<-sprintf("%.3f",mean(diff))
      difftest<-t.test(diff)
      lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1]))
      ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2]))
      Ocn.out$five.diff[i]<-diffmean
      Ocn.out$five.difflcb[i]<-lcb
      Ocn.out$five.diffucb[i]<-ucb
      
      #for tidier display of fitted regression   (full series)  
      ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
      ddd1<-predict.lm(m1,ddd)
      #only display regression if significant
      if(P.val<0.05)
      {
        CIcurves(y~x)
        if(P.val<0.001)
        {P.val="P < 0.001"}
        else
        {P.val=paste("P =",P.val)}
      } else 
      {P.val=paste("P =",P.val)}
      
      #now make final text strings and add them to figures using 'legend'
      mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
      legend(x=-0.08,y=0.97,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.1,y=1.07,
             c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
             box.lty=0,box.col=0,bg="transparent",cex=1.2)
      legend(x=-0.08,y=0.90,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.08,y=0.83,CVtext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      legend(x=-0.08,y=0.76,CVrattext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
      
      # add 1:1 line for reference and put a box around the plot
      abline(a=0,b=1,lty=2,lwd=1.25)
      box(lwd=2)
      #************************************************************************
      #************************************************************************
      #************************************************************************
      
      #Now add a single x-axis and y-axis for each page     
      mtext("Chinook Model ER",cex=1.2,side = 2,outer=TRUE,font=1)
      mtext("Coded Wire Tag ER",cex=1.2,side = 1,outer=TRUE,font=1)
    i=i+1
    }
    
    #write output files to spreadsheet
    #write.csv(x=Ocn.out, file = paste(this_is_the_place,"\\","OceanOut.csv",sep=""))
    #write.csv(x=Total.out, file = paste(this_is_the_place,"\\","TotalOut.csv",sep=""))
    
    dev.off() #done!
    #************************************************************************************
    #************************************************************************************
    #************************************************************************************
if(fish==10)
{
  dev.off() #done!
}
fish<-fish+1
tick<-tick+4
    

}







#bastard version for plotting
#side-by-side DIFFERENCE Ocean & Total ER scatterplots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window
i=1
fish<-1
tick<-1
while(fish<=round(length(levels(ModCWT))/4,0)) #Outer loop for figures
{
  
  #win.metafile(file = paste(this_is_the_place,"\\","DiffTotal&OceanER.emf",sep=""),height = 11, width = 8.5)
  jpeg(filename = paste(this_is_the_place,"\\",fish,"DiffTotal&OceanER.jpg",sep=""),
       width = 8.5, height = 11, units = "in",res=800,quality = 75)
  
  #pdf(file = paste(this_is_the_place,"\\","Total&OceanER.pdf",sep=""),height = 11, width = 8.5)
  pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
  par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
  
  Max<-tick+3
  if(Max>=length(levels(ModCWT)))
  {
    Max<-length(levels(ModCWT))
  }
  
  while(i<=Max)
  {
    #subset the dataset (total ER) for stock i
    stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
    name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
    x<-BroodYear[IndicatorAcronym==stk]
    y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
    
    #Single title for columns of figures (keep empty 'cept for top one)  
    mainname<-""
    
    pagetest=0
    for(k in 1:length(pages))
    {
      if(pages[k]==i)
      {pagetest<-1}
    }
    
    if(pagetest==1)
    {mainname<-"TOTAL"}
    
    #************************************************************************
    #*********TOTAL ER Diff Series First*************************************
    #************************************************************************
    #Compute differences for y axis
    diff<-y
    diffmean<-sprintf("%.3f",mean(diff)) #mean difference
    difftest<-t.test(diff) #paired t-test of H0: diff = 0
    tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
    Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
    lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
    ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
    difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
    
    #plot the series
    plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
         ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
    lines(x,y)
    points(x,y,pch=21,cex=1.5,col="black",bg="gray")
    
    #Add stock details and summary stats on difference to figure
    legend(x=1974.6,y=-.5,
           c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
           box.lty=0,box.col=0,bg="transparent",cex=1.2)
    legend(x=1975.5,y=-0.7,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
    
    #Add horizontal 0 reference line and a box
    abline(h=0,lty=2,lwd=1.25)
    box(lwd=2)
    
    #************************************************************************
    #*********Ocean ER Diff Series First*************************************
    #************************************************************************
    if(pagetest==1)
    {mainname<-"OCEAN"}  
    x<-BroodYear[IndicatorAcronym==stk]
    y<-mod.ocnER[IndicatorAcronym==stk]-TM_OcnER[IndicatorAcronym==stk]
    
    #Compute differences for y axis
    diff<-y
    diffmean<-sprintf("%.3f",mean(diff)) #mean difference
    difftest<-t.test(diff) #paired t-test of H0: diff = 0
    tstat<-format(as.numeric(difftest[1]),digits=3) # t-stat
    Pt<-format(as.numeric(difftest[3]),digits=3) # P-value for H0
    lcb<-sprintf("%.3f",as.numeric(difftest$conf.int[1])) #lower 95% CB of diff
    ucb<-sprintf("%.3f",as.numeric(difftest$conf.int[2])) #upper 95% CB of diff
    difftext<-paste("mean diff: ",diffmean," (",lcb,",",ucb,")",sep="") #for display
    
    #plot the series
    plot(x,y,ylim=c(-1,1),xlim=c(1976,2007),pch="",cex=2.5,col="gray",
         ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
    lines(x,y)
    points(x,y,pch=21,cex=1.5,col="black",bg="gray")
    
    #Add stock details and summary stats on difference to figure
    legend(x=1974.6,y=-.5,
           c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
           box.lty=0,box.col=0,bg="transparent",cex=1.2)
    legend(x=1975.5,y=-0.7,difftext,box.lty=0,bg="transparent",box.col=0,cex=0.9)
    
    #Add horizontal 0 reference line and a box
    abline(h=0,lty=2,lwd=1.25)
    box(lwd=2)
    
    #finally, add single x- and y-axes to page
    mtext("Model ER - CWT ER",cex=1.2,side = 2,outer=TRUE,font=1)
    mtext("Brood Year",cex=1.2,side = 1,outer=TRUE,font=1)
    i<-i+1
  }
  
  #write output files to spreadsheet
  #write.csv(x=Ocn.out, file = paste(this_is_the_place,"\\","OceanOut.csv",sep=""))
  #write.csv(x=Total.out, file = paste(this_is_the_place,"\\","TotalOut.csv",sep=""))
  
  dev.off() #done!
  #************************************************************************************
  #************************************************************************************
  #************************************************************************************
  if(fish==10)
  {
    dev.off() #done!
  }
  fish<-fish+1
  tick<-tick+4
}
min(by)






#bastard version for plotting
#The TIM Picture of the world
#side-by-side DIFFERENCE Ocean & Total ER scatterplots
#************************************************************************************
#************************************************************************************
#i=2 #for testing --> just choose an i and work through code within i loop to view in plot window
#i=1
#fish<-1
#tick<-1
#while(fish<=round(length(levels(ModCWT))/4,0)) #Outer loop for figures
#{
  
  #win.metafile(file = paste(this_is_the_place,"\\","DiffTotal&OceanER.emf",sep=""),height = 11, width = 8.5)
  #jpeg(filename = paste(this_is_the_place,"\\",fish,"DiffvCWTER.jpg",sep=""),
       #width = 8.5, height = 11, units = "in",res=800,quality = 75)
  
pdf(file = paste(this_is_the_place,"\\","TimTown.pdf",sep=""),height = 11, width = 8.5)
pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
for(i in 1:length(levels(ModCWT)))
{
  #Max<-tick+3
  #if(Max>=length(levels(ModCWT)))
  #{
    #Max<-length(levels(ModCWT))
  #}
  
  
    #subset the dataset (total ER) for stock i
    stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
    name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
    x<-TM_TotalER[IndicatorAcronym==stk]
    y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
    
    #Single title for columns of figures (keep empty 'cept for top one)  
    mainname<-""
    
    pagetest=0
    for(k in 1:length(pages))
    {
      if(pages[k]==i)
      {pagetest<-1}
    }
    
    if(pagetest==1)
    {mainname<-"TOTAL"}
    
    #************************************************************************
    #*********TOTAL ER Diff Series First*************************************
    #************************************************************************
  
    #additional variables to isolate the last five brood years
    x1<-x[(length(x)-4):length(x)]
    y1<-y[(length(x)-4):length(x)]
    
    
    #Finally, let's make a graph    
    plot(x,y,ylim=c(-1,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
         ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
    points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
    
    #fit regression and extract summary stats for figure and output file
    m1<-lm(y~x) #regression
    #summary(m1)
    R2<-format(round(summary(m1)$r.squared,3),digits=2)
    P.val<-format(round(anova(m1)[1,5],3),digits=3)
    

    #for tidier display of fitted regression   (full series)  
    ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
    ddd1<-predict.lm(m1,ddd)
    #only display regression if significant
    if(P.val<0.05)
    {
      CIcurves(y~x)
      if(P.val<0.001)
      {P.val="P < 0.001"}
      else
      {P.val=paste("P =",P.val)}
    } else 
    {P.val=paste("P =",P.val)}
    
    #now make final text strings and add them to figures using 'legend'
    mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
    legend(x=-0.08,y=0.8,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
    legend(x=-0.1,y=1.07,
           c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
           box.lty=0,box.col=0,bg="transparent",cex=1.2)

    # add 1:1 line for reference and put a box around the plot
    abline(h=0,lty=2,lwd=1.25)
    box(lwd=2)

    
    
    
    
    
    
    #************************************************************************
    #*********Ocean ER Diff Series First*************************************
    #************************************************************************
    if(pagetest==1)
    {mainname<-"OCEAN"}  
    x<-TM_OcnER[IndicatorAcronym==stk]
    y<-mod.ocnER[IndicatorAcronym==stk]-TM_OcnER[IndicatorAcronym==stk]
    
    #additional variables to isolate the last five brood years
    x1<-x[(length(x)-4):length(x)]
    y1<-y[(length(x)-4):length(x)]
    
    
    #Finally, let's make a graph    
    plot(x,y,ylim=c(-1,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
         ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
    points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
    
    #fit regression and extract summary stats for figure and output file
    m1<-lm(y~x) #regression
    #summary(m1)
    R2<-format(round(summary(m1)$r.squared,3),digits=2)
    P.val<-format(round(anova(m1)[1,5],3),digits=3)
    

    #for tidier display of fitted regression   (full series)  
    ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
    ddd1<-predict.lm(m1,ddd)
    #only display regression if significant
    if(P.val<0.05)
    {
      CIcurves(y~x)
      if(P.val<0.001)
      {P.val="P < 0.001"}
      else
      {P.val=paste("P =",P.val)}
    } else 
    {P.val=paste("P =",P.val)}
    
    #now make final text strings and add them to figures using 'legend'
    mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
    legend(x=-0.08,y=0.8,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
    legend(x=-0.1,y=1.07,
           c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
           box.lty=0,box.col=0,bg="transparent",cex=1.2)
    
    # add 1:1 line for reference and put a box around the plot
    abline(h=0,lty=2,lwd=1.25)
    box(lwd=2)
    
    #finally, add single x- and y-axes to page
    mtext("Model ER - CWT ER",cex=1.2,side = 2,outer=TRUE,font=1)
    mtext("CWT ER",cex=1.2,side = 1,outer=TRUE,font=1)
    i<-i+1
  }
  
  #write output files to spreadsheet
  #write.csv(x=Ocn.out, file = paste(this_is_the_place,"\\","OceanOut.csv",sep=""))
  #write.csv(x=Total.out, file = paste(this_is_the_place,"\\","TotalOut.csv",sep=""))
  
  dev.off() #done!
  #************************************************************************************
  #************************************************************************************
  #************************************************************************************
#  if(fish==10)
  {
#    dev.off() #done!
  }
#  fish<-fish+1
#  tick<-tick+4
#}

pdf(file = paste(this_is_the_place,"\\","TimTown_what.pdf",sep=""),height = 8.5, width = 11)
pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
par(mfrow=c(1,1),oma=c(2,2,4,2),mar=c(5,5,2,2)) #mar = B, L, T, R
  
  stk<-IndicatorAcronym
  name<-ModelStockAbbrev
  x<-TM_TotalER
  y<-mod.ER-TM_TotalER
  
  plot(x,y,ylim=c(-1,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
       xlab="CWT ER",ylab="Model ER - CWT ER",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
    #fit regression and extract summary stats for figure and output file
for(i in 1:length(levels(ModCWT)))
{

  
  #subset the dataset (total ER) for stock i
  stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
  name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
  x<-TM_TotalER[IndicatorAcronym==stk]
  y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
  
  
  #fit regression and extract summary stats for figure and output file
  m1<-lm(y~x) #regression

  R2<-format(round(summary(m1)$r.squared,3),digits=2)
  P.val<-format(round(anova(m1)[1,5],3),digits=3)
  
  #for tidier display of fitted regression   (full series)  
  ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
  ddd1<-predict.lm(m1,ddd)
  #only display regression if significant
  if(P.val<0.05)
  {
    CIcurves(y~x)
  }
}

x<-TM_TotalER
y<-mod.ER-TM_TotalER
  #text(x,y,labels=stk)
  m1<-lm(y~x) #regression
  #summary(m1)
  R2<-format(round(summary(m1)$r.squared,3),digits=2)
  P.val<-format(round(anova(m1)[1,5],3),digits=3)
  
  
  #for tidier display of fitted regression   (full series)  
  ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
  ddd1<-predict.lm(m1,ddd)
  #only display regression if significant
  if(P.val<0.05)
  {
    CIcurves(y~x)
    if(P.val<0.001)
    {P.val="P < 0.001"}
    else
    {P.val=paste("P =",P.val)}
  } else 
  {P.val=paste("P =",P.val)}
  
  #now make final text strings and add them to figures using 'legend'
  mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
  legend(x=0.0,y=0.95,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
  legend(x=-0.05,y=1.05,
         "All ERA and Model Stocks",
         box.lty=0,box.col=0,bg="transparent",cex=1.2)
  
  # add 1:1 line for reference and put a box around the plot
  abline(h=0,lty=2,lwd=1.25)
  box(lwd=2)
dev.off()











pdf(file = paste(this_is_the_place,"\\","TimTown.pdf",sep=""),height = 11, width = 8.5)
pages<-seq(1,48,4) #annoying variable for sequencing figures and adding general headers and axis labels 
par(mfrow=c(4,2),oma=c(2,2,4,2),mar=c(3,3,2,2)) #mar = B, L, T, R
for(i in 1:length(levels(ModCWT)))
{
  #Max<-tick+3
  #if(Max>=length(levels(ModCWT)))
  #{
  #Max<-length(levels(ModCWT))
  #}
  
  
  #subset the dataset (total ER) for stock i
  stk<-IndicatorAcronym[ModCWT==levels(ModCWT)[i]][1]
  name<-ModelStockAbbrev[IndicatorAcronym==stk][1]#CWTIndicatorName[IndicatorAcronym==stk][1]
  x<-TM_TotalER[IndicatorAcronym==stk]
  y<-mod.ER[IndicatorAcronym==stk]-TM_TotalER[IndicatorAcronym==stk]
  
  #Single title for columns of figures (keep empty 'cept for top one)  
  mainname<-""
  
  pagetest=0
  for(k in 1:length(pages))
  {
    if(pages[k]==i)
    {pagetest<-1}
  }
  
  if(pagetest==1)
  {mainname<-"TOTAL"}
  
  #************************************************************************
  #*********TOTAL ER Diff Series First*************************************
  #************************************************************************
  
  #additional variables to isolate the last five brood years
  x1<-x[(length(x)-4):length(x)]
  y1<-y[(length(x)-4):length(x)]
  
  
  #Finally, let's make a graph    
  plot(x,y,ylim=c(-1,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
       ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
  points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
  
  #fit regression and extract summary stats for figure and output file
  m1<-lm(y~x) #regression
  #summary(m1)
  R2<-format(round(summary(m1)$r.squared,3),digits=2)
  P.val<-format(round(anova(m1)[1,5],3),digits=3)
  
  
  #for tidier display of fitted regression   (full series)  
  ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
  ddd1<-predict.lm(m1,ddd)
  #only display regression if significant
  if(P.val<0.05)
  {
    CIcurves(y~x)
    if(P.val<0.001)
    {P.val="P < 0.001"}
    else
    {P.val=paste("P =",P.val)}
  } else 
  {P.val=paste("P =",P.val)}
  
  #now make final text strings and add them to figures using 'legend'
  mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
  legend(x=-0.08,y=0.8,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
  legend(x=-0.1,y=1.07,
         c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
         box.lty=0,box.col=0,bg="transparent",cex=1.2)
  
  # add 1:1 line for reference and put a box around the plot
  abline(h=0,lty=2,lwd=1.25)
  box(lwd=2)
  
  
  
  
  
  
  
  #************************************************************************
  #*********Ocean ER Diff Series First*************************************
  #************************************************************************
  if(pagetest==1)
  {mainname<-"OCEAN"}  
  x<-TM_OcnER[IndicatorAcronym==stk]
  y<-mod.ocnER[IndicatorAcronym==stk]-TM_OcnER[IndicatorAcronym==stk]
  
  #additional variables to isolate the last five brood years
  x1<-x[(length(x)-4):length(x)]
  y1<-y[(length(x)-4):length(x)]
  
  
  #Finally, let's make a graph    
  plot(x,y,ylim=c(-1,1),xlim=c(0,1),pch=21,cex=2.5,bg="white",col="black",
       ylab="",xlab="",cex.lab=1.5,cex.axis=1.2,main=mainname,cex.main=2)
  points(x1,y1,pch=21,bg="darkgray",col="black",cex=2.5)
  
  #fit regression and extract summary stats for figure and output file
  m1<-lm(y~x) #regression
  #summary(m1)
  R2<-format(round(summary(m1)$r.squared,3),digits=2)
  P.val<-format(round(anova(m1)[1,5],3),digits=3)
  
  
  #for tidier display of fitted regression   (full series)  
  ddd<-data.frame(x=seq(from=min(x,na.rm=T),to=max(x,na.rm=T),0.01))
  ddd1<-predict.lm(m1,ddd)
  #only display regression if significant
  if(P.val<0.05)
  {
    CIcurves(y~x)
    if(P.val<0.001)
    {P.val="P < 0.001"}
    else
    {P.val=paste("P =",P.val)}
  } else 
  {P.val=paste("P =",P.val)}
  
  #now make final text strings and add them to figures using 'legend'
  mmmm<-bquote(paste(R^2, " = ", .(R2),", ",.(P.val)))
  legend(x=-0.08,y=0.8,mmmm,box.lty=0,bg="transparent",box.col=0,cex=0.9)
  legend(x=-0.1,y=1.07,
         c(paste("Model stock", as.character(name),"vs.", as.character(stk))),
         box.lty=0,box.col=0,bg="transparent",cex=1.2)
  
  # add 1:1 line for reference and put a box around the plot
  abline(h=0,lty=2,lwd=1.25)
  box(lwd=2)
  
  #finally, add single x- and y-axes to page
  mtext("Model ER - CWT ER",cex=1.2,side = 2,outer=TRUE,font=1)
  mtext("CWT ER",cex=1.2,side = 1,outer=TRUE,font=1)
  i<-i+1
}

#write output files to spreadsheet
#write.csv(x=Ocn.out, file = paste(this_is_the_place,"\\","OceanOut.csv",sep=""))
#write.csv(x=Total.out, file = paste(this_is_the_place,"\\","TotalOut.csv",sep=""))

dev.off() #done!


