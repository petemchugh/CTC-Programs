rm(list=ls()) #Clear the workspace


this_is_the_place<-file.path(choose.dir()) #Get file path string for writing output file 
fc1<-read.table(file.choose(),header=T,sep="\t")
attach(fc1)

#...................................................................................
# NOTE: ALL FIGURES ARE WRITTEN AS PDFS BUT PNG OPTION ALSO EXISTS FOR EACH;
#       IF PNG OPTION IS USED, THE FIGS NEED TO BE RUN IN BATCHES B/C PNG DOESN'T
#       KNOW TO START A NEW PAGE WHEN THE PLOT SPACE IS FULL (PDF DOES)
# *** THERE ARE A HANDFUL OF LOCATIONS WHERE STK NUM or YR ARE HARD ENTERED
#     THESE LOCATIONS ARE FLAGGED WITH *** AND SHOULD BE MODIFIED AS NEEDED
#     EACH YEAR
#...................................................................................


#...................................................................................
# FC FIGS PART 1: Stock-by-stock FC performance (FC/Actual) plots; 
# black symbols correspond to agency-supplied forecasts, white symbols is model FC
#...................................................................................

pdf(file=paste(this_is_the_place,"\\","StockByStockPerformance.pdf",sep=""),height=7,width=10)
#Comment pdf and uncomment png if png is preferred
#png(file=paste(this_is_the_place,"\\","StockByStockPerformance.png",sep=""),width=10,height=7,units="in",res=400)
    i=1
    par(mfrow=c(3,5))
    while(i<27) # ***
    {
        stknam<-levels(Stk)[i]
        y<-1/(actual[Stk==stknam]/fc[Stk==stknam])
        x<-Year[Stk==stknam]
        colx<-as.character(kel[Stk==stknam])
        
        plot(x,y,
             pch="",bg="black",xlab="",ylab="Forecast/Actual",ylim=c(0,2),
             cex=2,main=stknam)
        j = 1
        while(j<=length(y))
        {
          points(x[j],y[j],pch=21,bg=colx[j],cex=2)
          j=j+1
        }
        #yr = yr+1
        abline(h=1,lty="dashed")
        box(lwd=2)
        i = i+1
    }
dev.off()




#...................................................................................
# FC FIGS PART 2: All Stocks, Year-by-Year FC performance (FC/Actual) plots; 
# black symbols correspond to agency-supplied forecasts, white symbols is model FC
#...................................................................................

pdf(file=paste(this_is_the_place,"\\","StkByStkWithinYRFCperf.pdf",sep=""),height=7,width=10)
#Comment pdf and uncomment png if png is preferred
#png(file=paste(this_is_the_place,"\\","StkByStkWithinYRFCperf.png",sep=""),width=10,height=7,units="in",res=400)
    yr=1999 #  ***   CHANGE START YEAR AS NEEDED/DESIRED
    par(mfrow=c(3,2))
    while(yr<=2013) # *** CHANGE END  YEAR AS NEEDED/DESIRED
    {
        y<-1/(actual[Year==yr]/fc[Year==yr])
        colx<-as.character(kel[Year==yr])
        cexx<-ptcex[Year==yr]
        x<-c(1:27)#levels(Stk[Year==yr])
        plot(x,y,pch="",xlab="",xaxt="n",ylab="Forecast/Actual",cex=2,main=yr,ylim=c(0,2.5))
          abline(h=1,lty="dashed")
        axis(1,cex.axis=1,lwd=2,at=c(1:27),labels=as.character(Stk[Year==yr]),las=2)
        box(lwd=2)
        j = 1
        while(j<=length(y))
        {
          points(x[j],y[j],pch=sym[Year==yr][j],bg=colx[j],cex=cexx[j])
          j=j+1
        }
        yr = yr+1
    }
dev.off()



















