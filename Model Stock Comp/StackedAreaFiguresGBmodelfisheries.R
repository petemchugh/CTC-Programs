###################################################################################################
#This program is designed to create stacked area time series figures of Chinook-model derived
#stock composition of actual landed catch; the R code is fairly straightforward; however, the 
#prep of modeloutput as needed for the program requires additional work, spearheaded by Gayle in 2012
#NOTE: This is another somewhat painful use of ggplot, but it gets it done as needed...
#      ALSO, be sure to relabel as needed the APPENDIX LETTER AND YEAR associated with captions, 
#      which are written into image files here as a matter of convenience/function 
###################################################################################################


rm(list=ls()) #Clear the workspace and load necessary packages
require(ggplot2)
require(gridExtra)

#set directory for file writing
this_is_the_place<-file.path(choose.dir())

#read in the data file
modcomp<-read.table(file.choose(),header=T)



figs = max(modcomp$FisheryNum)
i = 1

#This can be written into a single multi-page pdf OR as multiple individual jpgs; 
#the latter imports readily into MS Word but is sloppier for transport/other uses perhaps
#if doing pdf option, uncomment (1) the pdf line, (2) the print line and (3) the dev.off()
#************************************************************************************
#pdf(file=paste(this_is_the_place,"\\","ModStockComp.pdf",sep=""),height=8,width=13)
while(i<=figs)
{

    #take subset of data for each plot
    sub1<-subset(modcomp,modcomp$FisheryNum==i)
    
    #determine while running each fishery what the axis tick breaks should be
    lim<-max(tapply(sub1$Catch,sub1$Year,sum))
    if(lim <= 50000)
    {tickseq = seq(0,50,5)} else
      if(lim >= 300000)
      {tickseq = seq(0,1000,50)} else
        if(lim >= 200000)
        {tickseq = seq(0,300,30)} else
          if(lim >100000)
          {tickseq = seq(0,200,20)} else
            {tickseq = seq(0,100,10)}
      
    #the figure meat
    image<-ggplot(sub1, aes(x=sub1$Year, y=sub1$Catch/1000, fill=sub1$StockGroup)) + 
      geom_area(colour="black", size=.2, alpha=1) + 
      scale_fill_brewer(palette="Set3",name="Stock Group",
                        breaks=c('ORCST','CR-tule','CR-sp&su','CR-bright','WACST','PSD',
                                 'FR-late','FR-early','GS','WCVI','NCBC','SEAK')) + 
      theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=0, size=15,color="black")) +
      theme(axis.title.y = element_text(size=20),axis.text.y  = element_text(angle=0, size=15,color="black")) +
      labs(title = as.character(sub1$FisheryNameLong[1])) +
      theme(plot.title = element_text(size = rel(2))) +
      ylab("Catch (thousands)")+xlab("")+
      scale_x_continuous(expand=c(0,0),breaks=seq(1980,2010,5))+
      scale_y_continuous(expand=c(0,0),breaks=tickseq)+
      theme(legend.title = element_text(size=15, face="bold"))+
      theme(legend.text = element_text(size = 15))+
      theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
      theme(panel.background = element_blank())+ #removes the gray filled back
      theme(panel.border = element_rect(fill=NA,linetype = "solid",colour = "black")) #Adds a border to plot 
      
      xx<-as.character(sub1$FisheryNameLong)[1]
      Caption<-paste("Appendix E",i,"  Chinook Model estimates of landed catch stock composition for ", xx," 1979-2012",sep="")
      # the above bit adds a caption to the figures
    
      image <- arrangeGrob(image,sub=textGrob(Caption,x = 0, hjust = 0, vjust=0.1,
                                              gp = gpar( fontsize = 14)))
      #now save each one
      #Sloppy exception handling since file names can't have "\" in them      
      fname<-as.character(sub1$FisheryNameLong)[1]
      if(fname=="Washington/Oregon Troll")
        {fname<-"WashingtonOregon Troll"}
      if(fname=="North/Central BC Sport")
        {fname<-"NorthCentral BC Sport"}  
    
    ggsave(paste(this_is_the_place,"\\",i,fname,".jpg",sep=""),plot=image, width=13, height=8,dpi=800)
    
    #print(image)
    
    i = i+1
}
dev.off()
#************************************************************************************

