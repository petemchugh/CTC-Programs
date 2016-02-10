###################################################################################################
#This program is designed to create stacked bar figure versions of the distribution table outputs
#I apologize in advance for the seemingly punitive use of ggplot2--there's no pretty way about it
#but once it's set up you're pretty much set; note also that this program requires some decisions
#about data aggregation/subsetting that need to be defined in the input file (see xlsx/txt files)
###################################################################################################


rm(list=ls()) #Clear the workspace and load necessary packages
require(ggplot2)
require(reshape2)
require(grid)
require(RColorBrewer)

#set directory for writing outputs
this_is_the_place<-file.path(choose.dir())

#Read in All data
distntab<-read.table(file.choose(),header=T,stringsAsFactors=FALSE)
#Subset 2009+ Data
#***NOTE: Subsetting by a different period requires action in the preparation of the input data
#***i.e., during the translation of DistnTable output into text files for R
distntab2<-subset(distntab,Period =="a09on")
#Subset 1999-2008 Data
#***NOTE: Subsetting by a different period requires action in the preparation of the input data
#***i.e., during the translation of DistnTable output into text files for R
distntab<-subset(distntab,Period =="a99to08")

#Transform the summary table format from distn table into a dataframe useable in analysis
disttabDF<-melt(distntab, id=c("Period","Abbrev","FullName","StkNum","Group","GroupNum","Nyears"))
disttabDF2<-melt(distntab2, id=c("Period","Abbrev","FullName","StkNum","Group","GroupNum","Nyears"))


#Set color scheme
my.cols <- brewer.pal(10, "Paired")
my.cols[10]<-"#000000"  #Add an additional color (dark gray) for strays



#---------------------------------------------------------------------------------
#************MAKE THE GRAPHS NOW********************
#---------------------------------------------------------------------------------

#---------------------------------------------------------------------------------
#First set of figures--1999-2008 Agreement Period
i=1
numfig<-max(disttabDF$GroupNum) #how many regional groupings are in the dataset?

while (i<=numfig)
{

    group<-as.character(disttabDF$Group[disttabDF$GroupNum==i][1]) #for subsetting
    sub1<-subset(disttabDF,disttabDF$Group==group)
    nbar<-length(levels(as.factor(sub1$Abbrev))) #how many stocks in group?
    wid<-13 #total width to scale to (for 12 groups = PS)
    hig<-8 #default heighth
    rescale<-2.91+0.841*nbar #this is for rescaling variably depending on number of bars included
    
      #the guts, ggplot2 is slightly unweildy
      p = ggplot(data=sub1,aes(x=rev(factor(sub1$Abbrev)),
                               y=rev(sub1$value),
                               fill = rev(factor(sub1$variable))))
      p = p + geom_bar(width = 0.85,color="black",alpha=0.8,stat="identity") #this is the bar portion
      p = p + scale_fill_manual(values=my.cols,name="Fishery", #change the color scheme as needed
                                breaks=c('SEAK','NBC','WCVI','CanOther','CanGS',
                                        'WAORcst','PgSnd','Term','Esc','Strays'))+
      
    #   p = p + scale_fill_brewer(palette="Paired",name="Fishery", #change the color scheme as needed
    #                             breaks=c('SEAK','NBC','WCVI','CanOther','CanGS',
    #                                      'WAORcst','PgSnd','Term','Esc','Strays'))+
        theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=0, size=16,color="black")) +
        theme(axis.title.y = element_text(size=20,vjust = 0.3),axis.text.y=element_text(angle=0, size=16,color="black"))+ #resize tick labels, move axis lablel (vjust)
            ylab("Total Mortality (%)")+xlab("")+
        labs(title = "1999-2008")+ 
        theme(legend.title = element_text(size=15, face="bold"))+
        theme(legend.text = element_text(size = 15))+
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
        theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
        theme(panel.background = element_blank())+ #removes the gray filled back
        scale_y_continuous(expand=c(0,0), #this gets labels closer to axis
                           breaks=c(0,0.20,0.40,0.6,0.8,1),
                           labels=c("0","20","40","60","80","100"))+
        scale_x_discrete(expand=c(0,0))+ #this gets labels closer to axis, gets rid of gap    
        theme(panel.background = element_rect(colour="black",size=1)) #this adds a border
        p = p+theme(plot.title = element_text(size = 28,vjust=1))
    
    
    
        #print(p) #for viewing in development/testing mode
        ggsave(paste(this_is_the_place,"\\",group,"_99To08.png",sep=""),p,height=hig,width=rescale,dpi=400)
      i = i+1
}
#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------
#Second set of figs -- 2009 To Present
i=1
numfig<-max(disttabDF2$GroupNum) #how many regional groupings are in the dataset?

while (i<=numfig)
{
  
      group<-as.character(disttabDF2$Group[disttabDF2$GroupNum==i][1]) #for subsetting
      sub1<-subset(disttabDF2,disttabDF2$Group==group)
      nbar<-length(levels(as.factor(sub1$Abbrev))) #how many stocks in group?
      wid<-13 #total width to scale to (for 12 groups = PS)
      hig<-8 #default heighth
      rescale<-2.91+0.841*nbar #this is for rescaling variably depending on number of bars included
      
      #the guts, ggplot2 is slightly unweildy
      p = ggplot(data=sub1,aes(x=rev(factor(sub1$Abbrev)),
                               y=rev(sub1$value),
                               fill = rev(factor(sub1$variable))))
      p = p + geom_bar(width = 0.85,color="black",alpha=0.8,stat="identity") #this is the bar portion
      p = p + scale_fill_manual(values=my.cols,name="Fishery", #change the color scheme as needed
                                breaks=c('SEAK','NBC','WCVI','CanOther','CanGS',
                                         'WAORcst','PgSnd','Term','Esc','Strays'))+
        
        #   p = p + scale_fill_brewer(palette="Paired",name="Fishery", #change the color scheme as needed
        #                             breaks=c('SEAK','NBC','WCVI','CanOther','CanGS',
        #                                      'WAORcst','PgSnd','Term','Esc','Strays'))+
        theme(axis.title.x = element_text(size=20),axis.text.x  = element_text(angle=0, size=16,color="black")) +
        theme(axis.title.y = element_text(size=20,vjust = 0.3),axis.text.y=element_text(angle=0, size=16,color="black"))+ #resize tick labels, move axis lablel (vjust)
        ylab("Total Mortality (%)")+xlab("")+
        labs(title = "2009 to Present")+ 
        theme(legend.title = element_text(size=15, face="bold"))+
        theme(legend.text = element_text(size = 15))+
        theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+ #removes the background grid
        theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())+ #removes the background grid
        theme(panel.background = element_blank())+ #removes the gray filled back
        scale_y_continuous(expand=c(0,0), #this gets labels closer to axis
                           breaks=c(0,0.20,0.40,0.6,0.8,1),
                           labels=c("0","20","40","60","80","100"))+
        scale_x_discrete(expand=c(0,0))+ #this gets labels closer to axis, gets rid of gap    
        theme(panel.background = element_rect(colour="black",size=1)) #this adds a border
      p = p+theme(plot.title = element_text(size = 28,vjust=1))
      
      
      
      #print(p) #for viewing in development/testing mode
      ggsave(paste(this_is_the_place,"\\",group,"_09ToPres.png",sep=""),p,height=hig,width=rescale,dpi=400)
      i = i+1
}
#---------------------------------------------------------------------------------
