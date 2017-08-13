#analyze data:


AnalyzeKelp <- function(){
  
  print("This program displays pictures randomly and asks the user to score up to 3 individual kelp blades that can be witnessed in the frame. Blades must be from seperate individuals")
  
  
  datafiles <- list.files("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data")
  datalength <- as.numeric(length(datafiles))
  
  #randomly select photos
  PicList <- sample(seq(from = 1, to = datalength, by = 1), replace = FALSE)
  
  print("hit esc to exit program at any time. Bad photos will be remembered and disregarded, and photos you analyze will be saved in the current run.")
  
  print("Which run are you on?")
  a <- readline("Enter an integer: ")
  a <- as.numeric(a)
  
  #  if(file.exists("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_suckvec.csv")){
  #  knownsuck <- read.csv("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_suckvec.csv")
  #  suckvec <- data.frame(knownsuck)[,2]
  #  PicList <- PicList[-c(data.frame(knownsuck)[,2])]
  #  }
  #  else{
  #    suckvec <-c()
  #  }
  
  #  if(file.exists(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_usefulvec_run",a,".csv"))){
  #    knownuse <- read.csv(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_usefulvec_run",a,".csv"))
  #    usefulvec <- data.frame(knownuse)[,2]
  #    PicList <- PicList[-c(data.frame(knownuse)[,2])]
  #  }
  #  else{
  #    usefulvec <-c()
  #  }
  
  if(file.exists(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run log#",a,".csv"))){
    save <- read.csv(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run log#",a,".csv"))
    #    usefulvec <- data.frame(knownuse)[,2]
    PicList <- PicList[-c(data.frame(save)[,2])]
    #  }
    #  else{
    #    usefulvec <-c()
  }
  
  sitedata <- read.csv("/Users/danielwoodrich/Desktop/WWU Directory/WWU fourth year 2015-2016 classes/Spring classes 2016/Algae/Kelpdata_NWSA.csv")
  
  sitedata$InVis <- ""
  sitedata$RuffleScore <- ""
  sitedata$WidthScore <- ""
  sitedata$PicNum <- ""
  
  if(file.exists(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run #",a,".csv"))){
    analdata <- read.csv(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run #",a,".csv"))
  }else{
    analdata <- sitedata
    analdata$InVis <- ""
    analdata$RuffleScore <- ""
    analdata$WidthScore <- ""
    analdata$PicNum <- ""
    for(x in 1:length(sitedata)){
      analdata[,names(sitedata)[x]] <- ""
    }
  }
  
  if(file.exists(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run log#",a,".csv"))){
    nframe <- save
    nlist <- c(save$nlist)
    dnames <- c(save$dnames)
  }else{
    nframe <- data.frame("n","datafiles.n.")
    nlist <- c()
    dnames <- c()
  }
  counter <- 0
  for(n in PicList){
    nlist <- c(nlist,n)
    dnames <- c(dnames,datafiles[n])
    nframe <- data.frame(nlist,dnames)
    counter <- counter+1
    print(paste("You are on picture number",counter,"out of",length(PicList)),sep=" ")
    print("Please wait...")
    par(oma=c(0.1,0.1,0.1,0.1),mar=c(0,0,0,0))
    plot(c(100, 200), c(300, 400), type = "n", xlab = "", ylab = "",bty="n",xaxt='n',yaxt='n',xaxs='i',yaxs='i')
    rasterImage(readJPEG(paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data/",datafiles[n],sep=""),native=TRUE), 100, 300, 200, 400,interpolate=TRUE)
    
    b <- substring(datafiles[n],1,3)
    print(b)
    c <- as.vector(sitedata[which(sitedata$Site_ID==b),])
    print(c)
    
    print("How many distinct kelp blades would you like to score?")
    d <- readline("Enter an integer from 0 to 3: ")
    
    if(d==0){
      # suckvec <- c(suckvec,n)
    }
    
    else{
      #  usefulvec <- c(usefulvec,n)
      rufvec <- c()
      widvec <- c()
      invec <- c()
      for(i in 1:d){
        e <- as.numeric(readline(paste("Kelp blade #",i,": Estimate inches of blade visible ")))
        invec <- c(invec,e)
        f <- as.numeric(readline(paste("Kelp blade #",i,": Enter ruffle score 1-5 ")))
        rufvec <- c(rufvec,f)
        g <- as.numeric(readline(paste("Kelp blade #",i,": Enter width score 1-5 ")))
        widvec <- c(widvec,g)
      }
      
      dt <- data.frame(c)
      if(d ==2){
        dt <- rbind(dt,c)
      }
      if(d ==3){
        dt <- rbind(dt,c)
        dt <- rbind(dt,c)
      }
      
      if(nchar(datafiles[n])==16){
        PicNum <- substring(datafiles[n],5,6) 
      }else{
        PicNum <- substring(datafiles[n],5,5) 
      }
      print(n)
      print(PicNum)
      print(datafiles[n])
      dt$InVis <- invec
      dt$RuffleScore <- rufvec
      dt$WidthScore <- widvec
      dt$PicNum <- as.numeric(PicNum)
      
      #two extra columns can randomly show up in analdata after output
      if(length(dt)<length(analdata)){
        dt$X.1<-NA
        dt$X <-NA
      }
      analdata <- rbind(analdata,dt)
      
      contab <- data.frame(table(analdata$Site_ID))
      print("check1")
      if(any(contab$Freq>5)){
        print("check2")
        h <- as.character(contab[which(contab$Freq>5),1])
        analdata_h <- subset(analdata,analdata$Site_ID==h)
        analdata <- analdata[-c(which(analdata$Site_ID==h)),]
        print("check3")
        while(nrow(analdata_h)>=6){
          print("check4")
          analdata_h <- analdata_h[-which(analdata_h$InVis==min(analdata_h$InVis))[1],]
        }
        analdata <- rbind(analdata,analdata_h)
      }
      
      
    }
    
    
    write.csv(analdata,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run #",a,".csv"),row.names=FALSE)
    
    #write.csv(suckvec,"/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_suckvec.csv")
    
    #write.csv(usefulvec,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_usefulvec_run",a,".csv"))
    
    write.csv(nframe,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run log#",a,".csv"),row.names=FALSE)
    
    dev.off()
  }
  
  write.csv(analdata,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run #",a,".csv"),row.names=FALSE)
  
  write.csv(nframe,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata run log#",a,".csv"),row.names=FALSE)
  
  #write.csv(suckvec,"/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_suckvec.csv")
  
  #write.csv(usefulvec,paste("/Volumes/Seagate Backup Plus Drive/Bull kelp subfolders/Data export/analdata_usefulvec_run",a,".csv"))
  
  print("End Program")
}

AnalyzeKelp()
