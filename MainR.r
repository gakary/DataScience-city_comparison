install.packages("tidyverse")
install.packages("dplyr")
Sys.setlocale("LC_ALL", "English")
View(BirmManPropertyStatus)

library(readxl)
library(dplyr)
require(tidyverse)






p<-ggplot(data=BandM_salesVolume, aes(x=dose,y=len))+geom_bar(stat="identity")



BirmManPropertyStatus <- read_excel("C:\\Users\\Desktop\\BirmManPropertyStatus.xlsx")


#Existing House Price Index :BirmManPropertyStatus




#Birmingham

BirmManPropertyStatus %>%
  filter(Name == "Birmingham") %>%
  ggplot(aes(x=`Pivotable date`,y=`House price index Existing properties`))+
  geom_point(size=2)+geom_line(size=1.5,colour="black")+
  geom_text(aes(label=`Average price Existing properties`),hjust=1, vjust=3)+
  geom_smooth()+
  labs(title = "The Existing House Price Index Of Birmingham")+
  ylab("Price Index")+
  xlab("Date")



      
#Existing House Price Index :BirmManPropertyStatus
#Manchester
  BirmManPropertyStatus %>%
  filter(Name == "Manchester") %>%
  ggplot(aes(x=`Pivotable date`,y=`House price index Existing properties`))+
  geom_point(size=4)+geom_line(size=1.5,colour="black")+
    geom_text(aes(label=`Average price Existing properties`),hjust=1, vjust=3)+
    geom_smooth(colour="red")+
  labs(title = "The Existing House Price Index Of Manchester")+
  ylab("Price Index")+
  xlab("Date")
    
  

#New Build House Price Index :BirmManPropertyStatus
#Birmingham
  BirmManPropertyStatus %>%
    filter(Name == "Birmingham") %>%
    ggplot(aes(x=`Pivotable date`,y=`House price index New build`))+
    geom_point(size=2)+geom_line(size=1.5,colour="black")+
    geom_text(aes(label=`Average price New build`),hjust=1, vjust=3)+
    geom_smooth()+
    labs(title = "The New Build House Price Index Of Birmingham")+
    ylab("Price Index")+
    xlab("Date")
  
  
  
#New Build House Price Index :BirmManPropertyStatus
#Manchester
  BirmManPropertyStatus %>%
    filter(Name == "Manchester") %>%
    ggplot(aes(x=`Pivotable date`,y=`House price index New build`))+
    geom_point(size=2)+geom_line(size=1.5,colour="black")+
    geom_text(aes(label=`Average price New build`),hjust=1, vjust=3)+
    geom_smooth(colour="red")+
    labs(title = "The New Build House Price Index Of Manchester")+
    ylab("Price Index")+
    xlab("Date")
  
  
  
#Property Sales Volume :BirmManPropertyStatus
  ##Compare two city their total Volume 
  
  aggregate(`Sales volume`~Name,BirmManPropertyStatus,sum)
  
  BandM_salesVolume <- data.frame(city=c("Birmingham","Manchester"),
                  salesVolume = c(14537,46072))
  
  
  ggplot(data=BandM_salesVolume, aes(x = city,y=salesVolume,fill=city))+geom_bar(stat="identity")+
    theme_minimal()+
    geom_text(aes(label=salesVolume),vjust=1.6,size=5,color="white")+
  ylab("Sales Volume")+
  xlab("City")+
  labs(title="The total property sales volume of two cities")
  
  #Birmingham Sales volume New build and Sales volume Existing properties
  
  aggregate(`Sales volume New build`~Name,BirmManPropertyStatus,sum)
  aggregate(`Sales volume Existing properties`~Name,BirmManPropertyStatus,sum)
  
  #Birmingham
  B_Sales <- data.frame(HouseType=c("New build","Existing properties"),
              salesVolume = c(1163,13374))
  
  
  c1  <- ggplot(data=B_Sales, aes(x=HouseType,y=salesVolume,fill=HouseType))+geom_bar(stat="identity")+
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
    theme_minimal()+
    geom_text(aes(label=salesVolume),vjust=1,hjust=1.4,size=5)+
    xlab("House Type")+
    ylab("Sales Volume")+
    labs(title="Compare two types of houses sales volume of Birmingham")
    c1 + coord_flip()
    
  
  
  #Manchester
  M_Sales <- data.frame(HouseType=c("New build","Existing properties"),
  salesVolume = c(4037,42035))
  
  c2<- ggplot(data=M_Sales, aes(x=HouseType,y=salesVolume,fill=HouseType))+geom_bar(stat="identity")+
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
    theme_minimal()+
    geom_text(aes(label=salesVolume),vjust=1,hjust=1.4,size=5)+
    xlab("House Type")+
    ylab("Sales Volume")+
    labs(title="Compare two types of houses sales volume of Manchester")
    c2 + coord_flip()
  

    library(readxl)
    population <- read_excel("population.xlsx", 
                             sheet = "2019", skip = 1)

    
    population %>%
      filter(variable == "Birmingham")%>%
      ggplot(aes(x=))

    
    
 
    
    library(readxl)
    QualityofLifeIndex <- read_excel("QualityofLifeIndex.xlsx")

    
    #Compare two city Quality of Life Index
    LifeIndex <- data.frame(Level=c("Moderate","High"),
                   QualityofLifeIndex=c(144.29,147.51),
                   City=c("Birmingham","Manchester"))
      
    
    ggplot(data=LifeIndex,aes(x=City,y=QualityofLifeIndex,fill=Level))+
      geom_bar(stat="identity",color="black",size=2)+
      geom_text(aes(label=QualityofLifeIndex),hjust=0.5, vjust=2)+
      ylab("Quality of Life Index")+
      labs(title="Compare two city Quality of Life Index")
      theme_minimal()
    
                    
      
    #Birmingham vs Manchester - Cost of Living Comparison
    #https://livingcost.org/cost/birmingham/manchester
    
    
    library(readxl)
    CostOfLiving <- read_excel("CostOfLiving.xlsx")
 
    
  
    Cost_City <-c(rep(CostOfLiving$City,7),c(rep(CostOfLiving$City,7)))
    Filed <-rep(c("Cost of living One person","Cost of living Family","One person rent","Family rent","Food Expenses","Transport Expenses","Monthly salary after tax"),2)
    Cost_V <-c(1284,3115,697,1154,327,114,1699,1274,3115,699,1179,337,135,2327)
    Cost_data <-data.frame(Cost_City,Filed,Cost_V)

    ggplot(Cost_data,aes(fill=Filed,y=Cost_V,x=Cost_City))+
      geom_bar(position="dodge",stat="identity")+
      ylab("Pounds (£GBP)")+
      xlab("City")+
      ylim(NA,3500)+
      labs(title="Cost of Living Comparison")
      
    library(readxl)
    AirPollution <- read_excel("AirPollution.xlsx")
#https://www.numbeo.com/pollution/compare_cities.jsp?country1=United+Kingdom&city1=Manchester&country2=United+Kingdom&city2=Birmingham
    
    AirPollution_City <-c(rep("Birmingha",8),c(rep("Mancheste",8)))
    PollutionFiled <- rep(c("Air Pollution","Drinking Water Pollution and Inaccessibility","Dissatisfaction with Garbage Disposal","Dirty and Untidy","Noise and Light Pollution","Water Pollution","Dissatisfaction to Spend Time in the City","Dissatisfaction with Green and Parks in the City"),2)
    Air_V <-c(46.51,26.28,46.25,58.75,53.12,25.62,35.37,41.45,47.55,28.12,54.69,63.02,55.43,37.5,40.82,47.28)
    Air_data <-data.frame(AirPollution_City,Pollution_Filed,Air_V)
    
    ggplot(Air_data,aes(y=Air_V,x=AirPollution_City,fill=PollutionFiled))+
      geom_bar(position="dodge",stat="identity")+
      ylab("Index")+
      xlab("City")+
      labs(title="Pollution Comparison Of Two Cities")+
      ylim(NA,70)
      
    
    
    #Purity and Cleanliness Comparison of Two Cities
    #https://www.numbeo.com/pollution/compare_cities.jsp?country1=United+Kingdom&city1=Manchester&country2=United+Kingdom&city2=Birmingham
    Air_Quality_City <-c(rep("Birmingha",8),c(rep("Mancheste",8)))
    AirQuality_Filed <-rep(c("Air Quality","Drinking Water Quality and Accessibility","Garbage Disposal Satisfaction","Clean and Tidy","Quiet and No Problem with Night Lights","Water Quality","Comfortable to Spend Time in the City","Quality of Green and Parks"),2)
    AirQuality_V <-c(53.49,73.72,53.75,41.25,46.88,74.38,64.63,58.55,52.45,71.88,45.31,36.98,44.57,62.5,59.18,52.72)
    Air_Quality_data <-data.frame(Air_Quality_City,AirQuality_Filed,AirQuality_V)
    
    ggplot(Air_Quality_data,aes(y=AirQuality_V,x=Air_Quality_City,fill=AirQuality_Filed))+
      geom_bar(position="dodge",stat="identity")+
      ylab("Index")+
      xlab("City")+
      labs(title="Purity and Cleanliness Comparison of Two Cities")+
      ylim(NA,80)
    

    #Purity and Cleanliness indexes component of Birmingham
    library(readxl)
    PurityandCleanlinessLevelIndex <- read_excel("PurityandCleanlinessLevelIndex.xlsx")
    PurityandCleanlinessLevelIndex %>%
      filter(City=="Birmingham") %>%
      ggplot(aes(x=`Types of Index`,y=Index,colour=`Types of Index`))+
      geom_point(size=5)+
      geom_text(aes(label=`Level of Purity and Cleanliness`),vjust=1.5,hjust=0,size=7)+
      labs(title="Purity and Cleanliness indices component of Birmingham")
      
    
    
    #Purity and Cleanliness indexes component of Manchester
    PurityandCleanlinessLevelIndex %>%
      filter(City=="Manchester") %>%
      ggplot(aes(x=`Types of Index`,y=Index,colour=`Types of Index`))+
      geom_point(size=5)+
      geom_text(aes(label=`Level of Purity and Cleanliness`),vjust=1.5,hjust=0,size=7)+
      labs(title="Purity and Cleanliness indices component of Manchester")
    
    
    
    
      #Living indexes component of Birmingham
      QualityofLifeIndex %>%
        filter(City=="Birmingham")%>%
        ggplot(aes(x=`Type of Index`,y=Index,colour=`Type of Index`))+
        geom_point(size=5)+
        ylim(NA,100)+
        labs(title="Living indexes component of Birmingham")+
        geom_text(aes(label=`Level of quality`),vjust=1.5,hjust=0.8,size=7)
       
      
      
    

    #Living indexes component of Manchester
    QualityofLifeIndex %>%
      filter(City=="Manchester")%>%
      ggplot(aes(x=`Type of Index`,y=Index,colour=`Type of Index`))+
      geom_point(size=5)+
      ylim(NA,100)+
      labs(title="Living indexes component of Manchester")+
      geom_text(aes(label=`Level of quality`),vjust=1.5,hjust=0.8,size=7)
      
    
    #Pollution Indexes component of Birmingham
    library(readxl)
    QualityofPollution <- read_excel("QualityofPollution.xlsx")
    
    QualityofPollution %>%
      filter(City=="Birmingham")%>%
      ggplot(aes(x=`Type of Index`,y=Index,colour=`Type of Index`))+
      geom_point(size=5)+
      labs(title="Pollution indices component of Birmingham")+
      geom_text(aes(label=`Level of Quality`),vjust=1.5,hjust=0,size=7)
      
    
    #Pollution Indexes component of Manchester

    
    QualityofPollution %>%
      filter(City=="Manchester")%>%
      ggplot(aes(x=`Type of Index`,y=Index,colour=`Type of Index`))+
      geom_point(size=5)+
      labs(title="Pollution indices component of Manchester")+
      geom_text(aes(label=`Level of Quality`),vjust=1.5,hjust=0,size=7)
    

    #programmer average income #class door
    library(readxl)
    JobInCome <- read_excel("JobInCome.xlsx", 
                            skip = 4)
    
    
    
    Prog_income <-data.frame(Region=c("UK","Mancenter","Birmingham"),
                             (Programmer_Income=c(44024,37347,34569)))
    ggplot(data=Prog_income,aes(x=Region,y=Programmer_Income,fill=Region))+
      geom_bar(stat="identity",colour="black",size=2)+
      ylab("Pounds (£GBP)")+
      labs(title="Software Developer Average Income (Annual full-time)")+
      geom_text(aes(label=Programmer_Income),vjust=1.5,hjust=0.5,size=5)
    
    
    
    
    #UK average income(Annual) 
    #https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8
   

    
    
    
    