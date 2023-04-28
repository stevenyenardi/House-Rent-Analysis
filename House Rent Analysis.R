install.packages("ggplot2")
install.packages("Hmisc")
install.packages("gridExtra")
install.packages("ggcorrplot")
install.packages("ggstatsplot")
install.packages("ggside")
library(ggplot2)
library(Hmisc)
library(gridExtra)
library(ggcorrplot)
library(ggstatsplot)
library(ggside)
library(lubridate)
library(tidyverse)

#import dataset
houserent = read.csv("C:\\Users\\Republic Of Gamers\\OneDrive - Asia Pacific University\\YEAR 2 SEM 1\\PROGRAMMING FOR DATA ANALYSIS (R)\\House_Rent_Dataset.csv", 
                     header = TRUE)
houserent
View(houserent)

#remove outliers
outliers = function(df, column, low, high) {
  quartiles = quantile(df[, column], probs=c(low, high), na.rm = FALSE)
  IQR <- IQR(df[, column])
  
  Lower =quartiles[1] - 1.5 * IQR
  High = quartiles[2] + 1.5 * IQR
  
  noOutlier = df[(df[, column] > Lower) & (df[, column] < High), ]
  
  return(noOutlier)
}

houseRentOutlier <- outliers(houserent, "Rent", 0.15, 0.97)

#number of column of the dataset
ncol(houserent)

#number of row of the dataset
nrow(houserent)

#data structure used 
class(houserent)

#summary of the dataset (mean, median, mode, min, max)
summary(houserent)

#head of dataset
head(houserent)

#tail of dataset
tail(houserent)

#number of categories in Furnishing.Status column
describe(houserent$Furnishing.Status)

#number of categories in Point.of.Contact column
poc = unique(houserent$Point.of.Contact)
length(poc)
poc
describe(houserent$Point.of.Contact)

#number of categories in Area.type column
floor = unique(houserent$Area.Type)
length(floor)
floor
describe(houserent$Area.Type)


#number of categories in Tenant.Preferred
tenant = unique(houserent$Tenant.Preferred)
length(tenant)
table(houserent$Tenant.Preferred)
tenant

#number of bedrooms
bedrooms = unique(houserent$BHK)
length(bedrooms)
table(houserent$BHK)
describe(houserent$BHK)
bedrooms

#number of bathrooms
bathrooms = unique(houserent$Bathroom)
length(bathrooms)
bathrooms

#Question 1: What type of house preferred by Bachelors
#ANALYSIS 1-1: The rent preferred by Bachelors
editBachelors = houseRentOutlier$Tenant.Preferred == "Bachelors"
Rent = houseRentOutlier$Rent
editRent = ggplot(houseRentOutlier[editBachelors, ], aes(x = Rent)) + 
          geom_histogram(color = "slateblue", fill = "slategrey") + 
          ggtitle("Histogram of Rent Price Preferred by Bachelors") +
          labs(y = "Frequency", x = "Rent Price")

bachelors = houserent$Tenant.Preferred == "Bachelors"
oriRent = ggplot(houserent[bachelors, ], aes(x = Rent)) + 
          geom_histogram(color = "slateblue", fill = "slategrey") + 
          ggtitle("Histogram of Rent Price Preferred by Bachelors") +
          labs(y = "Frequency", x = "Rent Price")

grid.arrange(oriRent, editRent, nrow = 2)

bachelors = houserent$Tenant.Preferred == "Bachelors"
Rent = houserent$Rent
ggplot(houserent[bachelors, ], aes(x = Rent)) + 
  geom_histogram(color = "slateblue", fill = "slategrey") + 
  ggtitle("Histogram of Rent Price Preferred by Bachelors") +
  labs(y = "Frequency", x = "Rent Price")

#ANALYSIS 1-2: Number of BHK preferred by Bachelors
bachelors = houserent$Tenant.Preferred == "Bachelors"
BHK = houserent$BHK
ggplot(houserent[bachelors, ], aes(x = BHK)) + 
  ggtitle("Histogram of Number of BHK Preferred by Bachelors") +
  geom_histogram(color = "black", fill = "deeppink") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#ANALYSIS 1-3: The Density of house size that is preferred by Bachelors
bachelors = houserent$Tenant.Preferred == "Bachelors"
size = houserent$Size
ggplot(houserent, aes(x = size)) + geom_density(color = "black", fill ="cyan") + 
  ggtitle("DENSITY OF HOUSE SIZE PREFERRED BY BACHELORS") +
  labs(y = "Density", x = "Bachelor House Size")

#ANALYSIS 1-4: What are the Area Type do Bachelors prefer
areaTypeBachelors = houserent$Area.Type[houserent$Tenant.Preferred == "Bachelors"]
areaType_df = data.frame(areaTypeBachelors)

ggplot(areaType_df) + 
  geom_bar(aes(areaTypeBachelors), colour = "black", fill = c("cadetblue2", "dodgerblue3")) +
  ggtitle("AREA TYPE PREFERRED BY BACHELORS") +
  labs(y = "Frequency", x = "Area Type")

#ANALYSIS 1-5: Point of Contact that most bachelors contacted for rental
bachelors_poc = houserent$Point.of.Contact[houserent$Tenant.Preferred == "Bachelors"]
poc_df = data.frame(bachelors_poc)

ggplot(poc_df) + geom_bar(aes(x = bachelors_poc), color = "black", fill = c("mediumseagreen","medium purple")) + 
  ggtitle("Point of Contact That Most Bachelors Contacted For Rental") + 
  labs(y ="Frequency", x = "Point of Contact")

#ANALYSIS 1-6: Bachelors' Furnishing Preference 
bachelors_furnishing = houserent$Furnishing.Status[houserent$Tenant.Preferred == "Bachelors"]
furnishing_df = data.frame(bachelors_furnishing)

ggplot(furnishing_df) + geom_bar(aes(bachelors_furnishing), col = "black", fill = c("darkorange", "gold1", "deepskyblue")) + 
  ggtitle("BACHELOR'S FURNISHING PREFERENCE") + 
  labs(y = "frequency", x = "Furnish Status")

#ANALYSIS 1-7: The city that have the most Bachelor tenants
bachelors_city = houserent$City[houserent$Tenant.Preferred == "Bachelors"]
city_df = data.frame(bachelors_city)

ggplot(city_df) + 
  geom_bar(aes(bachelors_city), color = "black", 
           fill = c("lightpink","lightsalmon","lightskyblue","seagreen1","orangered","steelblue1")) + 
  ggtitle("CITIES WITH MOST BACHELOR TENANTS") + 
  labs(y ="Frequency", x = "Cities")

#Analysis 1-8: Comparison between the number of bachelors prefer 1 bedroom with different furnishing status

bed1_bachelors_furnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                            (houserent$BHK == "1") & 
                                            (houserent$Furnishing.Status == "Furnished"), ])

bed1_bachelors_unfurnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                              (houserent$BHK == "1") & 
                                              (houserent$Furnishing.Status == "Unfurnished"), ])

bed1_bachelors_semiFurnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                                (houserent$BHK == "1") & 
                                                (houserent$Furnishing.Status == "Semi-Furnished"), ])


furnishingStatus= c(bed1_bachelors_furnished , bed1_bachelors_unfurnished , bed1_bachelors_semiFurnished)
percentageFurnishingStatus = round(furnishingStatus/sum(furnishingStatus)*100)
statusName = c("Furnished","Unfurnished","Semi-Furnished")
statusLabel = paste(statusName,"-", percentageFurnishingStatus, "%", sep = "")
pie(furnishingStatus ,statusLabel,radius = 1, 
    main ="PROPORTION OF FURNISHING STATUS PREFERRED BY BACHELORS WITH 2 BHK", 
    col = c("bisque","coral","cadetblue"), clockwise = TRUE)

#Analysis 1-9: Piechart for the percentage of bachelors prefer 2 BHK with different furnishing status
bed2_bachelors_furnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                            (houserent$BHK == "2") & 
                                            (houserent$Furnishing.Status == "Furnished"), ])


bed2_bachelors_unfurnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                              (houserent$BHK == "2") & 
                                              (houserent$Furnishing.Status == "Unfurnished"), ])

bed2_bachelors_semiFurnished = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                                (houserent$BHK == "2") & 
                                                (houserent$Furnishing.Status == "Semi-Furnished"), ])

furnishingStatus= c(bed2_bachelors_furnished , bed2_bachelors_unfurnished , bed2_bachelors_semiFurnished)
percentageFurnishingStatus = round(furnishingStatus/sum(furnishingStatus)*100)
statusName = c("Furnished","Unfurnished","Semi-Furnished")
statusLabel = paste(statusName,"-", percentageFurnishingStatus, "%", sep = "")
pie(furnishingStatus ,statusLabel,radius = 1, 
    main ="PROPORTION OF FURNISHING STATUS PREFERRED BY BACHELORS WITH 2 BHK", 
    col = c("lightskyblue2","navajowhite","salmon"), clockwise = TRUE)

#Analysis 1 - 10: Comparison of Bachelors that pays the rent price less than 10000 and Bachelors who pays more than 10000
bachLess = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent $Rent < 10000), ])

bachMore = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$Rent > 10000), ])

RentPrice= c(bachLess, bachMore)
percentageRent = round(RentPrice/sum(RentPrice)*100)
statusName = c("< 10000","> 10000")
statusLabel = paste(statusName,"-", percentageRent, "%", sep = "")
pie(RentPrice ,statusLabel,radius = 1, 
    main ="PROPORTIONS OF BACHELORS WHO PAYS MORE THAN 10000 AND LESS THAN 10000", 
    col = c("lavenderblush3","lavenderblush4"), clockwise = TRUE)

#Analysis 1-11: Comparison of the proportion of Bachelors in each City that rented from an Owner
bach_Kolkata_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                      (houserent$City == "Kolkata") & 
                                      (houserent$Point.of.Contact == "Contact Owner"), ])

bach_Mumbai_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                     (houserent$City== "Mumbai") &
                                     (houserent$Point.of.Contact== "Contact Owner"), ])

bach_Chennai_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                      (houserent$City== "Chennai") & 
                                      (houserent$Point.of.Contact== "Contact Owner"), ])

bach_Delhi_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                    (houserent$City== "Delhi") & 
                                    (houserent$Point.of.Contact== "Contact Owner"), ])

bach_Bangalore_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                        (houserent$City== "Bangalore") & 
                                        (houserent$Point.of.Contact== "Contact Owner"), ])


bach_Hyderabad_Owner = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                        (houserent$City== "Hyderabad") & 
                                        (houserent$Point.of.Contact== "Contact Owner"), ])


bachCitiesOwner= c(bach_Kolkata_Owner, bach_Mumbai_Owner, bach_Delhi_Owner, bach_Chennai_Owner, bach_Bangalore_Owner, bach_Hyderabad_Owner)
percentageBachCitiesOwner= round(bachCitiesOwner/sum(bachCitiesOwner)*100)
statusName = c("Kolkata","Mumbai","Delhi","Chennai", "Bangalore", "Hyderabad")
statusLabel = paste(statusName,"-", percentageBachCitiesOwner, "%", sep = "")
pie(bachCitiesOwner ,statusLabel,
    radius = 1, 
    main ="BACHELOR'S PROPORTION IN EACH CITY WHO RENTED FROM OWNERS", 
    col = c("lightgoldenrod","indianred1","lightblue1","mistyrose1","salmon","plum1"), 
    clockwise = TRUE)

#Analysis 1-12: Comparison of the proportion of Bachelors in each City that rented from an Agent
bach_Kolkata_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                      (houserent$City == "Kolkata") & 
                                      (houserent$Point.of.Contact == "Contact Agent"), ])

bach_Mumbai_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                     (houserent$City== "Mumbai") & 
                                     (houserent$Point.of.Contact== "Contact Agent"), ])


bach_Chennai_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                      (houserent$City== "Chennai") & 
                                      (houserent$Point.of.Contact== "Contact Agent"), ])


bach_Delhi_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                    (houserent$City== "Delhi") & 
                                    (houserent$Point.of.Contact== "Contact Agent"), ])


bach_Bangalore_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                        (houserent$City== "Bangalore") & 
                                        (houserent$Point.of.Contact== "Contact Agent"), ])

bach_Hyderabad_Agent = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & 
                                        (houserent$City== "Hyderabad") & 
                                        (houserent$Point.of.Contact== "Contact Agent"), ])

bachCitiesAgent= c(bach_Kolkata_Agent, bach_Mumbai_Agent, bach_Delhi_Agent, bach_Chennai_Agent, bach_Bangalore_Agent, bach_Hyderabad_Agent)
percentageBachCitiesAgent= round(bachCitiesAgent/sum(bachCitiesAgent)*100)
statusName = c("Kolkata","Mumbai","Delhi","Chennai", "Bangalore", "Hyderabad")
statusLabel = paste(statusName,"-", percentageBachCitiesAgent, "%", sep = "")
pie(bachCitiesAgent ,statusLabel,
    radius = 1, 
    main ="BACHELOR'S PROPORTION IN EACH CITY WHO RENTED FROM AGENTS", 
    col = c("lightgoldenrod","indianred1","lightblue1","mistyrose1","salmon","plum1"), 
    clockwise = TRUE)

#Question 2: What are the factors that effects the rent price

#Analysis 2-1: Kolkota's rent density plot

kolkata = houserent$City == "Kolkata"
Rent = houseRentOutlier$Rent
editKolkata = ggplot(houseRentOutlier[kolkata, ], aes(x = Rent)) + 
              geom_density(fill = "deepskyblue") + labs(y = "Density", x = "Rent Price") + 
              ggtitle("Kolkata's Rent Density Plot(houseRentOutlier)")

oriKolkata = ggplot(houserent[kolkata, ], aes(x = Rent)) + 
              geom_density(fill = "deepskyblue") + labs(y = "Density", x = "Rent Price") + 
              ggtitle("Kolkata's Rent Density Plot(houserent)")

grid.arrange(oriKolkata, editKolkata, nrow = 2)


#Analysis 2-2: Bangalore's rent density plot
bangalore = houserent$City == "Bangalore"
Rent = houseRentOutlier$Rent
editBangalore = ggplot(houseRentOutlier[bangalore, ], aes(x = Rent)) + 
                geom_density(fill = "darkolivegreen1") + 
                labs(y = "Density", x = "Rent Price") + 
                ggtitle("Bangalore's Rent Density Plot (houseRentOutlier)")

oriBangalore =ggplot(houserent[bangalore, ], aes(x = Rent)) + 
              geom_density(fill = "darkolivegreen1") + 
              labs(y = "Density", x = "Rent Price") + 
              ggtitle("Bangalore's Rent Density Plot (houserent)")

grid.arrange(editBangalore, oriBangalore, nrow = 2)


#Analysis 2-3: Mumbai's rent density plot
mumbai = houserent$City == "Mumbai"
editMumbai= ggplot(houseRentOutlier[mumbai, ], aes(x = Rent)) + 
             geom_density(fill = "goldenrod1") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Mumbai's Rent Density Plot (houseRentOutlier)")

oriMumbai = ggplot(houserent[mumbai, ], aes(x = Rent)) + 
            geom_density(fill = "goldenrod1") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Mumbai's Rent Density Plot (houserent)")

grid.arrange(oriMumbai, editMumbai, nrow = 2)

#Analysis 2-4: Delhi's rent density plot
delhi = houserent$City == "Delhi"
editDelhi = ggplot(houseRentOutlier[delhi, ], aes(x = Rent)) + 
            geom_density(fill = "slateblue") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Delhi's Rent Density Plot (houseRentOutlier")

oriDelhi = ggplot(houserent[delhi, ], aes(x = Rent)) + 
             geom_density(fill = "slateblue") + 
          labs(y = "Density", x = "Rent Price") + 
           ggtitle("Delhi's Rent Density Plot (houserent)")

grid.arrange(oriDelhi, editDelhi, nrow = 2)

#Analysis 2-5: Chennai's rent density plot
chennai = houserent$City == "Chennai"
Rent = houserent$Rent
editChennai = ggplot(houseRentOutlier[chennai, ], aes(x = Rent)) + 
              geom_density(fill = "azure2") + 
              labs(y = "Density", x = "Rent Price") + 
               ggtitle("Chennai's Rent Density Plot (houseRentOutlier)")

oriChennai =  ggplot(houserent[chennai, ], aes(x = Rent)) + 
              geom_density(fill = "azure2") + 
              labs(y = "Density", x = "Rent Price") + 
               ggtitle("Chennai's Rent Density Plot (houserent)")

grid.arrange(oriChennai, editChennai, nrow = 2)

#Analysis 2-6: Hyderabad's rent density ploy
hyderabad = houserent$City == "Hyderabad"
Rent = houserent$Rent
editHyderabad = ggplot(houseRentOutlier[hyderabad, ], aes(x = Rent)) + 
                geom_density(fill = "lightpink") + 
                labs(y = "Density", x = "Rent Price") + 
                ggtitle("Hyderabad's Rent Density Plot (houseRentOutlier")

oriHyderabad = ggplot(houserent[hyderabad, ], aes(x = Rent)) + 
                geom_density(fill = "lightpink") + 
               labs(y = "Density", x = "Rent Price") + 
                ggtitle("Hyderabad's Rent Density Plot (houserent)")

grid.arrange(oriHyderabad, editHyderabad, nrow = 2)

#Analysis 2-7: How does city effects the rent price
densityPlotOvr = ggplot(houseRentOutlier, aes(x=Rent, colour=City)) + geom_density() + 
                  ggtitle("Overall Rent Price prefered by tenants based on each City") + 
                  labs(y="Density", x="Rent Price")

histogramOvr = ggplot(houseRentOutlier, aes(x=Rent, colour=City)) + geom_histogram() + 
                ggtitle("Overall Rent Price prefered by tenants based on each City") + 
                labs(y="Frequency", x="Rent Price")

grid.arrange(densityPlotOvr, histogramOvr, nrow = 2)

#Analysis 2 -8: Super Area's rent density plot
superArea = houserent$Area.Type == "Super Area"
editSuper = ggplot(houseRentOutlier[superArea, ], aes(x = Rent)) + 
            geom_density(fill = "chocolate4") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Super Area Rent Density Plot (houseRentOutlier)")

oriSuper = ggplot(houserent[superArea, ], aes(x = Rent)) + 
            geom_density(fill = "chocolate4") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Super Area Rent Density Plot (houserent)")

grid.arrange(oriSuper, editSuper, nrow = 2)

#Analysis 2-9: Carpet Area's rent density plot
carpetArea = houserent$Area.Type== "Carpet Area"
editCarpet = ggplot(houseRentOutlier[carpetArea, ], aes(x = Rent)) + 
              geom_density(fill = "cyan2") + 
              labs(y = "Density", x = "Rent Price") + 
              ggtitle("Carpet Area Rent Density Plot (houseRentOutlier)")

oriCarpet = ggplot(houserent[carpetArea, ], aes(x = Rent)) + 
            geom_density(fill = "cyan2") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Carpet Area Rent Density Plot  (houserent)")

grid.arrange(oriCarpet, editCarpet, nrow = 2)

#Analysis 2-10: Built Area's rent density plot
builtArea = houserent$Area.Type == "Built Area"
editBuilt = ggplot(houseRentOutlier[builtArea, ], aes(x = Rent)) + 
            geom_density(fill = "brown") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Built Area's Rent Density Plot (houseRentOutlier)")

oriBuilt = ggplot(houserent[builtArea, ], aes(x = Rent)) + 
            geom_density(fill = "brown") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Built Area's Rent Density Plot (houserent)")

grid.arrange(oriBuilt, editBuilt, nrow = 2)

#Analysis 2-11: How does area type effect the rent price
densityAreaOvr = ggplot(houseRentOutlier, aes(x=Rent, colour = Area.Type)) + 
                  geom_density() + ggtitle("Overall Rent Price prefered by tenants based on each Area Type") + 
                  labs(y="Density", x="Rent Price")

histogramAreaOvr =  ggplot(houseRentOutlier, aes(x=Rent, colour = Area.Type)) + 
                    geom_histogram() + ggtitle("Overall Rent Price prefered by tenants based on each Area Type") + 
                    labs(y="Density", x="Rent Price")

grid.arrange(histogramAreaOvr, densityAreaOvr, nrow = 2)

#Analysis 2-12: Unfurnished rent density plot
unfurnished = houserent$Furnishing.Status == "Unfurnished"
editUnfurnished = ggplot(houseRentOutlier[unfurnished, ], aes(x = Rent)) + 
                  geom_density(fill = "burlywood2") + 
                  labs(y = "Density", x = "Rent Price") + 
                  ggtitle("Unfurnished house's rent density plot (houseRentOutlier)")

oriUnfurnished = ggplot(houserent[unfurnished, ], aes(x = Rent)) + 
                  geom_density(fill = "burlywood2") + 
                  labs(y = "Density", x = "Rent Price") + 
                  ggtitle("Unfurnished house's rent density plot (houserent)")

grid.arrange(oriUnfurnished,editUnfurnished, nrow = 2)

#Analysis 2-13: Semi-Furnished rent density plot
semi_furnished = houserent$Furnishing.Status == "Semi-Furnished"
editSemi = ggplot(houseRentOutlier[semi_furnished, ], aes(x = Rent)) + 
             geom_density(fill = "mediumpurple1") + 
            labs(y = "Density", x = "Rent Price") + 
            ggtitle("Semi-furnished house's rent density plot (houseRentOutlier)")

oriSemi = ggplot(houserent[semi_furnished, ], aes(x = Rent)) + 
          geom_density(fill = "mediumpurple1") + 
          labs(y = "Density", x = "Rent Price") + 
          ggtitle("Semi-furnished house's rent density plot (houserent)")

grid.arrange(oriSemi, editSemi, nrow = 2)

#Analysis 2-14: Furnished house's rent density plot
furnished = houserent$Furnishing.Status == "Furnished"
editFurnished = ggplot(houseRentOutlier[furnished, ], aes(x = Rent)) + 
                geom_density(fill = "lightsteelblue4") + 
                labs(y = "Density", x = "Rent Price") + 
                ggtitle("Furnished house's rent density plot (houseRentOutlier)")

oriFurnished = ggplot(houserent[furnished, ], aes(x = Rent)) + 
                geom_density(fill = "lightsteelblue4") + 
                labs(y = "Density", x = "Rent Price") + 
                ggtitle("Furnished house's rent density plot (houserent)")

grid.arrange(oriFurnished, editFurnished, nrow = 2)

#Analysis 2-15: Comparisons of the rent for each Furnishing Status
violinFurOvr = ggplot(houseRentOutlier, aes(x=Furnishing.Status, y=Rent, fill = Furnishing.Status)) + 
  geom_violin() + ggtitle("Boxplot of Furnishing Status and Rent Price") + 
  labs(y="Rent Price", x="Furnishing Status")

densityFurOvr = ggplot(houseRentOutlier, aes(x=Rent, colour=Furnishing.Status)) + geom_density() + 
                  ggtitle("Overall prices for each type of furnishing") + 
                  labs(y="Density", x="Rent Price")

grid.arrange(densityFurOvr, violinFurOvr, nrow = 2)

#Question 3: What type of house and tenants Point of Contacts have
#Analysis 3-1: Furnishing status from Contact Owner
contactOwner = houserent$Furnishing.Status[houserent$Point.of.Contact == "Contact Owner"]
poc_df = data.frame(contactOwner)

ggplot(poc_df, aes(x = contactOwner)) + geom_bar(colour = "black",fill = c("chocolate4","aquamarine2","darkgoldenrod1"), size = 1)  +
  ggtitle("Furnishing status from owner") +
  labs(y = "frequency", x = "Furnishing Status") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
  

#Analysis 3-2: Furnishing status from Contact Agent
contactAgent = houserent$Furnishing.Status[houserent$Point.of.Contact == "Contact Agent"]
poc_df = data.frame(contactAgent)

ggplot(poc_df, aes(x = contactAgent)) + 
  geom_bar(colour = "black", fill = c("mediumpurple","mediumspringgreen","mediumvioletred"), size = 1) +
  ggtitle("Furnishing status from agent") + 
  labs(y = "frequency", x = "Furnishing Status") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#Analysis 3-3: Histogram to show the house size of Contact Agent

poc = houserent$Point.of.Contact == "Contact Agent"
ggplot(houserent[poc, ], aes(x=Size)) + 
  geom_histogram(colour = "green", fill = "black") + 
  ggtitle("Contact Agent House Size Histogram") + 
  labs(y="Frequency", x="House Size")

#Analysis 3-4: Histogram to show the house size of Contact Owner

poc = houserent$Point.of.Contact == "Contact Owner"
ggplot(houserent[poc, ], aes(x=Size)) + 
  geom_histogram(colour = "purple", fill = "black") + 
  ggtitle("Contact Owner House Size Histogram") + 
  labs(y="Frequency", x="House Size")

#Analysis 3-5: The overall density of the house size of all point of contacts
ggplot(houserent, aes(x=Point.of.Contact, y=Size, fill = Point.of.Contact)) + 
  geom_violin() + 
  ggtitle("Overall house size from each point of contacts") + 
  labs(y="Density", x="House Size")

#Analysis 3-6: Barchart to show the proportion of area type of owners
poc = houserent$Point.of.Contact == "Contact Owner"
ggplot(houserent[poc, ], aes(x= Area.Type)) + 
  geom_bar(colour = "black", fill = c("darkorange","darkseagreen2","darkslategray2"), size = 1) + 
  ggtitle("Contact Owner House Area Type") + 
  labs(y="Frequency", x="House Area Type") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#Analysis 3-7: Barchart to show the proportion of area type of agents
poc = houserent$Point.of.Contact == "Contact Agent"
ggplot(houserent[poc, ], aes(x= Area.Type)) + 
  geom_bar(colour = "black", fill = c("seagreen2","violet"), size = 1) + 
  ggtitle("Contact Agent House Area Type") + 
  labs(y="Frequency", x="House Area Type") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  coord_flip()

#Analysis 3-8: Barchart to show which tenants preferred to rent with owners
poc = houserent$Point.of.Contact == "Contact Owner"
ggplot(houserent[poc, ], aes(x= Tenant.Preferred)) + 
  geom_bar(colour = "black", fill = c("purple","deeppink","lightpink"), size = 1) + 
  ggtitle("Contact Owner Tenant Preferred") + 
  labs(y="Frequency", x="Tenants") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)


#Analysis 3-9: Barchart to show which tenants preferred to rent with agents
poc = houserent$Point.of.Contact == "Contact Agent"
ggplot(houserent[poc, ], aes(x= Tenant.Preferred)) + 
  geom_bar(colour = "black", fill = c("firebrick1","deepskyblue","mediumspringgreen"), size = 1) + 
  ggtitle("Contact Agent Tenant Preferred") + 
  labs(y="Frequency", x="Tenants") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  coord_flip()

#Analysis 3-10: The proportions of each point of contacts
owners = nrow(houserent[houserent$Point.of.Contact == "Contact Owner",])

agent = nrow(houserent[houserent$Point.of.Contact == "Contact Agent",])

builder = nrow(houserent[houserent$Point.of.Contact == "Contact Builder",])

contacts = c(owners, agent, builder)
percentage_contact = round(contacts/sum(contacts)*100)
contacts_name = c("Owner","Agent","Builder")
contact_label = paste(contacts_name,"-", percentage_contact, "%", sep = "")
pie(contacts,
    contact_label,
    radius = 1, 
    main ="POINT OF CONTACTS", 
    col = c("aliceblue", "antiquewhite", "aquamarine","bisque","coral","cadetblue"), 
    clockwise = TRUE)
  
#Analysis 3-11: The number of agents in each city
poc = houserent$Point.of.Contact == "Contact Agent"
ggplot(houserent[poc, ], aes(x= City)) + 
  geom_bar(colour = "black", fill = c("firebrick1","deepskyblue","mediumspringgreen","aliceblue", "chocolate", "coral3"), size = 1) + 
  ggtitle("Contact Agent Frequency in each city") + 
  labs(y="Frequency", x="Cities") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
  

#Analysis 3-12: The number of owners in each city
poc = houserent$Point.of.Contact == "Contact Owner"
ggplot(houserent[poc, ], aes(x= City)) + 
  geom_bar(colour = "black", fill = c("darkgreen","darkgrey","darkkhaki","darkmagenta", "darkorange", "dodgerblue4"), size = 1) + 
  ggtitle("Contact Owner Frequency in each city") + 
  labs(y="Frequency", x="Tenants") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) + coord_flip()


#Question 4: What are the houses that is preferred by families
#Analysis 4-1: The price rent that is preferred by family
familyRent = houserent$Tenant.Preferred == "Family"
ggplot(houserent[familyRent, ], aes(x = Rent)) + 
  geom_histogram(color = "lightcyan", fill = "lightcyan3") + 
  ggtitle("Histogram of Rent Price Preferred by Families") +
  labs(y = "Frequency", x = "Rent Price")

#Analysis 4-2: The area type that is preferred by families
family_areaType = houserent$Area.Type[houserent$Tenant.Preferred == "Family"]
furnishing_df = data.frame(family_areaType)

ggplot(furnishing_df) + geom_bar(aes(family_areaType), col = "black", fill = c("slateblue3", "tomato3")) + 
  ggtitle("FAMILY'S AREA TYPE PREFERENCE") + 
  labs(y = "frequency", x = "Area Type") +
  coord_flip()

#Analysis 4-3: The house size that is preferred by families
familySize = houserent$Tenant.Preferred == "Family"
ggplot(houserent[familySize, ], aes(x = Size)) + 
  geom_density(color = "lavenderblush3", fill = "lavenderblush4") + 
  ggtitle("Density of house size Preferred by Families") +
  labs(y = "Frequency", x = "House Size")

#Analysis 4-4: The furnishing status that is preferred by families
family_furnishing = houserent$Furnishing.Status[houserent$Tenant.Preferred == "Family"]
furnishing_df = data.frame(family_furnishing)

ggplot(furnishing_df,aes(family_furnishing)) + geom_bar(col = "black", fill = c("peachpuff3", "plum", "pink3")) + 
  ggtitle("FAMILY'S FURNISHING PREFERENCE") + 
  labs(y = "frequency", x = "Furnishing Status") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
  

#Analysis 4-5: The Point of Contact that family prefers
family_poc = houserent$Point.of.Contact[houserent$Tenant.Preferred == "Family"]
poc_df = data.frame(family_poc)

ggplot(poc_df) + geom_bar(aes(family_poc), col = "black", fill = c("paleturquoise2", "turquoise2")) + 
  ggtitle("POINT OF CONTACT PREFERENCE") + 
  labs(y = "frequency", x = "Point of Contact") +
  coord_flip()

#Analysis 4-6: The number of BHK preferred by families
family_BHK = houserent$BHK[houserent$Tenant.Preferred == "Family"]
bhk_df = data.frame(family_BHK)

ggplot(bhk_df,aes(family_BHK), fill = family_BHK) + geom_bar(col = "black") + 
  ggtitle("BHK PREFERENCE") + 
  labs(y = "frequency", x = "BHK") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  coord_flip()

#Analysis 4 - 7: The number of bathroom preferred by families
family_Bathroom = houserent$Bathroom[houserent$Tenant.Preferred == "Family"]
bathroom_df = data.frame(family_Bathroom)

ggplot(bathroom_df,aes(family_Bathroom), fill = family_Bathroom) + geom_bar(col = "black") + 
  ggtitle("Bathroom PREFERENCE") + 
  labs(y = "frequency", x = "Bathroom") +
  geom_text(stat='count', aes(label=..count..), vjust=-1)

#Question 5: Correlogram
#Analysis 5-1: The relationship of each column
crhouserent = houserent[,c(-1,-5,-6,-7,-8,-9,-10,-12)]
corr = round(cor(crhouserent),1)
corr

#Analysis 5-2: Corellogram of the dataset
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method = "square",
           colors = c("green", "white", "blue"),
           title = "Correlogram of Houserent",
           ggtheme = ggplot2::theme_minimal
           )

#Analysis 5-3: The proof of BHK and Size correlation
boxplot(Size ~ BHK, 
        data = houserent, 
        xlab = "Number of BHK", 
        ylab = "House Size", 
        main ="Correlation between BHK and Size", 
        col = "bisque2")

#Analysis 5-4: The proof of BHK and Bathroom correlation
ggscatterstats(
  data = crhouserent,
  x = Bathroom,
  y = BHK,
  bf.message = FALSE
)

#Analysis 5-5: The proof of BHK and Rent correlation

ggscatterstats(
  data = crhouserent,
  x = Rent,
  y = BHK,
  bf.message = FALSE
)

#Analysis 5-6: The proof of Rent and Bathroom correlation

plot(houserent$Bathroom, houserent$Rent, 
     main = "Correlation between Rent and Bathroom", 
     xlab = "Bathroom", 
     ylab ="Rent Price", 
     pch = 19, 
     frame = FALSE)
abline(lm(houserent$Rent ~ houserent$Bathroom, data = houserent), col = "deeppink")

#Analysis 5-7: The proof of Rent and Size correlation
plot(houserent$Size, houserent$Rent, 
     main = "Correlation between Rent and Size", 
     xlab = "House Size", 
     ylab ="Rent Price", 
     pch = 19, 
     frame = FALSE)
abline(lm(houserent$Rent ~ houserent$Size, data = houserent), col = "blue")

#Analysis 5- 8: The proof of Size and Bathroom correlation
boxplot(Size ~ Bathroom,
        data = houserent, 
        xlab = "House Size", 
        ylab = "Bathroom", main = "Correlation between Bathroom and Size", 
        col = "darkseagreen2")
abline(lm(houserent$Size ~ houserent$Bathroom, data = houserent), col = "blue")

#Question 6: What are the proportion of tenants in each city
#Analysis 6-1: Piechart to show the proportion of rental in each cities

Kolkata = nrow(houserent[houserent$City == "Kolkata",])
Mumbai = nrow(houserent[houserent$City == "Mumbai",])
Bangalore = nrow(houserent[houserent$City == "Bangalore",])
Delhi = nrow(houserent[houserent$City == "Delhi",])
Chennai = nrow(houserent[houserent$City == "Chennai",])
Hyderabad = nrow(houserent[houserent$City == "Hyderabad",])

cities = c(Kolkata, Mumbai, Bangalore, Delhi, Chennai, Hyderabad)
percentage = round(cities/sum(cities)*100)
names = c("Kolkata", "Mumbai","Bangalore","Delhi","Chennai", "Hyderabad")
label = paste(names,"-", percentage, "%", sep = "")
pie(cities,label,radius = 1, main ="CITIES", 
    col = c("green", "blue", "red","pink","purple","aliceblue"), 
    clockwise = TRUE)

#Analysis 6-2: Piechart to show the comparison of proportion of Bachelors tenants on each city
bach_Kolkata= nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City == "Kolkata"), ])

bach_Mumbai = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City== "Mumbai"), ])

bach_Chennai = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City== "Chennai"), ])

bach_Delhi = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City== "Delhi"), ])

bach_Bangalore = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City== "Bangalore"), ])

bach_Hyderabad= nrow(houserent[(houserent$Tenant.Preferred == "Bachelors") & (houserent$City== "Hyderabad"), ])

bachCities= c(bach_Kolkata, bach_Mumbai, bach_Delhi, bach_Chennai, bach_Bangalore, bach_Hyderabad)
percentageBachCities= round(bachCities/sum(bachCities)*100)
statusName = c("Kolkata","Mumbai","Delhi","Chennai", "Bangalore", "Hyderabad")
statusLabel = paste(statusName,"-", percentageBachCities, "%", sep = "")
pie(bachCities ,
    statusLabel,
    radius = 1, 
    main ="BACHELOR'S PROPORTION IN EACH CITY", 
    col = c("antiquewhite3","aquamarine3","azure3","darkkhaki","chocolate3","coral3"), 
    clockwise = TRUE)

#Analysis 6-3: Piechart to show the comparison of proportion of Family tenants on each city
fam_Kolkata= nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City == "Kolkata"), ])

fam_Mumbai = nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City== "Mumbai"), ])

fam_Chennai = nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City== "Chennai"), ])

fam_Delhi = nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City== "Delhi"), ])

fam_Bangalore = nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City== "Bangalore"), ])

fam_Hyderabad= nrow(houserent[(houserent$Tenant.Preferred == "Family") & (houserent$City== "Hyderabad"), ])

famCities= c(fam_Kolkata, fam_Mumbai, fam_Delhi, fam_Chennai, fam_Bangalore, fam_Hyderabad)
percentageFamCities= round(famCities/sum(famCities)*100)
statusName = c("Kolkata","Mumbai","Delhi","Chennai", "Bangalore", "Hyderabad")
statusLabel = paste(statusName,"-", percentageFamCities, "%", sep = "")
pie(famCities ,
    statusLabel,
    radius = 1, 
    main ="FAMILY'S PROPORTION IN EACH CITY", 
    col = c("mediumpurple","lightskyblue1","lightslateblue","midnightblue","mediumseagreen","navajowhite3"), 
    clockwise = TRUE)

#Analysis 6-4: Piechart to show the comparison of proportion of Bachelors/Family tenants on each city
bach_fam_Kolkata= nrow(houserent[(houserent$Tenant.Preferred == "Bachelors/Family") & (houserent$City == "Kolkata"), ])

bach_fam_Mumbai = nrow(houserent[(houserent$Tenant.Preferred == "Bacahelors/Family") & (houserent$City== "Mumbai"), ])

bach_fam_Chennai = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors/Family") & (houserent$City== "Chennai"), ])

bach_fam_Delhi = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors/Family") & (houserent$City== "Delhi"), ])

bach_fam_Bangalore = nrow(houserent[(houserent$Tenant.Preferred == "Bachelors/Family") & (houserent$City== "Bangalore"), ])

bach_fam_Hyderabad= nrow(houserent[(houserent$Tenant.Preferred == "Bachelors/Family") & (houserent$City== "Hyderabad"), ])

bachFamCities= c(bach_fam_Kolkata, bach_fam_Mumbai, bach_fam_Delhi, bach_fam_Chennai, bach_fam_Bangalore, bach_fam_Hyderabad)
percentageBachFamCities= round(bachFamCities/sum(bachFamCities)*100)
statusName = c("Kolkata","Mumbai","Delhi","Chennai", "Bangalore", "Hyderabad")
statusLabel = paste(statusName,"-", percentageBachFamCities, "%", sep = "")
pie(bachFamCities ,
    statusLabel,
    radius = 1, 
    main ="BACHELOR/FAMILY'S PROPORTION IN EACH CITY", 
    col = c("cyan2","brown","aquamarine2","blanchedalmond","azure3","antiquewhite2"), 
    clockwise = TRUE)

#Creating new dataset where the Posted.On column are separated by day, month, year
houserentDate = separate(houserent, Posted.On, c("Month", "Day", "Year"), sep = "/")
houserentDate
View(houserentDate)

#Question 7: The rentals in each day and month
#Analysis 7-1: The Frequency of rentals in each day
str(houserentDate)
ggplot(houserentDate, aes(x= Day, fill = Day), fill = Day) + 
  geom_bar(size = 1) + 
  ggtitle("Contact Owner Frequency in each city") + 
  labs(y="Frequency", x="Day") +
  geom_text(stat='count', aes(label=..count..), vjust=-1) + coord_flip()

#Analysis 7-2: The Frequency of rentals in each month
aprilFreq = nrow(houserentDate[(houserentDate$Month == "4"),])
mayFreq = nrow(houserentDate[(houserentDate$Month == "5"),])
juneFreq = nrow(houserentDate[(houserentDate$Month == "6"),])
julyFreq = nrow(houserentDate[(houserentDate$Month == "7"),])
monthFreq = c(aprilFreq, mayFreq, juneFreq, julyFreq)
monthNames = c("April", "May", "June", "July")
monthFreq_df = data.frame(monthNames, monthFreq)

dataMonth <- monthFreq_df                                                
dataMonth$monthNames <- factor(dataMonth$monthNames,                                    
                  levels = c("April", "May", "June", "July"))

ggplot(dataMonth, aes(x= monthNames, y = monthFreq, group = 1)) + 
  geom_line(linetype = "solid", size = 1, color = "black") + geom_point(stat = "identity") + 
  labs(y="Frequency", x="Month")

#Analysis 7-3: The Frequency of tenants' rentals in each month
ggplot(houserentSep, aes(x= Month, fill = Tenant.Preferred)) + 
  geom_bar(position = "dodge", size = 1) +
  ggtitle("The frequency of rentals by tenants in each month") +
  labs(y="Frequency", x="Month")

#Analysis 7-4: The Frequency of houses with different furnishing status being rented each month
ggplot(houserentSep, aes(x= Month, fill = Furnishing.Status)) + 
  geom_bar(position = "dodge", size = 1) +
  ggtitle("The Frequency of houses with different furnishing status being rented each month") +
  labs(y="Frequency", x="Month")

#Analysis 7-5: The frequency of houses with different area type being rented each month
ggplot(houserentSep, aes(x= Month, fill = Area.Type)) + 
  geom_bar(position = "dodge", size = 1) +
  ggtitle("The Frequency of houses with different area type being rented each month") +
  labs(y="Frequency", x="Month")

#Analysis 7-6: The frequency of houses owned by different point of contact that are rented in each month
ggplot(houserentSep, aes(x= Month, fill = Point.of.Contact)) + 
  geom_bar(position = "dodge", size = 1) +
  ggtitle("The Frequency of houses with different point of contact that are being rented each month") +
  labs(y="Frequency", x="Month")
