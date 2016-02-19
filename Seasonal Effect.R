require(dplyr)

                                                                      


start.date.data <- mydata %>% group_by(loc.LinePN.CustType) %>% summarise(Total = n(),
                                                                          Start.Date = min(Day.of.Sale.Order.date))


start.date.data$FNS <- ifelse (start.date.data$Start.Date < as.Date("2015-01-01") & start.date.data$ADI <= 7| , "Very Fast",
                  ifelse (start.date.data$Start.Date < as.Date("2015-01-01") & start.date.data$ADI <= 30, "Fast",
                          ifelse (start.date.data$Start.Date < as.Date("2015-01-01") & start.date.data$ADI <= 30, "Fast",        
                          )
                          
                  )
)
mydata$Cust.Type <- ifelse(mydata$Cust.Supertype.Name == "OTHER","Other","Non Other")
mydata$loc.LinePN.CustType <- paste(mydata$Stock.Loc.Num,"+",mydata$Line.PN,"+",mydata$Cust.Type)
mydata[is.na(mydata)] <- 0


MyDate <- mydata$Day.of.Sale.Order.date

# Function for calculating effects of seasonality
effect.season.month <- function(MyDate, QT) {
  MyDate <- as.Date(MyDate, format = "%Y-%m-%d")
  MyDate <- format(MyDate, "%m-%Y")
  
  working.data <- data.frame(Time = MyDate, QTY = mydata$Ordered.Quantity)
  working.data <- working.data %>% group_by(Time) %>% summarise(QTY = sum(QTY))
  
  monh.list <- ifelse(nchar(1:12)==1, paste0("0", 1:12), 1:12)
  monh.list <- c(paste(monh.list, "2014", sep = "-"), paste(monh.list, "2015", sep = "-")) 
  
  L1 <- match(monh.list, working.data$Time)
  working.data <- working.data[L1, ]
  working.data$Time <- monh.list
  
  working.data[is.na(working.data)] <- 0
  ACF <- acf(working.data$QTY, plot=F)

  season.eff <- ifelse(ACF$acf[13]>0.404, 1, 0)
  
  return(season.eff)
}

seasonss <- mydata[1:1000,] %>% group_by(loc.LinePN.CustType) %>% summarise(Eff =  effect.season.month(Day.of.Sale.Order.date, Ordered.Quantity))
