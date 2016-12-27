#######FUNCTIONS MAKING R easy
#########--------------------------------------------
# This function normalizes the column names.
# INPUT -- Table with messed up column names.
# OUTPUT -- Table with proper column names.
#--------------------------------------------

proper_feature_names <- function(input_table){

colnames(input_table) <- tolower(colnames(input_table))

colnames(input_table) <- gsub('([[:punct:]])|\\s+','_',colnames(input_table))

while (any(grepl("__",colnames(input_table),fixed = TRUE)) == TRUE){
  colnames(input_table) <- gsub("__","_",colnames(input_table),fixed = TRUE) 
}

colnames(input_table) <- gsub("\\*$", "",colnames(input_table))

return(input_table)
}



#######REMOVING ZERO VARIANCE QUICKLY
data<- data[sapply(data, function(x) length(levels(factor(x,exclude=NULL)))>1)
            
          
###function to graph variable for continous variables        
tr <- function(a){
      ggplot(data = traindata, aes(x= a, y=..density..)) + 
      geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
      geom_density()            
      }            
            
####function to graph variable against output for categorical variable

all_bar <- function(i){
  ggplot(train,aes(x=i,fill=output$status_group))+geom_bar(position = "dodge",  color="black")+scale_fill_brewer(palette = "Pastel1")+theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
}


## Correlation matrix with p-values
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

View(flattenSquareMatrix(cor.prob(all_data)))

###dummy variables
dmy <- dummyVars(" ~ .", data = data)
diabetes_dummy <- data.frame(predict(dmy, newdata = data))

##########function to convert 5% less frequent values to 1
for(i in names(ins_fact)){
  p <- 5/100
  ld <- names(which(prop.table(table(ins_fact[[i]])) < p))
  levels(ins_fact[[i]])[levels(ins_fact[[i]]) %in% ld] <- "Other"
}            


####making the missing values unavailable
for (i in seq_along(insurance_data)) set(insurance_data, i=which(is.na(insurance_data[[i]])), j=i, value="Unavailable")

##Concordance

Concordance = function(GLM.binomial) {
  outcome_and_fitted_col = cbind(GLM.binomial$y, GLM.binomial$fitted.values)
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0}
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
} 

Concordance(lgOut)

  LogRegData$Score <- predict(lgOut,LogRegData,type="response")
##Lift chart
  
LogRegData$Score <- predict(lgOut,LogRegData,type="response")


Pctl_tbl<-as.vector(quantile(LogRegData[,"Score"], probs=c(.10,.20, .30,.40,.50, .60,.70, .80,.90)))
Pctl_tbl<-data.frame(c("P10","P20","P30","P40","P50","P60","P70","P80","P90"),Pctl_tbl)
colnames(Pctl_tbl)<-c("quantiles","Score")



LogRegData$Segment <-ifelse(LogRegData[,"Score"] > Pctl_tbl[Pctl_tbl$quantiles=="P90",2] , 10, 
                            ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P80",2], 9, 
                                   ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P70",2], 8,
                                          ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P60",2],7,
                                                 ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P50",2],6,
                                                        ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P40",2],5,
                                                               ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P30",2],4,
                                                                      ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P20",2],3,ifelse(LogRegData[,"Score"] >= Pctl_tbl[Pctl_tbl$quantiles=="P10",2],                                                  2, 1)))))))))

LiftChartData<-data.frame(CrossTable(LogRegData$Default_On_Payment,LogRegData$Segment) )
LiftChartData <- LiftChartData[LiftChartData$prop.col.x==1,c("t.Freq","prop.col.y")]
LiftChartData
