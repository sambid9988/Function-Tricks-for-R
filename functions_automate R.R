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



