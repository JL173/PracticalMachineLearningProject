# helper functions

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


clean_dataframe <- function(training, col_na, col_selection){
  
  training <- select(training, -c(all_of(col_na)))
  
  training$user_name <- as.factor(training$user_name)
  training$new_window <- as.factor(training$new_window)
  training$classe <- as.factor(training$classe)
  
  training <- select(training, -c("...1", "new_window",
                                  "num_window", "user_name",
                                  "raw_timestamp_part_1",
                                  "raw_timestamp_part_2",
                                  "cvtd_timestamp"))
  
  training <- training[, c(col_selection)]
}


plot1 <- function(df){

  df_long <- melt(df, id.vars = "classe")
  
  df_long$classe <- factor(df_long$classe,
                           levels = c("A", "B", "C", "D", "E"))
  
  ggplot(data = df_long, aes(x = classe,
                                 y = value,
                                 fill = classe)) +
    ylim(-1000, 1000)+
    geom_boxplot() + 
    facet_wrap(. ~ variable, nrow = 10)
}


acc <- function(a, b){
  
  acc_ <- c()
  
  for (index in 1:nrow(b)){
    if (a[index] == b[[index, 1]]){
      acc_ <- c(acc_, 1)
      
    } else {
      acc_ <- c(acc_, 0)
    }
    
    mean(acc_)
  }
}