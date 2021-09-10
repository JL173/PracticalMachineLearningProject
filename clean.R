# helper functions

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


clean_dataframe <- function(training, col_na = col_na){
  
  training <- select(training, -c(all_of(col_na)))
  
  training$user_name <- as.factor(training$user_name)
  training$new_window <- as.factor(training$new_window)
  training$classe <- as.factor(training$classe)
  
  training <- select(training, -c("..1"))

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
    facet_grid(. ~ variable)
}