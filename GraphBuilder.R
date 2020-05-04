library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)

create.graph <- function(file1, nvid1, file2 = NULL, nvid2 = NULL){
  # Read data and clean
  if(!is.null(file2)){
    df.1 <- read.csv(file1, skip = nvid1 + 14, stringsAsFactors = FALSE)
    df.2 <- read.csv(file2, skip = nvid2 + 14, stringsAsFactors = FALSE)
    
    df <- rbind(df.1, df.2)[, c('Time', 'Subject', 'Behavior')]
  } else {
    df <- read.csv(file1, skip = nvid1 + 14, stringsAsFactors = FALSE)
  }

  df <- df %>%
    rowwise() %>%
    mutate(Subject = strsplit(Subject, '-')[[1]][1])
  
  # dcast
  df.dcast <- dcast(df, Time ~ Subject, value.var = 'Behavior')
  df.fill <- fill(df.dcast, -Time) %>%
    select(-c('NA'))
  df.fill[is.na(df.fill)] <- 'StudentExit'
  
  cols.students <- colnames(df.fill)[2:ncol(df.fill)]
  
  cols.comb <- unlist(lapply(combn(colnames(df.fill)[2:ncol(df.fill)], 2, simplify = FALSE), paste, 
                             collapse = ':'))
  
  #Aggregate times and contruct student interactions dataframe
  times.vec <- rep(0, length(cols.comb))
  names(times.vec) <- cols.comb
  
  for (row in 1:(nrow(df.fill) - 1)){
    for (col1 in 1:(ncol(df.fill) - 1)){
      for (col2 in (col1 + 1):ncol(df.fill)){
        if((df.fill[row, col1] == df.fill[row, col2]) & (df.fill[row, col1] != 'StudentExit')){
          times.vec[paste(cols.students[col1 - 1], cols.students[col2 - 1], sep = ':')] <- times.vec[paste(cols.students[col1 - 1], cols.students[col2 - 1], sep = ':')] + df.fill[(row + 1), 'Time'] - df.fill[row, 'Time']
        }
      }
    }
  }
  
  df.times <- data.frame(times.vec)
  df.times$Students <- row.names(df.times)
  row.names(df.times) <- c()
  df.times <- df.times %>%
    rowwise() %>%
    mutate(Student1 = strsplit(Students, ':')[[1]][1],
           Student2 = strsplit(Students, ':')[[1]][2],
           Time = round(times.vec)) %>%
    select(-c('Students', 'times.vec')) %>%
    filter(Time > 0)
  
  # Set vertex/edge attributes
  g = graph_from_data_frame(df.times, directed = FALSE)
  
  E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 1), 
                       'within', 'between')
  
  E(g)$Time = df.times$Time
  E(g)[E(g)$group == 'within']$Time = 1
  E(g)$weight = E(g)$Time/max(E(g)$Time) * 5
  
  V(g)$group <- substr(V(g)$name, 1, 1)

  
  E(g)$color <- adjustcolor('black', 0.5)
  
  V(g)$centr.deg <- centralization.degree(g)$res
  
  V(g)$size <- (V(g)$centr.deg + 1)/max(V(g)$centr.deg) * 15
  
  return(g)
  
}