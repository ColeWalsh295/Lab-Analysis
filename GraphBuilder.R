library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)

create.graph <- function(c.vertex = 1, c.edge = 40, gender.vec = NULL, network = 'between', 
                         file1, file2, nvid1, nvid2){
  # Read data and clean
  df.1 <- read.csv(file1, skip = nvid1 + 14, stringsAsFactors = FALSE)
  df.2 <- read.csv(file2, skip = nvid2 + 14, stringsAsFactors = FALSE)
  
  df <- rbind(df.1, df.2)[, c('Time', 'Subject', 'Behavior')]
  
  df <- df %>%
    rowwise() %>%
    mutate(Subject = strsplit(Subject, '-')[[1]][1])
  
  # dcast
  df.dcast <- dcast(df, Time ~ Subject)
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
  E(g)$weight = df.times$Time/c.edge
  
  if(!is.null(gender.vec)){
    V(g)$gender <- gender.vec
    V(g)$color <- ifelse(V(g)$gender == 'M', "green", "orange")
  }
  
  E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 1), 
                       'within', 'between')
  
  E(g)$color <- adjustcolor('black', 0.5)
  
  g.btw <- subgraph.edges(g, E(g)[E(g)$group == 'between'], delete.vertices = FALSE)
  
  V(g.btw)$centr.deg <- centralization.degree(g.btw)$res
  
  V(g.btw)$size <- (V(g.btw)$centr.deg + 1) * c.vertex
  
  if(network == 'between'){
    return(g.btw)
  } else {
    return(g)
  }
  
}