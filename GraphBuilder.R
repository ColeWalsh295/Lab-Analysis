library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
library(gtools)

create.graph <- function(file1, nvid1, offset1 = 0, file2 = NULL, nvid2 = NULL, offset2 = 0, method = 1){
  # Read data and clean
  if(!is.null(file2)){
    df.1 <- read.csv(file1, skip = nvid1 + 14, stringsAsFactors = FALSE)
    df.1$Time <- df.1$Time + offset1
    df.2 <- read.csv(file2, skip = nvid2 + 14, stringsAsFactors = FALSE)
    df.2$Time <- df.2$Time + offset2
    
    df <- rbind(df.1, df.2)
  } else {
    df <- read.csv(file1, skip = nvid1 + 14, stringsAsFactors = FALSE)
    df$Time <- df$Time + offset1
  }
  
  if(method == 1){
    df <- df[, c('Time', 'Subject', 'Behavior')] %>%
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
    g <- graph_from_data_frame(df.times, directed = FALSE)
    
    E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 1), 
                         'within', 'between')
    
    E(g)$time <- df.times$Time/60
    E(g)[E(g)$group == 'within']$time <- 1/60
    E(g)$weight <- E(g)$time/max(E(g)$time) * 5
    
    E(g)$line.type <- 2 * (E(g)$group == 'within') + 1
  } else {
    df <- df[, c('Time', 'Subject', 'Behavior', 'Comment')] %>%
      rowwise() %>%
      mutate(Subject = strsplit(Subject, '-')[[1]][1],
             Behavior = strsplit(Behavior, '->')[[1]][2])
    
    # need to add a TA handler too
    df.ta <- df %>%
      filter(Subject == 'TA' | Behavior == 'TA')
    
    df <- df %>%
      filter(Subject != 'TA' & Behavior != 'TA')
    
    df.table <- do.call("rbind", replicate(3, df[(df$Behavior == 'Table') & (!is.na(df$Behavior)),],
                                           simplify = FALSE))
    
    df.table[, 'Behavior'] <- c(rep('A', nrow(df.table)/3), rep('B', nrow(df.table)/3), 
                                rep('C', nrow(df.table)/3))
    
    df.table <- df.table %>%
      rowwise() %>%
      mutate(Behavior = paste(substr(Subject, 1, 1), Behavior, sep = '')) %>%
      filter(Subject != Behavior) %>%
      select(-c('Comment'))
    
    df <- df %>%
      filter(Behavior != 'Table')
    
    df.other <- df %>%
      filter(Behavior == 'Other') %>%
      rowwise() %>%
      mutate(Behavior = strsplit(Comment, '-')[[1]][1]) %>%
      select(-c('Comment'))
    
    df <- df %>%
      filter(Behavior != 'Other') %>%
      select(-c('Comment')) %>%
      rowwise() %>%
      mutate(Behavior = paste(substr(Subject, 1, 1), Behavior, sep = ''))
    
    df <- rbind(df, df.table, df.other)
    df$Interval <- round(df$Time/120)
    df <- df[!duplicated(df[, c('Subject', 'Behavior', 'Interval')]),]
    df$ordered <- as.numeric(apply(asc(df$Subject), 2, paste, 
                                   collapse = '')) < as.numeric(apply(asc(df$Behavior), 2, paste, 
                                                                      collapse = ''))
    df[df$ordered == FALSE, c('Subject', 'Behavior')] <- df[df$ordered == FALSE, c('Behavior', 'Subject')]
    df <- df %>%
      select(-c('ordered'))
    
    df.two.way <- df[duplicated(df[, c('Subject', 'Behavior', 'Interval')]),] %>%
      group_by(Subject, Behavior) %>%
      summarize(N.ints = n())
    
    g <- graph_from_data_frame(df.two.way, directed = FALSE)
    
    E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 1), 
                         'within', 'between')
    
    E(g)$count <- df.two.way$N.ints
    E(g)$weight <- E(g)$count/max(E(g)$count) * 5
    
    E(g)$line.type <- 1
  }
  
  g <- add.graph.attributes(g)
  return(g)
  
}

add.graph.attributes <- function(g, weight = TRUE){
  
  if(!weight){
    E(g)$weight <- E(g)$count/max(E(g)$count) * 5
  }
  
  V(g)$group <- substr(V(g)$name, 1, 1)
  
  E(g)$color <- adjustcolor('black', 0.5)
  
  V(g)$centr.deg <- centralization.degree(g)$res
  
  V(g)$size <- (V(g)$centr.deg + 1)/max(V(g)$centr.deg) * 15
  
  return(g)
  
}