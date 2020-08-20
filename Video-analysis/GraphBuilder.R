library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
library(gtools)
library(RColorBrewer)
library(docstring)


boris.to.adjacency <- function(file1, nvid1, offset1 = 0, file2 = NULL, nvid2 = NULL, 
                         offset2 = 0, method = 'scan', ignore.begin = FALSE, 
                         group.level = FALSE, TA.adjacency = NULL, 
                         filename = 'adjacencyMatrix.csv'){
#' Create a graph
#' 
#' Create graph object from up to two BORIS files using the SCAN/SKIP methods
#' 
#' @param file1 First BORIS file
#' @param nvid1 Number of videos used in first BORIS file
#' @param offset1 Manual offset to apply to file1 times --- to be used only if offset not 
#' applied in BORIS
#' @param file2 Second BORIS file
#' @param nvid2 Number of videos used in second BORIS file
#' @param offset2 Manual offset to apply to file2 times --- to be used only if offset not 
#' applied in BORIS
#' @param method Either scan or skip
#' @param ignore.begin binary, whether to only include interactions after class start
#' @param group.level binary, whether to collapse interactions to the group level
#' @param TA.adjacency file name of TA method adjacency matrix to merge with at group level
#' @param filename filename and path of exported adjacency matrix
#' 
#' returns a graph object

  
  # Read data
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
  
  # Extract only time, subject, and behaviour information
  if(method == 'scan'){
    df <- df[, c('Time', 'Subject', 'Behavior')] %>%
      rowwise() %>%
      mutate(Subject = strsplit(Subject, '-')[[1]][1]) # extract labels before hyphens
    
    df[df$Behavior == 'StartClass', 'Subject'] <- NA
    df <- df[!duplicated(df$Behavior) | (df$Behavior != 'StartClass'),]
    if(ignore.begin){
      StartTime <- df[df$Behavior == 'StartClass', 'Time']
      df[df$Time < StartTime, 'Time'] <- StartTime
    }
    
    # dcast so we have location of each student at each timestamp in the data
    df.dcast <- dcast(df, Time ~ Subject, value.var = 'Behavior')
    if('NA' %in% names(df.dcast)){ # remove NA subjects if they exist
      df.dcast <- df.dcast %>% 
        select(-c('NA'))
    }
    
    df.fill <- fill(df.dcast, -Time) # down fill data until students change location

    df.fill[is.na(df.fill)] <- 'StudentExit' # fill empty locations with StudentExit
    
    if(group.level){
      # get all pairs of directed table to table interactions possible
      tables <- unique(df$Behavior)[unique(df$Behavior) %like% 'Table']
      interactions.vec <- c(unlist(lapply(combn(tables, 2, simplify = FALSE), paste, 
                                          collapse = ':')), 
                            unlist(lapply(combn(rev(tables), 2, simplify = FALSE), 
                                          paste, collapse = ':'))) 
    } else {
      students <- colnames(df.fill)[2:ncol(df.fill)]
      interactions.vec <- unlist(lapply(combn(students, 2, simplify = FALSE), paste, 
                                        collapse = ':')) # get every pair of possible students
    }
    
    # create empty vector of interaction times for all pairs of students
    times.vec <- rep(0, length(interactions.vec))
    names(times.vec) <- interactions.vec
    
    # aggregate times for all student-student interactions
    for (row in 1:(nrow(df.fill) - 1)){
      if(group.level){
        for (subject in tables){
          for (location in tables){
            if(location %in% df.fill[row, names(df.fill) %like% substr(subject, 
                                                                       nchar(subject), 
                                                                       nchar(subject)) | 
                                     names(df.fill) == 'Time']){
              times.vec[paste(subject, location, 
                              sep = ':')] <- times.vec[paste(subject, location, 
                                                             sep = ':')] +
                df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
              
            }
          }
        }
      } else {
        for (col1 in 1:(ncol(df.fill) - 1)){
          for (col2 in (col1 + 1):ncol(df.fill)){
            if((df.fill[row, col1] == df.fill[row, col2]) & 
               (df.fill[row, col1] != 'StudentExit')){
              times.vec[paste(cols.students[col1 - 1], cols.students[col2 - 1], 
                              sep = ':')] <- times.vec[paste(cols.students[col1 - 1], 
                                                             cols.students[col2 - 1], 
                                                             sep = ':')] + 
                df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
            }
          }
        }
      }
    }
    
    # construct dataframe of interactions
    df.times <- data.frame(times.vec)
    df.times$Subjects <- row.names(df.times)
    row.names(df.times) <- c()
    df.times <- df.times %>%
      rowwise() %>%
      mutate(Subject1 = strsplit(Subjects, ':')[[1]][1],
             Subject2 = strsplit(Subjects, ':')[[1]][2],
             Time = round(times.vec)) %>%
      select(-c('Subjects', 'times.vec')) %>%
      filter(Time > 0)
    
    g <- graph_from_data_frame(df.times, directed = TRUE)
    E(g)$time <- df.times$Time
    
    if(!is.null(TA.adjacency)){
      matrix <- read.csv(TA.adjacency, header = TRUE, row.names = 1, check.names = FALSE, 
                         na.strings = "")
      matrix[is.na(matrix)] <- 0
      
      g.TA <- graph_from_adjacency_matrix(as.matrix(matrix), mode = "directed", 
                                       weighted = 'time')
      g <- union(g, g.TA)
      E(g)$time_1[is.na(E(g)$time_1)] <- 0
      E(g)$time_2[is.na(E(g)$time_2)] <- 0
      E(g)$time <- E(g)$time_1 + E(g)$time_2
      g <- delete_edge_attr(g, 'time_1')
      g <- delete_edge_attr(g, 'time_2')
    }
    
    if(!group.level){
      # Set within group times to one second
      g <- graph_from_data_frame(df.times, directed = FALSE)
      
      E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 
                                                                      1), 'within', 'between')
      E(g)[E(g)$group == 'within']$time <- 1
    }
  } else { # for skip method, we used the comment information as well
    df <- df[, c('Time', 'Subject', 'Behavior', 'Comment')] %>%
      rowwise() %>%
      mutate(Subject = strsplit(Subject, '-')[[1]][1], # extract only labels before hyphens
             Behavior = strsplit(Behavior, '->')[[1]][2])
    
    # need to add a TA handler too
    df.ta <- df %>%
      filter(Subject == 'TA' | Behavior == 'TA')
    
    df <- df %>%
      filter(Subject != 'TA' & Behavior != 'TA')
    
    df.table <- do.call("rbind", replicate(3, df[(df$Behavior == 'Table') & 
                                                   (!is.na(df$Behavior)),], 
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
                                   collapse = '')) < as.numeric(apply(asc(df$Behavior), 2, 
                                                                      paste, collapse = ''))
    df[df$ordered == FALSE, c('Subject', 'Behavior')] <- df[df$ordered == FALSE, 
                                                            c('Behavior', 'Subject')]
    df <- df %>%
      select(-c('ordered'))
    
    df.two.way <- df[duplicated(df[, c('Subject', 'Behavior', 'Interval')]),] %>%
      group_by(Subject, Behavior) %>%
      summarize(N.ints = n())
    
    g <- graph_from_data_frame(df.two.way, directed = FALSE)
    
    # E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 
    #                                                                 1), 'within', 'between')
    # 
    E(g)$count <- df.two.way$N.ints
    # E(g)$weight <- E(g)$count/max(E(g)$count) * 5
    # 
    # E(g)$line.type <- 1
  }
  
  # g <- add.graph.attributes(g, name = name)
  g <- igraph::permute(g, match(V(g)$name, sort(V(g)$name)))
  a <- as_adjacency_matrix(g, attr = ifelse(method == 'scan', 'time', 'count'), type = 'both',
                           sparse = FALSE)
  a[a == 0] <- ''
  write.csv(a, filename)
  return(g)
  
}

graph.from.adjacency <- function(file, method, name = ''){
#' Create a graph
#' 
#' Creates a graph object from an adjacency matrix
#' 
#' @param file adjacency matrix in csv format
#' @param method the method used to produce BORIS files, either scan, skip, or TA
#' @param name optional parameter for title of graph
#' 
#' returns a graph object
  
  matrix <- read.csv(file, header = TRUE, row.names = 1, check.names = FALSE, 
                     na.strings = "")
  matrix[is.na(matrix)] <- 0
  
  if(method != 'TA'){
    g <- graph_from_adjacency_matrix(as.matrix(matrix), mode = "undirected", 
                                     weighted = ifelse(method == 'scan', 'time', 'count'))
    V(g)$group <- substr(V(g)$name, 1, 1)
    E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 
                                                                    1), 'within', 'between')
  } else {
    g <- graph_from_adjacency_matrix(as.matrix(matrix), mode = "directed", 
                                     weighted = 'interaction')
  }
  
  g <- add.graph.attributes(g, name = name, method = method)
  
  return(g)
}

add.graph.attributes <- function(g, name, method){
#' Add graph attributes
#' 
#' Adds certain attributes that will be used in graph plotting and/or analysis
#' 
#' @param g graph object
#' @param name title of graph
#' @param method method used to produce BORIS files, either scan, skip, or TA
#' 
#' returns a graph object
  
  E(g)$color <- adjustcolor('black', 0.5)
  
  V(g)$centr.deg <- centralization.degree(g)$res
  
  if(method == 'scan'){
    E(g)$time <- E(g)$time/60 # put time in units of minutes
    E(g)$weight <- E(g)$time/max(E(g)$time) * 5
    E(g)$line.type <- 2 * (E(g)$group == 'within') + 1
    V(g)$size <- (V(g)$centr.deg + 1)/max(V(g)$centr.deg) * 15
  } else if(method == 'skip') {
    E(g)$weight <- E(g)$count/max(E(g)$count) * 5
    E(g)$line.type <- 1
    V(g)$size <- (V(g)$centr.deg + 1)/max(V(g)$centr.deg) * 15
  } else {
    E(g)$weight <- E(g)$time/max(E(g)$time) * 5
    E(g)$line.type <- 1
    # normalize against max non-TA node
    V(g)$size <- (V(g)$centr.deg + 1)/max(V(g)$centr.deg[2:length(V(g))]) * 15
  }
  
  g$name <- name
  
  return(g)
  
}

plot.graph <- function(g, method){
#' Plot a graph
#' 
#' Plots a SNA graph with specified attributes
#' 
#' @param g graph object
#' @param method method used to generate BORIS files, either scan, skip, or TA
#' 
#' returns a plot object
#' 
  pal <- brewer.pal(length(unique(V(g)$group)), "Set1")
  par(mar = c(0, 0, 0, 0))
  
  if(method != 'TA'){
    g$palette <- categorical_pal(max(V(g)$group))
    V(g)$color <- V(g)$group
    
    plot(g, edge.width = E(g)$weight, edge.color = E(g)$color, vertex.size = V(g)$size, 
         layout = layout_with_gem(g), edge.curved = 0.2, vertex.label = NA, 
         edge.lty = E(g)$line.type)
  } else {
    g$palette <- categorical_pal(2)
    V(g)$color <- ifelse(V(g)$name == 'TA', 1, 2)
    V(g)$size[1] <- max(V(g)$size[2:length(V(g))])
    
    plot(g, edge.width = E(g)$weight, edge.color = E(g)$color, vertex.size = V(g)$size, 
         layout = layout_with_gem(g), edge.curved = 0.2, edge.lty = E(g)$line.type)
  }

  title(g$name, line = -20, adj = 0.1)
}