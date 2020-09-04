library(data.table)
library(dplyr)
library(reshape2)
library(tidyr)
library(igraph)
library(gtools)
library(RColorBrewer)
library(zoo)
library(docstring)


boris.to.adjacency <- function(file1, nvid1, offset1 = 0, file2 = NULL, 
                               nvid2 = NULL, offset2 = 0, method = 'scan-student', 
                               ignore.begin = FALSE, collapse.group = FALSE, 
                               TA.adjacency = NULL, directed = FALSE, 
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
#' @param method Either scan-student, scan-group, or skip
#' @param ignore.begin binary, whether to only include interactions after class start
#' @param collapse.group binary, whether to collapse scan-student to scan-graph;
#' only applies if method is scan-student
#' @param TA.adjacency file name of TA method adjacency matrix to merge with at 
#' group level; only applies if collapse.group is TRUE
#' @param directed binary, whether to include direction of interactions
#' @param filename filename and path of exported adjacency matrix
#' 
#' returns a graph object

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
  
  df[df$Behavior == 'StartClass', 'Subject'] <- NA # sometimes there is a leftover subject here
  df <- df[!duplicated(df$Behavior) | (df$Behavior != 'StartClass'),]
  if(ignore.begin){ # set everything before class start to the class start time, so we don't count it
    StartTime <- df[df$Behavior == 'StartClass', 'Time']
    df[df$Time < StartTime, 'Time'] <- StartTime
  }
  
  if(method == 'scan-student'){
    df <- df[, c('Time', 'Subject', 'Behavior')] %>%
      rowwise() %>%
      mutate(Subject = strsplit(Subject, '-')[[1]][1]) # extract labels before hyphens
    
    # dcast so we have location of each student at each timestamp in the data
    df.dcast <- dcast(df, Time ~ Subject, value.var = 'Behavior')
    if('NA' %in% names(df.dcast)){ # remove NA subjects if they exist
      df.dcast <- df.dcast %>% 
        select(-c('NA'))
    }
    df.fill <- fill(df.dcast, -Time) # down fill data until students change location
    df.fill[is.na(df.fill)] <- 'StudentExit' # fill empty locations with StudentExit
    
    students <- colnames(df.fill)[2:ncol(df.fill)]
    if(collapse.group){
      # get all pairs of directed table to table interactions possible
      tables <- unique(df$Behavior)[unique(df$Behavior) %like% 'Table']
      interactions.vec <- c(unlist(lapply(combn(tables, 2, simplify = FALSE), paste, 
                                          collapse = ':')), 
                            unlist(lapply(combn(rev(tables), 2, simplify = FALSE), 
                                          paste, collapse = ':')))
    } else { # all pairs of directed student interactions
      interactions.vec <- c(unlist(lapply(combn(students, 2, simplify = FALSE), paste, 
                                          collapse = ':')), 
                            unlist(lapply(combn(rev(students), 2, simplify = FALSE), 
                                          paste, collapse = ':')))
    }

    # create empty vector of interaction times for all pairs of students/tables
    times.vec <- rep(0, length(interactions.vec))
    names(times.vec) <- interactions.vec
    df.fill <- df.fill[, c(students, 'Time')]
    
    # aggregate times for all student-student interactions
    for (row in 1:(nrow(df.fill) - 1)){
      if(collapse.group){
        for (subject in tables){
          for (location in tables){ # keep time column and columns with subject at location
            if(location %in% df.fill[row, names(df.fill) %like% substr(subject, 6, 6) | 
                                     names(df.fill) == 'Time']){
              times.vec[paste(subject, location, 
                              sep = ':')] <- times.vec[paste(subject, location, 
                                                             sep = ':')] +
                df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
              
            }
          }
        }
      } else {
        for (student1 in 1:(length(students) - 1)){
          for (student2 in (student1 + 1):length(students)){
            if((df.fill[row, student1] == df.fill[row, student2]) & 
               (df.fill[row, student1] != 'StudentExit')){
              if(!directed){
                times.vec[paste(students[student1], students[student2], 
                                sep = ':')] <- times.vec[paste(students[student1], 
                                                               students[student2], 
                                                               sep = ':')] + 
                  df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
              } else if(students[student1] %like% substr(df.fill[row, student1], 
                                                         6, 6)){
                # check which table students are at...if it is not either student's table 
                # we ignore the directed interaction
                times.vec[paste(students[student2], students[student1], 
                                sep = ':')] <- times.vec[paste(students[student2], 
                                                               students[student1], 
                                                               sep = ':')] + 
                  df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
              } else if(students[student2] %like% substr(df.fill[row, student2], 
                                                         6, 6)){
                times.vec[paste(students[student1], students[student2], 
                                sep = ':')] <- times.vec[paste(students[student1], 
                                                               students[student2], 
                                                               sep = ':')] + 
                  df.fill[(row + 1),  'Time'] - df.fill[row, 'Time']
              }
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
      select(-c('Subjects', 'times.vec'))
    
    g <- graph_from_data_frame(df.times, directed = directed)
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
    
    if(!collapse.group){
      # Set within group times to one second
      E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 1) == substr(ends(g, E(g))[, 2], 1, 
                                                                      1), 'within', 'between')
      E(g)[E(g)$group == 'within']$time <- 1
    }
  } else if(method == 'scan-group'){
    df <- df[, c('Time', 'Subject', 'Behavior', 'Status')] %>% # state events rather than point
      rowwise() %>%
      mutate(Behavior = strsplit(Behavior, '->')[[1]][2]) %>%
      filter(Behavior != 'ClassStart')
    
    df <- df %>% group_by(Subject, Behavior) %>%
      mutate(time.diff = rollapplyr(Time, width = 2, FUN = diff, partial = TRUE)) %>%
      filter(Status == 'START') %>%
      select(Subject, Behavior, time.diff) %>%
      group_by(Subject, Behavior) %>%
      summarize(time = sum(time.diff))
    
    g <- graph_from_data_frame(df, directed = directed)
    E(g)$time <- df$time
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
    df$Interval <- round(df$Time/120) # partition into 2min intervals
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
    
    g <- graph_from_data_frame(df.two.way, directed = FALSE) # we did not keep track of direction using this method
    E(g)$count <- df.two.way$N.ints
  }
  
  # g <- add.graph.attributes(g, name = name)
  g <- igraph::permute(g, match(V(g)$name, sort(V(g)$name)))
  a <- as_adjacency_matrix(g, attr = ifelse(method %like% 'scan', 'time', 'count'), type = 'both',
                           sparse = FALSE)
  a[(a == 0) | is.na(a)] <- ''
  write.csv(a, filename)
  return(g)
  
}

graph.from.adjacency <- function(file, method, directed = FALSE, name = '', 
                                 excludeNodes = NULL){
#' Create a graph
#' 
#' Creates a graph object from an adjacency matrix
#' 
#' @param file adjacency matrix in csv format
#' @param method Either scan-student, scan-group, or skip
#' @param directed binary; whether graph is directed or undirected
#' @param name optional parameter for title of graph
#' @param excludeNodes vector of nodes to remove from the graph
#' 
#' returns a graph object
  
  matrix <- read.csv(file, header = TRUE, row.names = 1, check.names = FALSE, 
                     na.strings = "")
  matrix[is.na(matrix)] <- 0
  g <- graph_from_adjacency_matrix(as.matrix(matrix), 
                                   mode = ifelse(directed, "directed", "undirected"),
                                   weighted = TRUE)
  if(!is.null(excludeNodes)){
    g <- induced_subgraph(g, !(V(g)$name %in% excludeNodes))
  }
  
  if(method != 'scan-group'){
    V(g)$group <- substr(V(g)$name, 1, 1)
    if(method == 'scan-student'){
      # in case the adjacency was built manually and not standardized within-groups
      E(g)$group <- ifelse(substr(ends(g, E(g))[, 1], 1, 
                                  1) == substr(ends(g, E(g))[, 2], 1, 1), 'within', 
                           'between')
    }
  }
  
  V(g)$centrality.total <- igraph::degree(g, mode = 'all')
  V(g)$strength.total <- strength(g, mode = 'all')
  if(is_directed(g)){
    V(g)$centrality.in <- igraph::degree(g, mode = 'in')
    V(g)$strength.in <- strength(g, mode = 'in')
    V(g)$centrality.out <- igraph::degree(g, mode = 'out')
    V(g)$strength.out <- strength(g, mode = 'out')
  }
  
  return(g)
}

plot.graph <- function(g, vertex.scale = 1, edge.scale = 1, standardNodes = NULL,
                       layout = NULL, vertex.label = TRUE){
#' Plot a graph
#' 
#' Plots a SNA graph with specified attributes
#' 
#' @param g graph object
#' @param vertex.scale constant to be multiplied by all vertex sizes
#' @param edge.scale constant to be multiplied by all edge sizes
#' @param standardNodes vector of nodes to be excluded from scaling (e.g., 'TA' or 
#' 'EveryoneTable')
#' @param layout how to arrange nodes in network; currently accepts 'B03' or 'B22',
#' anything else will use gem layout. gem layout will be used for any student-level
#' graph.
#' @param vertex.label binary; whether to label nodes
#' 
#' returns a plot object
  
  E(g)$color <- adjustcolor('black', 0.5)
  E(g)$size <- E(g)$weight/max(E(g)$weight) * edge.scale
  if('group' %in% edge_attr_names(g)){
    E(g)$line.type <- 2 * (E(g)$group == 'within') + 1
  } else {
    E(g)$line.type <- 1
  }
  
  if(!is.null(standardNodes)){
    # add 1 to centralities to deal with zeros
    V(g)$size <- ifelse(V(g)$name %in% standardNodes, 1,
                        (V(g)$centrality.total + 1)/max(V(g)$centrality.total[!(V(g)$name 
                                                                                %in% 
                                                                                  standardNodes)]))
  } else {
    V(g)$size <- (V(g)$centrality.total + 1)/max(V(g)$centrality.total)
  }
  V(g)$size <- V(g)$size * vertex.scale
  
  layout.df <- data.frame(row.names = c('Table0', 'Table1', 'Table2', 'Table3', 
                                        'Table4', 'Table5', 'Table6', 'Table7', 
                                        'Table8', 'TA', 'EveryoneTable'), 
                          B03_x = c(-1, -1, -1, -1, -1, 1, 1, 1, 1, 0, 0.5), 
                          B03_y = c(0, 1, 1.5, 2.5, 3, 1, 1.5, 2.5, 3, 4, 4), 
                          B22_x = c(NA, -1, 0, -0.5, 0.5, 1.5, 1, 1.5, NA, 0.5, 0), 
                          B22_y = c(NA, 1, 1, 0, 0, 0.5, 1, 1.5, NA, 1.5, 1.5))
  if(layout == 'B03' & !('group' %in% edge_attr_names(g))){
    l = unname(as.matrix(df[V(g)$name, c('B03_x', 'B03_y')]))
  } else if(layout == 'B22' & !('group' %in% edge_attr_names(g))){
    l = unname(as.matrix(df[V(g)$name, c('B22_x', 'B22_y')]))
  } else { # if no layout or invalid layout given, go to gem
    l = layout_with_gem(g)
  }
  
  pal <- brewer.pal(length(unique(V(g)$group)), "Set1")
  par(mar = c(0, 0, 0, 0))
  if('group' %in% vertex_attr_names(g)){
    g$palette <- categorical_pal(max(V(g)$group))
    V(g)$color <- V(g)$group
  } else {
    g$palette <- categorical_pal(2)
    V(g)$color <- ifelse(V(g)$name %in% c('TA', 'EveryoneTable'), 1, 2)
  }

  # need vectors of vertex.label binaries to go with ifelse()
  plot(g, edge.width = E(g)$size, edge.color = E(g)$color, 
       vertex.size = V(g)$size, layout = l, edge.curved = 0.2, 
       edge.lty = E(g)$line.type, vertex.label = ifelse(rep(vertex.label, 
                                                            length(V(g))), 
                                                        V(g)$name, NA))
  title(g$name, line = -20, adj = 0.1)
}