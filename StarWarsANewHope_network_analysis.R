####### R network analysis of Star Wars - A New Hope (IV)
####### In Celebration of the 40th Anniversary of Star Wars (5/25/1977-5/25/2017)
library(tidyverse)
library(reshape2)
library(igraph)
library(d3Network)
library(networkD3)
########################### Parsing the Data for Easy Reading/Analysis #########################
### I found the script in the easiest-to-read,
### windows doc format here: www.theforce.net/timetales/ep4se.doc ,
### which I converted into txt format for convenience in parsing/analysis.

### Read in file line by line instead of all at once.
raw_script <- readLines("star-wars-a-new-hope.txt")
### Create data frame from the raw data.
lines <- data_frame(raw_script = raw_script) 
### Get rid of all extra (leading and trailing) white spaces.
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
raw_script <- trim(raw_script)
df1 <- data_frame(raw_script = raw_script) 
### Get rid of the empty lines.
df2 <- filter(df1, raw_script != "")
### Detect scenes: detect lines that begin with EXT. or INT.
df3 <-  mutate(df2, is_scene = str_detect(raw_script, "T."),scene = cumsum(is_scene)) 
### Drop lines that start with EXT. or INT.
df4 <- filter(df3,!is_scene)
### Distinguish characters from what they say.
df5 <- separate(df4, raw_script, c("speaker", "dialogue"), 
                   sep = ":", fill = "left",extra='drop')
### Read in aliases (from Evelina's post/GitHub).
aliases <- read.table('C:/Users/Alyssa Columbus/Desktop/aliases.csv',sep=',',header=T,colClasses = "character")
#aliases$Alias #uncomment to check
#aliases$Name
### Assign unique name to characters.
multipleReplace <- function(x, what, by) {
  stopifnot(length(what)==length(by))               
  ind <- match(x, what)
  ifelse(is.na(ind),x,by[ind])
}
df6 <- mutate(df5,speaker=multipleReplace(speaker,what=aliases$Alias,by=aliases$Name))
### Read in actual names (from Evelina's post/GitHub).
actual.names <- read.csv('C:/Users/Alyssa Columbus/Desktop/characters.csv',header=F,colClasses = "character")
actual.names <- c(as.matrix(actual.names))
### Filter out non-characters (very important).
df7 <- filter(df6,speaker %in% actual.names)
### Group by scene.
df8 <- group_by(df7, scene, line = cumsum(!is.na(speaker))) 
df9 <- summarize(df8, speaker = speaker[1], 
                    dialogue = str_c(dialogue, collapse = " "))
### Count the lines-per-scene-per-character.
by_speaker_scene <- count(df9, scene, speaker)
### Turn the result into a binary speaker-by-scene matrix.
speaker_scene_matrix <-acast(by_speaker_scene , speaker ~ scene, fun.aggregate = length)

################################## ANALYSIS ###############################################
### Create a cooccurrence matrix containing how many times two characters share scenes.
cooccur <- speaker_scene_matrix %*% t(speaker_scene_matrix)
### Graphical representation of the network.
graphrep <- graph.adjacency(cooccur, weighted = TRUE, mode = "undirected", diag = FALSE)
### Get A Nicer Representation of the Network 
simpgraph <- simplify(graphrep)
df <- get.edgelist(graphrep, names=TRUE)
df <- as.data.frame(df)
colnames(df) <- c('source', 'target')
df$value <- rep(1, nrow(df))
### Get Communities.
fc <- fastgreedy.community(graphrep)
com <- membership(fc)
node.info <- data.frame(name=names(com), group=as.vector(com))
links <- data.frame(source=match(df$source, node.info$name)-1,
                    target=match(df$target, node.info$name)-1,value=df$value)
forceNetwork(Links = links, Nodes = node.info,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 1, opacityNoHover=1)