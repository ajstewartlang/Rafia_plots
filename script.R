library(tidyverse)

# the following function takes the parameters of a dataset and uses them to 
# build a dataframe. 
create_df <- function(my_df){
  set.seed(1234)
  y <- c(rnorm(10, my_df$gp1_pop_mean, my_df$gp1_pop_sd), 
         rnorm(10, my_df$gp2_pop_mean, my_df$gp2_pop_sd))
  x <- c(rep(my_df$gp1_label, 10), rep(my_df$gp2_label, 10))
  df <- as_tibble(cbind(y, x)) %>%
    mutate(y = as.double(y)) %>%
    mutate(x = as.factor(x))
}

# creating a wrapper function to wrap the title text
wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

# the following function takes a tibble, a label for the x-axis, a label for 
# the y-axis, and a title and constructs a jittered graph
# this graph is passed to 'raw'
# the y axis limits are passed to a vector
my_graph <- function(df, labx, laby, title) {
  set.seed(1234)
  raw <- df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_jitter(width = .1, alpha = .75, height = 0) +
    labs(x = labx, y = laby, title = title) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank()) +
    expand_limits(y = 0) +
    ggtitle(wrapper(title, width = 30)) +
    theme(panel.background = element_rect(fill = "white"))
  
  vec <- ggplot_build(raw)$layout$panel_params[[1]]$y.range
  return(list(raw, vec))
  
}

my_bar_graph <- function(df, labx, laby, title) {
  set.seed(1234)
  df %>%
    ggplot(aes(x = x, y = y)) +
    geom_jitter(aes(y = y), width = .1, alpha = 0, height = 0) +
    stat_summary(geom = "col", fun.y = mean) +
    labs(x = labx, y = laby, title = title) +
    theme(text = element_text(size = 18),
          panel.grid = element_blank()) +
    expand_limits(y = 0) +
    ggtitle(wrapper(title, width = 30)) +
    theme(panel.background = element_rect(fill = "white"))
}


my_blank_graph <- function(df, labx, laby, title) {
  set.seed(1234)
  df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_jitter(width = .1, alpha = 0, height = 0) +
    labs(x = labx, y = laby, title = title) +
    theme(text = element_text(size = 18)) +
    expand_limits(y = 0) +
    ggtitle(wrapper(title, width = 30)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_line(colour = "black"))


  # adding gridlines manually - don't use
          #majors <- ggplot_build(p)$layout$panel_params[[1]]$y.major_source
          #multiplier <- 4
          #minors <- seq(from = 0,
                        #to = ((nth(majors, 2)) + (max(majors))),
                        #to = max(majors),
                        #length.out = ((length(majors) - 1) * multiplier) + 1)
          #p <- p + scale_y_continuous(minor_breaks = minors)
          #return(list(p, majors))

}


# the following function takes a graph and saves it in the graphs folder, 
# generates summary statistics of the data and saves them as a .csv file in the 
# summary_stats folder. In both cases a unique index is created to allow the
# graph and the summary stats to be paired up later durind coding of the 
# respones of participants. 

save_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_scatter_", index, ".jpg"), current_graph)
  build_this_one %>%
    group_by(x) %>%
    summarise(mean = mean(y), sd = sd(y)) %>%
    cbind(index) %>%
    write_csv(paste0("summary_stats/summary", index, ".csv"))
}  

save_bar_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_bar_", index, ".jpg"), current_graph)
  build_this_one %>%
    group_by(x) %>%
    summarise(mean = mean(y), sd = sd(y)) %>%
    cbind(index) %>%
    write_csv(paste0("summary_stats/summary_bar", index, ".csv"))
}  

save_blank_graph <- function(current_graph){
  ggsave(paste0("graphs/graph_blank_", index, ".jpg"), current_graph)
  build_this_one %>%
    group_by(x) %>%
    summarise(mean = mean(y), sd = sd(y)) %>%
    cbind(index) %>%
    write_csv(paste0("summary_stats/summary_blank", index, ".csv"))
}  

# MAIN CODE ####
# this reads in the .csv file that contains the parameters of the graphs to be
# generated
my_graphs <- read_csv("graphs_book.csv")

# this loops through the my_graphs which contains the paramters of the 
# graphs to be generated.  It runs once per each unique graph_id 
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  raw_graph <- my_graph(build_this_one, 
                        my_graphs[my_graphs$graph_id == index,]$x_label, 
                        my_graphs[my_graphs$graph_id == index,]$y_label, 
                        my_graphs[my_graphs$graph_id == index,]$title) 
  
  save_graph(raw_graph[[1]])
  
  raw_graph[[2]] %>%
    enframe() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    cbind(index) %>%
    write_csv(., paste0("summary_stats/summary", index, "axis.csv"))
}

# build bar graphs
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  my_bar_graph(build_this_one, 
           my_graphs[my_graphs$graph_id == index,]$x_label, 
           my_graphs[my_graphs$graph_id == index,]$y_label, 
           my_graphs[my_graphs$graph_id == index,]$title) %>%
    save_bar_graph()
}

# build blank graphs
for(index in my_graphs$graph_id) {
  
  build_this_one <- my_graphs %>%
    filter(graph_id == index) %>%
    create_df() 
  
  my_blank_graph(build_this_one, 
                 my_graphs[my_graphs$graph_id == index,]$x_label,
                 my_graphs[my_graphs$graph_id == index,]$y_label, 
                 my_graphs[my_graphs$graph_id == index,]$title) %>%
    save_blank_graph()
  
  # only use when plot is named with assignment operator
  #blank_graph <- my_blank_graph(build_this_one, 
                          #my_graphs[my_graphs$graph_id == index,]$x_label,
                          #my_graphs[my_graphs$graph_id == index,]$y_label, 
                          #my_graphs[my_graphs$graph_id == index,]$title)
  
    #save_blank_graph(blank_graph[[1]])
  
}

# I HAVEN'T YET RUN THIS
# generate summary stats ####
data_path <- "summary_stats"   # path to the data
files <- dir(data_path, pattern = "*.csv") # get file names

# the following reads in all the .csv files from the summary_stats folder
# and creates one master df that is then saved as "all_summary.csv"
my_data <- files %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

write_csv(my_data, file.path(data_path, "all_summary.csv"))

# the same for the files containing the y axis limits
axisfiles <- dir(data_path, pattern = "*axis.csv") # get file names

axis_data <- axisfiles %>%
  map(function(x) read_csv(file.path(data_path, x))) %>%  
  reduce(rbind) %>%
  mutate(graph_id = index) %>%
  select(- index)

write_csv(axis_data, file.path(data_path, "axis_summary.csv"))

