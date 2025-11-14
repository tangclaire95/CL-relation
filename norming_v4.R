#loading packages 
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(pinyin)
library(ggplot2)


# norming v3 pilot data processing
# importing all of the csv's from the participants (UTF-8)
# combine into one data frame
# Specify the directory path
directory <- "/Users/xiaoyitang/Dropbox/Mac/Downloads/norming_v4_full"
# Specify the file names
all_file_names <- sprintf("v4sub%02d.csv", 1:36)
# File names to exclude
excluded_names <- c("v4sub06.csv", "v4sub20.csv","v4sub27.csv","v4sub28.csv","v4sub29.csv")
# Exclude specific file names
file_names <- setdiff(all_file_names, excluded_names)

## file_names <- c( ,, )
file_paths <- file.path(directory, file_names)

# Read and merge the CSV files
myMergedData <- do.call(rbind, lapply(file_paths, function(file) {
  if (file.exists(file)) {
    read.csv(file, encoding = "UTF-8")
  } else {
    warning(paste("File", file, "does not exist. Skipping..."))
    NULL
  }
}))




# Filter out the unneeded rows and columns
columns_to_keep <- c("trial_type", "trial_index", "time_elapsed", "rt", "response", "task", "clType", "clExemplar", "clSide", "containerType", "containerDirection", "blockNumber", "version", "curTrialNum")
df1 <- myMergedData [ , columns_to_keep] #filter columns
df1 %>% filter(task=="response") -> df2 #filter rows
head(df2)

# Convert JSON-formatted response column to separate columns
df_backup <- df2

df2 %>% 
  mutate(response = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(response)
#translate the CL to CL_py
#load a dictionary 
mypy <- pydic(dic='pinyin2',method='toneless') 

# df2$CL_py <- py(df2$CL, dic = mypy, sep="")
#df2 <- df2 %>% mutate(CL_py = py(CL, dic = mypy, sep = ""))
df2 <- df2 %>%
  mutate(response = map(response, ~ fromJSON(.) %>% as.data.frame() %>%
                          mutate(CL_py = py(CL, dic = mypy, sep = "")))) %>%
  unnest(response)

#df2 is filtered and translated 

#trim an extra space
df2$CL_py <- trimws(df2$CL_py)


#df3 only displays clType, clExemplar, CL_py
df3 <- df2[, c("clType", "clExemplar", "CL_py","blockNumber")]
# Create a new column (cl_Type_Num) by merging clType and clExemplar
df3$cl_Type_Num <- paste(df3$clType, df3$clExemplar, sep = "_")

# check if cl_py matches the target cl -> create match_in_isolation (norming)
df3$match_in_isolation <- 0
df3$match_in_isolation <- as.integer(df3$CL_py == df3$clType)
df3$match_in_isolation[df3$clType == "tuo"] <- as.integer(df3$CL_py[df3$clType == "tuo"] %in% c("tuo", "dui"))
df3$match_in_isolation[df3$clType == "jie"] <- as.integer(df3$CL_py[df3$clType == "jie"] %in% c("jie", "duan"))
df3$match_in_isolation[df3$clType == "ban"] <- as.integer(df3$CL_py[df3$clType == "ban"] %in% c("ban", "fen"))
df3$match_in_isolation[df3$clType == "quan"] <- as.integer(df3$CL_py[df3$clType == "quan"] %in% c("quan", "juan", "pan"))


# check if cl_py matches the alternative cl -> create match_in_relation
df3$match_in_relation <- 0
#(for each clType)
# to test multiple possible alternative matches
df3$match_in_relation[df3$clType == "tiao"] <- as.integer(df3$CL_py[df3$clType == "tiao"] %in% c("gen","ge"))
df3$match_in_relation[df3$clType == "pian"] <- as.integer(df3$CL_py[df3$clType == "pian"] %in% c("mian","zhang","ge"))
df3$match_in_relation[df3$clType == "di"] <- as.integer(df3$CL_py[df3$clType == "di"] %in% c("ke","li","ge"))
df3$match_in_relation[df3$clType == "quan"] <- as.integer(df3$CL_py[df3$clType == "quan"] %in% c("tuo","ge"))
df3$match_in_relation[df3$clType == "tuan"] <- as.integer(df3$CL_py[df3$clType == "tuan"] %in% c("tuo","ge"))
df3$match_in_relation[df3$clType == "tuo"] <- as.integer(df3$CL_py[df3$clType == "tuo"] %in% c("kuai","ge"))
df3$match_in_relation[df3$clType == "ba"] <- as.integer(df3$CL_py[df3$clType == "ba"] %in% c("zhi","ge"))
df3$match_in_relation[df3$clType == "jie"] <- as.integer(df3$CL_py[df3$clType == "jie"] %in% c("kuai","ge"))
df3$match_in_relation[df3$clType == "ban"] <- as.integer(df3$CL_py[df3$clType == "ban"] %in% c("kuai","ge"))

head(df3)




##see how many responses they use 
df_pinyin <- df3 %>%
  group_by(clType, cl_Type_Num) %>%
  summarise(CL_py_list = paste(CL_py, collapse = ", "))  

df_pinyin <- df3 %>%
  group_by(cl_Type_Num, CL_py) %>%
  summarise(count = n())

df_pinyin2 <- df_pinyin %>% 
group_by(cl_Type_Num) %>%
  summarise(CL_py_list = paste(CL_py, count, collapse = ", "))  



#for loop cl types generated for each one_of
#two plots: match-bar graphs, match accuracy table


## match-bar graphs
#a stacked bar graph, where the x-axis is each shape (the actual shape/exemplar)
#and the y axis is the frequency of each type of response 
library(ggplot2)
library(reshape2)
library(dplyr)

# Calculate counts for match_in_isolation, match_in_relation and others for each cl_Type_Num
# load plyr first and then dplyr (or load only dplyr)
df_counts <- df3 %>%
  group_by(clType, cl_Type_Num) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation))
# make a proportion column for each shape, based on the total number of responses 
df_counts <- df3 %>%
  group_by(clType, cl_Type_Num) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation),
            total_responses = n()) %>%
  mutate(prop_match_in_isolation = match_in_isolation / total_responses,
         prop_match_in_relation = match_in_relation / total_responses,
         prop_others = others / total_responses)

head(df_counts)


library(tidyr)
df_long <- df_counts %>%
  pivot_longer(cols = starts_with("prop"), names_to = "match_type", values_to = "proportion")

# Create the stacked bar plot
#for ALL SHAPES
ggplot(df_long, aes(x = factor(cl_Type_Num), y = proportion, fill = match_type)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion") +
  xlab("cl_Type_Num") +
  ggtitle("Stacked Bar Plot of Proportions by cl_Type_Num- All shapes") +
  theme_minimal()
## THIS !!


### only first block
df_counts <- df3 %>%
  group_by(clType, cl_Type_Num, blockNumber) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation))
# make a proportion column for each shape, based on the total number of responses 
df_counts <- df3 %>%
  group_by(clType, cl_Type_Num, blockNumber) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation),
            total_responses = n()) %>%
  mutate(prop_match_in_isolation = match_in_isolation / total_responses,
         prop_match_in_relation = match_in_relation / total_responses,
         prop_others = others / total_responses)

df_block0 <- df_counts[df_counts$blockNumber == 0,]
library(tidyr)
df_block0_long <- df_block0 %>%
  pivot_longer(cols = starts_with("prop"), names_to = "match_type", values_to = "proportion")
# Create the stacked bar plot
ggplot(df_block0_long, aes(x = factor(cl_Type_Num), y = proportion, fill = match_type)) +
  geom_bar(stat = "identity", position = "stack") +
  ylab("Proportion") +
  xlab("cl_Type_Num") +
  ggtitle("Stacked Bar Plot of Proportions by cl_Type_Num- block 0") +
  theme_minimal()



# TO MAKE seperate plots
# Load the gridExtra package for arranging plots
clTypes <- unique(df_counts$clType)
plotlist <- list()
for (i in 1:length(clTypes)) {
  currentCLType <- clTypes[i]
  
  df_long <- df_counts %>%
    filter(clType == currentCLType) %>%
    pivot_longer(cols = starts_with("prop"), names_to = "match_type", values_to = "proportion")
  
  plot <- ggplot(df_long, aes(x = factor(cl_Type_Num), y = proportion, fill = match_type)) +
    geom_bar(stat = "identity", position = "stack") +
    ylab("Proportion") +
    xlab("cl_Type_Num") +
    ggtitle(paste("Stacked Bar Plot of Proportions by cl_Type_Num -", currentCLType)) +
    theme_minimal()
  
  plotlist[[i]] <- plot
}

# At this point, plotlist will contain individual plots for each clType.
# You can now access each plot using plotlist[[1]], plotlist[[2]], and so on.
# To view the plots, you can use print(plotlist[[i]]) or use a loop to print all the plots:
for (i in 1:length(plotlist)) {
  print(plotlist[[i]])
}

## OR--- (make all plots together)
library(gridExtra)  
grid.arrange(grobs = plotlist, ncol = 3)  







####



library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


plotlist <- list()
titles <- character(0)  # To store titles for each plot

for (i in 1:length(clTypes)) {
  currentCLType <- clTypes[i]
  
  df_long <- df_counts %>%
    filter(clType == currentCLType) %>%
    pivot_longer(cols = starts_with("prop"), names_to = "match_type", values_to = "proportion")
  
  plot <- ggplot(df_long, aes(x = factor(cl_Type_Num), y = proportion, fill = match_type)) +
    geom_bar(stat = "identity", position = "stack") +
    ylab("Proportion") +
    xlab("cl_Type_Num") +
    ggtitle(paste("Stacked Bar Plot of Proportions -", currentCLType)) +
    theme_minimal() +
    guides(fill = FALSE)  # Hide individual legends
  plotlist[[i]] <- plot
  titles <- c(titles, paste("Stacked Bar Plot of Proportions  -", currentCLType))
}

# Combine all the plots into a grid using patchwork
grid_plot <- wrap_plots(plotlist, ncol = 3)
print(grid_plot)


#### 
# Create a common legend manually
common_legend <- get_legend(plotlist[[1]])
common_legend <- common_legend + theme(legend.position = "bottom")  # Adjust legend position

# Add the common legend and title to the grid
final_plot <- grid_plot + common_legend + plot_layout(title = titles[1])

# Display the final grid
print(final_plot)


###



clTypes <- unique(df_counts$clType) 
# Get a list of all the classifier types (you use unique to get only the unique values since it repeats in the full data frame)

plotlist <- list() 
# Set up a list of plots; lists can hold different kinds of objects

for (i in 1:length(clTypes)) {
  currentClType <- clTypes[i] 
  # Get the current clType
  
  # Get a subset of the data that matches the current clType
  subset_data <- df_long[df_long$clType == currentClType,]
  
  # Create the stacked bar plot for the current clType
  plot <- ggplot(subset_data, aes(x = factor(cl_Type_Num), y = proportion, fill = match_type)) +
    geom_bar(stat = "identity", position = "stack") +
    ylab("Proportion") +
    xlab("cl_Type_Num") +
    ggtitle(paste("Stacked Bar Plot of Proportions for clType =", currentClType)) +
    theme_minimal()
  
  plotlist[[i]] <- plot
}

# At this point, plotlist will contain individual plots for each clType.
# You can now access each plot using plotlist[[1]], plotlist[[2]], and so on.

# To view the plots, you can use print(plotlist[[i]]) or use a loop to print all the plots:
for (i in 1:length(plotlist)) {
  print(plotlist[[i]])
}


library(patchwork)
combined_plot <- wrap_plots(plotlist, nrow = 3)
print(combined_plot)









df3_sum <- ddply(df3, .(clType, clExemplar), transform, sum.n = sum(clExemplar == clExemplar))
df3_proportions <- ddply(df3_sum, .(clType, clExemplar, cl_py), summarise, n = length(cl_py), prop = n / sum.n)

#ALL CLs 
df_counts_long <- reshape2::melt(df_counts, id.vars = "cl_Type_Num")
# Filter to include only 3 variables
df_counts_filtered <- df_counts_long[df_counts_long$variable %in% c("match_in_isolation", "match_in_relation", "others"), ]
# Create the stacked bar plot
ggplot(df_counts_filtered, aes(x = cl_Type_Num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("match_in_isolation" = "blue","match_in_relation" = "green", "others" = "gray")) +
  xlab("cl_Type_Num") +
  ylab("Count") +
  labs(fill = "Legend") +
  theme_minimal()



 


# Create the stacked bar plot
ggplot(df_counts_long, aes(x = cl_Type_Num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("match_in_isolation" = "blue","match_in_relation" = "green", "others" = "gray")) +
  xlab("cl_Type_Num") +
  ylab("Count") +
  labs(fill = "Legend") +
  theme_minimal()




# Filter the df_counts data frame for specific cl_Type_Num values
df_some <- df_counts[df_counts$clType %in% c("ban"), ]

# Reshape the data to long format
df_some_long <- reshape2::melt(df_some, id.vars = "cl_Type_Num")

# Create the stacked bar plot
ggplot(df_some_long, aes(x = cl_Type_Num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("others" = "gray", "match_in_relation" = "green","match_in_isolation" = "blue" )) +
  xlab("cl_Type_Num") +
  ylab("Count") +
  labs(fill = "Legend") +
  theme_minimal()




# stacked bar plot with facet_wrap
# Melt the data for plotting
df_counts_long <- reshape2::melt(df_counts, id.vars = c("clType", "cl_Type_Num"))

# Create the stacked bar plot with facet_wrap
ggplot(df_counts_filtered, aes(x = cl_Type_Num, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("others" = "gray", "match_in_relation" = "green", "match_in_isolation" = "blue")) +
  xlab("cl_Type_Num") +
  ylab("Count") +
  labs(fill = "Legend") +
  theme_minimal() +
  facet_wrap(~clType, nrow = 3)


### try this 
clTypes <- unique(df$clType) # get a list of all the classifier types (you use unique to get only the unique values since it repeats in the full data frame)
plotlist <- list() # set up a list of plots; lists can just hold different kinds of objects
for (i in 1:length(clTypes)) {
  currentClType <- clTypes[i] # get the current clType
  [insert code to get a subset of the data that matches the t clType]
  plotlist[[i]] <- [insert plot code here using that data subset]
}





# Then use grid.arrange to set up the different plots in a 3 by 3 grid

##we should switch from counts to proportions, 
df3_sum <- ddply(df3, .(clType, clExemplar), transform, sum.n = sum(clExemplar == clExemplar))
df3_proportions <- ddply(df3_sum, .(clType, clExemplar, cl_py), summarise, n = length(cl_py), prop = n / sum.n)



#count the number of rows of the combinations of cl_Type_Num, and CL_py.
df4 <- ddply(df3, .(clType, clExamplar, CL_py), summarise, count = length(clType))

##match accuracy table 
#accuracy by cl_Type_Num
df_counts_backup <- df_counts 

df_counts %>% group_by (cl_Type_Num) %>% mutate (accuracy=mean(match_in_isolation/6)) -> df_counts
#accuracy by clType
df_counts <- df_counts %>%
  group_by(clType) %>%
  mutate(accuracy_type = mean(match_in_isolation / (match_in_isolation + match_in_relation + others)))

head(df_counts)

# Calculate mean accuracy
df_counts <- df_counts %>%
  group_by(cl_Type_Num) %>%
  summarise(accuracy = mean(match_in_isolation / 6))

## mean accuracy plots 
ggplot(data = df_counts, aes(x = cl_Type_Num, y = accuracy)) +
  geom_point() +
  xlab("cl_Type_Num") +
  ylab("Mean Accuracy") +
  ggtitle("Mean Accuracy Plot")

## nice plots 
df_counts$clType <- factor(df_counts$clType, levels = unique(df_counts$clType))

ggplot(data = df_counts, aes(x = cl_Type_Num, y = accuracy)) +
  geom_point() +
  xlab("clType") +
  ylab("Mean Accuracy") +
  ggtitle("Mean Accuracy Plot") +
  facet_wrap(~ clType, nrow = 3, ncol = 3)







# Create summary table
summary_table <- as.data.frame(table(df_counts$cl_Type_Num, df_counts$CL_py,df_counts$accuracy))
# Rename the columns for clarity
colnames(summary_table) <- c("cl_Type_Num", "response", "frequency")





df4$CL_py=factor(df4$CL_py)
df4$clType=factor(df4$clType)

# Calculate SEM
df4_sum <- df4_sum %>%
  group_by(clType) %>%
  summarise(meanAccuracy = mean(meanAccuracy),
            sem = sd(meanAccuracy) / sqrt(n()))




# Calculate SEM
df4_sum <- df4_sum %>%
  group_by(clType) %>%
  summarise(meanAccuracy = mean(meanAccuracy),
            sem = sd(meanAccuracy) / sqrt(n()))

# Plot meanAccuracy with error bars
ggplot(data = df4_sum, aes(x = clType, y = meanAccuracy)) +
  geom_point() +
  geom_errorbar(aes(ymin = meanAccuracy - sem, ymax = meanAccuracy + sem),
                width = 0.1) +
  xlab("clType") +
  ylab("Mean Accuracy") +
  ggtitle("Mean Accuracy Plot")






# Plot the data
#you want the fill to be CL_response not clExemplar
ggplot(df, aes(x = cl_Type_Num, fill = CL_py)) +
  geom_bar(aes(weight = Freq), position = "stack") 

# Filter the data frame to include only "quan" in clType
df_filtered <- df[grep("quan", df$clType), ]
# Create a new column by merging clType and clExemplar
df_filtered$combined <- paste(df_filtered$clType, df_filtered$clExamplar, sep = "_")
# Plot the filtered data
ggplot(df_filtered, aes(x = combined, fill = CL_py)) +
  geom_bar(aes(weight = Freq), position = "stack") 









##### else 

py(df2$CL, dic = mypy, sep="") #translate

class(py(df2$CL, dic = mypy, sep=""))
length(py(df2$CL, dic = mypy, sep=""))
data.frame(py(df2$CL, dic = mypy, sep=""))

#pinyin example
py("春眠不觉晓，处处闻啼鸟", dic = mypy) #translate






