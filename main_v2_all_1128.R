## 
#loading packages 
library(jsonlite)
#library(dplyr)
library(purrr)
library(tidyr)
library(pinyin)
library(ggplot2)
library(reshape2)
library(Rmisc)
library(tidyverse)
library(lme4)
library(simr)


# MAIN_V1 pilot data processing
# importing all of the csv's from the participants (UTF-8)
# combine into one data frame
# Specify the directory path
directory <- "/Users/xiaoyitang/Dropbox/Mac/Downloads/101723_main_v2"
# Specify the file names
# subject 01-69
v2_file_names <- sprintf("mainV2_sub%02d.csv", 1:80)

# Combine the two sets of file names
# all_file_names <- c(v4_file_names, v3_file_names)

# File names to exclude
#exclude 10, 24, 26, 35, 42, 46, 47, 49, 59, 61,75,76
excluded_names <- c("mainV2_sub10.csv","mainV2_sub24.csv","mainV2_sub26.csv",
                    "mainV2_sub35.csv","mainV2_sub42.csv","mainV2_sub46.csv",
                    "mainV2_sub47.csv","mainV2_sub49.csv","mainV2_sub59.csv",
                    "mainV2_sub61.csv", "mainV2_sub75.csv", "mainV2_sub76.csv")

# Exclude specific file names
file_names <- setdiff(v2_file_names, excluded_names)

## file_names <- c( ,, )
file_paths <- file.path(directory, file_names)

# Read and merge the CSV files
#use bind_rows

myMergedData <- do.call(bind_rows, lapply(file_paths, function(file) {
  if (file.exists(file)) {
    read.csv(file, encoding = "UTF-8", tryLogical = F)
  } else {
    warning(paste("File", file, "does not exist. Skipping..."))
    NULL
  }
}))


# (optional)
##check info
columnsinfo_to_keep <- c("trial_index", "response")
df_info1 <- myMergedData [ , columnsinfo_to_keep] #filter columns
df_info1 %>% filter(trial_index=="3") -> df2info #filter rows





# Filter out the unneeded rows and columns
columns_to_keep <- c("subject_id","trial_type", "trial_index", "time_elapsed", "rt", "response", "task", "blockNumber","curTrialNum","clType", "clExemplar", "clSide", "containerType","sceneType", "version")
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


#df3 only displays several parameters 
df3 <- df2[, c("subject_id","blockNumber","CL_py", "clType", "clExemplar", "clSide","sceneType" )]




# Create a new column (cl_Type_Num) by merging clType and clExemplar
df3$cl_Type_Num <- paste(df3$clType, df3$clExemplar, sep = "_")

# check if cl_py matches the target cl -> create match_in_isolation (norming)
df3$match_in_isolation <- 0
df3$match_in_isolation <- as.integer(df3$CL_py == df3$clType)
df3$match_in_isolation[df3$clType == "tuo"] <- as.integer(df3$CL_py[df3$clType == "tuo"] %in% c("tuo", "dui"))
df3$match_in_isolation[df3$clType == "jie"] <- as.integer(df3$CL_py[df3$clType == "jie"] %in% c("jie", "duan"))
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

head(df3)


# don't run this
df_counts <- df3 %>%
  group_by(clType, cl_Type_Num, sceneType, subject_id) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation))



# item counts 
df_item1 <- df3 %>%
  group_by(clType, sceneType) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation),
            total_responses = n()) %>%
  mutate(prop_match_in_isolation = match_in_isolation / total_responses,
         prop_match_in_relation = match_in_relation / total_responses,
         prop_others = others / total_responses)


head(df_item1)


#subject counts 
df_subj1 <- df3 %>%
  group_by(subject_id, sceneType) %>%
  summarise(match_in_isolation = sum(match_in_isolation),
            match_in_relation = sum(match_in_relation),
            others = n() - sum(match_in_isolation) - sum(match_in_relation),
            total_responses = n()) %>%
  mutate(prop_match_in_isolation = match_in_isolation / total_responses,
         prop_match_in_relation = match_in_relation / total_responses,
         prop_others = others / total_responses)

head(df_subj1)




#prepare for plots 
#item plots
df_item2 <- df_item1 [,c("clType","sceneType", "prop_match_in_isolation","prop_match_in_relation","prop_others" )]
#subject plots
df_subj2 <- df_subj1 [,c("subject_id", "sceneType", "prop_match_in_isolation","prop_match_in_relation","prop_others" )]





#########
## PLOTS


## specific 
### specific by subject
dc3 <- summarySEwithin(df_subj2, measurevar="prop_match_in_isolation", withinvars=c("sceneType"),
                       idvar="subject_id", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl3 <- ggplot(data = df_subj2, aes(x=sceneType, y=prop_match_in_isolation)) +
  # now the average data
  geom_line(data = dc3, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc3, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc3, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('isolation', 'relation')) +
  labs(y = 'mean "specific" classifier matches') 

pl3



### specific by item 
dc1 <- summarySEwithin(df_item2, measurevar="prop_match_in_isolation", withinvars=c("sceneType"),
                       idvar="clType", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl1 <- ggplot(data = df_item2, aes(x=sceneType, y=prop_match_in_isolation)) +
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_isolation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=clType),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=clType), 
             position = jitter) +
  geom_text(aes(label=clType, alpha=0.4), 
            position = jitter) +
  # now the average data
  geom_line(data = dc1, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc1, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc1, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('isolation', 'relation')) +
  labs(y = 'mean "specific" classifier matches') 

pl1





###general 
### general by subject 
dc4 <- summarySEwithin(df_subj2, measurevar="prop_match_in_relation", withinvars=c("sceneType"),
                       idvar="subject_id", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl4 <- ggplot(data = df_subj2, aes(x=sceneType, y=prop_match_in_relation)) +
  # now the average data
  geom_line(data = dc4, size=1, aes(x=sceneType, y=prop_match_in_relation, group=1)) +
  geom_errorbar(data = dc4, size=1, aes(group = 1, x=sceneType, y=prop_match_in_relation, ymin=prop_match_in_relation-ci, ymax=prop_match_in_relation+ci), width=.03) +
  geom_point(data = dc4, aes(x=sceneType, y=prop_match_in_relation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('isolation', 'relation')) +
  labs(y = 'mean "general" classifier matches') 
pl4


### general by item
dc2 <- summarySEwithin(df_item2, measurevar="prop_match_in_relation", withinvars=c("sceneType"),
                       idvar="clType", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl2 <- ggplot(data = df_item2, aes(x=sceneType, y=prop_match_in_relation)) +
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_relation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=clType),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=clType), 
             position = jitter) +
  geom_text(aes(label=clType, alpha=0.4), 
            position = jitter) +
  # now the average data
  geom_line(data = dc2, size=1, aes(x=sceneType, y=prop_match_in_relation, group=1)) +
  geom_errorbar(data = dc2, size=1, aes(group = 1, x=sceneType, y=prop_match_in_relation, ymin=prop_match_in_relation-ci, ymax=prop_match_in_relation+ci), width=.03) +
  geom_point(data = dc2, aes(x=sceneType, y=prop_match_in_relation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('isolation', 'relation')) +
  labs(y = 'mean "general" classifier matches') 

pl2











##########################
## item plots
# item_isolation 
dc1 <- summarySEwithin(df_item2, measurevar="prop_match_in_isolation", withinvars=c("sceneType"),
                       idvar="clType", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl1 <- ggplot(data = df_item2, aes(x=sceneType, y=prop_match_in_isolation)) +
  #geom_hline(yintercept=0.5, alpha = 0.75, size=1, linetype = 'dashed') + 
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_isolation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=clType),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=clType), 
             position = jitter) +
  # now the average data
  geom_line(data = dc1, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc1, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc1, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "specific" classifier matches by classifier type') 

pl1


#no violin
pl11 <- ggplot(data = df_item2, aes(x=sceneType, y=prop_match_in_isolation)) +
  geom_line(data = dc1, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc1, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc1, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "specific" classifier matches by classifier type') +
  ylim(0.35,0.55)

pl11



## item_relation 

dc2 <- summarySEwithin(df_item2, measurevar="prop_match_in_relation", withinvars=c("sceneType"),
                       idvar="clType", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl2 <- ggplot(data = df_item2, aes(x=sceneType, y=prop_match_in_relation)) +
  #geom_hline(yintercept=0.5, alpha = 0.75, size=1, linetype = 'dashed') + 
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_relation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=clType),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=clType), 
             position = jitter) +
  # now the average data
  geom_line(data = dc2, size=1, aes(x=sceneType, y=prop_match_in_relation, group=1)) +
  geom_errorbar(data = dc2, size=1, aes(group = 1, x=sceneType, y=prop_match_in_relation, ymin=prop_match_in_relation-ci, ymax=prop_match_in_relation+ci), width=.03) +
  geom_point(data = dc2, aes(x=sceneType, y=prop_match_in_relation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "general" classifier matches by classifier type') +
  ylim(-0.02,1.02)

pl2








### subject plots 

dc3 <- summarySEwithin(df_subj2, measurevar="prop_match_in_isolation", withinvars=c("sceneType"),
                       idvar="subject_id", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl3 <- ggplot(data = df_subj2, aes(x=sceneType, y=prop_match_in_isolation)) +
  #geom_hline(yintercept=0.5, alpha = 0.75, size=1, linetype = 'dashed') + 
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_isolation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=subject_id),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=subject_id), 
             position = jitter) +
  # now the average data
  geom_line(data = dc3, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc3, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc3, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "specific" classifier matches by subject') +
  ylim(0,1)
pl3




#no violin
pl33 <- ggplot(data = df_subj2, aes(x=sceneType, y=prop_match_in_isolation)) +
  # now the average data
  geom_line(data = dc3, size=1, aes(x=sceneType, y=prop_match_in_isolation, group=1)) +
  geom_errorbar(data = dc3, size=1, aes(group = 1, x=sceneType, y=prop_match_in_isolation, ymin=prop_match_in_isolation-ci, ymax=prop_match_in_isolation+ci), width=.03) +
  geom_point(data = dc3, aes(x=sceneType, y=prop_match_in_isolation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "specific" classifier matches by subject') +
  ylim(0.35,0.55)
pl33




##### 
##subj_ relation 

dc4 <- summarySEwithin(df_subj2, measurevar="prop_match_in_relation", withinvars=c("sceneType"),
                       idvar="subject_id", na.rm=FALSE, conf.interval=.95)

jitter <- position_jitter(width = 0.07, height = 0.01, seed=12345)

pl4 <- ggplot(data = df_subj2, aes(x=sceneType, y=prop_match_in_relation)) +
  # geom_hline(yintercept=0.5, alpha = 0.75, size=1, linetype = 'dashed') + 
  # individual data
  geom_violin(aes(x=sceneType, y=prop_match_in_relation, alpha=0.3, fill=sceneType, width=0.4)) +
  scale_fill_manual(values=c("#2F5597", "#C55A11")) +
  geom_line(color="gray", aes(alpha=0.4, group=subject_id),
            position = jitter) +
  geom_point(color="gray", fill="gray", shape=21, size=2, aes(alpha=0.4, group=subject_id), 
             position = jitter) +
  # now the average data
  geom_line(data = dc4, size=1, aes(x=sceneType, y=prop_match_in_relation, group=1)) +
  geom_errorbar(data = dc4, size=1, aes(group = 1, x=sceneType, y=prop_match_in_relation, ymin=prop_match_in_relation-ci, ymax=prop_match_in_relation+ci), width=.03) +
  geom_point(data = dc4, aes(x=sceneType, y=prop_match_in_relation, group=1), shape=21, size=3, fill=c("#2F5597", "#C55A11")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(size = 24),
        axis.title.x = element_blank(), 
        axis.text = element_text(size = 28),
        axis.text.x = element_text(face="bold", color=c("#2F5597", "#C55A11")),
        strip.text = element_text(size = 28),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "transparent",colour = NA), 
        text = element_text(family="Gill Sans")) +
  scale_x_discrete(labels = c('in_isolation', 'in-relation')) +
  labs(y = 'mean "general" classifier matches by subject') +
  ylim(0,1)
pl4













############################
############################
## Mixed-effects logistic regression
############################
############################

## Prep the data
curdata <- df3

# factorize certain variables
curdata$subject_id <- factor(curdata$subject_id)
curdata$clType <- factor(curdata$clType)
curdata$clExemplar <- factor(curdata$clExemplar)

# make sure relevant variables are factorized and contrast/sum-coded, 
# i.e., -1 and 1 (rather than treatment coded, i.e., 0 and 1)
# important for testing both main effects and interactions between variables
curdata$sceneType <- factor(curdata$sceneType)
contrasts(curdata$sceneType) <- contr.sum(length(levels(curdata$sceneType)))

# create a centered blockNum variable
curdata$blockNum_c <- scale(curdata$blockNumber, center = T, scale = F)

# the response variable too
curdata$match_in_isolation <- factor(curdata$match_in_isolation)
curdata$match_in_relation <- factor(curdata$match_in_relation)


# other vars to possibly model:
# clExemplar (this could be nested within clType, like: `(1|clType/clExemplar)`, 
# which expands to `(1|clType) + (1|clExemplar:clType)`equivalently, 
#could make a `iT_clExemplar` variable (concatenate clType and number) so that lme4 knows the repeated numerals for clExemplar across clType are not the same
# and then model it like: `(1|clType) + (1|iT_clExemplar)` (i.e., if we make that new variable, specifying the nesting explicitly is redundant, I believe: https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified)
# I believe it models each clExemplar as a unique level, all its own (so 2*8 = 16 clExemplars)
# blockNum_c (i.e., block number centered)
# containerType (bowl or box); varies across clType

# vars probably not necessary to model:
# (which clExemplar was first --> although I don't know how to model this really!)
# (imageSide --> only matters for in-isolation)
# (relationTypeOrder --> for each clType, which they got first; although how would we do this? I guess the idea would be that only for *specific* clTypes, the order could matter)
# but I think really, blockNum would capture this

curdata$sceneType_num <- as.character(curdata$sceneType)
curdata$sceneType_num[curdata$sceneType_num == 'in-isolation'] <- 1
curdata$sceneType_num[curdata$sceneType_num == 'in-relation'] <- -1
curdata$sceneType_num <- as.numeric(curdata$sceneType_num)


####
### isolation 
## Run the models
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 + sceneType | subject_id) +
                (1 + sceneType | clType),
              data = curdata,
              family = binomial())

summary(lm.0)

# make the more complex models (using the "update" syntax)
lm.1 <- update(lm.0, . ~ . + sceneType)
lm.2 <- update(lm.0, . ~ . + sceneType * blockNum_c)

# compare models to see which is the best fit
anova(lm.0, lm.1, lm.2)

# summarize the best-fitting model
summary(lm.1)

# coefficients
coefs <- summary(lm.1)$coefficients[3,]
coefs

#OR 
#beta
coefs[1]
#95% CI
coefs[1] + c(-1,1)*1.96*coefs[2]
#z score
coefs[3]
#p value
coefs[4]

#odds-ratio beta
exp(coefs[1])
#odds-ratio 95% CI 
exp(coefs[1] + c(-1,1)*1.96*coefs[2])




### relation 
## Run the models
lm.0 <- glmer(match_in_relation ~
                1 + blockNum_c + 
                (1 + sceneType | subject_id) +
                (1 + sceneType | clType),
              data = curdata,
              family = binomial())

summary(lm.0)

# make the more complex models (using the "update" syntax)
lm.1 <- update(lm.0, . ~ . + sceneType)
lm.2 <- update(lm.0, . ~ . + sceneType * blockNum_c)


# compare models to see which is the best fit
anova(lm.0, lm.1, lm.2)

# summarize the best-fitting model
summary(lm.1)

# coefficients
coefs <- summary(lm.1)$coefficients[3,]
coefs

#OR 
#beta
coefs[1]
#95% CI
coefs[1] + c(-1,1)*1.96*coefs[2]
#z score
coefs[3]
#p value
coefs[4]

#odds-ratio beta
exp(coefs[1])
#odds-ratio 95% CI 
exp(coefs[1] + c(-1,1)*1.96*coefs[2])




# in a footnote you can run the intercept-only model
# it shows similar results (with no boundary singular fit warning)
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 | subject_id) +
                (1 | clType/clExemplar),
              data = curdata,
              family = binomial())
summary(lm.0)



###################

# define null/base model (one that converges)
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 + sceneType | subject_id) +
                (1 + sceneType | clType/clExemplar),
              data = curdata,
              family = binomial())

summary(lm.0)


# example of what to do if the base model does not converge: 
# first taking out clExemplar stuff
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 + sceneType | subject_id) +
                (1 + sceneType | clType),
              data = curdata,
              family = binomial())

summary(lm.0)


# take out correlation between random slopes and intercepts
# first making the random slope variable continuous (because that's just what one has to do, apparently)
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 + sceneType_num || subject_id) +
                (1 + sceneType_num || clType),
              data = curdata,
              family = binomial())

summary(lm.0)

### this 
#main model 
#intercept only 
lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 + sceneType | subject_id) +
                (1 + sceneType | clType),
              data = curdata,
              family = binomial())

summary(lm.0)




# make the more complex models (using the "update" syntax)
lm.1 <- update(lm.0, . ~ . + sceneType)
lm.2 <- update(lm.0, . ~ . + sceneType * blockNum_c)

# compare models to see which is the best fit
anova(lm.0, lm.1, lm.2)

# summarize the best-fitting model
summary(lm.1)




# t.tests
subj.data <- ddply(curdata, .(subject_id, sceneType), summarise,
                   meanProp_match_in_isolation = mean(as.numeric(as.character(match_in_isolation))),
                   n = length(as.numeric(as.character(match_in_isolation))),
                   elog_match_in_isolation = log( (meanProp_match_in_isolation + 0.5/n) / (1 - meanProp_match_in_isolation + 0.5/n) ))
item.data <- ddply(curdata, .(clType, sceneType), summarise,
                   meanProp_match_in_isolation = mean(as.numeric(as.character(match_in_isolation))),
                   n = length(as.numeric(as.character(match_in_isolation))),
                   elog_match_in_isolation = log( (meanProp_match_in_isolation + 0.5/n) / (1 - meanProp_match_in_isolation + 0.5/n) ))

t.test(meanProp_match_in_isolation ~ sceneType, paired = T, data = subj.data)
t.test(elog_match_in_isolation ~ sceneType, paired = T, data = subj.data)

t.test(meanProp_match_in_isolation ~ sceneType, paired = T, data = item.data)
t.test(elog_match_in_isolation ~ sceneType, paired = T, data = item.data)





#### power

nExt <- 160
nSubjs <- c(40, 80, 120, 160)
nSims <- 250

lm.0 <- glmer(match_in_isolation ~
                1 + blockNum_c + 
                (1 | subject_id) +
                (1 | clType),
              data = curdata,
              family = binomial())
lm.1 <- update(lm.0, . ~ . + sceneType)

model_final <- lm.1

# power curve, comparing to a simpler model without the key interaction
p_curve2 <- powerCurve(model_final, nsim=nSims,
                       test = compare(
                         match_in_isolation ~
                           1 + blockNum_c + 
                           (1 | subject_id) +
                           (1 | clType)
                       ),
                       along="subject_id",
                       breaks = nSubjs)
p_curve2
plot(p_curve2)











