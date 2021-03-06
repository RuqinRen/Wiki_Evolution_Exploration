library(dplyr)
library(igraph)
library(Matrix)
library(tidyr)
library(readxl)
library(coin)
library(influenceR)
library(egor)
#####################
## article content data
######################


article_data <- read.csv("/home/rstudio/WikiEvolution/article_data.csv",header = TRUE)

length(unique(article_data$articleId))
#394
colnames(article_data)
#[1] "articleId"           "articleName"         "Y.W"                
#[4] "time"                "revisionID"          "contentLength"      
#[7] "numOfreferences"     "numOfPageLinks"      "nunOfCiteTemplates" 
#[10] "numOfCategories"     "imagesByLength"      "hasBox"             
#[13] "numOfLv2Heading"     "flesch_reading_ease" "coleman_liau_index" 
#[16] "difficult_words"     "quality"             "pageView"  


#create the following columns:
#week / DV_page_views
#aggregated population total in a week: sum(each article's page views) = weekly total pageview

#check for missing values
summary(article_data$pageView)
# 174 na values
article_data <- article_data %>% filter(!is.na(pageView))
# because two articles were created only after the observation period start. Hence missing 174 rows of data

popu_weekly_pageview <- article_data %>% 
 group_by(Y.W) %>% 
 summarise(weekly_pageview = sum(pageView, na.rm = TRUE))

#categorize numerical features into binary features by breaking from mean value
# 10 features
#getting 10 features' mean values
# article_data %>% select(c(6:16)) %>% summary()
#Mean   : 4893       Mean   :  7.727       Mean   : 40.91         Mean   :12.19
# "contentLength" "numOfreferences"        "numOfPageLinks"      "nunOfCiteTemplates" 
# Mean   : 4.286         Mean   :0.00042         binary     Mean   : 7.141       Mean   : 47.31  
# "numOfCategories"     "imagesByLength"      "hasBox"      "numOfLv2Heading"    "flesch_reading_ease" 
# Mean   :15.09           Mean   : 234.7  
#"coleman_liau_index"   "difficult_words" 
  
bi_population_trait <- article_data %>% 
  mutate(
    bi_content_length=cut(contentLength, breaks=c(-Inf,4893,Inf), labels=c("low","high")),
    bi_num_references=cut(numOfreferences, breaks=c(-Inf,7.727,Inf), labels=c("low","high")),
    bi_num_page_links=cut(numOfPageLinks, breaks=c(-Inf,40.91,Inf), labels=c("low","high")),
    bi_num_cite_temp=cut(nunOfCiteTemplates,breaks=c(-Inf,12.19,Inf), labels=c("low","high")),
    bi_num_categories=cut(numOfCategories, breaks=c(-Inf,4.286,Inf), labels=c("low","high")),
    bi_image_by_length=cut(imagesByLength, breaks=c(-Inf,0.00042,Inf), labels=c("low","high")),
    bi_num_lv2_heading=cut(numOfLv2Heading, breaks=c(-Inf,7.141,Inf), labels=c("low","high")),
    bi_flesch_reading_score=cut(flesch_reading_ease, breaks=c(-Inf,47.31,Inf), labels=c("low","high")),
    bi_coleman_liau_index=cut(coleman_liau_index, breaks=c(-Inf,15.09,Inf), labels=c("low","high")),
    bi_difficult_words=cut(difficult_words, breaks=c(-Inf,234.7,Inf), labels=c("low","high")),
  )

write.csv(bi_population_trait, "/home/rstudio/WikiEvolution/bi_population_trait.csv")

############################
# network data
###########################

coeditor_network <- read.csv("/home/rstudio/WikiEvolution/network_data.csv", header = TRUE)
colnames(coeditor_network)
#[1] "articleId"   "articleName" "revid"       "dateOfWeek"  "time"        "userid"     
#[7] "user"        "article"   

#####################
#  clean up 
######################

# check that all articleId belongs to this 394 nodes pool
#conditional removal of these nodes, based on condition
#condition <- coeditor_network$articleId %in% as.list(levels(coeditor_network$articleId))
#table(condition)
#there is no additional nodes that do not belong to the pool
#skip this step: hypernet_removed <- hypernet_raw[condition2, ]

# assign user IP as userid
coeditor_network$uid <- NA

# if userid ==0, meanign it is an IP address, assign it an ID from username
# ifelse, keep the original ID
# For the columns that are factors, you can only assign values that are factor levels. 
# If you wanted to assign a value that wasn't currently a factor level,
# you would need to create the additional level first:
levels(coeditor_network$uid) <-c(levels(coeditor_network$user))
coeditor_network$uid <- ifelse(coeditor_network$userid ==0,as.character(coeditor_network$user),as.character(coeditor_network$userid))
class(coeditor_network$uid)
coeditor_network <- coeditor_network[,-c(6:7)]
colnames(coeditor_network)
#[1] "articleId"   "articleName" "revid"       "dateOfWeek"  "time"       
#[6] "article"     "uid"           
#remove the two old user id columns to only keep the new uid column

#####################
## weekly network construction
######################

# desired output columns: weekofYear, articleId, network metric1, network metric2 ...

#reassignment of dateOfWeek value: if before 2017w00, assign "before 2016w99"
coeditor_network$year <- NA
coeditor_network$year <- substr(coeditor_network$dateOfWeek, start = 1, stop = 4)
levels(coeditor_network$dateOfWeek) <- c(levels(coeditor_network$dateOfWeek), '2016w99')
coeditor_network$dateOfWeek[coeditor_network$year<2017] <- '2016w99'
#num of revisions / dateOfWeek distribution
table(coeditor_network$dateOfWeek)

#check the distribution of article-revision counts during this period of time
#revision_count <- coeditor_network %>% 
#  group_by(articleId) %>%
#  summarise(num_revision = n_distinct(revid))

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.00   10.00   15.00   23.87   25.00  438.00 
# 25% of articles have 10 or less revisions
# a<-revision_count %>% 
#  count(num_revision) %>% 
#  mutate(Cum = cumsum(n)/sum(n))
#10% of articles have 6 or less revisions

#split by dateOfWeek
network_df_split <- split(coeditor_network[,c(2,7)], coeditor_network$dateOfWeek)

#convert each weekly df into a sparse adjacency matrix (first column will be kept)
getFirstColumnOneModeNet<- function(df) {
  A <- spMatrix(nrow=length(unique(df[,1])),
                ncol=length(unique(df[,2])),
                i = as.numeric(factor(df[,1])),
                j = as.numeric(factor(df[,2])),
                x = rep(1, length(as.numeric(df[,1]))) )
  row.names(A) <- levels(factor(df[,1]))
  colnames(A) <- levels(factor(df[,2]))
  Arow <- A %*% t(A)
  return(Arow)
}

adjacency_mat<- lapply(network_df_split, function(x) getFirstColumnOneModeNet(x) )
remove(network_df_split)

makemetrics <- function(gr) {
  data.frame(Degree_undir = degree(gr, mode = "all", normalized = TRUE), 
             Closeness_undir= closeness(gr, mode="all"),
             Betweenness_undir = igraph::betweenness(gr,directed = F, normalized = TRUE),
             eigen_undir = eigen_centrality(gr, directed = F, scale = TRUE)$vector,
             page_rank = page_rank(gr,directed = FALSE, damping = 0.85)$vector,
             hub = hub_score(gr, scale = TRUE)$vector,
             authority = authority_score(gr, scale = TRUE)$vector,
             constraint = constraint(gr),
             transitivity = transitivity(gr, type="local"),
             eff_size = ens(gr),
             density = unlist(lapply(make_ego_graph(gr), function(x) edge_density(x, loops= FALSE) ))
  )
}

#from sparse adjacency network to a graph object
net_gr<- lapply(adjacency_mat, function(x) graph_from_adjacency_matrix(x, mode = "max",weighted = TRUE, diag = F))

#calculate network metrics
net_gr <-lapply(net_gr, function(x) makemetrics(x))

#build df
net_metrics <- lapply(net_gr, function(x) as.data.frame(x))
net_metrics <- lapply(net_metrics, function(x) add_rownames(x, var = "ArticleName"))
net_metrics <- do.call(rbind,net_metrics)
net_metrics_time <- add_rownames(net_metrics, var="Timestamp")
#remove na values
net_metrics_time[is.na(net_metrics_time)] <- 0

#re-organize time stamp
net_time <- separate(net_metrics_time,col="Timestamp",into = c("Timestamp1","Timestamp2" ), convert = FALSE)
# separate and unite, a useful combination  %>%unite_(., "Timestamp", c("Timestamp1","Timestamp2"))
#clean up space
remove(net_metrics_time, net_metrics, net_gr, adjacency_mat, network_df_split)

table(is.na(net_time))

#dichotomize by mean value
net_bi_time <- net_time %>%
  group_by(Timestamp1) %>%
  mutate(bi_Degree_undir = Degree_undir > mean(net_time$Degree_undir), 
         bi_Closeness_undir = Closeness_undir > mean(net_time$Closeness_undir),
         bi_Betweenness_undir = Betweenness_undir > mean(net_time$Betweenness_undir),
         bi_eigen_undir = eigen_undir > mean(net_time$eigen_undir),
         bi_page_rank = page_rank > mean(net_time$page_rank),
         bi_hub = hub > mean(net_time$hub),
         bi_authority = authority > mean(net_time$authority),
         bi_constraint = constraint > mean(net_time$constraint),
         bi_transitivity = transitivity > mean(net_time$transitivity),
         bi_eff_size = eff_size > mean(net_time$eff_size),
         bi_density = density > mean(net_time$density)
  )
names(net_bi_time)
remove(net_time)
#2017w00, 2017W00, there are inconsistent cases of W, make them all upper case
net_bi_time$Timestamp1 <- tolower(net_bi_time$Timestamp1)
write.csv(net_bi_time,"/home/rstudio/WikiEvolution/weekly_net_bi.csv")

#####################
## binary network + article content traits
######################
net_bi_time <- read.csv("/home/rstudio/WikiEvolution/weekly_net_bi.csv", header = TRUE)
bi_population_trait <- read.csv("/home/rstudio/WikiEvolution/bi_population_trait.csv",header = TRUE)
net_trait_bi_time <- left_join( bi_population_trait, net_bi_time, 
                              by=c( "Y.W"= "Timestamp1", "articleName"="ArticleName"))
net_trait_bi_time<- unique(net_trait_bi_time)
names(net_trait_bi_time)
net_trait_bi_time <- subset(net_trait_bi_time, select=-c(X.x, X.y,time,revisionID,Timestamp2))
#net_trait_bi_time has 44 columns: 
# 11 network metrics *2
# 11(has info box is binary already) content metrics * 2 -1 
# two outcome variables: quality and pageview

#fill 27:48 columns because network measures are not repeated on weekly basis. They are only captured when values change
# use dplyr:fill function
net_trait_bi_time <- net_trait_bi_time %>% fill(c(27:48), .direction='updown')
table(is.na(net_trait_bi_time)) #check again
write.csv(net_trait_bi_time, '/home/rstudio/WikiEvolution/weekly_net_trait_bi.csv')

#####################
## traits into fitness
######################

net_trait_bi_time <- read.csv("/home/rstudio/WikiEvolution/weekly_net_trait_bi.csv", header = TRUE)

#trait fitness summary with the following columns
#Generation_time /Trait / Trait_fitness_page_views
#e.g. time1/traitA/ low_partition/ sum /fitness
#e.g. time1/traitA/ high_partition/ sum /fitness

#building empty lists
list_of_df_lowpartition <- as.list(rep("", 22)) 
list_of_df_highpartition <- as.list(rep("", 22)) 

#list of 11 trait names
list_of_traits <- c("bi_content_length"  ,     "hasBox" ,
                    "bi_num_references"  ,     "bi_num_page_links"  ,     "bi_num_cite_temp"  ,     
                    "bi_num_categories"  ,     "bi_image_by_length" ,     "bi_num_lv2_heading"  ,      
                    "bi_flesch_reading_score", "bi_coleman_liau_index" ,  "bi_difficult_words",
                    "bi_Degree_undir"   ,      "bi_Closeness_undir" ,     "bi_Betweenness_undir" ,
                    "bi_eigen_undir"    ,      "bi_page_rank"  ,          "bi_hub" ,
                    "bi_authority" ,           "bi_constraint"    ,       "bi_transitivity"  ,  
                    "bi_eff_size"    ,         "bi_density" )


for(i in 1:22) { 
  list_of_df_highpartition[[i]]  <-  net_trait_bi_time %>% 
    group_by(Y.W, get(list_of_traits[[i]])) %>% #there are implicit NA levels due to missing data, each trait generates three rows: low/high/NA
    summarise( page_view_sum = sum(pageView,na.rm = TRUE)) %>%
    filter(page_view_sum > 0) %>% #NA rows has 0 value, remove them
    filter(row_number() %% 2 == 1)  %>% #high before low, so %%2 ==1 means high, odd number
    ungroup()
  
  list_of_df_lowpartition[[i]]  <-  net_trait_bi_time %>% 
    group_by(Y.W, get(list_of_traits[[i]])) %>% 
    summarise( page_view_sum = sum(pageView, na.rm = TRUE)) %>%
    filter(page_view_sum > 0) %>% 
    filter(row_number() %% 2 == 0)  %>%
    ungroup()
  
  names(list_of_df_lowpartition[[i]])  <-  c("Y.W",
                                             list_of_traits[[i]],
                                             paste("page_views",list_of_traits[[i]],"low",sep="_")
  )
  
  names(list_of_df_highpartition[[i]])  <-  c("Y.W",
                                              list_of_traits[[i]],
                                              paste("page_views",list_of_traits[[i]],"high",sep="_")
  )
} 

#the output are two lists of traits at low values pageView sum/ high values pageView sum
#combine them into dfs

for(i in 1:22) { 
  list_of_df_lowpartition[[i]] <- 
    list_of_df_lowpartition[[i]] %>% 
     select(ends_with('low'))
  
  list_of_df_highpartition[[i]] <- 
    list_of_df_highpartition[[i]] %>% 
    select( ends_with("high") )
} 

trait_partition_fitness_low <- do.call(cbind, list_of_df_lowpartition)
trait_partition_fitness_high <- do.call(cbind, list_of_df_highpartition)
trait_partition_fitness <- cbind(trait_partition_fitness_low, trait_partition_fitness_high,popu_weekly_pageview)
remove(trait_partition_fitness_low)
remove(trait_partition_fitness_high)

totalYW = as.integer(nrow(trait_partition_fitness))
trait_partition_only_fitness <- as.data.frame(matrix(NA, nrow = totalYW, ncol = 44))
trait_partition_only_fitness <- cbind(trait_partition_only_fitness, popu_weekly_pageview)

seq = c(1:44, 46) #skip Y.W column, which does not have fitness 
for(i in seq){
  raw = trait_partition_fitness[[i]]
  ahead = lead(raw)
  w_fitness = ahead/raw
  
  trait_partition_only_fitness[,i] = w_fitness
}

names(trait_partition_only_fitness) <- names(trait_partition_fitness)
# output is a df with columns:fitness calculated from each partition

#combined into a df of 165 time periods, 44 = 22*2 low/high page view sum
#first week is wrong, remove the first week 2017w00
trait_partition_only_fitness <- trait_partition_only_fitness[-c(1,164,163),]
write.csv(trait_partition_only_fitness, "/home/rstudio/WikiEvolution/trait_population_fitness.csv" )

#####################
## fitness to natural selection (Price Equation)
######################

trait_popu_fitness <- read.csv("/home/rstudio/WikiEvolution/trait_population_fitness.csv", header = TRUE )
# when read in this file, R auto-creates a column called X
trait_popu_fitness <- trait_popu_fitness[,-1]
#store nc results as a vector
list_nc_page_views <- as.list(rep("",22))  #22 traits of var(high,low)/popu_mean

#calculation of nc
for(i in 1:22){
  temp <- as.data.frame(trait_popu_fitness[,c(i,22+i,46)]) #i=trait low, i+22 =trait high, 46=fitness
  vars<-apply(temp[1:2],1,var)
  nc <- vars/temp[["weekly_pageview"]]
  list_nc_page_views[[i]] <- nc
}

#re-organize into df
nc_df <- do.call(cbind, list_nc_page_views)
nc_df <- as.data.frame(nc_df)
names(nc_df) <-  paste(list_of_traits ,"page_views",sep="_")
nc_df <- cbind(nc_df, trait_popu_fitness$Y.W) #add timestamp

#transform raw variance into equidistant percentile rank

all_nc_value <- as.vector(as.matrix(nc_df[,c(1:22)])) #note: pay attention to column names
nc_rank <- rank(all_nc_value)/length(all_nc_value) #rank and calculate percentile
nc_rank <- as.data.frame(split(nc_rank, sample(1:22))) #split a long vector back to equal size chunks
names(nc_rank) <- names(nc_df[,c(1:22)])
nc_rank <- cbind(nc_rank, nc_df[,23])

nc_rank <- nc_rank %>%
  gather(key = type_of_DV,
         value = ns_rank,
         1:22) #only gather 1:20, leave out the time column. Time will be duplicated 

nc_rank$DV_two_group <- rep(c("trait_based","network_based"),times = c(162*11, 162*11))
colnames(nc_rank)[1] <- "weekly"

#write
write.csv(nc_rank, "/home/rstudio/WikiEvolution/nc_rank.csv")


#############################
## comparison of two groups: trait vs network
#############################

nc_rank <- read.csv("/home/rstudio/WikiEvolution/nc_rank.csv", header = TRUE)
nc_rank$weekly <- as.factor(nc_rank$weekly)
nc_rank$DV_two_group <- as.factor(nc_rank$DV_two_group)
nc_rank$type_of_DV <- as.factor(nc_rank$type_of_DV)

#consider weekly factor
a <-oneway_test(ns_rank ~ DV_two_group |weekly , data = nc_rank, alternative="greater",distribution = approximate(nresample = 10000))
a
pvalue(a)

#DV_two_group     mean
#<fct>           <dbl>
#1 network_based 0.506
#2 trait_based   0.495

#############################
## comparison: among network configs
#############################

network_nc_rank <- nc_rank %>% filter(DV_two_group == 'network_based') %>% select(-DV_two_group)
network_nc_rank$network_three_group <- NA
levels(network_nc_rank$network_three_group) <- c("connectivity","embeddedness","redundancy")
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_Betweenness_undir_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_Closeness_undir_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_eigen_undir_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_Degree_undir_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_page_rank_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_hub_page_views"] <- 'embeddedness'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_authority_page_views"] <- 'embeddedness'

network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_eff_size_page_views"] <- 'redundancy'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_constraint_page_views"] <- 'redundancy'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_density_page_views"] <- 'connectivity'
network_nc_rank$network_three_group[network_nc_rank$type_of_DV == "bi_transitivity_page_views"] <- 'connectivity'


network_nc_rank %>% group_by(network_three_group) %>% summarise(mean = mean(ns_rank))
network_nc_rank %>% group_by(type_of_DV) %>% summarise(mean = mean(ns_rank))

# # A tibble: 11 x 2
# type_of_DV                       mean
# <chr>                           <dbl>
# 1 bi_authority_page_views         0.507
# 2 bi_Betweenness_undir_page_views 0.510
# 3 bi_Closeness_undir_page_views   0.501
# 4 bi_constraint_page_views        0.495
# 5 bi_Degree_undir_page_views      0.477
# 6 bi_density_page_views           0.509
# 7 bi_eff_size_page_views          0.510
# 8 bi_eigen_undir_page_views       0.517
# 9 bi_hub_page_views               0.493
# 10 bi_page_rank_page_views         0.488
# 11 bi_transitivity_page_views      0.493


network_nc_rank$weekly <- as.factor(network_nc_rank$weekly)
network_nc_rank$network_three_group <- as.factor(network_nc_rank$network_three_group)
network_nc_rank$type_of_DV <- as.factor(network_nc_rank$type_of_DV)

write.csv(network_nc_rank, "/home/rstudio/WikiEvolution/network_nc_rank.csv")

# compare group vs group
network_nc_small <- network_nc_rank %>% filter(network_three_group != 'embeddedness')
b <-oneway_test(ns_rank ~ network_three_group |weekly , data = network_nc_small, alternative="greater",distribution = approximate(nresample = 10000))
b

network_nc_small <- network_nc_rank %>% filter(network_three_group != 'connectivity')
b <-oneway_test(ns_rank ~ network_three_group |weekly , data = network_nc_small, alternative="two.sided",distribution = approximate(nresample = 10000))
b

network_nc_small <- network_nc_rank %>% filter(network_three_group != 'redundancy')
b <-oneway_test(ns_rank ~ network_three_group |weekly , data = network_nc_small, alternative="two.sided",distribution = approximate(nresample = 10000))
b

# compare each pair of network configuration
network_list <- c("bi_Degree_undir_page_views"    ,      "bi_Closeness_undir_page_views"   ,  "bi_constraint_page_views", 
                 "bi_Betweenness_undir_page_views",     "bi_eigen_undir_page_views" ,        "bi_page_rank_page_views",  
                 "bi_transitivity_page_views" ,        "bi_eff_size_page_views",       "bi_density_page_views",
                 "bi_authority_page_views",   "bi_hub_page_views")
network_pairs <- as.data.frame(t(combn(network_list, m=2))) # 9 elements choose 2, get combinations. Transpose into tidy format
names(network_pairs) <- c("firstvalue",'secondvalue')
network_pairs$perm.test.greater <- NA
network_pairs$perm.test.less <- NA
network_pairs$perm.test.twoside <- NA

for(i in 1:55){
  getA <- network_pairs[i,1]
  getB <- network_pairs[i,2]
  temp <- network_nc_rank %>% filter(type_of_DV == as.character(get('getA')) | type_of_DV == as.character(get('getB')) )
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="greater", distribution = approximate(nresample = 10000))
  network_pairs[i,3] <- pvalue(model)[1]
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="two.sided", distribution = approximate(nresample = 10000))
  network_pairs[i,5] <- pvalue(model)[1]
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="less", distribution = approximate(nresample = 10000))
  network_pairs[i,4] <- pvalue(model)[1]
}

# calculated all pairwise comparison p value

network_pairs %>% filter(perm.test.greater < 0.2) 

# compare each pair of traits + network pairs
network_list <- c("bi_Degree_undir_page_views"    ,      "bi_Closeness_undir_page_views"   ,  "bi_constraint_page_views", 
                  "bi_Betweenness_undir_page_views",     "bi_eigen_undir_page_views" ,        "bi_page_rank_page_views",  
                  "bi_transitivity_page_views" ,        "bi_eff_size_page_views",       "bi_density_page_views",
                  "bi_authority_page_views",           "bi_hub_page_views")
trait_list <- c("bi_coleman_liau_index_page_views",   "bi_content_length_page_views",    "bi_difficult_words_page_views",
                "bi_flesch_reading_score_page_views", "bi_image_by_length_page_views" ,  "bi_num_categories_page_views",
                "bi_num_cite_temp_page_views",        "bi_num_lv2_heading_page_views",   "bi_num_page_links_page_views",
                "bi_num_references_page_views",       "hasBox_page_views" )
trait_network_pairs <- crossing(trait_list, network_list)
trait_network_pairs$perm.test.greater <- NA
trait_network_pairs$perm.test.less <- NA
trait_network_pairs$perm.test.twoside <- NA

for(i in 1:121){
  getA <- trait_network_pairs[i,1]
  getB <- trait_network_pairs[i,2]
  temp <- nc_rank %>% filter(type_of_DV == as.character(get('getA')) | type_of_DV == as.character(get('getB')) )
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="greater", distribution = approximate(nresample = 10000))
  trait_network_pairs[i,3] <- pvalue(model)[1]
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="two.sided", distribution = approximate(nresample = 10000))
  trait_network_pairs[i,5] <- pvalue(model)[1]
  
  model <-oneway_test(ns_rank ~ type_of_DV | weekly , data = temp, alternative="less", distribution = approximate(nresample = 10000))
  trait_network_pairs[i,4] <- pvalue(model)[1]
}

trait_network_pairs %>% filter(perm.test.greater < 0.2) 

#############################
##input: nc_rank, network_nc_rank
##final output
#############################
write.csv(trait_network_pairs, "/home/rstudio/WikiEvolution/trait_network_pairs_pvalue.csv")
write.csv(network_pairs, "/home/rstudio/WikiEvolution/onlynetwork_pairs_pvalue.csv")
