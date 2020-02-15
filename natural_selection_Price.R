library(dplyr)
library(lavaan)
library(igraph)
library(tidyr)
library(ggplot2)

maindf <- read.csv(file="WikiEvolutionMain.csv", header=TRUE, sep=",")
editornet <- read.csv(file = "WikiEditorNet.csv", header=TRUE, sep=",")
articlenet <- read.csv(file = "WikiHyperNet.csv", header=TRUE, sep=",")

#review column names and make them consistent
head(editornet)
head(articlenet)

#population average fitness summary with the following columns
#Generation_time / DV_search_traffic / DV_page_views

popu_generation_df <- maindf %>% 
  group_by(Time) %>% 
  summarise(DV_search_traffic =sum(search_traffic),DV_page_views = sum(page_views))
# I also manipulated data in excel, so process not shown here. Use "popu_generation_df"
popu_generation_df <- read.csv("popu_summary_df.csv", header = T, sep = ",")

trait_df <- maindf[-c(1:3,25,26)]
trait_summary_vector <- summary(trait_df)
trait_summary_vector[3,-1]


#categorize 
bi_population_trait <- maindf %>% 
  mutate(
         bi_content_length=cut(content_length, breaks=c(-Inf,5617,Inf), labels=c("low","high")),
         bi_num_references=cut(num_references, breaks=c(-Inf,9,Inf), labels=c("low","high")),
         bi_num_page_links=cut(num_page_links, breaks=c(-Inf,33,Inf), labels=c("low","high")),
         bi_num_cite_temp=cut(num_cite_temp,   breaks=c(-Inf,2,Inf), labels=c("low","high")),
         bi_num_non_cite_temp=cut(num_non_cite_temp, breaks=c(-Inf,6,Inf), labels=c("low","high")),
         bi_num_categories=cut(num_categories, breaks=c(-Inf,4,Inf), labels=c("low","high")),
         bi_image_by_length=cut(image_by_length, breaks=c(-Inf,0.0003,Inf), labels=c("low","high")),
         bi_info_noise_score=cut(info_noise_score, breaks=c(-Inf,0.63,Inf), labels=c("low","high")),
         bi_num_lv2_heading=cut(num_lv2_heading, breaks=c(-Inf,5,Inf), labels=c("low","high")),
         bi_num_lv3_heading=cut(num_lv3_heading, breaks=c(-Inf,1,Inf), labels=c("low","high")),
         bi_flesch_reading_score=cut(flesch_reading_score, breaks=c(-Inf,42.85,Inf), labels=c("low","high")),
         bi_flesch_kincaid_grade=cut(flesch_kincaid_grade, breaks=c(-Inf,14.3,Inf), labels=c("low","high")),
         bi_smog_index=cut(smog_index, breaks=c(-Inf,14.8,Inf), labels=c("low","high")),
         bi_coleman_liau_index=cut(coleman_liau_index, breaks=c(-Inf,13.36,Inf), labels=c("low","high")),
         bi_ARI_score=cut(ARI_score, breaks=c(-Inf,18.6,Inf), labels=c("low","high")),
         bi_difficult_words=cut(difficult_words, breaks=c(-Inf,109,Inf), labels=c("low","high")),
         bi_dale_chall_score=cut(dale_chall_score, breaks=c(-Inf,8.25,Inf), labels=c("low","high")),
         bi_linsear_write_score=cut(linsear_write_score, breaks=c(-Inf,11.757,Inf), labels=c("low","high")),
         bi_gunning_fog_score=cut(gunning_fog_score, breaks=c(-Inf,15.39,Inf), labels=c("low","high"))
         )

#trait fitness summary with the following columns
#Generation_time /Trait / Trait_fitness_search_traffic / Trait_fitness_page_views
#time1/traitA/ low_partition/ sum /sum/fitness/fitness
#time1/traitA/ high_partition/ sum/sum/fitness/fitness

list_of_df_lowpartition <- as.list(rep("", 20)) 
list_of_df_highpartition <- as.list(rep("", 20)) 

list_of_traits20 <- c("bi_content_length"  ,   "has_infobox" ,
                    "bi_num_references"  ,     "bi_num_page_links"  ,     "bi_num_cite_temp"  ,     
                     "bi_num_non_cite_temp" ,   "bi_num_categories"  ,     "bi_image_by_length" ,    
                     "bi_info_noise_score" ,    "bi_num_lv2_heading"  ,    "bi_num_lv3_heading" ,    
                     "bi_flesch_reading_score", "bi_flesch_kincaid_grade", "bi_smog_index",          
                     "bi_coleman_liau_index" ,  "bi_ARI_score"   ,         "bi_difficult_words" ,    
                     "bi_dale_chall_score" ,    "bi_linsear_write_score" , "bi_gunning_fog_score")


for(i in 1:20) { 
 list_of_df_lowpartition[[i]]  <-  bi_population_trait %>% 
    group_by(Time, get(list_of_traits[[i]])) %>% 
    summarise(search_traffic_sum =sum(search_traffic),
              page_view_sum = sum(page_views)) %>%
   filter(row_number() %% 2 == 1)  %>%
   ungroup()
 
 list_of_df_highpartition[[i]]  <-  bi_population_trait %>% 
   group_by(Time, get(list_of_traits[[i]])) %>% 
   summarise(search_traffic_sum =sum(search_traffic),
             page_view_sum = sum(page_views)) %>%
   filter(row_number() %% 2 == 0)  %>%
   ungroup()
 
 names(list_of_df_lowpartition[[i]])  <-  c("Time",
                                            list_of_traits[[i]],
                                            paste("search_traffic",list_of_traits[[i]],"low", sep="_"),
                                            paste("page_views",list_of_traits[[i]],"low",sep="_")
 )
 
 names(list_of_df_highpartition[[i]])  <-  c("Time",
                                             list_of_traits[[i]],
                                             paste("search_traffic",list_of_traits[[i]],"high", sep="_"),
                                             paste("page_views",list_of_traits[[i]],"high",sep="_")
 )
} 

for(i in 1:20) { 
  list_of_df_lowpartition[[i]] <- 
    list_of_df_lowpartition[[i]] %>% 
    select( ends_with("low") )
  
  list_of_df_highpartition[[i]] <- 
    list_of_df_highpartition[[i]] %>% 
    select( ends_with("high") )
} 

trait_partition_fitness_low <- do.call(cbind, list_of_df_lowpartition)
trait_partition_fitness_high <- do.call(cbind, list_of_df_highpartition)
trait_partition_fitness <- cbind(trait_partition_fitness_low, trait_partition_fitness_high)

mat_temp <- as.data.frame(matrix(NA, nrow = 36, ncol = 80))

for(i in 1:80){
  raw = trait_partition_fitness[[i]]
  ahead = lead(raw)
  w_fitness = ahead/raw
  
  mat_temp[,i] = w_fitness
}

trait_partition_only_fitness <- as.data.frame(mat_temp)
names(trait_partition_only_fitness) <- names(trait_partition_fitness)

# got a matrix /df with columns:fitness calculated from each partition * two possible outcome measures

write.csv(trait_partition_only_fitness, "trait_partition_only_fitness.csv" )

trait_popu_fitness <- cbind(trait_partition_only_fitness, popu_generation_df[,c(5:6)])

write.csv(trait_partition_fitness, "trait_partition_fitness.csv" )

#get hyperlink network 
hypernet_raw <- read.csv("WikiHyperNet.csv", header = T, sep=",")
head(hypernet_raw)

#this df has many nodes that do not belong to the 400 main df
# exclude these nodes first
#conditional removal of rows, based on condition
condition1 <- hypernet_raw$Article_From %in% as.list(levels(maindf$ArticleName))
condition2 <- hypernet_raw$Article_To %in% as.list(levels(maindf$ArticleName))

hypernet_removed <- hypernet_raw[condition2, ]

#calculate graph metrics
makemetrics <- function(gr) {
  data.frame(Degree_undir = degree(gr, mode = "all", normalized = TRUE), 
             Degree_in = degree(gr,mode = "in",normalized = TRUE), 
             Degree_out = degree(gr,mode = "out",normalized = TRUE), 
             Closeness_out = closeness(gr, mode="out"), 
             Closeness_in = closeness(gr, mode="in"), 
             Closeness_undir= closeness(gr, mode="all"),
             constraint = constraint(gr),
             Betweenness_dir = betweenness(gr,directed = TRUE, normalized = TRUE),
             Betweenness_undir = betweenness(gr,directed = F, normalized = TRUE),
             eigen_dir = eigen_centrality(gr, directed = TRUE, scale = TRUE)$vector,
             eigen_undir = eigen_centrality(gr, directed = F, scale = TRUE)$vector,
             hub = hub_score(gr, scale = TRUE)$vector,
             authority = authority_score(gr, scale = TRUE)$vector,
             page_rank = page_rank(gr,directed = TRUE, damping = 0.85)$vector,
             transitivity = transitivity(gr, type="local")
             )
}

hyper_df_split <- split(hypernet_removed, hypernet_removed$Timestamp)
hypernet_gr<- lapply(hyper_df_split, function(x) makemetrics(graph_from_data_frame(x[,c(2:3)],direct = TRUE) ))
hypernet_metrics <- lapply(hypernet_gr, function(x) as.data.frame(x))
hypernet_metrics <- lapply(hypernet_metrics, function(x) add_rownames(x, var = "ArticleName"))

hypernet_metrics_time <- do.call(rbind,hypernet_metrics)
hypernet_metrics_time <- add_rownames(hypernet_metrics_time, var="Timestamp")

hypernet_time <- separate(hypernet_metrics_time,col="Timestamp",into = c("Timestamp1","Timestamp2" ), 
                          convert = FALSE) %>%
unite_(., "Timestamp", c("Timestamp1","Timestamp2"))
#re-organize time stamp

names(hypernet_time)
hypernet_time[is.na(hypernet_time)] <- 0

#dichotomize by mean value
hypernet_bi_time <- hypernet_time %>%
  group_by(Timestamp) %>%
  mutate(bi_Degree_undir = Degree_undir > mean(hypernet_time$Degree_undir), 
         bi_Degree_in = Degree_in > mean(hypernet_time$Degree_in),
         bi_Degree_out = Degree_out > mean(hypernet_time$Degree_out),
         bi_Closeness_out = Closeness_out > mean(hypernet_time$Closeness_out),
         bi_Closeness_in = Closeness_in > mean(hypernet_time$Closeness_in),
         bi_Closeness_undir = Closeness_undir > mean(hypernet_time$Closeness_undir),
         bi_constraint = constraint > mean(hypernet_time$constraint),
         bi_Betweenness_dir = Betweenness_dir > mean(hypernet_time$Betweenness_dir),
         bi_Betweenness_undir = Betweenness_undir > mean(hypernet_time$Betweenness_undir),
         bi_eigen_dir = eigen_dir > mean(hypernet_time$eigen_dir),
         bi_eigen_undir = eigen_undir > mean(hypernet_time$eigen_undir),
         bi_hub = hub > mean(hypernet_time$hub),
         bi_authority = authority > mean(hypernet_time$authority),
         bi_page_rank = page_rank > mean(hypernet_time$page_rank),
         bi_transitivity = transitivity > mean(hypernet_time$transitivity)
         )

hypernet_bi_popu <- left_join(hypernet_bi_time, 
          bi_population_trait, 
          by=c("Timestamp" ="Time","ArticleName" ="ArticleName"))
hypernet_bi_popu <- hypernet_bi_popu[,-c(33,34)]
#hypernet_bi_popu has 73 columns: 1/2 are basic names, 
#3:32 are 15*2 network metrics
#33:73 are 20+19(has info box is binary already) content metrics
# two outcome variables: search traffice and pageview


#calculate fitness for each metric
#create an empty list for the low and high partition each
list_of_df_lowpartition <- as.list(rep("", 15)) 
list_of_df_highpartition <- as.list(rep("", 15)) 

list_networktraits_15 <- c( "bi_Degree_undir", "bi_Degree_in" ,           "bi_Degree_out" ,  "bi_Closeness_out",       
                       "bi_Closeness_in" ,      "bi_Closeness_undir",      "bi_constraint",       
                      "bi_Betweenness_dir"  ,    "bi_Betweenness_undir" ,   "bi_eigen_dir"  ,         
                        "bi_eigen_undir"  ,        "bi_hub"  ,                "bi_authority",           
                        "bi_page_rank" ,    "bi_transitivity"  )


for(i in 1:15) { 
  list_of_df_lowpartition[[i]]  <-  hypernet_bi_popu %>% 
    group_by(Timestamp, get(list_networktraits_15[[i]])) %>% 
    summarise(search_traffic_sum =sum(search_traffic),
              page_view_sum = sum(page_views)) %>%
    filter(row_number() %% 2 == 1)  %>%
    ungroup()
  
  list_of_df_highpartition[[i]]  <- hypernet_bi_popu %>% 
    group_by(Timestamp, get(list_networktraits_15[[i]])) %>% 
    summarise(search_traffic_sum =sum(search_traffic),
              page_view_sum = sum(page_views)) %>%
    filter(row_number() %% 2 == 0)  %>%
    ungroup()
  
  names(list_of_df_lowpartition[[i]])  <-  c("Time",
                                             list_networktraits_15[[i]],
                                             paste("search_traffic",list_networktraits_15[[i]],"low", sep="_"),
                                             paste("page_views",list_networktraits_15[[i]],"low",sep="_")
  )
  
  names(list_of_df_highpartition[[i]])  <-  c("Time",
                                              list_networktraits_15[[i]],
                                              paste("search_traffic",list_networktraits_15[[i]],"high", sep="_"),
                                              paste("page_views",list_networktraits_15[[i]],"high",sep="_")
  )
} 

for(i in 1:15) { 
  list_of_df_lowpartition[[i]] <- 
    list_of_df_lowpartition[[i]] %>% 
    select( ends_with("low") )
  
  list_of_df_highpartition[[i]] <- 
    list_of_df_highpartition[[i]] %>% 
    select( ends_with("high") )
} 

network_partition_fitness_low <- do.call(cbind, list_of_df_lowpartition)
network_partition_fitness_high <- do.call(cbind, list_of_df_highpartition)
network_partition_fitness <- cbind(network_partition_fitness_low, network_partition_fitness_high)

mat_temp <- as.data.frame(matrix(NA, nrow = 36, ncol = 60))

for(i in 1:60){
  raw = network_partition_fitness[[i]]
  ahead = lead(raw)
  w_fitness = ahead/raw
  
  mat_temp[,i] = w_fitness
}

network_partition_only_fitness <- as.data.frame(mat_temp)
names(network_partition_only_fitness) <- names(network_partition_fitness)
network_partition_only_fitness <- network_partition_only_fitness[1:35,] #remove the last row of NA

#both network_partition_only_fitness and trait_popu_fitness are ready

trait_popu_fitness <- trait_popu_fitness[1:35,]
#removed last row of NAs

trait_popu_fitness[[81]] <-as.numeric(levels(trait_popu_fitness[[81]]))[trait_popu_fitness[[81]]]
trait_popu_fitness[[82]] <-as.numeric(levels(trait_popu_fitness[[82]]))[trait_popu_fitness[[82]]]

#trait
nc_trait <- as.data.frame(matrix(NA, nrow = 35, ncol = 40))
nc_trait_search_traffic <- trait_popu_fitness %>%
  select(contains("search_traffic")) 
nc_trait_page_views <- trait_popu_fitness %>%
  select(contains("views") ) 

#store nc results as a vector...

list_nc_search_traffic <- as.list(rep("",20)) #20 traits of var(high,low)/popu_mean
list_nc_page_views <- as.list(rep("",20)) 

for(i in 1:20){
  temp <- as.data.frame(nc_trait_search_traffic[,c(i,20+i,41)])
  vars<-apply(temp[1:2],1,var)
  nc <- vars/temp[["DV_search_traffic_fitness"]]
  list_nc_search_traffic[[i]] <- nc
  }

for(i in 1:20){
  temp <- as.data.frame(nc_trait_page_views[,c(i,20+i,41)])
  vars<-apply(temp[1:2],1,var)
  nc <- vars/temp[["DV_pageviews_fitness"]]
  list_nc_page_views[[i]] <- nc
}


nc_search_traffic_df <- do.call(cbind, list_nc_search_traffic)
nc_search_traffic_df <- as.data.frame(nc_search_traffic_df)
names(nc_search_traffic_df) <-  paste(list_of_traits20 , "search_traffic",sep="_")

nc_page_views_df <- do.call(cbind, list_nc_page_views)
nc_page_views_df <- as.data.frame(nc_page_views_df)
names(nc_page_views_df) <-  paste(list_of_traits20 ,"page_views",sep="_")


#network...
network_partition_only_fitness <- cbind(network_partition_only_fitness,trait_popu_fitness[,81:82])
#62columns = 60 network metric + 2 outcome variable

nc_network <- as.data.frame(matrix(NA, nrow = 35, ncol = 60))

nc_network_search_traffic <- network_partition_only_fitness %>%
  select(contains("search_traffic")) 
nc_network_page_views <- network_partition_only_fitness %>%
  select(contains("views") ) 

list_nc_search_traffic_network <- as.list(rep("",15))
list_nc_page_views_network <- as.list(rep("",15)) 

for(i in 1:15){
  temp <- as.data.frame(nc_network_page_views[,c(i,15+i,31)])
  vars<-apply(temp[1:2],1,var)
  nc <- vars/temp[["DV_pageviews_fitness"]]
  list_nc_page_views_network[[i]] <- nc
}

for(i in 1:15){
  temp <- as.data.frame(nc_network_search_traffic[,c(i,15+i,31)])
  vars<-apply(temp[1:2],1,var)
  nc <- (vars)/temp[["DV_search_traffic_fitness"]] #add a small number to remove the 0s
  list_nc_search_traffic_network[[i]] <- nc
}

nc_network_search_traffic_df <- do.call(cbind, list_nc_search_traffic_network)
nc_network_search_traffic_df <- as.data.frame(nc_network_search_traffic_df)
names(nc_network_search_traffic_df) <-  paste(list_networktraits_15 , "search_traffic",sep="_")

nc_network_page_views_df <- do.call(cbind, list_nc_page_views_network)
nc_network_page_views_df <- as.data.frame(nc_network_page_views_df)
names(nc_network_page_views_df) <-  paste(list_networktraits_15 , "page_views",sep="_")

#now combine trait + network
nc<- as.data.frame(cbind(nc_search_traffic_df, 
                         nc_page_views_df,
                         nc_network_page_views_df,
                         nc_network_search_traffic_df))
nc$ID <- seq.int(nrow(nc))
nc$trait_mean_search_traffic <- rowMeans(nc[1:20])
nc$trait_mean_page_view <- rowMeans(nc[21:40])
nc$network_mean_page_view <- rowMeans(nc[41:55])
nc$network_mean_search_traffic <- rowMeans(nc[56:70])
# write.csv(nc, "nc_06162019.csv")
write.csv(nc,"nc_071219.csv")


#melt from wide to tall, for ggplot purpose
nc[,c(71:75)] %>%
  gather(key=type_of_DV,value=Natural_selection,
         trait_mean_search_traffic,            
         # trait_mean_page_view,                  
         # network_mean_page_view,               
         network_mean_search_traffic
         ) %>%
  ggplot(aes(x=ID, y=Natural_selection, colour=type_of_DV)) +
  geom_line()

