#update: 2020-02-26
library(dplyr)

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
# article_data %>% select(articleName, Y.W, pageView) %>% filter(is.na(pageView))
# because two articles were created during this period of time.Hence not enough data

popu_weekly_pageview <- article_data %>% 
 group_by(Y.W) %>% 
 summarise(weekly_pageview = sum(pageView, na.rm = TRUE))

#categorize numerical features into binary features by breaking from mean value
# 10 features
#getting 10 features' mean values
article_data %>% select(c(6:16)) %>% summary()
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

#trait fitness summary with the following columns
#Generation_time /Trait / Trait_fitness_page_views
#e.g. time1/traitA/ low_partition/ sum /sum/fitness
#e.g. time1/traitA/ high_partition/ sum /sum/fitness

#building empty lists
list_of_df_lowpartition <- as.list(rep("", 10)) 
list_of_df_highpartition <- as.list(rep("", 10)) 

#list of 11 trait names
list_of_traits <- c("bi_content_length"  ,     "hasBox" ,
                      "bi_num_references"  ,     "bi_num_page_links"  ,     "bi_num_cite_temp"  ,     
                      "bi_num_categories"  ,     "bi_image_by_length" ,     "bi_num_lv2_heading"  ,      
                      "bi_flesch_reading_score", "bi_coleman_liau_index" ,  "bi_difficult_words")


for(i in 1:11) { 
  list_of_df_lowpartition[[i]]  <-  bi_population_trait %>% 
    group_by(Y.W, get(list_of_traits[[i]])) %>% #there are implicit NA levels due to missing data, each trait generates three rows: low/high/NA
    summarise( page_view_sum = sum(pageView,na.rm = TRUE)) %>%
    filter(page_view_sum > 0) %>% #NA rows has 0 value, remove them
    filter(row_number() %% 2 == 1)  %>% #selet the "low"value rows because low appears after high
    ungroup()
  
  list_of_df_highpartition[[i]]  <-  bi_population_trait %>% 
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

#the output are two lists of 11 traits at low values pageView sum/ high values pageView sum
#combine them into dfs

for(i in 1:11) { 
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
remove(trait_partition_fitness_low)
remove(trait_partition_fitness_high)

#combined into a df of 165 time periods, 22=11*2 low/high page view sum
#first week is wrong, remove the first week 2017w00
trait_partition_fitness <- trait_partition_fitness[-1,]
  
totalYW = as.integer(nrow(trait_partition_fitness))

trait_partition_only_fitness <- as.data.frame(matrix(NA, nrow = totalYW, ncol = 22))

for(i in 1:22){
  raw = trait_partition_fitness[[i]]
  ahead = lead(raw)
  w_fitness = ahead/raw
  
  trait_partition_only_fitness[,i] = w_fitness
}

names(trait_partition_only_fitness) <- names(trait_partition_fitness)
# output is a df with columns:fitness calculated from each partition

trait_popu_fitness <- cbind(trait_partition_only_fitness, popu_weekly_pageview[-1,])
write.csv(trait_popu_fitness, "trait_population_fitness.csv" )

