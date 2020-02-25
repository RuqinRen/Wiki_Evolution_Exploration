#update: 2020-02-25
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
