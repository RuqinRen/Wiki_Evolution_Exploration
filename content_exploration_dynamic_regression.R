library(tm)
library(dplyr)
library(tidyr)
library(SnowballC)
library(dLagM)

dist2d <- function(a,b,c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  d <- abs(det(m))/sqrt(sum(v1*v1))
} 

# sample_n_groups = function(grouped_df, size, replace = FALSE, weight=NULL) {
#   grp_var <- grouped_df %>% 
#     groups %>%
#     unlist %>% 
#     as.character
#   random_grp <- grouped_df %>% 
#     summarise() %>% 
#     sample_n(size, replace, weight) %>% 
#     mutate(unique_id = 1:NROW(.))
#   grouped_df %>% 
#     right_join(random_grp, by=grp_var) %>% 
#     group_by_(grp_var) 
# }
# 
#############################
##calculate content exploration
#############################

coeditor_network <- read.csv("/home/rstudio/WikiEvolution/network_data.csv", header = TRUE)
names(coeditor_network)
coeditor_network <- coeditor_network[,c("articleId","articleName",'dateOfWeek','article')]
coeditor_network <- unique(coeditor_network)

coeditor_network$year <- NA
coeditor_network$year <- substr(coeditor_network$dateOfWeek, start = 1, stop = 4)
levels(coeditor_network$dateOfWeek) <- c(levels(coeditor_network$dateOfWeek), '2016W99')
coeditor_network$dateOfWeek[coeditor_network$year<2017] <- '2016W99'
#num of revisions / dateOfWeek distribution
table(coeditor_network$dateOfWeek)


#create a unique doc_id that is in temporal order

article_data <- read.csv("/home/rstudio/WikiEvolution/article_data.csv",header = TRUE)
time_df <-as.data.frame(toupper(unique(article_data$Y.W)))
time_df <- cbind(time_df, uniqueId = (2:166))
levels(time_df$`toupper(unique(article_data$Y.W))`) <-c(levels(time_df$`toupper(unique(article_data$Y.W))`), '2016W99') 
time_df <- rbind(c("2016W99",1), time_df)
names(time_df) <- c("dateOfWeek","weekly")

remove(article_data)

coeditor_network <- left_join(coeditor_network, time_df,  by = c('dateOfWeek'))
names(coeditor_network)

#split df into each article's df
coeditor_network_split <- split(coeditor_network, coeditor_network$articleName)
remove(coeditor_network)


# set.seed(12)
# try <- coeditor_network %>% group_by(articleName) %>% sample_n_groups(1) %>% group_by(weekly) %>% 
#   filter(row_number()==n()) %>%  arrange(dateOfWeek) # some weeks have more than one revision, so only keep the last row of that weekly group

#get each article df by index

datalist = list()

for(groupid in 101:length(coeditor_network_split))
  {
try <- coeditor_network_split[[groupid]] %>% group_by(weekly) %>% filter(row_number()==n()) %>%  arrange(dateOfWeek) %>% ungroup()
text_df <- data.frame(doc_id = try$weekly, text = try$article)
article_corpus <- Corpus(DataframeSource(text_df)) 
#####################
#cleaning
#####################
mydata <- tm_map(article_corpus, content_transformer(tolower))
#remove ������ what would be emojis
mydata<-tm_map(mydata, content_transformer(gsub), pattern="\\W",replace=" ")
# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeURL)
)
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
mydata <- tm_map(mydata, content_transformer(removeNumPunct))
# remove stopwords
mydata <- tm_map(mydata, removeWords, stopwords("english"))
#u can create custom stop words using the code below.
#myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"use", "see", "used", "via", "amp")
#mydata <- tm_map(mydata, removeWords, myStopwords)
# remove extra whitespace
mydata <- tm_map(mydata, stripWhitespace)
# Remove numbers
mydata <- tm_map(mydata, removeNumbers)
# Remove punctuations
mydata <- tm_map(mydata, removePunctuation)
#stemming
mydata <- tm_map(mydata, stemDocument)

#####################
#Bag of Words representation
# create V_i vector
#####################

doc_mat <- TermDocumentMatrix(mydata)
X <- t(as.matrix(doc_mat))
X <- rbind(rep(0,ncol(X)),X) #initiate the empty V0
# X <- unique(as.data.frame(X))
a <- tibble::rownames_to_column(as.data.frame(X), var = "weekly")[,1] #the fake empty V0 willnot have a row name

X<- as.matrix(X) #for faster speed
n <- nrow(X) 
m <- matrix(nrow=n, ncol=n) #create a new matrix of N*N to hold results

for(i in seq_len(n))
  for(j in seq(i, n))
    m[j, i] <- m[i, j] <- sum(X[i,] != X[j,])

#create S_i vector, length = 1+n corpuses, because there is an S_0
s <- matrix(nrow = n, ncol = 2)

for(i in seq(1,n-1)){
  s[i+1,1] <- m[i+1,1]
  s[i+1,2] <- m[i+1,n]
}

s[1,] <-c(0,s[n,1])
# create a column showing S vector name
s <- cbind(paste(c(rep("s", n)),c(0:(n-1)),sep=""), s) 
#add back the weely variable of that vector
s <- cbind(a, s)

#####################
# convert to a two dimensional space
# dist2line
#####################

## two-dimensional case:
s0 <- as.numeric(s[1,3:4])
sR <- as.numeric(s[n,3:4])
s <- cbind(s, rep(NA, n)) #create a new empty column

for(i in seq(n-1)){
  current_coordinate <- as.numeric(s[i+1,3:4])
  dist <- dist2d(current_coordinate, s0, sR) # distance of point a from line (b,c) in 2D space
  s[i+1,5] <- dist
}

s <- cbind(s, rep(groupid, n))
s <- as.data.frame(s)
names(s) <- c('weekly','s_vector','X_coor',"Y_coor","dist2lin",'group_i')
s <- left_join(try[,-4],s, by="weekly")

datalist[[groupid]] <- s # add it to your list
}
remove(coeditor_network_split)
#combine results back to original df
result <- do.call(rbind, datalist)
names(result)
cols = c(7,8,9);    
result[,cols] = apply(result[,cols], 2, function(x) as.numeric(as.character(x)));
result[is.na(result)] <- 0
summary(result)

write.csv(result, "/home/rstudio/WikiEvolution/exploration_result.csv")
#############################
##combine with main df
#############################
exploration <- read.csv("/home/rstudio/WikiEvolution/exploration_result.csv", header = TRUE)
weekly <- read.csv("/home/rstudio/WikiEvolution/weekly_net_trait_bi.csv", header = TRUE) #main df with IDs, traits, networks
exploration <- exploration[,c(3,4,10)]
names(exploration)
names(weekly_net_trait_bi)

weekly$Y.W <- toupper(weekly$Y.W)
weekly<- left_join(weekly, exploration, by = c("articleName" ='articleName', "Y.W" = 'dateOfWeek'))
#remove two articles that do not span over the 165 time periods: Pelvicachromis silviae,Feeder shrimp
weekly <- weekly %>% filter(articleName != "Pelvicachromis silviae" & articleName != "Feeder shrimp")

#assign value of 0 to Y.W = 2017W00
weekly <- weekly %>% 
  mutate(dist2lin = replace(dist2lin, Y.W=="2017W00", 0)) %>% 
  fill(c(50), .direction='down')

weekly$weekly <- NA
weekly$weekly <- as.factor(rep(seq_len(165), 393))
  
write.csv(weekly, "/home/rstudio/WikiEvolution/traits2exploration.csv")

#############################
##dynamic regression
#############################
library(forecast)
library(tseries)
library(ggplot2)
library(TSA)
library(Ecdat)
library(Hmisc)
library(astsa)

weekly_avg <- weekly %>% group_by(weekly) %>% 
  summarise( contentLength =mean(contentLength),
             numOfreferences=mean(numOfreferences),
             numOfPageLinks=mean(numOfPageLinks),
             nunOfCiteTemplates=mean(nunOfCiteTemplates),
             numOfCategories=mean(numOfCategories),
             imagesByLength  =mean(imagesByLength),
            numOfLv2Heading=mean(numOfLv2Heading),
            flesch_reading_ease=mean(flesch_reading_ease),
            coleman_liau_index=mean(coleman_liau_index),
            difficult_words=mean(difficult_words),
            Degree_undir = mean(Degree_undir),
            Closeness_undir = mean(Closeness_undir) ,
            Betweenness_undir   = mean(Betweenness_undir),   
            eigen_undir   = mean(eigen_undir)          ,
            page_rank = mean(page_rank)   , 
            hub = mean(hub)    ,
            authority = mean(authority)    ,
            constraint    = mean(constraint)   ,           
            transitivity = mean(transitivity)    ,
            eff_size   = mean(eff_size)              ,  
            density  = mean(density)  ,
            exploration = mean(dist2lin))
  

write.csv(weekly_avg,"/home/rstudio/WikiEvolution/weekly_avg.csv" )

ggtsdisplay(log(weekly_avg$exploration),ylab='log exploration',xlab='Weekly',main='Exploration by weekly')

fit<-auto.arima(weekly_avg$exploration, seasonal=FALSE)
fit
#ARIMA(3,2,1) 

exploration_D <- diff(weekly_avg$exploration) #difference of DV

degree_Z <- weekly_avg$Degree_undir - mean(weekly_avg$Degree_undir) #mean adjustment of IV
Closeness_Z = weekly_avg$Closeness_undir - mean(Closeness_undir) 
Betweenness_Z  = weekly_avg$Betweenness_undir -mean(Betweenness_undir)
eigen_Z   = weekly_avg$eigen_undir -mean(eigen_undir)          
page_rank_Z =weekly_avg$page_rank - mean(page_rank)    
hub_Z = weekly_avg$hub -mean(hub)    
authority_Z =weekly_avg$authority - mean(authority)   
constraint_Z    =weekly_avg$constraint - mean(constraint)             
transitivity_Z =weekly_avg$transitivity - mean(transitivity)   
eff_size_Z   = weekly_avg$eff_size -mean(eff_size)              
density_Z  =weekly_avg$density - mean(density)  

contentLength_Z = weekly_avg$contentLength -mean(contentLength)
numOfreferences_Z=weekly_avg$numOfreferences - mean(numOfreferences)
numOfPageLinks_Z=weekly_avg$numOfPageLinks -mean(numOfPageLinks)
nunOfCiteTemplates_Z=weekly_avg$nunOfCiteTemplates -mean(nunOfCiteTemplates)
numOfCategories_Z=weekly_avg$numOfCategories -mean(numOfCategories)
imagesByLength_Z  =weekly_avg$imagesByLength -mean(imagesByLength)
numOfLv2Heading_Z=weekly_avg$numOfLv2Heading -mean(numOfLv2Heading)
flesch_reading_ease_Z=weekly_avg$flesch_reading_ease -mean(flesch_reading_ease)
coleman_liau_index_Z=weekly_avg$coleman_liau_index -mean(coleman_liau_index)
difficult_words_Z=weekly_avg$difficult_words - mean(difficult_words)

list <- c(  "contentLength_Z" ,      "numOfreferences_Z" ,    "numOfPageLinks_Z",      "nunOfCiteTemplates_Z" , "numOfCategories_Z" ,   
            "imagesByLength_Z" ,     "numOfLv2Heading_Z"  ,   "flesch_reading_ease_Z", "coleman_liau_index_Z",  "difficult_words_Z"  )
result <- list()


tfm=arimax(weekly_avg$exploration,
           order=c(3,2,1),
           xtransf= cbind(Closeness_Z, eigen_Z, hub_Z, authority_Z),
           xreg = imagesByLength_Z,
           transfer=list(c(0,1),c(0,1),c(0,1),c(0,1)),
           method='ML')
tfm
#calculate p value
(1-pnorm(abs(tfm$coef)/sqrt(diag(tfm$var.coef))))*2

#calculate R sqrd
cor(fitted(tfm3)[-c(0:6)],exploration_2diff[-c(0:6)], use = "pairwise.complete.obs")^2

##############################
# Final version: ARDL model in dLagM package.
# did not use ARIMAX function
#############################
formula2 <- exploration ~  numOfreferences +    imagesByLength 
remove = list(p = list(imagesByLength = c(1,2,3,4,5,6,7,8),
                       numOfreferences =c(1,2,3,4,5,6,7,8)))

# eff_size = c(0,1,2,3,4,5),
# hub = c(1,2,3,4,5,6,7),
# eigen_undir = c(1,2,3,4,5,6,7),
# density = c(1,2,3,4,5,6,7)))
fit1 <- ardlDlm(formula = formula2,  data = weekly_avg, p = 8,  q = 1, remove = remove)
summary(fit1)

formula3 <- exploration ~  numOfreferences +   imagesByLength +  eff_size + density 

remove = list(p = list(
  imagesByLength = c(1,2,3,4,5,6,7,8),
  numOfreferences =c(1,2,3,4,5,6,7,8),
  eff_size = c(0,1,2,3,4,5,6,8),
  density = c(0,1,2,3,4,5,7,8)))

fit3 <- ardlDlm(formula = formula3,  data = weekly_avg, p = 8,  q = 1, remove = remove)
summary(fit3)

#GoF test
Box.test(x, type="Ljung-Box")
Box.test(residuals(fit3),type="Ljung-Box")
adf.test(residuals(fit3))

