library(lattice)
library(grid)
library(nnet)
library(MASS)
library(devtools)
library(png)

source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')

main.page <- read_html(x = "http://www.presidency.ucsb.edu/inaugurals.php")
# Get link URLs
# f.speechlinks is a function for extracting links from the list of speeches. 
inaug=f.speechlinks(main.page)
inaug=inaug[-nrow(inaug),]
inaug.list=read.csv("/Users/jiangyiran/Documents/GitHub/Spring2018-Project1-YiranJiang/data/inauglist.csv", stringsAsFactors = FALSE)

inaug.list=cbind(inaug.list, inaug)

inaug.list$fulltext=NA
for(i in seq(nrow(inaug.list))) {
  text <- read_html(inaug.list$urls[i]) %>% # load the page
    html_nodes(".displaytext") %>% # isloate the text
    html_text() # get the text
  inaug.list$fulltext[i]=text

}

colnames(inaug.list)


date <- inaug.list$links 
date <- substring(date,nchar(date)-3,nchar(date))

GDP <- read.csv("/Users/jiangyiran/Documents/GitHub/Spring2018-Project1-YiranJiang/data/GDP.csv")
GDP <- rbind(c("1900",as.numeric(20.7)),GDP)
colnames(GDP) <- c("Year","Growth")
GDP$Growth <- as.numeric(GDP$Growth)
rate <- NULL
for (i in 1:nrow(GDP)){
  if (i == 1){
    rate <- c(NaN)
  }
  this.rate <- (GDP$Growth[i] - GDP$Growth[i-1])/GDP$Growth[i]
  rate <- c(rate,this.rate)
}

GDP$Growth_rate <- rate
GDP <- GDP %>%
  filter(GDP$Growth_rate, Year %in% date)

NewGDP <- data.frame(rbind(c("2013",0,0.017),c("2017",0,0.026)))
colnames(NewGDP) <- c("Year","Growth","Growth_rate")
GDP <- rbind(GDP,NewGDP)

emotions_per_year <- NULL
for(i in 1:nrow(inaug.list)){
  sentences=sent_detect(inaug.list$fulltext[i],
                        endmarks = c("?", ".", "!", "|",";"))
  if(length(sentences)>0){
    emotions=get_nrc_sentiment(sentences)
    word.count=word_count(sentences)
    emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
    emotions <- as.data.frame(emotions)
    emotions <- emotions %>%
      summarise(anger = mean(anger,na.rm =TRUE), 
                anticipation = mean(anticipation,na.rm =TRUE),
                disgust = mean(disgust,na.rm =TRUE),
                fear = mean(fear,na.rm =TRUE),
                joy = mean(joy,na.rm =TRUE),
                sadness = mean(sadness,na.rm =TRUE),
                surprise = mean(surprise,na.rm =TRUE),
                trust = mean(trust,na.rm =TRUE),
                negative = mean(negative,na.rm =TRUE),
                positive = mean(positive,na.rm =TRUE))
    emotions_per_year <- rbind(emotions_per_year,emotions)
      
  }
}
emotions_per_year$Year <- date

emotions_per_year <- emotions_per_year %>%
  filter(emotions_per_year$anger, Year %in% GDP$Year)

norm.data <- scale(emotions_per_year[,1:10]) 

GDP_Growth <- as.numeric(GDP$Growth_rate)

set.seed(123)

sample <- sample(1:30,30)

New_Growth <- GDP_Growth[sample]
New.data <- norm.data[sample,]

nn <- nnet(New_Growth[1:24]~., New.data[1:24,], size = 10 , decay = 0.01, 
           maxit = 1000, linout = T, trace = F) 

summary(nn)
prediction <- predict(nn, norm.data)
newdate <- seq(1901,2017,by=4)
predictdata <- data.frame(prediction,GDP_Growth,newdate)
predictyear <- newdate[sample][25:30]

plot.nnet(nn)


img <- readPNG('/Users/jiangyiran/Documents/GitHub/Spring2018-Project1-YiranJiang/figs/Trump.png')

library(ggplot2)

ggplot(data = predictdata)+
  annotation_custom(rasterGrob(img, width=unit(1,"npc"), height=unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_line(mapping = aes(x = newdate, y = prediction,colour = 'red'),size=1.5)+
  geom_line(mapping = aes(x = newdate, y = GDP_Growth,colour = 'black'),size=1.5)+
  scale_color_discrete(name = "Meaning", labels = c("Prediction Line", "Real Line"))+
  geom_point(data = subset(predictdata, newdate %in% predictyear),
             mapping = aes(x = newdate , y = prediction),
             col = 'blue', shape = 8 )+
  geom_text(hjust = 1, data = subset(predictdata, newdate %in% predictyear),
             mapping = aes(x = newdate , y = prediction,
                           label = newdate))+
  labs(x = 'Year', y = 'GDP Growth Rate')+
  ggtitle('The GDP Growth Over Years')


