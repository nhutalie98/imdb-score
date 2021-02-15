setwd("~/DATA MINING")
imdb.df <- read.csv("IMDB Movie Dataset.csv")

# summary statistic
summary(imdb.df)

# Box plot to show IMDB Score for each country  
by_country <- aggregate(imdb.df[,c(3,9,23,28)],by = list(imdb.df$country, imdb.df$imdb_score, imdb.df$title_year), FUN = sum, na.rm = TRUE)
by_country
names(by_country) <- c("Country", "IMDB Score","Critics", "Gross", "Budget", "FB Likes")

plot(by_country$Country,by_country$`IMDB Score`, xlab = "Country", ylab = "IMDB Score", las = 2)


#Barplot for Movie type vs FB Likes
install.packages("dyplr")
install.packages("tidyverse")
install.packages("ggplot2")
library(dplyr)
library(tidyverse)
library(ggplot2)

movie_color <- imdb.df %>% group_by(ï..color) %>% filter(ï..color == "Color" | ï..color == " Black and White") %>% summarise(fb_likes = sum(movie_facebook_likes))
ggplot(movie_color, aes(ï..color, fb_likes)) + geom_col() + xlab("Type") + ylab("FB_Likes") + geom_text(aes(label = fb_likes), vjust = -0.25)


# Finding NA values and subsitute with median value
x <- c(imdb.df$num_critic_for_reviews,imdb.df$duration,imdb.df$director_facebook_likes,
       imdb.df$actor_3_facebook_likes,imdb.df$actor_1_facebook_likes,imdb.df$gross,
       imdb.df$facenumber_in_poster,imdb.df$num_user_for_reviews,imdb.df$budget,imdb.df$title_year,
       imdb.df$actor_2_facebook_likes,imdb.df$aspect_ratio)
x[is.na(x)] = median(x, na.rm = TRUE)
x

# Dummy variables
color <- ifelse(imdb.df$ï..color == "Color",1,0)
bw <- ifelse(imdb.df$ï..color == " Black and White",1,0)
imdb.df1 <- cbind(imdb.df[, c(1,2,3,4,29,30)])
head(imdb.df1, n = 10)









