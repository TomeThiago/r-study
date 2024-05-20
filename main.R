path <- file.path(getwd(), "data", "googleplaystore.csv")
dados <- read.csv(file = path)

#View the table
View(dados)

#View the first five rows of the table
head(dados)

#View the last five rows of the table
tail(dados)

#Show table string variables
str(dados)

#Download a lib
#sudo apt-get install build-essential
install.packages("cli")
install.packages("glue")
install.packages("lifecycle")
install.packages("magrittr")
install.packages("pillar")
install.packages("rlang")
install.packages("tibble")
install.packages("tidyselect")
install.packages("vctrs")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

#Shows data from a specific column in the table
table(dados$Rating)

#Create a histogram of rating data
hist(dados$Rating, xlim = c(1,5))

#Create a histogram of classification data with ggplot2
ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating))
rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm = TRUE, breaks = seq(1,5)) + xlim(c(1,5))

rating.Histogram

#Create a bar chart of categories data with ggplot2
ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip()

#Create a bar chart of categories data with ggplot2 and order by asc
category.Freq <- data.frame(table(dados$Category))
ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

#Create top 10 categories data
category.Top10 <- category.Freq[(order(-category.Freq$Freq)), ] 
category.Top10 <- category.Top10[1:10, ]

ggplot(data = category.Top10) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()


#Create a bar chart of categories data with ggplot2 and order by desc
category.Freq <- data.frame(table(dados$Category))
ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, -Freq), y = Freq), stat = "identity") + coord_flip()

category.Bottom10 <- category.Freq[(order(category.Freq$Freq)), ]
category.Bottom10 <- category.Bottom10[1:10, ]

ggplot(data = category.Bottom10) + geom_bar(mapping = aes(x = reorder(Var1, -Freq), y = Freq), stat = "identity") + coord_flip()

#Data correction and new attributes
dados_2 <- dados %>% filter(Category != "1.9")

min(dados_2$Rating)
max(dados_2$Rating)

dados_2 %>% filter(is.na(Rating)) %>% count()

summary(dados_2$Rating)

dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% count()

mean.Category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>% summarise(media = mean(Rating))

mean.Category

for(i in 1:nrow(dados_2)) {
  if(is.na(dados_2[i, "Rating"])) {
    dados_2[i, "newRating"] <- mean.Category[mean.Category$Category == dados_2[i, "Category"], "media"]
  } else {
    dados_2[i, "newRating"] <- dados_2[i, "Rating"]
  }
}

summary(dados_2$newRating)
min(dados_2$newRating)
max(dados_2$newRating)

dados_2 <- dados_2 %>% mutate(rating_class = if_else(newRating < 2, "ruim", if_else(newRating > 4, "bom", "regular")))

ggplot(dados_2) + geom_bar(aes(rating_class), stat = "count") 

#Create a bar chart of x data with ggplot2
type.Freq <- data.frame(table(dados_2$Type))

type.Plot <- ggplot(type.Freq) + geom_bar(aes(x = "", y = Freq, fill = Var1), stat = "identity", width = 1) + coord_polar(theta = "y", start = 0)

type.Plot

freq.Size <- data.frame(table(dados_2$Size))

sapply(X = dados_2$Size, FUN = function(x) {
  print(x)
})