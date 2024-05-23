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
#install.packages("cli")
#install.packages("glue")
#install.packages("lifecycle")
#install.packages("magrittr")
#install.packages("pillar")
#install.packages("rlang")
#install.packages("tibble")
#install.packages("tidyselect")
#install.packages("vctrs")
#install.packages("dplyr")
#install.packages("ggplot2")

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
freq.Category.Plot <- ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()

#Create top 10 categories data
category.Top10 <- category.Freq[(order(-category.Freq$Freq)), ] 
category.Top10 <- category.Top10[1:10, ]

freq.Category.Plot <- ggplot(data = category.Top10) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), stat = "identity") + coord_flip()
freq.Category.Plot

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

dados_2$kb <- sapply(X = dados_2$Size, FUN = function(y) {
  if(grepl("M", y, ignore.case = T)) {
    y <- as.numeric(gsub(pattern = "M", replacement = "", x = y)) * 1024
  } else if (grepl("M", y , ignore.case = T)) {
    y <- gsub("k |\\+", replacement = "", x = y)
  } else {
    y <- "nd"
  }
})

hist(as.numeric(dados_2$kb))

options(scipen = 999)

size.app <- dados_2 %>% filter(kb != "nd") %>% mutate(kb = as.numeric(kb))
size.app.Plot <- ggplot(size.app) + geom_histogram(aes(kb))


# date manipulation
#install.packages("lubridate")

library(lubridate)

path <- file.path(getwd(), "data", "user_reviews.csv")
notas <- read.csv(file = path)

notas$data_2 <- ymd_hms(notas$data)

ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))

notas$data_2 <- parse_date_time(format(notas$data_2, "%Y-%m"), "ym")

notas$data_2

media_nota <- notas %>% group_by(data_2) %>% summarise(media = mean(Sentiment_Polarity))

nota_plot <- ggplot(media_nota) + geom_line(aes(x = data_2, y = media))

nota_plot

#histogram style
rating.Histogram <- rating.Histogram + ggtitle("Histograma Rating")

rating.Histogram <- rating.Histogram + theme(plot.title = element_text(hjust = 0.5))

rating.Histogram <- rating.Histogram + theme_bw()

rating.Histogram

freq.Category.Plot <- freq.Category.Plot + ggtitle("Quantidade de Apps por categoria")
freq.Category.Plot <- freq.Category.Plot + xlab("Categoria") + ylab("Quantidade")

freq.Category.Plot + geom_bar(aes(Var1, Freq), fill = "#d50000", stat = "identity")
freq.Category.Plot + geom_bar(aes(Var1, Freq, fill = Freq), stat = "identity")

freq.Category.Plot <- freq.Category.Plot + geom_bar(aes(Var1, Freq), fill = "darkcyan", stat = "identity")
freq.Category.Plot <- freq.Category.Plot + theme_bw()

blank_theme <- theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
  )

type.Plot <- type.Plot + blank_theme

library(scales)

type.Plot <- type.Plot + geom_text(aes(x = "", y = Freq/2, label = percent(Freq/sum(Freq))), size = 4)
type.Plot <- type.Plot + scale_fill_discrete(name = "Tipo")
type.Plot <- type.Plot + ggtitle("Tipos de Aplicativos") + theme(plot.title = element_text(hjust = 0.5))

type.Plot

size.app.Plot <- size.app.Plot + ggtitle("Histograma tamanho dos aplicativos")
size.app.Plot <- size.app.Plot + geom_histogram(aes(kb, fill = ..x..)) + scale_fill_gradient(low = "blue", high = "yellow") + guides(fill = FALSE)
size.app.Plot <- size.app.Plot + xlab("Tamanho App (em kb)") + ylab("Quantidade de Apps")
size.app.Plot <- size.app.Plot + theme_minimal()

size.app.Plot

#install.packages("gridExtra")
library(gridExtra)
grid.arrange(rating.Histogram, freq.Category.Plot, type.Plot, size.app.Plot, nota_plot, nrow = 2, ncol=3)
