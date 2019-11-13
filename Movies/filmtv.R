---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.width = 12)
knitr::opts_chunk$set(fig.align = 'center')
```


library(data.table)
library(ggplot2)
library(magrittr)
library(purrr)
library(fastDummies)

filmtv <- fread("data/filmtv_movies.csv", stringsAsFactors = TRUE)
summary(filmtv)
levels(filmtv$genre)


filmtv_mod <- filmtv[genre == "" | genre == "MĂ©lo", genre := NA]
filmtv_mod[, genre := droplevels(genre)]
summary(filmtv_mod)
filmtv_mod[, .N, by = genre]
levels(filmtv_mod$genre)

drop_na <- filmtv_mod[genre != is.na(genre)]
levels(drop_na$genre)

filmtv_mod_genre_dummies <- dummy_cols(drop_na, select_columns = "genre")


ggplot(filmtv_mod_genre_dummies, aes(x = genre_Action, y = votes)) + geom_col()
  xlim(1890, 2020)



```{r}
filmtv_mod_genre_dummies[, 
                         .(cor(.SD, use = "na.or.complete")), .SDcols = c(5, 9, 10, 13:38)][
                           V1 > 0.1 & V1 < 1]
```

ggplot(drop_na, aes(x = , y = )) +
  geom_point() +
  geom_smooth(method = "lm")



#b
```{r}
filmtv_mod_numeric <- filmtv_mod_genre_dummies[, c(5, 9, 10, 13:38)]
corr <- cor(filmtv_mod_numeric)
corr[corr > abs(0.15) & corr!= abs(1)]  #0.1930386 0.1690126 0.1930386 0.1690126
which(corr > abs(0.15) & corr!= abs(1))  #32  41  60 321
```

filmtv_mod_numeric[, .(corr = cor(.SD))][corr > abs(0.15) & corr!= abs(1)]




#c
reg <- lm(duration ~ ., data = filmtv)
summary(reg)

residuals <- data.table(residuals = reg$residuals, fitted = reg$fitted.values)
ggplot(residuals, aes(fitted, residuals)) +
  geom_point(alpha = .3) +
  theme_minimal()

data <- fread("data/winemag-data_first150k.csv")

summary(data)
cor(data$price, data$points, use = "na.or.complete")

data_1 <- fread("data/winemag-data-130k-v2.csv")
summary(data_1)

data_2 <- fread("data/20190928-reviews.csv")
