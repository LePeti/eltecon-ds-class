library(data.table)
library(ggplot2)
library(magrittr)
library(purrr)

options(datatable.print.class = TRUE)

fb <- fread("data/dataset_Facebook.csv")
summary(fb)
head(fb)

colnames(fb)
#A szókozök gondot okoznak a datatableben, ezeket _-vel helyettesítem
colnames(fb) <- gsub(" ", "_", colnames(fb))
#Néhány változót nagybetűvel írtak
colnames(fb) <- tolower(colnames(fb))


ggplot(fb, aes(page_total_likes)) + geom_histogram(bins = 10)
#nincs kiugró érték

ggplot(fb, aes(like)) + geom_histogram(bins = 10)
fb[like > 1000][order(-like)]
#5172 nagyon kiugró

clearOutlierLike <- function(fb) {
  fb[like > 2000, like := NA]
}


ggplot(fb, aes(share)) + geom_histogram(bins = 10)
fb[share > 100][order(-share)]
#790 kiugró

clearOutlierShare <- function(fb) {
  fb[share > 700, share := NA]
}


ggplot(fb, aes(comment)) + geom_histogram(bins = 10)
fb[comment > 60][order(-comment)]
#372 kiugró, 3 db >100

clearOutlierComment <- function(fb) {
  fb[comment > 300, comment := NA]
}

#A hisztogramokról látható, hogy nagyon nagy a szórás.

clearOutlierLike(fb)
clearOutlierShare(fb)
clearOutlierComment(fb)


#total_interactions a "comment", a "like", és a "share" összege. mivel ez nem
#függvényként van megadva (az új NA-kat nem kezeli), ezt is módosítani kell

fb[, total_interactions := comment + like + share]

fb[, lapply(.SD, uniqueN)] #nincsenek duplikátumok

# 3. házi
# fizetett tartalomnak van-e hatása a likeok számára
mean_like_dt <- fb[, 
              .(mean_like = mean(like, na.rm = TRUE), var_like = var(like, na.rm = TRUE)), 
              by = paid]

mean_like_dt <- mean_like_dt[1:2, ] #NA-k kiszűrése

mean_like_dt %>%
  .[, CI_lower := mean_like - (1.96 * sqrt(var_like))] %>%
  .[, CI_higher := mean_like + (1.96 * sqrt(var_like))] %>%
  ggplot(aes(x = paid, y = mean_like)) +
  geom_col() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_higher))

#Nem szignifikáns a különbség

#Monte-Carlo
set.seed(1234)
#mintánként számolja ki az átlagot
mean_like_mc_samples <- map_df(1:1000, ~{
                mean_like_dt[,
               .(samples = rnorm(500, mean_like, var_like)),
                 by = paid
  ] %>% .[
    , mean(samples), by = paid
  ]
})
mc_sample = 1:500
mean_like_mc_samples <- like_mc_samples[, mean(s), by = paid]


mean_like_mc_samples_cast <- dcast(
  mean_like_mc_samples, mc_sample ~ paid, value.var = "mean_like_mc"
)

colnames(mean_like_mc_samples_cast)[3] = "not_paid"
colnames(mean_like_mc_samples_cast)[4] = "paid"

uplift_from_mc_samples <- copy(mean_like_mc_samples_cast) %>%
  .[, uplift := paid / not_paid - 1]

ggplot(uplift_from_mc_samples, aes(x = uplift)) +
  geom_histogram(bins = 10) +
  geom_vline(
    xintercept = uplift_from_mc_samples[, mean(uplift)],
    color = "blue"
  ) +
  geom_vline(
    xintercept = uplift_from_mc_samples[, quantile(uplift, 0.025, na.rm = TRUE)],
    color = "red"
  ) +
  geom_vline(
    xintercept = uplift_from_mc_samples[, quantile(uplift, 0.975, na.rm = TRUE)],
    color = "red"
  )

#Bootstrapping
set.seed(1)
single_sample <- fb[sample(.N, .N, replace = TRUE)] %>%
  .[,
    .(bootstrap_id = 1,
      mean_like = mean(like)),
    by = paid
    ] %>%
  dcast(bootstrap_id ~ paid, value.var = "mean_like")
  
  colnames(single_sample)[3] = "not_paid"
  colnames(single_sample)[4] = "paid"
  
  single_sample[, uplift := paid / not_paid - 1]

ggplot() +
  geom_histogram(
    data = single_sample,
    mapping = aes(x = uplift),
    fill = "red"
  ) +
  xlim(c(-0.12, 0.5))

set.seed(1234)
bootstrapped_stats <- map_df(1:10000, ~{
  fb[sample(.N, .N, replace = TRUE)] %>%
    .[,
      .(bootstrap_id = .x,
        mean_like = mean(like)),
      by = paid
      ]
})

bs_uplift <- dcast(bootstrapped_stats, bootstrap_id ~ paid, value.var = "mean_like")

colnames(bs_uplift)[3] = "not_paid"
colnames(bs_uplift)[4] = "paid"

bs_uplift[, uplift := paid / not_paid - 1]

#bs_uplift %>% .[]

ggplot(bs_uplift, aes(x = uplift)) + geom_histogram()

CI_from_bs <- bs_uplift[, .(
  CI_lower = quantile(uplift, 0.025, na.rm = TRUE),
  CI_higher = quantile(uplift, 0.975, na.rm = TRUE)
)]
CI_from_bs

ggplot(bs_uplift, aes(x = uplift)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = CI_from_bs[, CI_lower], color = "red") +
  geom_vline(xintercept = CI_from_bs[, CI_higher], color = "red")

