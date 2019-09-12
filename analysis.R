library(ggplot2)
library(data.table)

data_file <- "data/sales_sample.csv"
sales <- fread(data_file)

ggplot(sales, aes(x = purchase_date, y = sales_amount)) +
  geom_line()

