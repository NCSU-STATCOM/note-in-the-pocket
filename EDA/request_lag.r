library(ggplot2)
library(dplyr)
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")


recv_date = as.Date(orders2020_df$`Date Received`, format="%y-%m-%d")
fill_date = as.Date(orders2020_df$`Date Filled`, format="%y-%m-%d")

date_diff = fill_date - recv_date

plot_df = data.frame(recv_date, date_diff)
plot_df = plot_df %>% filter(date_diff > 0)

ggplot(plot_df, aes(x = recv_date, y = date_diff)) +
  geom_point(size=2) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time")
