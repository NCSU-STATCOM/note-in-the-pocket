library(ggplot2)
library(dplyr)
orders2019_df <- readRDS("intermediary_data/orders2019_df.rds")
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
orders2021_df <- readRDS("intermediary_data/orders2021_df.rds")
orders_df = rbind(orders2019_df, orders2020_df, orders2021_df)


recv_date = as.Date(orders_df$`Date Received`, format="%y-%m-%d")
fill_date = as.Date(orders_df$`Date Filled`, format="%y-%m-%d")
female = orders_df$gt != 0 & !is.na(orders_df$gt)

date_diff = fill_date - recv_date

plot_df = data.frame(recv_date, date_diff, female)

ggplot(plot_df, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, aes(color = female), alpha = 0.5) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time")  + 
  geom_vline(xintercept = as.Date("2020-03-15"))


plot_df2 = plot_df %>% filter(date_diff >= 5)
ggplot(plot_df2, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, aes(color = female), alpha = 0.5) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time")  + 
  geom_vline(xintercept = as.Date("2020-03-15"))


plot_df3 = plot_df %>% filter(date_diff <= 5)
ggplot(plot_df3, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, aes(color = female), alpha = 0.5) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time")  + 
  geom_vline(xintercept = as.Date("2020-03-15"))
