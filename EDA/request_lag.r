library(ggplot2)
library(dplyr)
library(lubridate)
orders2019_df <- readRDS("intermediary_data/orders2019_df.rds")
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
orders2021_df <- readRDS("intermediary_data/orders2021_df.rds")
orders_df = rbind(orders2019_df, orders2020_df, orders2021_df)


recv_date = as.Date(orders_df$`Date Received`, format="%y-%m-%d")
recv_date_y = year(recv_date)
recv_date_md = recv_date
year(recv_date_md) = 2000

fill_date = as.Date(orders_df$`Date Filled`, format="%y-%m-%d")
female = (orders_df$gt != 0 | orders_df$wt != 0) & !is.na(orders_df$gt) 

date_diff = fill_date - recv_date

plot_df = data.frame(recv_date, recv_date_y, recv_date_md, date_diff, female)

ggplot(plot_df, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, aes(color = female), alpha = 0.5) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time")  + 
  geom_vline(xintercept = as.Date("2020-03-15")) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y")  + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 90)) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"))



plot_df2 = plot_df %>% filter(date_diff >= 5)
ggplot(plot_df2, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, aes(color = female), alpha = 0.5) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time > 5 days")  + 
  geom_vline(xintercept = as.Date("2020-03-15")) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y")  + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 90)) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"))

plot_df3 = plot_df %>% filter(female)
ggplot(plot_df3, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, alpha = 0.5, color = "red") +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment for females")  + 
  geom_vline(xintercept = as.Date("2020-03-15")) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y")  + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 90))

plot_df4 = plot_df %>% filter(!female)
ggplot(plot_df4, aes(x = recv_date, y = date_diff, color)) +
  geom_point(size=2, alpha = 0.5, color = "blue") +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment for males")  + 
  geom_vline(xintercept = as.Date("2020-03-15")) +
  scale_x_date(date_breaks="1 month", date_labels="%B-%Y")  + 
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 90))


# stack everything on year
ggplot(plot_df, aes(x = recv_date_md, y = date_diff, color)) +
  geom_point(size=2, aes(color = recv_date_y), alpha = 0.25) +
  ylab("Date differential") +
  xlab("Date Received") + 
  ggtitle("Request fulfillment lag time stacked monthly")  + 
  scale_x_date(date_breaks="1 month", date_labels="%B")  +
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 12, angle = 90)) + 
  geom_smooth() + 
  scale_color_manual(values = c("2019" = "blue", "2020" = "red", "2021" = "orange"))


