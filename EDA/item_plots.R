library(tidyverse)
library(ggpubr)
#orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
#orders2020_agg_df <- readRDS("intermediary_data/orders2020_agg_received.rds")
#orders2019_agg_df <- readRDS("intermediary_data/orders2019_agg_received.rds")
#orders2021_agg_df <- readRDS("intermediary_data/orders2021_agg_received.rds")
orders_agg_df <- readRDS("intermediary_data/orders_allyears_agg_received.rds")

### Rename
orders_agg_df <- orders_agg_df %>%
  rename(date_received = `Date Received`)
orders_agg_df$date_received <- lubridate::as_date(orders_agg_df$date_received)

### Months with the most requests received, requests filled.
recv_date <- lubridate::as_date(orders_agg_df$date_received)
fill_date <- lubridate::as_date(orders_agg_df$earliest_filled)
table(lubridate::month(recv_date))
table(lubridate::month(fill_date))

Table_recv <- table(lubridate::month(recv_date)) %>% 
  data.frame() %>% 
  rename(recv_date = Var1) %>%
  arrange(desc(Freq)) 
Table_fill <- table(lubridate::month(fill_date)) %>% 
  data.frame() %>% 
  rename(fill_date = Var1) %>%
  arrange(desc(Freq)) 

### Plot of items by date
items_List <- c("Acc",
                "gt", "gb", "gu", "gs", "gc",
                "bt", "bb", "bu", "bs", "bc",
                "wt", "wb", "wu", "ws", "wc",
                "mt", "mb", "mu", "ms", "mc")

for (item in items_List) {
  plot_item <- str_c(item, "_plot")
  assign(plot_item, 
         ggplot(data = orders_agg_df) +
           geom_point(aes_string(x = "date_received", y = item)))
}

# ggplot(data = orders_agg_df) +
#   geom_point(aes_string(x = "date_received", y = item)) +
#   scale_x_date(date_breaks = "1 month", 
#                limits = as.Date(c(min(orders_agg_df$date_received), 
#                                   max(orders_agg_df$date_received)), format="%m"))


pAcc <- ggarrange(Acc_plot)
pg <- ggarrange(gt_plot,
          gb_plot,
          gu_plot,
          gs_plot,
          gc_plot)
pb <- ggarrange(bt_plot,
                bb_plot,
                bu_plot,
                bs_plot,
                bc_plot)
pw <- ggarrange(wt_plot,
                wb_plot,
                wu_plot,
                ws_plot,
                wc_plot)
pm <- ggarrange(mt_plot,
                mb_plot,
                mu_plot,
                ms_plot,
                mc_plot)
pAcc
pg
pb
pw
pm
