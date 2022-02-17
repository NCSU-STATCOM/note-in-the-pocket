library(tidyverse)
orders2020_df <- readRDS("intermediary_data/orders2020_df.rds")
orders2020_agg_df <- readRDS("intermediary_data/orders2020_agg_received.rds")

### Months with the most requests received, requests filled.
recv_date <- lubridate::as_date(orders2020_df$`Date Received`)
fill_date <- lubridate::as_date(orders2020_df$`Date Filled`)
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
cbind(Table_recv, Table_fill)

### Plot of items by date
items_List <- c("gt", "gb", "gu", "gs", "gc",
                "bt", "bb", "bu", "bs", "bc",
                "wt", "wb", "wu", "ws", "wc",
                "mt", "mb", "mu", "ms", "mc")

for (item in items_List) {
  plot_item <- str_c(item, "_plot")
  assign(plot_item, 
         ggplot(data = orders2020_agg_df) +
           geom_point(aes_string(x = "`Date Received`", y = item)))
}

gt_plot
gb_plot
gu_plot
gs_plot
gc_plot
