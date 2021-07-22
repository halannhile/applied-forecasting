
library(fpp3)

# Question 1  

gafa_stock

gafa_stock %>% 
  autoplot(Close)


gafa_stock %>% 
  filter(Close == max(Close))

gafa_stock

# Question 2

tute1 <- readr::read_csv("tute1.csv")
View(tute1)

my_time_series_1 <- tute1 %>%
  mutate(Quarter = yearquarter(Quarter)) %>%
  as_tsibble(index = Quarter)
my_time_series_1

my_time_series_1 %>% 
  pivot_longer(-Quarter, names_to = "Key", values_to = "Value") %>% 
  ggplot(aes(x = Quarter, y = Value, colour = Key)) + 
  geom_line() + 
  facet_grid(vars(Key), scales = "free_y") 

# Question 2 

tourism_timeseries <- readxl::read_excel("tourism.xlsx") %>% 
  mutate(Quarter = yearmonth(Quarter)) %>% 
  as_tsibble(index = Quarter, 
             key = c(State, Region, Purpose))
tourism_timeseries


tourism_3 <- tourism_timeseries %>% 
  select(Quarter, Region, Purpose, Trips) %>%
  pivot_wider(names_from = c('Region', 'Purpose'), values_from = 'Trips') 

tourism_4 <- tourism_3 %>% 
  select(-Quarter) 
tourism_4

 str(sort(colSums(tourism_4[,1:length(tourism_4)]), decreasing = TRUE)[1])

tourism_timeseries %>% 
  as_tibble() %>% 
  group_by(Region, Purpose) %>% 
  summarise(total_trips_by_region_purpose = sum(Trips)) %>% 
  arrange(desc(total_trips_by_region_purpose))

tourism_timeseries

tourism_state <- tourism_timeseries %>% 
  group_by(State) %>% 
  summarise(total_trips_by_state = sum(Trips))
tourism_state

tourism_state_colsum <- tourism_state %>% 
  pivot_wider(names_from = State, values_from = total_trips_by_state) %>% 
  select(-Quarter) %>% 
  colSums() 
tourism_state_colsum

tourism_timeseries %>% 
  as_tibble() %>% 
  group_by(State) %>% 
  summarise(total_trips_by_state = sum(Trips)) %>% 
  arrange(desc(total_trips_by_state))

# Question 4

aus_production

bricks_plot <- aus_production %>% 
  autoplot(Bricks)
bricks_plot

lynx_plot <- pelt %>% 
  autoplot(Lynx)
lynx_plot

close_plot <- gafa_stock %>% 
  autoplot(Close)
close_plot

demand_plot <- vic_elec %>% 
  autoplot(Demand) + 
  ggtitle("Half-hourly total electricity demand in Melbourne, Victoria") + 
  ylab("Total electricity demand") + 
  xlab("Year")
  
demand_plot

# Question 5: 

aus_arrivals <- aus_arrivals %>% 
  mutate(Quarter = yearquarter(Quarter))
aus_arrivals

aus_autoplot <- aus_arrivals %>%
  autoplot(Arrivals) 
aus_autoplot

aus_season <- aus_arrivals %>% 
  mutate(Arrivals = Arrivals/1e3) %>% 
  gg_season(Arrivals) + 
  ggtitle("Seasonal plot: International Arrivals to ")
aus_season

aus_subseries <- aus_arrivals %>% 
  gg_subseries(Arrivals)
aus_subseries

# Question 6: 

set.seed(30100259) 
my_aus_retail_timeseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1)) 
my_aus_retail_timeseries

my_aus_retail_timeseries %>% 
  autoplot(Turnover)

my_aus_retail_timeseries %>% 
  gg_season(Turnover) 

my_aus_retail_timeseries %>% 
  gg_subseries(Turnover)

my_aus_retail_timeseries %>% 
  filter(year(Month) >= 2013) %>% 
  gg_lag(Turnover, geom = "point")

my_aus_retail_timeseries %>% 
  ACF(Turnover, lag_max = 9) %>% 
  autoplot()

# Question 10:

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))
dgoog


dgoog %>% select(diff, trading_day) %>% ACF(diff) %>% autoplot()

