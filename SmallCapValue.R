library(scales)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)

# Get data of returns from Shiller
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Read data from French
sv <- read.csv("6_Portfolios_2x3_Wout_Div.CSV",
               skip = 15)

# Keep only date and return column and calculate returns
sp500_to_join <- full_data %>% 
  select(Date = dates,
         SP500 = P) %>% 
  mutate(Date = ymd(paste0(Date, "-01")),
         SP500 = SP500 / lag(SP500))

sv_to_wrangle <- sv %>% 
  select(Date = X,
         SmallValue = SMALL.HiBM) %>% 
  mutate(Date = ymd(paste0(Date, "-01")),
         SmallValue = as.numeric(as.character(SmallValue)))

# Keep only the value weighted daily returns for the French data
sv_to_join <- sv_to_wrangle %>%
  slice(1:(first(which(is.na(sv_to_wrangle))) - 1)) %>% 
  mutate(SmallValue = SmallValue / 100 + 1)

# Join S&P 500 and small cap value returns
returns <- inner_join(sp500_to_join, sv_to_join) %>% 
  na.omit()

# Calculate returns for the two strategies without reinvested dividends
last(cumprod(returns$SP500)) ^
  (1/(last(year(returns$Date)) -
        first(year(returns$Date))))

last(cumprod(returns$SmallValue)) ^
  (1/(last(year(returns$Date)) -
        first(year(returns$Date))))

# Make xts for calculating drawdowns
sp500_xts <- xts(returns$SP500 - 1,
                 order.by = returns$Date)
names(sp500_xts) <- "SP500"

# Get information from drawdowns
drawdowns <- findDrawdowns(sp500_xts$SP500) %>% 
  sortDrawdowns()

# Put the information to data frame and keep only bear markets
bearmarkets <- data.frame(return = drawdowns$return,
                          from = returns$Date[drawdowns$from] - months(2),
                          trough = returns$Date[drawdowns$trough] - months(2),
                          to = returns$Date[drawdowns$to] - months(2),
                          length = drawdowns$length,
                          peaktotrough = drawdowns$peaktotrough) %>% 
  filter(return < -0.2)

# Nest date sequences for plotting
date_sequences <- bearmarkets %>% 
  group_by(return, from, trough, to, length, peaktotrough) %>% 
  summarise(dates = list(unique(seq.Date(ymd(unique(from)),
                                         ymd(unique(to)),
                                         by = "months"))))

# Get all dates of all bear markets
bear_dates <- data.frame(Date = unique(as.Date(unlist(date_sequences$dates))))

# Add the dates to the data frame
returns <- returns %>% 
  left_join(bear_dates)

# Get the beginning dates of all bear markets
beginning <- bearmarkets %>% 
  ungroup() %>% 
  select(from) %>% 
  mutate(bearstart = TRUE) %>% 
  group_by(from, bearstart) %>% 
  summarise(Date = list(unique(seq.Date(ymd(unique(from)),
                                        ymd(unique(from)) +
                                          years(10),
                                        by = "months")))) %>% 
  unnest()

# Add the dates to the data frame
# Notice there are duplicates in the data frame after this action
# due to overlapping bear markets
returns <- returns %>% 
  left_join(beginning)

# Get the trough dates of all bear markets
bottoms <- bearmarkets %>% 
  ungroup() %>% 
  select(trough) %>% 
  mutate(bottom = TRUE) %>% 
  group_by(trough, bottom) %>% 
  summarise(Date = list(unique(seq.Date(ymd(unique(trough)),
                                        ymd(unique(trough)) +
                                          years(10),
                                        by = "months")) +
                          months(2))) %>% 
  unnest()

# Add the dates to the data frame
returns <- returns %>% 
  left_join(bottoms)

# From the peaks ----------------------------------------------------------
# Calculate cumulative returns for each bear market
to_plot_peak <- returns %>% 
  select(Date, SP500, SmallValue, from) %>% 
  na.omit() %>% 
  group_by(from) %>% 
  mutate(SP500 = cumprod(SP500) - 1,
         SmallValue = cumprod(SmallValue) - 1)

# Format the peak dates into month names and years
to_plot_peak <- to_plot_peak %>% 
  mutate(Bear = paste(month(from, label = TRUE), year(from))) %>% 
  ungroup()

# Calculate average returns for the index and strategy
to_plot_peak %>% 
  group_by(Bear) %>% 
  summarise(SP500 = last(SP500),
            SmallValue = last(SmallValue)) %>% 
  ungroup() %>% 
  summarise(mean(SP500),
            mean(SmallValue))

# Make the peak dates to factor for ordering
to_plot_peak <- to_plot_peak %>% 
  mutate(Bear = factor(Bear, levels = unique(to_plot_peak$Bear))) %>% 
  pivot_longer(cols = SP500:SmallValue)

# Plot all bear markets from peaks in nominal values
ggplot(to_plot_peak, aes(x = Date, y = value)) +
  geom_line(aes(color = name)) +
  facet_wrap(~Bear, scales = "free") +
  geom_hline(yintercept = -0.2) +
  theme_minimal() +
  theme(legend.position = c(0.75, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(name = "Returns",
                      values = c("SP500" = "Black",
                                 "SmallValue" = "#7CAE00")) +
  ggtitle("Performance of small cap value in bear markets",
          subtitle = paste("Ten-year nominal returns from the peak,",
                           "black lines indicate bear markets")) +
  labs(caption = "Source: French, Shiller \n
Blog post at: databasedinvesting.blogspot.com") +
  xlab("") +
  ylab("")

# From the bottoms --------------------------------------------------------
# Calculate percentage returns for each bear market
to_plot_bottom <- returns %>% 
  select(Date, SP500, SmallValue, trough) %>% 
  na.omit() %>% 
  group_by(trough) %>% 
  mutate(SP500 = cumprod(SP500) - 1,
         SmallValue = cumprod(SmallValue) - 1)

# Format the bottom dates into month names and years
to_plot_bottom <- to_plot_bottom %>% 
  mutate(Bear = paste(month(trough, label = TRUE), year(trough))) %>% 
  ungroup()

# Calculate average returns for the index and strategy
to_plot_bottom %>% 
  group_by(Bear) %>% 
  summarise(SP500 = last(SP500),
            SmallValue = last(SmallValue)) %>% 
  ungroup() %>% 
  summarise(mean(SP500),
            mean(SmallValue))

# Make the bottom dates to factor for ordering
to_plot_bottom <- to_plot_bottom %>% 
  mutate(Bear = factor(Bear, levels = unique(to_plot_bottom$Bear))) %>% 
  pivot_longer(cols = SP500:SmallValue)

# Plot all bear markets from the bottoms in nominal values
ggplot(to_plot_bottom, aes(x = Date, y = value)) +
  geom_line(aes(color = name)) +
  facet_wrap(~Bear, scales = "free") +
  theme_minimal() +
  theme(legend.position = c(0.75, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0, lineheight = 0.5)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(name = "Returns",
                      values = c("SP500" = "Black",
                                 "SmallValue" = "#7CAE00")) +
  ggtitle("Performance of small cap value from bear market bottoms",
          subtitle = paste("Ten-year nominal returns from the bottom")) +
  labs(caption = "Source: French, Shiller \n
Blog post at: databasedinvesting.blogspot.com") +
  xlab("") +
  ylab("")
