library(rio) 

rm(list=ls())
gc()

cpi95 <- import("CPI95.xlsx")



#initial cleaning

cpi95 <- cpi95[rowSums(is.na(cpi95)) < ncol(cpi95)-4, ] # remove rows with almost all missing value

cpi95 <- unique(cpi95[ , 1:ncol(cpi95)]) # remove duplicated rows

rownames(cpi95) <- cpi95[,"item"] # changes row names to item 

names(cpi95) <- gsub("/", "", names(cpi95)) # removes /


# preparing dataset for plotting time-series

cpi95t <- as.data.frame(t(cpi95))[-1,] # transposes the dataframe and excludes item row

library(dplyr)

cpi95t <- cpi95t %>%  # putting treated goods at the beginning for visualization purposes
          select("روغن حيواني",
                 "لوبيا قرمز",
                 "گوشت مــرغ",
                 "برنج خارجـــي",
                 "گوشت دام",
                 "تخم مرغ",
                 "انواع کــره",
                 "شکــر",
                 "روزنامه و مجله",
                 "انواع چاي",
                 "لاستيک اتومبيل سواري",
                 "محصولات دارويي",
                 everything())


library(lubridate)

cpi95t$date <- ym(rownames(cpi95t)) # a particular column for dates (x-axis) with date format

cpi95t <- cpi95t[c(ncol(cpi95t), 1:ncol(cpi95t)-1)] # reorders columns

library(data.table)

molten.cpi95t <- melt(setDT(cpi95t), id.vars=c("date"), variable.name = "commodity", value.name = "price") # melts dataset in order to plot automatically arbitrary commodities 

molten.cpi95t$price <- as.numeric(molten.cpi95t$price)


molten.cpi95t



# specifying pairs1

pair1 <- c("روغن نباتي جامد", "روغن حيواني")
 
pair2 <- c("لوبيا قرمز",
           "لوبيا چيتي")

pair3 <- c("گوشت مــرغ",
            "کنسرو ماهي")

pair4 <- c("برنج خارجـــي",
           "برنج ايراني درجه 1")

pair5 <- c("گوشت دام",
           "انواع کالباس")

pair6 <- c("تخم مرغ",
           "انواع شير")

pair7 <- c("انواع کــره",
           "انواع پنير")

pair8 <- c("شکــر",
           "نمک")

pair9 <- c("روزنامه و مجله",
           "دفتر و دفترچه")

pair10 <- c("انواع چاي",
           "انواع نوشابه")

pair11 <- c("لاستيک اتومبيل سواري",
           "لنت ترمز اتومبيل سواري")

pair12 <- c("محصولات دارويي",
           "لوازم طبي و درماني")

# pair 13

pairs <- list(pair1, pair2, pair3, pair4,
              pair5, pair6, pair7, pair8,
              pair9, pair10, pair11, pair12)



# plotting and saving the graph for each pair

library(ggplot2)

i <- 1
for (p in pairs){
plots <- molten.cpi95t %>%
  filter(commodity %in% p, date>ymd(13960101), date<ymd(13980101)) %>% # what and when
  group_by(commodity) %>% 
  mutate(std=sd(price)) %>%
  ungroup() %>% 
  ggplot() + 
  geom_line(aes(x=date, y=price, color=commodity)) + # type of graph 
  geom_ribbon(aes(x=date, y=price, ymin = price - 0.25*std, ymax = price + 0.25*std, fill = commodity), alpha = .2) +
  labs( #labels
    title = "parallel trends",
    x = "Date",
    y = "Price (1395-100)"
  ) +
  scale_x_date(date_breaks = "3 months") +  # x axis ticks
  geom_vline(xintercept = ymd(13970101)) +  # event of interest
  theme(plot.title = element_text(hjust = 0.5), legend.position = c(0.1, 0.85)) + # title and legend position
  ggtitle(i)
  
ggsave(plots, file=paste0("parallel_trends_", i,".png"), width = 14, height = 7)  # format and size
i <- i+1
  }
