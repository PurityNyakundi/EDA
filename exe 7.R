require(ggplot2)
require(nycflights13)
require(dplyr)
summary(select(diamonds,x,y,z))
ggplot(data = diamonds)+
  geom_histogram(mapping=aes(x = x),binwidth = 0.5)
ggplot(data = diamonds)+
  geom_histogram(mapping=aes(x = y),binwidth = 0.5)
ggplot(data = diamonds)+
  geom_histogram(mapping=aes(x = z),binwidth = 0.5)
#observation x and y are large than z,these variables contain outliers also there might be data errors
#in collection of data these is shown by the big values of x y and z
diamonds%>%
  arrange(desc(z))%>%
  head()
ggplot(diamonds, aes(x = x, y = y)) +
  geom_point()
ggplot(filter(diamonds,price<2500),mapping = aes(x = price))+
  geom_histogram(binwidth = 0.1)
diamonds%>%
  filter(carat==0.99|carat ==1)%>%
  count(carat)
# observation is that the 1 carat is more times than the 0.99 carat by 70 
#it seems they rounded off values
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = price)) +
  xlim = c(100, 5000)+ 
  ylim = c(0, 3000)
#the unset of binwidth is not affected since before it zooms it is already calculated
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))%>%
  #arrange(diamonds2,NA)
print(diamonds2)
ggplot(diamonds2, aes(x = y)) +
  geom_histogram()
#observation that it removes 9 rows contain non infite values
diamonds %>%
  mutate(cut = if_else(runif(n()) < 0.1, NA_character_, as.character(cut))) %>%
  ggplot() +
  geom_bar(mapping = aes(x = cut))
sum(c(1,3,4,NA),na.rm = TRUE)
mean(c(4,5,6,NA),na.rm = TRUE)
#observation it removes the NA before calculating
ggplot(data = diamonds, mapping = aes(x = price)) + 
  geom_freqpoly(mapping = aes(colour = cut), binwidth = 500)
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))
# the best variable for predicting the price is the carat
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))
ggplot(diamonds, aes(x = cut, y = carat)) +
  geom_boxplot()
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = clarity, y = price))
#observation carat is the bset predictor for price
ggplot(data = diamonds) +
  geom_boxplot(mapping = aes(x = carat, y = price))
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
library("ggstance")

ggplot(data = mpg) +
  geom_boxploth(mapping = aes(y = reorder(class, hwy, FUN = median), x = hwy))
ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 1, scales = "free_y")
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin() +
  coord_flip()
ggplot(data = mpg) +
  geom_quasirandom(mapping = aes(
    x = reorder(class, hwy, FUN = median),
    y = hwy
  ))
#exe 7.5.2.1
diamonds %>%
  count(color, cut) %>%
  group_by(cut) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop))+
#limits = c(0,1)
  flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile()
  #labs(x = "Month", y = "Destination", fill = "Departure Delay")
diamonds %>%
  count(cut,color) %>%
  ggplot(mapping = aes(y = color, x = cut)) +
  geom_tile(mapping = aes(fill = n))
ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)),varwidth = TRUE)
ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")
ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_hex() +
  facet_wrap(~cut, nrow = 5)
ggplot(diamonds, aes(colour = cut_number(carat, 5), y = price, x = cut)) +
  geom_boxplot()
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))

