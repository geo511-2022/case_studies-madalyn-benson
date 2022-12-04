library(ggplot2)
library(gapminder) 
library(dplyr)
 
gapminder
gapminder_without_Kuwait <- gapminder %>% 
  filter(country != "Kuwait")
gapminder_without_Kuwait

plot1 <- ggplot(gapminder_without_Kuwait, aes(x=lifeExp, y= gdpPercap))+
  geom_point(aes(color=continent, size=pop/100000)) +
  scale_y_continuous(trans="sqrt")+
  facet_wrap(~year,nrow=1)+
  theme_bw()+
  labs(x = "Wealth and life expectancy through time",y="GDP per capita", size="Population (100k)")

plot1

ggsave("plot1.png", width=15, units="in")

gapminder_continent <- gapminder_without_Kuwait %>%
  group_by(continent,year) %>%
  filter(gdpPercap<=50000) %>%
  summarize(gdpPercapweighted = weighted.mean(x = gdpPercap,w = pop),pop = sum(as.numeric(pop)))

plot2 <- ggplot(gapminder_without_Kuwait, aes(x=year, y= gdpPercap))+
  geom_line(aes(color=continent,group=country)) +
  geom_point(aes(color=continent, group=country))+
  geom_line(data=gapminder_continent,aes(x=year,y=gdpPercapweighted)) + 
  geom_point(data=gapminder_continent,aes(x=year,y=gdpPercapweighted, size=pop/100000)) +
  facet_wrap(~continent,nrow=1)+
  theme_bw()+
  labs(x="Wealth and population distribution through time", y="GDP per capita", size="Population (100k)", color="Contintent")
plot2
ggsave("plot2.png", width=15, units="in")
