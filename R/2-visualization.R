library(tidyverse)
library(ggplot2)

#2.1 užduotis

data = readRDS("../data/702200.rds")
png(file="../img/1_Grafikas.png",
    width=600, height=350)
h1=hist(data$avgWage, breaks = 200, main = paste("Vidutinio atlyginimo histograma"),
        xlab = "Atlyginimas", ylab ="Kiekis", col = 'red')
dev.off()

#2.2 užduotis

top5 = data %>%
  group_by(name)%>%
  summarise(top5imonės = max(avgWage))%>%
  arrange(desc(top5imonės))%>%
  head(5)
h2 = data%>%
  filter(name%in% top5$name) %>%
  mutate(Months=ym(month)) %>%
  ggplot(aes(x = Months, y = avgWage, color = name)) + geom_line()+
  labs(title = "Top 5 įmonės", x = "Mėnesiai", y  = "Vidutinis atlyginimas")
ggsave("../img/2_Grafikas.png", h2, width = 2400, height = 1200, units = ("px"))

#2,3 užduotis

apdrausti = data %>%
  filter(name %in% top5$name) %>%
  group_by(name)%>%
  summarise(DaugApdrausti=max(numInsured))%>%
  arrange(desc(DaugApdrausti))


apdrausti$name = factor(apdrausti$name, levels = apdrausti$name[order(apdrausti$DaugApdrausti, decreasing = T)])


h3 = apdrausti%>%
  ggplot(aes(x = name, y = DaugApdrausti, fill = name)) +geom_col()+
  labs(title = "Top 5 įmonės", x = "Mėnesiai", y  = "Vidutinis atlyginimas")
ggsave("../img/3_Grafikas.png", h3, width = 2400, height = 1200, units = ("px"))
