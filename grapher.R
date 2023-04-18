library(tidyverse)
library(readxl)
library(forcats)
library(ggthemes)

raw_data <- read_excel("data.xlsx",sheet = "data")

#Data from https://www.health.harvard.edu/diseases-and-conditions/glycemic-index-and-glycemic-load-for-100-foods
clean_data <- raw_data %>%
  mutate(
    Glycemic_index = str_replace_all(`Glycemic Index`,
                                 pattern =  "(?s) .*",
                                 replacement = "") %>%
      as.numeric(),
    Glycemic_index_range = str_replace_all(`Glycemic Index`,
                                           pattern =  ".* (?s)",
                                           replacement = "")%>%
      as.numeric()
  ) %>%
  mutate(Glycemic_index_low = Glycemic_index - Glycemic_index_range,
         Glycemic_index_high = Glycemic_index + Glycemic_index_range) %>%
  mutate(Food = fct_reorder(Food,-Glycemic_index)) %>%
  mutate(`Ice cream` = (Food == "Ice cream"))

g <- ggplot(clean_data, 
       aes(x=Food,y=Glycemic_index,fill = Category,color = `Ice cream`))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin = Glycemic_index_low,
                ymax = Glycemic_index_high)) +
  scale_fill_solarized()+
  scale_color_manual(values = c("black","yellow"),guide="none")+
  coord_flip()+
  xlab("")+
  theme(legend.title = element_blank(),
        legend.position = c(0.87, 0.85))+
  ylab("Glycemic Index")

  ggsave(g,
         filename = "Glycemic Index Graph.png",
          width = 6,
         height = 8,
         units = "in")

