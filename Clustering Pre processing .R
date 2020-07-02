x= Chipotle$top1
Visits_to_chipotle= Chipotle$patronage
Important_to_be_healthy= Chipotle$importanthealthy
Think_chipotle_is_healthy= Chipotle$chipotlehealthy
Think_chipotle_is_convienient= Chipotle$chipotleconvenient
Think_health_is_important= Chipotle$healthyimportanttome


library(lattice)
library(ggplot2)
dotplot(Visits_to_chipotle~x)
dotplot(x2~x1)
dotplot(x2~x3)
dotplot(x2~x4)

str(Chipotle)
clustering_segment=data_frame(Chipotle)
names(Chipotle)
clustering_data1= Chipotle %>% 
  filter(patronage==0) %>% 
  filter(chipotlehealthy>2) %>% 
  filter(importanthealthy>2)
  
  

What_do_people_think_of_chipotle= data.frame(chipotleambience,chipotleconvenient, chipotlehealthy, chipotleprice, chipotletaste,chipotlevariety)
boxplot(What_do_people_think_of_chipotle)

What_do_people_want_in_a_restaurant= data.frame(importantambience,importantconvenience,importanthealthy,importantprice,importanttaste,importantvariety)
boxplot(What_do_people_want_in_a_restaurant)

self_perception= data.frame(plan,spending, buylocal, healthyimportanttome)
boxplot(self_perception)


ggplot(Chipotle, aes(x=Think_health_is_important, y=Think_chipotle_is_healthy))+
    geom_count()

