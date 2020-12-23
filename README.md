---
title: Homicide Report
author: Kheirallah Samaha
date: March 29, 2017
output:
  prettydoc::html_pretty:
    theme: architect
    highlight: github
---

Hello,

Welcome to my new Kernel about **Homicide Report** created for Kaggelers by the Kaggel Team member Abigail Larion. I do not promise doing so much things in this Kernel, as I am usually do some Data summary with exploratory plots, so to figure out the aim behind the data-set and ask a simple question that how those features can help me to get the target of finding a serial killer -if any- hiding between the rows and fields...


Although it is hard to achieve the said target and knowing that the process of features engineering has an essential rule here, I will try to do so, cautiously of course, since i am not a detective or a police officer and have no experience in doing crime investigation...

**Note:** you can see the codes by clicking on the button located at upper right corner

Let's load the data and the `libraries` that we are going to use in this kernel.

  - dplyr
  - forcats
  - ggplot2
  - gridExtra
  - Hmisc

```{r setup, include=TRUE, warning=FALSE, message=FALSE}
# Libraries

warn.conflicts = FALSE

suppressMessages(library(dplyr))
suppressMessages(library(forcats))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(Hmisc))

```

```{r}
homi.r <- read.csv("C:/Users/kheirallah.samaha/Desktop/Rscripts/Homicide_Reports_1980-2014/database.csv")
suppressMessages(attach(homi.r))
options(warn=-1)

```

## Summary

I am going to start by summarizing the data-set to understand it and then i will do filtering and grouping 

```{r}
str(homi.r)

```

```{r}
dim(homi.r)
```

```{r}
anyDuplicated(homi.r$Record.ID)
```

```{r}
names(homi.r)
```

**I think it is better to change the names to lower case.**


```{r}
names(homi.r) <- tolower(names(homi.r))

names(homi.r)

```


**Question :** what incident feature stands for in here? ...the minimum is **0** and the maximum is **999** incidents, considering that the age and the sex features are referring to one person... are they the incidents generated or affected by the main incident?

Although i believe it is a very important question, i will skip it for now.

Let' go ahead

```{r}
sapply(homi.r[1,],class)

```

```{r}
length(homi.r$crime.solved[homi.r$crime.solved == "Yes"])

names(homi.r) <- tolower(names(homi.r))
homi.r$victim.age[homi.r$victim.age==998]<-98

homi.r$perpetrator.age[is.na(homi.r$perpetrator.age)]<- 1


```

448172 / 638454 = 0.70  realy nice 70 %

```{r}
levels(homi.r$crime.solved)
```

No missing data , Great!



## Filtering

There are many ways to do filtering and many packages can make it even easier.

strangely, I do not know why I'm interesting in knowing Who Killed Who at this stage. Let's see..

I will create new Data frame and named homi.r.solved, so i will work with the solved crime first.

1. Filtring by relationship:

  + Girlfriend and Boyfriend,
  + Familicide,
  + Ex-Husband and Ex-Wife,
  + Weapon,
  + Employee and Employer,
  + Friends

2. Filtring by Years and states:

  + Years
  + States

Ok! I think I will create new data frame filtering the crime solved to "YES", i want to go from what we know to what we don't

```{r}
homi.r.solved <- homi.r %>% filter(
  crime.solved    == "Yes" &
    victim.sex      != "Unknown" &
    perpetrator.sex != "Unknown" &
    relationship    != "Unknown"
)%>%
  droplevels()


##############################################

girl.boy.crime<-homi.r.solved %>% filter(
                                    relationship    %in% c("Girlfriend","Boyfriend") &
                                    victim.age      >= 18 &
                                    perpetrator.age >= 18 &
                                    victim.sex      != "Unknown" &
                                    perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,victim.race,victim.ethnicity,perpetrator.sex,
         perpetrator.age,perpetrator.race,perpetrator.ethnicity,relationship,weapon,record.source)

# table(girl.boy.crime$victim.sex)
# Female   Male 
#   7468   3619
# table(girl.boy.crime$perpetrator.sex)
# Female   Male 
#   3460   7627

# something wrong !!!

girl.boy.crime<-girl.boy.crime[!(girl.boy.crime$victim.sex==girl.boy.crime$perpetrator.sex),]

# table(girl.boy.crime$victim.sex)
# Female   Male 
# 7434   3426 
# table(girl.boy.crime$perpetrator.sex)
# Female   Male 
# 3426   7434 

# now it is OK 

############################################

# Who killed Who (Familicide). Unfortunately !


homi.r.solved <- homi.r.solved %>% 
  mutate(who.killed.who = case_when(.$relationship=="Brother" &
                                      .$perpetrator.sex =="Male" ~ "Brother Killed by Brother",
                                    .$relationship=="Brother" & 
                                      .$perpetrator.sex =="Female" ~ "Brother Killed by Sister",
                                    .$relationship=="Sister" & 
                                      .$perpetrator.sex =="Female" ~ "Sister Killed by Sister",
                                    .$relationship=="Sister" & 
                                      .$perpetrator.sex =="Male" ~ "Sister Killed by Brother",
                                    .$relationship=="Mother" & 
                                      .$perpetrator.sex =="Male" ~ "Mother Killed by Son",
                                    .$relationship=="Mother" & 
                                      .$perpetrator.sex =="Female" ~ "Mother Killed by Sister",
                                    .$relationship=="Son" & 
                                      .$perpetrator.sex =="Female" ~ "Son Killed by Mother",
                                    .$relationship=="Son" & 
                                      .$perpetrator.sex =="Male" ~ "Son Killed by Father",
                                    .$relationship=="Daughter" & 
                                      .$perpetrator.sex =="Female" ~ "Daughter Killed by Mother",
                                    .$relationship=="Daughter" & 
                                      .$perpetrator.sex =="Male" ~ "Daughter Killed by Father",
                                    .$relationship=="Wife" & 
                                      .$perpetrator.sex =="Male" ~ "Wife Killed by Husband",
                                    .$relationship=="Husband" & 
                                      .$perpetrator.sex =="Female" ~ "Husband Killed by wife",
                                    .$relationship=="Father" & 
                                      .$perpetrator.sex =="Female" ~ "Father Killed by Daughter",
                                    .$relationship=="Father" & 
                                      .$perpetrator.sex =="Male" ~ "Father Killed by Son",
                                    TRUE~"UKN"))

# Im sure there is a simpler way to do the same, but really i do not have Time ..!

ex.husband.ex.wife.crime<-homi.r.solved %>% filter(
                                            relationship     %in% c("Ex-Husband","Ex-Wife") &
                                            victim.age       >= 18 &
                                            perpetrator.age  >= 18 &
                                            victim.sex      != "Unknown" &
                                            perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

# there are Female in both features victim .sex and perpetrator.sex ..and Male as well which is wrong..!
# so I consider that the victim sex to be my reference because it is the subject of this dataset.
ex.husband.ex.wife.crime$relationship <- ifelse(ex.husband.ex.wife.crime$victim.sex=="Male",
                                                "Ex-Husband","Ex-Wife")
# I think there was some typo here 

# OK let's move on

ex.husband.ex.wife.crime$older.or.younger <- ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Husband" &
                                                      ex.husband.ex.wife.crime$perpetrator.age < ex.husband.ex.wife.crime$victim.age,
                                                    "Ex-wife Killed an old Ex-Husband",
                                                    ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Husband" &
                                                             ex.husband.ex.wife.crime$perpetrator.age > ex.husband.ex.wife.crime$victim.age,
                                                           "Ex-wife Killed a young Ex-Husband",
                                                           ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Wife" &
                                                                    ex.husband.ex.wife.crime$perpetrator.age > ex.husband.ex.wife.crime$victim.age,
                                                                  "Ex- Husband Killed a young Ex-Wife",
                                                                  ifelse(ex.husband.ex.wife.crime$relationship=="Ex-Wife" &
                                                                           ex.husband.ex.wife.crime$perpetrator.age < ex.husband.ex.wife.crime$victim.age,
                                                                         "Ex-Husband Killed an old Ex-Wife","Smae Age"))))
ndf.by.weapon <- homi.r.solved %>% filter(
                                  weapon != "Unknown" &
                                  victim.sex != "Unknown"&
                                  perpetrator.sex != "Unknown" &
                                  victim.age       >= 18 &
                                  perpetrator.age  >= 18
) %>% 
  droplevels()%>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)


```

```{r}
homi.r.solved$who.killed.who.sex <- ifelse(homi.r.solved$perpetrator.sex=="Female"& homi.r.solved$victim.sex=="Male",
                                    "Male Killed by Female",
                                    ifelse(homi.r.solved$perpetrator.sex=="Male"& homi.r.solved$victim.sex=="Female",
                                          "Female Killed by Male",
                                          ifelse(homi.r.solved$perpetrator.sex =="Male" & homi.r.solved$victim.sex == "Male",
                                                 "Male Killed by Male",
                                                 ifelse(homi.r.solved$perpetrator.sex =="Female" & homi.r.solved$victim.sex == "Female",
                                                        "Female Killed by Female", "UNK"))))

```
```{r}
employee.employer.crime<-homi.r.solved %>% filter(
                                           relationship     %in% c("Employee","Employer") &
                                           victim.age       >= 18 &
                                           perpetrator.age  >= 18 &
                                           victim.sex      != "Unknown" &
                                           perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon,who.killed.who.sex)
friend.crime<-homi.r.solved %>% filter(
                                relationship       == "Friend" &
                                victim.sex      != "Unknown" &
                                victim.age       >= 18 &
                                perpetrator.age  >= 18 &
                                perpetrator.sex != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

friend.crime$who.killed.who.Friend.sex <- ifelse(friend.crime$perpetrator.sex == "Female" & friend.crime$victim.sex == "Male",
                                                 "Friends Male Killed by Female",
                                                 ifelse(friend.crime$perpetrator.sex == "Male"& friend.crime$victim.sex == "Female",
                                                        "Friends Female Killed by Male",
                                                        ifelse(friend.crime$perpetrator.sex == "Male" & friend.crime$victim.sex == "Male",
                                                               "Friends Male Killed by Male",
                                                               ifelse(friend.crime$perpetrator.sex == "Female" & friend.crime$victim.sex == "Female",
                                                                      "Friends Female Killed by Female", "UNK"))))


by.race <-homi.r.solved %>% filter(
    victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.race      != "Unknown" &
    perpetrator.race != "Unknown"
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.race,perpetrator.race,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon)

by.weapon <- summarise(group_by(ndf.by.weapon,weapon),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

by.weapon.sex <- summarise(group_by(ndf.by.weapon,weapon,victim.sex),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

ndf.by.weapon$sex.weapon.used <- sprintf("%s Killed by %s Using a %s",
                                         ndf.by.weapon$victim.sex,
                                         ndf.by.weapon$perpetrator.sex,
                                         ndf.by.weapon$weapon)


by.ndf.by.weapon.used <- summarise(group_by(ndf.by.weapon,sex.weapon.used),
                                   freq.by.weapon.used =n())%>%
  arrange(desc(freq.by.weapon.used))


ndf.by.weapon.geom.points <-ndf.by.weapon %>% filter(
    sex.weapon.used   %in% c("Male Killed by Male Using a Handgun",
                          "Male Killed by Male Using a Knife",
                          "Female Killed by Male Using a Handgun",
                          "Male Killed by Male Using a Blunt Object",
                          "Female Killed by Male Using a Knife")
) %>%
  droplevels() %>%
  select(record.id, agency.type,city,state,year,month,crime.type,
         crime.solved,victim.sex,victim.age,perpetrator.sex,
         perpetrator.age,relationship,weapon,sex.weapon.used)


homi.r.solved.age <- homi.r.solved %>% filter(
    victim.age       >= 18 &
    perpetrator.age  >= 18 &
    victim.race      != "Unknown" &
    perpetrator.race != "Unknown"
) %>%
  droplevels()



homi.r.solved.age$vic.age.group <- cut(homi.r.solved.age$victim.age, 
                       breaks = c(18, 30, 40, 50,60, 70, 80, 90, 100), 
                       labels = c("18-30 yrs", "30-40 yrs",
                                  "40-50 yrs", "50-60 yrs","60-70 yrs","70-80 yrs",
                                  "80-90 yrs","90-100 yrs"),
                       right = FALSE)

homi.r.solved.age$per.age.group <- cut(homi.r.solved.age$perpetrator.age, 
                               breaks = c(18, 30, 40, 50,60, 70, 80, 90, 100), 
                               labels = c("18-30 yrs", "30-40 yrs",
                                          "40-50 yrs", "50-60 yrs","60-70 yrs","70-80 yrs",
                                          "80-90 yrs","90-100 yrs"),
                               right = FALSE)

```


If you ask me why i have filtered the data frame by Age to be 18+, i would say that I found some typo here.

there were many observations from 0 up to 13 years age which is wired! so i think the relationship for those observations could be step-mother or step-father or it is just typo.

more over i found perpetrator .age with 0 age which i think it is Unknown but they put 0 as a number. Maybe?

It need more investigation.

## Summarizing and grouping

  1. by:
    + by year
    + by month
    + by family
    + by state
    + by weapon
    + by Employee and Employer
    + by race

```{r}
by.year <- summarise(group_by(homi.r.solved,year),freq.year =n())%>%
  arrange(desc(freq.year)) 

by.month <- summarise(group_by(homi.r.solved,month),freq.month =n())%>%
  arrange(desc(freq.month))


by.family <- summarise(group_by(homi.r.solved[homi.r.solved$who.killed.who!="UKN", ],who.killed.who),total.number.re =n())%>%
    arrange(desc(total.number.re))

by.state <- summarise(group_by(homi.r.solved,state),freq.by.state =n())%>%
  arrange(desc(freq.by.state))


by.weapon <- summarise(group_by(ndf.by.weapon,weapon),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

by.weapon.sex <- summarise(group_by(ndf.by.weapon,weapon,victim.sex),freq.by.weapon =n())%>%
  arrange(desc(freq.by.weapon))

ndf.by.weapon$sex.weapon.used <- sprintf("%s Killed by %s Using a %s",
                                         ndf.by.weapon$victim.sex,
                                         ndf.by.weapon$perpetrator.sex,
                                         ndf.by.weapon$weapon)


by.ndf.by.weapon.used <- summarise(group_by(ndf.by.weapon,sex.weapon.used),
                                   freq.by.weapon.used =n())%>%
  arrange(desc(freq.by.weapon.used))

```
```{r}
empyr.empee.sex <- employee.employer.crime %>%
                                                group_by(victim.sex, 
                                                perpetrator.sex,
                                                who.killed.who.sex,
                                                 relationship) %>%
                  summarise(sex.freq = n()) %>%
                  arrange(victim.sex, perpetrator.sex)

by.p.race.group <- summarise(group_by(by.race,perpetrator.race,victim.race),total.by.race = n()) %>%
  arrange(total.by.race)


by.p.race.group$who.killed.who.race <- sprintf("%s Killed by %s",
                                               by.p.race.group$victim.race,
                                               by.p.race.group$perpetrator.race)

by.p.race.group$freq <- round(by.p.race.group$total.by.race/sum(by.p.race.group$total.by.race)*100,2)


by.p.race.group$freq<- paste(by.p.race.group$freq,"%",sep="")


top4.by.race <- by.p.race.group[16:13,c(4,3,5)]


table.by.race <- tableGrob(top4.by.race[ ,c(1,3)], rows=NULL)

by.v.age.group <- summarise(group_by(homi.r.solved.age,vic.age.group),total.by.group.v = n()) %>%
                            arrange(total.by.group.v)

by.p.age.group <- summarise(group_by(homi.r.solved.age,per.age.group),total.by.group.p = n()) %>%
  arrange(total.by.group.p)


by.v.p.age.group <- summarise(group_by(homi.r.solved.age,vic.age.group,per.age.group,perpetrator.sex,
                                       victim.sex),
                              total.by.group.vp = n()) %>%
  arrange(total.by.group.vp)

```
## Plots

>First i would like to create a special theme for this Kernel, then will go through plots one by one.

+ Theme
+ Top 20 Female VS Male VS Weapon Used 18+
+ Female VS Male VS Weapon Used 18+
+ Weapon used
+ Female Killed by... Male killed by... 18+
+ Familicide
+ Freinds VS Sex
+ Employee VS Employer VS Sex
+ Ex-Husband VS Ex-Wife / Older or Younger
+ Girlfriend VS Boyfriend
+ Year
+ state



```{r}
#################################################
homi.theme<-theme(
  axis.text = element_text(size = 8),
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "grey"),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "snow1"),
  legend.position = "right",
  legend.justification = "top", 
  legend.background = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, size = 1))
####################################################

 tt.homi.f <- ttheme_minimal(
    core=list(bg_params = list(fill = "azure", col="darkblue"),
              fg_params=list(fontface=6)),
    colhead=list(fg_params=list(col="navyblue", fontface=4L)))

```


```{r}
ggplot(by.v.p.age.group,aes(x = per.age.group, vic.age.group,y = total.by.group.vp))+
  geom_bar(stat = "identity",fill="steelblue",width=0.5)+
  facet_wrap(~ vic.age.group)+
  homi.theme+
  theme(axis.text.x=element_text(size= 7, 
        angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))+
  theme(strip.text = element_text(size=10,color ="darkblue"))+
  ggtitle("Age Group 18+ \n Victim VS perpedator")+
  labs(x="Perpedator",
       y="Number of Incidents")
```


```{r}
ggplot(ndf.by.weapon.geom.points,aes(x=victim.age,y=perpetrator.age))+
  geom_point(stat="identity", col="darkred",size=0.8)+
  facet_wrap(~ sex.weapon.used)+
  geom_smooth(method="glm", size=1)+
  homi.theme+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))+
  theme(strip.text = element_text(size=8,color ="darkblue"))+
ggtitle("Top 4 \n incident VS Age VS Weapon")+
  labs(x="Victim Age",
       y="Perpetrator Age")
```



```{r}
gg.by.race <- ggplot(top4.by.race,aes(x=who.killed.who.race,y=total.by.race))+
  geom_bar(stat="identity",fill="darkred",width=0.5)+
  homi.theme+
  ggtitle("Top 4 \n Victim race VS perpetrator race \n 18+")+
  labs(x="Race",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))


grid.arrange(gg.by.race,
             table.by.race,ncol=2)
```

####Weapon used vs Gender.

```{r}
by.ndf.by.weapon.used$sex.weapon.used <- fct_inorder(by.ndf.by.weapon.used$sex.weapon.used )

plot.gender.weapon.used.T20 <- ggplot(by.ndf.by.weapon.used[1:20,],aes(x = sex.weapon.used,y=freq.by.weapon.used))+
  geom_bar(stat="identity",fill="darkred",width=0.5)+
  homi.theme+
  ggtitle("Top 20 \n Female VS Male VS Weapon Used \n 18+")+
  labs(x="Gender and Weapon Used",
       y="Number of Incidents")+
 theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

plot.gender.weapon.used.T20

```

```{r}
by.ndf.by.weapon.used$sex.weapon.used <- fct_inorder(by.ndf.by.weapon.used$sex.weapon.used )

plot.gender.weapon.used <- ggplot(by.ndf.by.weapon.used,
                                  aes(x = sex.weapon.used,y=freq.by.weapon.used))+
  geom_bar(stat="identity",fill="darkred",width=0.5)+
  homi.theme+
  ggtitle("Female VS Male VS Weapon Used \n 18+")+
  labs(x="Gender and Weapon Used",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

plot.gender.weapon.used

```

**Handgun** is the most used weapon by Male against Male and Female, then the **knife** and **Blunt object** as following:


  1- Male Killed by Male Using a Handgun,
  
  2- Male Killed by Male Using a Knife,
  
  3- Female Killed by Male Using a Handgun,
  
  4- Male Killed by Male Using a Blunt Object,
  
  5- Female Killed by Male Using a Knife,
  
  6- Male Killed by Female Using a Handgun,
  
  7- Male Killed by Male Using a Shotgun,
  
  8- Female Killed by Male Using a Blunt Object,
  
  9- Male Killed by Female Using a Knife and
  
  10- Male Killed by Male Using a Rifle.
  
```{r}
Plot.weapon.used <- ggplot(by.weapon,aes(x = weapon,y=freq.by.weapon))+
geom_bar(stat = "identity",fill="red",width = 0.5)+
  homi.theme+
  ggtitle("Weapon used")+
  labs(x="Weapon",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))


plot.Weapon.vs.gender <- ggplot(by.weapon.sex,aes(x = weapon,y=freq.by.weapon))+
geom_bar(stat = "identity",fill="red",width=0.5)+
  facet_wrap(~ victim.sex)+
  homi.theme+
  ggtitle("Female Killed by: \n Male killed by: \n 18+")+
  labs(x="Weapon",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

grid.arrange(Plot.weapon.used,
             plot.Weapon.vs.gender,
             ncol=1)

```

Numbers do not lie!

We still using the dataframe where the crime solved is "Yes"

#### Who Killed Who? ( Familicide )

```{r}
by.family$who.killed.who<- fct_inorder(by.family$who.killed.who)  
  
  plot.by.family <- ggplot(by.family,aes(x=who.killed.who, y=total.number.re ))+
    geom_bar(stat="identity", fill="darkred",width = 0.5)+
    homi.theme+
    ggtitle("Who Killed Who! \n \n Number of Incidents VS Family Relationship")+
    labs(x="Family relationship",
         y="Number of Incidents")+
    theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

 
 table.by.family <- tableGrob(by.family, rows=NULL,theme = tt.homi.f)
 
 grid.arrange(plot.by.family,
              table.by.family,ncol=2)

```
It is painful numbers, but good to know ... no need to explain what is already there... These two plots need a rich imagination and strong brainstorming.

####Friends VS Sex

```{r}
ggplot(friend.crime, aes(x= who.killed.who.Friend.sex))+
  geom_bar(alpha=0.8,fill="steelblue", width=0.3)+
  homi.theme+
  ggtitle("Victims \n Who Killed Who? \n Freinds VS Sex")+
  labs(x="Who Killed Who?",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

```

*Again men ...NO COMMENTS !*

####Employee VS Employer

```{r}
ggplot(empyr.empee.sex ,aes(x = who.killed.who.sex, y=sex.freq, fill=relationship))+
  geom_bar(stat="identity", alpha=0.4,col="gold", width=0.4)+facet_wrap(~ relationship)+
  homi.theme+
  ggtitle("Victims \n Employee VS Employer \n Male VS Female \n Who Killed by Who?")+
  labs(x= "Who Killed by Who",
       y= "Number of incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

```

```{r}
ggplot(employee.employer.crime ,aes(x=relationship,fill = relationship))+
  geom_bar(alpha=0.4,col="gold", width=0.4)+
  homi.theme+
  ggtitle("Victims \n Employee VS Employer")+labs(x= "Relationship",
                                                  y= "Number of incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

```

####Ex-Husband VS Ex-Wife / Older or Younger

```{r}
ex.h.vs.ex.w <- ggplot(ex.husband.ex.wife.crime ,aes(x=relationship,fill = relationship))+
  geom_bar(alpha=0.4,col="gold", width=0.4)+
  homi.theme+
  ggtitle("Victims \n Ex-Husband VS Ex-Wife")+labs(x= "Relationship",
                                                   y= "Number of incidents")+theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

ex.h.vs.ex.w.age <- ggplot(ex.husband.ex.wife.crime, aes(x= older.or.younger))+geom_bar(alpha=0.7,fill="gold3", width=0.3)+
  homi.theme+
  ggtitle("Victims \n Ex-Husband VS Ex-Wife VS Age")+
  labs(x="Older and Yonger",
       y="Number of Incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

grid.arrange(ex.h.vs.ex.w,
             ex.h.vs.ex.w.age,ncol=2)

```



*No Comments !*


####The incidents happened between the Boyfriends and Girlfriends.

```{r}
plot.gf.bf.vic<-ggplot(girl.boy.crime ,aes(x=victim.sex,fill=victim.sex))+
  geom_bar(alpha=0.4,col="gold", width=0.4)+
  homi.theme+
  ggtitle("Victims \n Girlfriend VS Boyfriend")+labs(x= "Gender",
                              y= "Number of incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

plot.gf.bf.pre<-ggplot(girl.boy.crime ,aes(x=perpetrator.sex, fill=perpetrator.sex))+
  geom_bar(alpha=0.4,col="gold", width = 0.4)+
  homi.theme+
  ggtitle("Perpetrators \n Girlfriend VS Boyfriend")+labs(x= "Gender",
                                     y= "Number of incidents")+
  theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))

grid.arrange(plot.gf.bf.vic,
             plot.gf.bf.pre,
             ncol=2)

```

Regardless of the crime type, we see that the Male is much more than female as Perpetrators and Vice Versa.

####Incidents per year and States

```{r}
plot.homic.years<-ggplot(data = by.year,
                         aes(x=as.numeric(year),
                             y=freq.year))+
    geom_line(size=2,col="yellow3")+
    homi.theme+
    ggtitle("Number of incidents occurred per Year")+
    labs(x="Year",
         y="Number of Incidents")+
    theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))
  
  plot.homic.years.points<-ggplot(data = by.year,
                                aes(x=as.factor(year),
                                    y=freq.year))+
    geom_point(size=1,col="blue")+
    homi.theme+
    ggtitle("Number of Incidents occurred per Year")+
    labs(x="Year",
         y="Number of Incidents")+
    theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))
  
  
  by.state$state <- fct_inorder(by.state$state)
  plot.by.state <- ggplot(data = by.state,
                          aes(x=as.factor(state),
                              y=freq.by.state))+
    geom_bar(stat= "identity", fill="darkred", width=0.5 )+
    homi.theme+
    ggtitle("Number of Incidents occurred per Year")+
    labs(x="State",
         y="Number of Incidents")+
    theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
        axis.title.y=element_text(size=10),
        axis.title.x=element_text(size=10))+
  theme(plot.title = element_text(size = 10))
  
  grid.arrange(arrangeGrob(plot.homic.years,plot.homic.years.points,ncol=2),
               plot.by.state)

```
# closing

1993 has the larger number of crimes, I wander why? is it a sensible question ? and from 1994 up to 2014 we can easily see that it is trend-down..which is very nice and generating a big question as well !

The next step is to see the time-line trend of each observation, to see for example the trend of Wife Killed by Husband along the time-line from 1980 to 2014..

Sorry the kernel ended at this stage... my time is too short, and hope i will proceed shortly... Thanks and more will follow.
You can check the Section below link for Wepon used..
