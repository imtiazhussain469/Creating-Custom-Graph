

library(dplyr)
library(ggplot2)
library(hms)
library(chron)
library(DT)
# Loading all three datasets.
codes <- read.csv("codes_export.csv")
incidents <- read.csv("incident2_export.csv")
address <- read.csv("incident_export.csv")


## ****************************** ADDING NEW COLUMNS **************************

# Converting the alarm and arrival column to date time format
incidents$alarm <- strptime(incidents$alarm, "%Y-%m-%d %H:%M:%S")
incidents$arrival <- strptime(incidents$arrival, "%Y-%m-%d %H:%M:%S")

# Adding two new columns to the dataset.
incidents <- incidents %>%
  mutate(Time_To_Arrival = difftime(arrival, alarm, units = "mins"),
         Time_To_Clear = difftime(lu.clear, arrival, units = "mins"))
head(incidents)


## *************************Graph Set 1****************************************

incidentTypeByFrequency <- incidents %>%
  group_by(inc.type) %>%
  summarise(Frequency = n()) %>%
  arrange(-Frequency)

g1_10 <- incidentTypeByFrequency[c(1:10),]
g11_20 <- incidentTypeByFrequency[c(11:20),]
g21_30 <- incidentTypeByFrequency[c(21:30),]
g31_40 <- incidentTypeByFrequency[c(31:40),]
g41_50 <- incidentTypeByFrequency[c(41:50),]
g51_60 <- incidentTypeByFrequency[c(51:60),]
g61_70 <- incidentTypeByFrequency[c(61:70),]
g71_80 <- incidentTypeByFrequency[c(71:80),]
g81_90 <- incidentTypeByFrequency[c(81:90),]
g91_100 <- incidentTypeByFrequency[c(91:100),]
g101_110 <- incidentTypeByFrequency[c(101:110),]
g111_120 <- incidentTypeByFrequency[c(111:120),]
g121_130 <- incidentTypeByFrequency[c(121:130),]
g131_137 <- incidentTypeByFrequency[c(131:137),]


ggplot(data = g1_10, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (1-10)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g11_20, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (11-20)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g21_30, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (21-30)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g31_40, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (31-40)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g41_50, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (41-50)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g51_60, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (51-60)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g61_70, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (61-70)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g71_80, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (71-80)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g81_90, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (81-90)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g91_100, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (91-100)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g101_110, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (101-110)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g111_120, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (111-120)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g121_130, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (121-130)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g131_137, 
       mapping = aes(x = inc.type, 
                     y = Frequency, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Incident Type (131-137)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

## ***************************** Graph Set 2*************************************

actionTypeByFrequency <- incidents %>%
  group_by(act.tak1) %>%
  summarise(Frequency = n()) %>%
  arrange(-Frequency)

g1_10 <- actionTypeByFrequency[c(1:10),]
g11_20 <- actionTypeByFrequency[c(11:20),]
g21_30 <- actionTypeByFrequency[c(21:30),]
g31_40 <- actionTypeByFrequency[c(31:40),]
g41_50 <- actionTypeByFrequency[c(41:50),]
g51_60 <- actionTypeByFrequency[c(51:59),]


ggplot(data = g1_10, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (1-10)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g11_20, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (11-20)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g21_30, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (21-30)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g31_40, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (31-40)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g41_50, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (41-50)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

ggplot(data = g51_60, 
       mapping = aes(x = act.tak1, 
                     y = Frequency, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Frequency of Action Type (51-60)") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


##*********************************** Graph Set 4***********************************


avgTimeByIncident <- incidents %>%
  group_by(inc.type) %>%
  summarise(Avg_Time = round(mean(Time_To_Arrival, na.rm = T), 4)) %>%
  arrange(-Avg_Time)

avgTimeByPropUse <- incidents %>%
  group_by(prop.use) %>%
  summarise(Avg_Time = round(mean(Time_To_Arrival, na.rm = T), 4)) %>%
  arrange(-Avg_Time)

avgTimeByActionType <- incidents %>%
  group_by(act.tak1) %>%
  summarise(Avg_Time = round(mean(Time_To_Arrival, na.rm = T), 4)) %>%
  arrange(-Avg_Time)

### Average Time To Arrival By Property Type:

datatable(avgTimeByPropUse)



## Graph of Top 10 Property Type:

g1_10 <- avgTimeByPropUse %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = prop.use, 
                     y = Avg_Time, 
                     fill = prop.use)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Porperty Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))




### Average Time To Arrival By Incident Type:

datatable(avgTimeByIncident)

## Graph of Top 10 Incident Type:

g1_10 <- avgTimeByIncident %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = inc.type, 
                     y = Avg_Time, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Incident Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


### Average Time To Arrival By Action Type:

datatable(avgTimeByActionType)


## Graph of Top 10 Action Type:

g1_10 <- avgTimeByActionType %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = act.tak1, 
                     y = Avg_Time, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Action Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))



##************************************ Graph Set 5***************************


avgTimeByIncident <- incidents %>%
  group_by(inc.type) %>%
  summarise(Avg_Time = round(mean(Time_To_Clear, na.rm = T), 4)) %>%
  arrange(-Avg_Time)

avgTimeByPropUse <- incidents %>%
  group_by(prop.use) %>%
  summarise(Avg_Time = round(mean(Time_To_Clear, na.rm = T), 4)) %>%
  arrange(-Avg_Time)

avgTimeByActionType <- incidents %>%
  group_by(act.tak1) %>%
  summarise(Avg_Time = round(mean(Time_To_Clear, na.rm = T), 4)) %>%
  arrange(-Avg_Time)


## Average Time To Clear By Property Type:

datatable(avgTimeByPropUse)



## Graph of Top 10 Property Type:


g1_10 <- avgTimeByPropUse %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = prop.use, 
                     y = Avg_Time, 
                     fill = prop.use)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Porperty Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))



### Average Time To Clear By Incident Type:

datatable(avgTimeByIncident)



## Graph of Top 10 Incident Type:

g1_10 <- avgTimeByIncident %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = inc.type, 
                     y = Avg_Time, 
                     fill = inc.type)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Incident Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))



### Average Time To Clear By Action Type:

datatable(avgTimeByActionType)


## Graph of Top 10 Action Type:

g1_10 <- avgTimeByActionType %>%
  arrange(-Avg_Time) %>%
  head(10)
ggplot(data = g1_10, 
       mapping = aes(x = act.tak1, 
                     y = Avg_Time, 
                     fill = act.tak1)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Action Types With \nHighest Average Time To Clear") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

