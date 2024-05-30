# 1) Libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)

# 2) Loading data
reading_data <- read_excel(file.choose())
reading_data
math_data <- read_excel(file.choose())
math_data
all_data <- read_excel(file.choose())



################ ALL GENDER AND AGE ##########
all_table_gender <- all_data %>% 
  group_by(all_data$GENDER) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


GENDER_all <- ggplot(all_table_gender, 
                   aes(x = "", y = perc, 
                       fill = all_table_gender$`all_data$GENDER`)) +
  geom_col(color = "black") +
  geom_text(aes(label = paste0(labels,"\n",
                               "(","\"",n,"\"",")")),
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Gender")) +
  scale_fill_brewer() +
  coord_polar(theta = "y") + 
  theme_void() + 
  ggtitle("Gender") +
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            size = 14),
    legend.position = 'bottom')

all_table_age <- all_data %>% 
  group_by(all_data$AGE) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

AGE_all <- ggplot(all_data, aes(x = all_data$AGE)) +
  geom_bar(lwd = 1, color = "black", fill = "lightblue") +
  geom_text(aes(label = ..count..),
            stat = "count",
            colour = "black", size = 5,
            position = position_dodge(width=0.5), vjust = -0.4) +
  ggtitle("Age") +
  xlab("Age") + ylab("Number of \nchildren") +
  scale_x_continuous(breaks=seq(7,13,1)) +
  scale_y_continuous(limits = c(0,30),
                     breaks=seq(0,30,5)) +
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            size = 14),
    axis.title.x = element_text(size = 12, 
                                face = "italic"),
    axis.title.y = element_text(size = 12, 
                                face = "italic"),
    axis.text.x = element_text(size = 10, 
                               face = "bold",
                               color = "black"),
    axis.text.y = element_text(size = 10, 
                               face = "bold",
                               color = "black"))

ab <- ggarrange(GENDER_all, AGE_all, 
               labels = c("A", "B"),
               ncol = 2, nrow = 1,
               common.legend = F, legend = "bottom")
ab
ggsave("AgeYGender_ALL.jpeg", ab, width = 14, height = 9, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")




################ ALL PREDISCAL MATRIX ################
L1 <- length(all_data$PREDISCAL_Phrases)
L2 <- length(all_data$`PREDISCAL_ Fluency`) 
PREDISCAL_data <- matrix(0, nrow = L1 + (L2*2),
                         ncol = 3)
PREDISCAL_data[c(1:104),1] <- c(all_data$PREDISCAL_Phrases)
PREDISCAL_data[c(105:208),1] <- c(all_data$`PREDISCAL_ Fluency`)
PREDISCAL_data[c(209:312),1] <- c(all_data$`PREDISCAL_ Calculation`)
for (i in 1:length(PREDISCAL_data[,1])) 
{ 
  if (PREDISCAL_data[i,1] <= 10)
  {
    PREDISCAL_data[i,2] = 1 # Severe
  }
  if (PREDISCAL_data[i,1] > 10 & PREDISCAL_data[i,1] <= 30)
  {
    PREDISCAL_data[i,2] = 2 # Moderate
  }
  if (PREDISCAL_data[i,1] > 30 & PREDISCAL_data[i,1] <= 70)
  {
    PREDISCAL_data[i,2] = 3 # No difficulty
  }
  if (PREDISCAL_data[i,1] > 70)
  {
    PREDISCAL_data[i,2] = 4 # High performance
  }
}
PREDISCAL_data[,3] <- c(rep(1,104),
                        rep(2,104),
                        rep(3,104))

PREDISCAL_data <- data.frame(PREDISCAL_data)
PREDISCAL_Phrases <- PREDISCAL_data[c(1:104), c(1:2)]
PREDISCAL_Fluency <- PREDISCAL_data[c(105:208), c(1:2)]
PREDISCAL_Calculation <- PREDISCAL_data[c(209:312), c(1:2)]

################ ALL PREDISCAL: Phrases Plot ################
Phrases_table <- PREDISCAL_Phrases %>% 
  group_by(PREDISCAL_Phrases[,2]) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

bins <- seq(0, 4)
bin_counts <- as.data.frame(table(cut(PREDISCAL_Phrases$X2, breaks = bins)))
Phrases_plot <- ggplot(bin_counts, aes(x = as.numeric(Var1), 
                                       y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", color="black",size=0.5) +
  scale_x_continuous(breaks = bins) +
  scale_fill_manual(values = c("red1","tomato1",
                               "darkolivegreen2","green4"),
                    name = "Category",
                    labels=c("Severe","Moderate",
                             "No difficulty",
                             "High performance")) +
  labs(y = "Number of children", 
       title = "  PREDISCAL_Phrases") +
  geom_text(aes(label = Freq),
            position = position_dodge(width=0.5), 
            vjust = -0.4, size = 4, color = "black") +
  scale_y_continuous(limits = c(0,60,5),
                     breaks=seq(0,60,5)) + 
  theme_void() +
  theme(
    plot.title=element_text(hjust=0.6, vjust=0.5, 
                            size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 14, 
                                face = "italic",
                                angle = 90),
    axis.text.x = element_blank(),
    axis.ticks.x= element_blank(),
    axis.text.y = element_text(size = 10, 
                               face = "bold"))
################ ALL PREDISCAL: Fluency Plot ################
Fluency_table <- PREDISCAL_Fluency %>% 
  group_by(PREDISCAL_Fluency[,2]) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

Fluency_plot <- ggplot(data = Fluency_table,
                       aes(x = `PREDISCAL_Fluency[, 2]`,
                           y = n,
                           fill = as.factor(`PREDISCAL_Fluency[, 2]`))) +
  geom_bar(stat = "identity", color = "black",
           width = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(label = n),
            position = position_dodge(width=0.5), 
            vjust = -0.4, size = 4, color = "black") +
  xlab("") + 
  ylab("") +
  ggtitle("PREDISCAL_Fluency") +
  # coord_equal(1/4) + 
  scale_y_continuous(limits = c(0,60),
                     breaks=seq(0,60,5)) +
  theme_void() +
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            size = 10),
    axis.title.y = element_text(vjust=-1,
                                size = 14, 
                                face = "italic", 
                                angle = 90),    axis.text.x = element_blank(),
    axis.ticks.x= element_blank(),
    axis.text.y = element_text(color = "white")) +
  scale_fill_manual(values = c("red1","tomato1",
                               "darkolivegreen2","green4"),
                    name="Category", 
                    breaks = c(1:4),
                    labels=c("Severe","Moderate",
                             "No difficulty",
                             "High performance"))

################ ALL PREDISCAL: Calculation Plot ################
Calculation_table <- PREDISCAL_Calculation %>% 
  group_by(PREDISCAL_Calculation[,2]) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

Calculation_plot <- ggplot(data = Calculation_table,
                           aes(x = `PREDISCAL_Calculation[, 2]`,
                               y = n,
                               fill = as.factor(`PREDISCAL_Calculation[, 2]`))) +
  geom_bar(stat = "identity", color = "black",
           width = 0.9, position = position_dodge(0.9)) +
  geom_text(aes(label = n),
            position = position_dodge(width=0.5), 
            vjust = -0.4, size = 4, color = "black") +
  xlab("") + 
  ylab("") +
  ggtitle("PREDISCAL_Calculation") +
  #coord_equal(1/4) + 
  scale_y_continuous(limits = c(0,60),
                     breaks=seq(0,60,5)) +
  theme_void() +
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5,
                            size = 10),
    axis.title.y = element_text(vjust=-1,
                                size = 14, 
                                face = "italic", 
                                angle = 90),
    axis.text.x = element_blank(),
    axis.ticks.x= element_blank(),
    axis.text.y = element_text(color = "white")) +
  scale_fill_manual(values = c("red1","tomato1",
                               "darkolivegreen2","green4"),
                    name="Category", 
                    breaks = c(1:4),
                    labels=c("Severe","Moderate",
                             "No difficulty",
                             "High performance"))

################ ALL PREDISCAL FINAL PLOT ################ 
d <- ggarrange(Phrases_plot, Fluency_plot, 
               Calculation_plot, 
               labels = c("   A) ", "  B)"," C)"), ncol = 3,
               common.legend = T,
               legend = "bottom")

d <- d + annotate("rect", xmin = 0.057, xmax = 0.37, ymin = 0.08, ymax = 1,
                  alpha = .05, fill = "navyblue") +
  annotate("rect", xmin = 0.37, xmax = 1, ymin = 0.08, ymax = 1,
           alpha = .05, fill = "gold3")
d
ggsave("PREDISCAL_ALL.jpeg", d, width = 15, height = 10, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")

################ ALL ERRORS IN DICTATION ################
errors <- all_data[,c(5,6,9)]
aux <- matrix(0,nrow = 208, ncol = 3)
aux[c(1:104),1] <- errors$Orthographic_ERR
aux[c(105:208),1] <- errors$Phonological_ERR
aux[c(1:104),2] <- rep("Orthographic",104) # 1 - clase homophone
aux[c(105:208),2] <- rep("Phonological",104) # 2 - clase heterophone
aux[,3] <- rep(errors$AGE,2) 

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(grid)

aux <- data.frame(aux)

f <- ggplot(aux, aes(x= as.factor(as.numeric(aux$X3)) , y= as.numeric(aux$X1), fill= aux$X2)) +
  geom_boxplot() +  # dentro de () para quitar outliers: outlier.shape = NA
  scale_fill_viridis(discrete = TRUE, alpha=0.8,
                     labels = c("Orthographic", "Phonological")) +
  theme_minimal() +
  theme(legend.position="right",
        plot.title = element_text(size=11,
                                  hjust=0.5,
                                  vjust=0.5,
                                  face='bold',
                                  color = "black"),
        axis.title.y = element_text(vjust= 1,
                                    size = 14,
                                    face = "italic",
                                    angle = 90,
                                    color = "black"),
        axis.title.x = element_text(size = 14,
                                    face = "italic",
                                    color = "black"),
        axis.text.x = element_text(size = 10, 
                                   face = "bold",
                                   color = "black"),
        axis.text.y = element_text(size = 10, 
                                   face = "bold",
                                   color = "black")) +
  ggtitle("Spelling Errors") +
  xlab("Age") + 
  ylab("Number of errors") +
  labs(fill = "Error Type")
f
ggsave("Dictation_Errors_ALL.jpeg", f, width = 15, height = 10, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")

################ ALL READING SPEED ################
library(gginnards)
Reading_Speed <- data.frame(reading_data[,c(4,8)])
aux <- split(Reading_Speed, Reading_Speed$AGE)
avg_age <- matrix(0, nrow = 1, ncol = 6)
avg_age[1] <- mean(aux$`7`$Reading_Speed)
avg_age[2] <- mean(aux$`8`$Reading_Speed)
avg_age[3] <- mean(aux$`9`$Reading_Speed)
avg_age[4] <- mean(aux$`10`$Reading_Speed)
avg_age[5] <- mean(aux$`11`$Reading_Speed)
avg_age[6] <- mean(aux$`12`$Reading_Speed)
avg_age <- round(avg_age,2)

SPEED <- ggplot(Reading_Speed, aes(x = AGE, 
                          y = Reading_Speed,
                          fill = AGE)) +
  scale_fill_viridis(discrete = F,
                     name = "Number of \n children",
                     labels = c(length(aux$`7`$AGE),
                                length(aux$`8`$AGE),
                                length(aux$`9`$AGE),
                                length(aux$`10`$AGE),
                                length(aux$`11`$AGE),
                                length(aux$`12`$AGE)))+
  geom_point(shape = 21, size = 4) +
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  xlab("Age") + ylab("Words per Minute") + 
  ggtitle("Reading Speed") + 
  scale_y_continuous(limits = c(min(Reading_Speed$Reading_Speed),
                                max(Reading_Speed$Reading_Speed)),
                     breaks=seq(15,150,15)) +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(size=11,
                                  hjust=0.5,
                                  vjust=0.5,
                                  face='bold',
                                  color = "black"),
        axis.title.y = element_text(vjust= 1,
                                    size = 14,
                                    face = "italic",
                                    angle = 90,
                                    color = "black"),
        axis.title.x = element_text(size = 14,
                                    face = "italic",
                                    color = "black"),
        axis.text.x = element_text(size = 10, 
                                   face = "bold",
                                   color = "black"),
        axis.text.y = element_text(size = 10, 
                                   face = "bold",
                                   color = "black")) 
################ ALL READING COMPREHENSION ################
comprehension <- data.frame(reading_data[,c(5,8)])
aux <- split(comprehension, comprehension$AGE)
avg_comp <- matrix(0, nrow = 1, ncol = 6)
avg_comp[1] <- mean(aux$`7`$Reading_.Comprehension)
avg_comp[2] <- mean(aux$`8`$Reading_.Comprehension)
avg_comp[3] <- mean(aux$`9`$Reading_.Comprehension)
avg_comp[4] <- mean(aux$`10`$Reading_.Comprehension)
avg_comp[5] <- mean(aux$`11`$Reading_.Comprehension)
avg_comp[6] <- mean(aux$`12`$Reading_.Comprehension)
avg_comp <- round(avg_comp,2)

COMP <- ggplot(comprehension, aes(x = AGE, 
                          y = Reading_.Comprehension,
                          fill = AGE)) +
  scale_fill_viridis(discrete = F,
                     name = "Number of \n children",
                     labels = c(length(aux$`7`$AGE),
                                length(aux$`8`$AGE),
                                length(aux$`9`$AGE),
                                length(aux$`10`$AGE),
                                length(aux$`11`$AGE),
                                length(aux$`12`$AGE)))+
  geom_point(shape = 21, size = 4) +
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red") +
  xlab("Age") + ylab("Score") + 
  ggtitle("Reading Comprehension") + 
  scale_y_continuous(breaks=seq(0, max(comprehension$Reading_.Comprehension), 1)) +
  theme_bw() +
  theme(legend.position="bottom",
        plot.title = element_text(size=11,
                                  hjust=0.5,
                                  vjust=0.5,
                                  face='bold',
                                  color = "black"),
        axis.title.y = element_text(vjust= 1,
                                    size = 14,
                                    face = "italic",
                                    angle = 90,
                                    color = "black"),
        axis.title.x = element_text(size = 14,
                                    face = "italic",
                                    color = "black"),
        axis.text.x = element_text(size = 10, 
                                   face = "bold",
                                   color = "black"),
        axis.text.y = element_text(size = 10, 
                                   face = "bold",
                                   color = "black")) 
################ ALL READING - SPEED AND COMPREHENSION ################
e <- ggarrange(SPEED, COMP, 
          labels = c("A)", "B)"), ncol = 2,
          common.legend = T,
          legend = "none")
e
ggsave("Reading_Speed_Comprehension_ALL.jpeg", e, width = 15, height = 10, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\7mo Semestre\\Base de datos psicométricos\\Data Validation\\Figures")


################ ALL WRAT-4 ################
wrat4 <- data.frame(math_data[,c(3)])

wrat4_table <- wrat4 %>% 
  mutate(
    # Create categories
    Category = dplyr::case_when(
      WRAT4_.Performance >= 55 & WRAT4_.Performance <= 69 ~ "Extremely low",
      WRAT4_.Performance >= 70 & WRAT4_.Performance <= 79 ~ "Low",
      WRAT4_.Performance >= 80 & WRAT4_.Performance <= 89 ~ "Average low",
      WRAT4_.Performance >= 90 & WRAT4_.Performance <= 109 ~ "Average",
      WRAT4_.Performance >= 110 & WRAT4_.Performance <= 119 ~ "Average high",
      WRAT4_.Performance >= 120 & WRAT4_.Performance <= 129 ~ "High",
      WRAT4_.Performance >= 130 ~ "15-Extremely high"),
    # Convert to factor
    Category = factor(Category,
                      levels = c("Extremely low", "Low", "Average low",
                                 "Average", "Average high",
                                 "High", "Extremely high")))

wrat4_info <- wrat4_table %>% 
  count(Category) %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))


bins <- seq(0, 7)
bin_counts <- as.data.frame(table(cut(as.numeric(wrat4_table$Category), breaks = bins)))
g <- ggplot(bin_counts, aes(x = as.numeric(Var1), 
                                       y = Freq, 
                            fill = as.factor(Var1))) +
  geom_bar(stat="identity",
           color = "black", width = 1) +
  geom_text(aes(label = Freq),
            position = position_stack(vjust = 0.5),
            size = 4, color = "black", fontface = "bold") +  
  ylab("Number of children") +
  xlab("Category") +
  ggtitle("WRAT-4 Performance") +
  coord_flip() + 
  theme_bw() +
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            face='bold', size = 10),
    axis.title.x = element_text(vjust=1,
                                size = 14,
                                face = "italic"),
    axis.title.y = element_text(size = 14,
                                face = "italic"),
    axis.text.x = element_text(size = 10, 
                               face = "bold",
                               color = "black"),
    axis.text.y = element_text(size = 10, 
                               face = "bold",
                               color = "black")) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks=seq(0,30,5),
                     limits = c(0,30)) +
  scale_x_continuous(labels = c("Extremely low", 
                                "Low", 
                                "Average low",
                                "Average",
                                "Average high",
                                "High", 
                                "Extremely high"),
                     breaks=seq(1,7,1))
g
ggsave("WRAT4_ALL.jpeg", g, width = 13, height = 8, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")


################ ALL ATTENTION ################
attention_all <- data.frame(all_data[,7])

# READING TABLES
attention_all_table <- attention_all %>% 
  mutate(
    # Create categories
    Category = dplyr::case_when(
      attention_all <= 77 ~ "Low",
      attention_all >= 78 & attention_all <= 92 ~ "Low-average",
      attention_all >= 93 & attention_all <= 107 ~ "Average",
      attention_all >= 108 & attention_all <= 122 ~ "High-average",
      attention_all >= 123 ~ "High"),
      # Convert to factor
    Category = factor(Category,
                      levels = c("Low", "Low-average",
                                 "Average", "High-average",
                                 "High")))

attention_all_info <- attention_all_table %>% 
  count(Category) %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# D2 READING PLOT
D2_plot <- ggplot(data = attention_all_info,
                       aes(x = Category,
                           y = n,
                           fill = Category)) +
  geom_bar(stat = "identity", color = "black",
           width = 0.9, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("red3",
                               "pink2",
                               "blue2",
                               "lightgreen",
                               "green4")) +
  geom_text(aes(label = n),
            position = position_dodge(width=0.5), 
            vjust = -0.4, size = 4, color = "black") +
  xlab("Category") + 
  ylab("Number of children") +
  ggtitle("Attention levels") +
  scale_y_continuous(limits = c(0,35),
                     breaks=seq(0,35,5)) + 
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            face='bold', size = 12),
    axis.title.x = element_text(vjust=1,
                                size = 12,
                                face = "italic"),
    axis.title.y = element_text(size = 12,
                                face = "italic"),
    axis.text.x = element_text(size = 9, 
                               color = "black",
                               angle = 45,
                               vjust=0.6),
    axis.text.y = element_text(size = 10, 
                               face = "bold",
                               color = "black"))

h <- ggarrange(D2_plot, 
               ncol = 1,
               common.legend = T,
               legend = "none")

h
ggsave("D2_attention_ALL.jpeg", h, width = 12, height = 7, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")

################ IQ ################
iq_all <- data.frame(all_data[,c(8:9)])

# READING TABLES
iq_all_table <- iq_all %>% 
  mutate(
    # Create categories
    Category = dplyr::case_when(
      iq_all >= 55 & iq_all <= 69 ~ "Extremely low",
      iq_all >= 70 & iq_all <= 79 ~ "Low",
      iq_all >= 80 & iq_all <= 89 ~ "Low-average",
      iq_all >= 90 & iq_all <= 109 ~ "Average",
      iq_all >= 110 & iq_all <= 119 ~ "High-average",
      iq_all >= 120 & iq_all <= 129 ~ "High",
      iq_all >= 130 ~ "Extremely high"),
    # Convert to factor
    Category = factor(Category,
                      levels = c("Extremely low","Low", 
                                 "Low-average", "Average",
                                 "High-average","High",
                                 "Extremely high")))

# IQ PLOT
bins <- seq(0, 7)
bin_counts <- as.data.frame(table(cut(as.numeric(iq_all_table$Category), breaks = bins)))
iq_plot <- ggplot(bin_counts, aes(x = as.numeric(Var1), y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", color="black",size=1) +
  scale_x_continuous(breaks = bins) +
  scale_fill_brewer(palette = "Pastel2",
                    name = "IQ",
                    labels = c("Extremely low","Low", 
                               "Low-average", "Average",
                               "High-average","High",
                               "Extremely high")) +
  labs(x = "Category", y = "Number of children", 
       title = "IQ") +
  geom_text(aes(label = Freq),
            position = position_dodge(width=0.5), 
            vjust = -0.4, size = 4, color = "black") +
  scale_y_continuous(limits = c(0,45),
                     breaks=seq(0,45,5)) + 
  theme(
    plot.title=element_text(hjust=0.5, vjust=0.5, 
                            face='bold', size = 12),
    axis.title.x = element_text(vjust=1,
                                size = 12,
                                face = "italic"),
    axis.title.y = element_text(size = 12,
                                face = "italic"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 10, 
                               face = "bold",
                               color = "black"))

i <- ggarrange(iq_plot, 
               ncol = 1,
               common.legend = T,
               legend = "right")

i
ggsave("IQ_ALLnew.jpeg", i, width = 15, height = 10, units = "cm", dpi = 300,
       path = "C:\\Users\\52477\\Desktop\\Doctorado\\6to Semestre\\Base de datos psicométricos\\Data Validation\\Figures")

