library(readxl)
library(GGally)
library(ggplot2)
library(ggpubr)

# Loading Psychometric Data for: 
# Reading: Use RD_data.xlsx
S1_read <- read_excel(file.choose())
S1_read
# Math: Use MD_data.xlsx
S1_math <- read_excel(file.choose())
S1_math

# Correlation Matrix for the Reading Group
r <- ggcorr(S1_math[,1:6], label = TRUE,
       label_size = 3, label_round = 2,
       hjust = 0.3, vjust = 1.2,
       size = 2.1, angle = 90) +
  annotate(geom="text", x=3.8, y=6.2, 
           label="Correlation Matrix",
           color="black", angle = 0,
           size = 5) + 
  annotate(geom="text", x=4, y=5.9, 
           label="Math Difficulties",
           color="black", angle = 0,
           size = 3, fontface = 'italic')  

# Correlation Matrix for the Math Group
m <- ggcorr(S1_read[,1:8], label = TRUE,
       label_size = 3, label_round = 2,
       hjust = 0.2, vjust = 1.2,
       size = 2, angle = 90) +
  annotate(geom="text", x=4.5, y=8.1, 
           label="Correlation Matrix",
           color="black", angle = 0,
           size = 5) +
  annotate(geom="text", x=4.6, y=7.7, 
           label="Reading Difficulties",
           color="black", angle = 0,
           size = 3, fontface = 'italic')

# Merge both plots in a single figure
corr_fig<- ggarrange(r, m, 
          labels = c("   a)", "  b)"), ncol = 2,
          common.legend = T,
          legend = "bottom")
corr_fig

# Merge both plots in a single figure
ggsave("CorrelationMatrix.jpeg", corr_fig, width = 16, height = 12, units = "cm", dpi = 300,
       path = "include your save path here")
