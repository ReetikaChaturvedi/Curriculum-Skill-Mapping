install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)
rc<-Project.Skills

## Converting our data to long data
install.packages("dplyr")
install.packages("tidyr")
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
rc_long <- rc %>% 
  pivot_longer(starts_with("Title"), values_drop_na = TRUE)
rc_long
rc_long %>% tabyl(value)
##converting data to wide data
rc_wide <- rc_long %>% 
  ##creating Binary Variable for each value
  mutate(checked = 1) %>% ##For creating binary variable 0 and 1
  pivot_wider(
    id_cols = Course.Code,
    names_from = value,
    values_from = checked,
    values_fill = list(checked = 0) # Fills in 0 for any value not "checked" above by the `mutate()` function
  )
rc_wide
write.csv(rc_long,"C:/Users/REETIKA AND SHRUTIKA/Desktop/reshaped data.csv", row.names = TRUE)
write.csv(rc_wide,"C:/Users/REETIKA AND SHRUTIKA/Desktop/Project.Skills.csv", row.names = FALSE)
##visualization 
#1) Correlation plot
library(corrplot)
corData <- cor(rc_wide[,c(19:20)], use = "pairwise")
corrplot(corData, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#2) plot the density and histogram for leadership capability and introduction to digital era
library(ggplot2)
ggplot(rc_wide, aes(x=rc_wide$`Developing Your Leadership Capability`+`Introduction to Digital Era`)) +
  geom_histogram(aes(y=..density..), binwidth = 2, colour="red", fill="white") +
  geom_density(alpha=0.2, fill="yellow")

#3) #Scatter Plot
# To find correlation between the skill labels and a set of skills) 
plot(rc_long$Skill.Labels...., rc_long$X,main = "Scatterplot" )







 