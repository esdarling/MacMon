#Budget calculations

#Year 2 - pivot table
#From Victoria's pivot table 
setwd("/Users/emilydarling/Dropbox/5_MacMon_Finance/Budgets/Quarterly reports")

d <- read.csv("101006_12.28.2015_pivot.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(d)
nrow(d)

#Melt long to programs
d2 <- melt(d, id.vars = 1:2, variable.name = "Program")
head(d2)

#Recode unknown US WBS element
d2$Program <- recode(d2$Program, "'X3US50' = 'X7US09'")
unique(d2$Program)

                 
#Remove NAs
d2 <- d2[-which(is.na(d2$value)),]
head(d2)

d3 <- d2 %>%
  select(MacMon.line.item, Program, value) %>%
  group_by(Program, MacMon.line.item) %>%
  summarize(total.value = sum(value))

#Reorder to match budget
head(d3)
levels(as.factor(d3$MacMon.line.item))
d3$MacMon.line.item <- factor(d3$MacMon.line.item, 
                     levels = c("Coordinator/Data Analyst","Database Developer/Manager",
                                "Field Science Team","Project Support Staff",
                                "National travel", "International travel",
                                "Office Rent, Insurance, and Utilities",
                                "Bank fees","Office Supplies","Meetings",
                                "Purchased services","Communications",
                                "Equipment","Printing"))

d4 <- dcast(d3, MacMon.line.item ~ Program)
d4[is.na(d4)] <- 0
d4

write.csv(d4, "summary by MacMon line item_Dec2015.csv", row.names=FALSE)


