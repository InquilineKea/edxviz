library(dplyr)
library(tidyr)
library(ggplot2)
library(gtools)
library(grid)
library(gridExtra)
library(ggthemes)
library(scales)

# adjust as necessary
fileloc = "./NYC-DSA/Project 1/datasets/HMXPC13_DI_v2_5-14-14.csv"

# get classes from sample, adjust and wrangle to ensure integrity and workability
edx.head <- read.csv(fileloc, nrows = 1000)
classes <- sapply(edx.head, class)

# almost all but course id should be character, we'll adjust dates to posix later
classes[c("LoE_DI","YoB","gender","final_cc_cname_DI",
          "userid_DI","course_id","start_time_DI","last_event_DI")] <- "character" 

# re-read with colClasses
edx <- read.csv(fileloc, colClasses = classes, na.string = c("", " ", NA))
edx.bk <- edx # make a backup because i can
dim(edx) # check that dimensions match up with raw (641338 in raw with header row)

# drop unneeded roles column (administrative, per spec)
# put incomplete data into separate df, deal later
edx$roles = NULL
edx.incomplete <- filter(edx, !is.na(incomplete_flag))
edx <- filter(edx, is.na(incomplete_flag))
edx$incomplete_flag = NULL; edx.incomplete$incomplete_flag = NULL; 

# change all blanks in factor columns to NA
edx <- edx %>% mutate(YoB = ifelse(YoB == "", NA, as.character(YoB)))
edx <- edx %>% mutate(gender = ifelse(gender == "", NA, as.character(gender)))
edx <- edx %>% mutate(LoE_DI = ifelse(LoE_DI == "", NA, as.character(LoE_DI)))
edx <- edx %>% mutate(start_time_DI = ifelse(start_time_DI == "", NA, as.character(start_time_DI)))
edx <- edx %>% mutate(last_event_DI = ifelse(last_event_DI == "", NA, as.character(last_event_DI)))
edx <- edx %>% mutate(final_cc_cname_DI = ifelse(final_cc_cname_DI == "", NA, as.character(final_cc_cname_DI)))

# parse dates to date class
edx <- edx %>% 
  mutate(start_time_DI = as.Date(start_time_DI, format = "%Y-%m-%d")) %>%
  mutate(., last_event_DI = as.Date(last_event_DI, format = "%Y-%m-%d"))

# parse course name info into new columns
edx <- edx %>%
  separate(course_id, into = c("school", "course", "semester"), sep = "/", remove=TRUE)

# get datediff from registration to last interaction date
# note that subtracting two Dates yields days, whereas posixCT would yield seconds
edx <- edx %>%
  mutate(active_length = as.integer(last_event_DI - start_time_DI))

# create age category
edx <- edx %>%
  mutate(age_cat = 
    ifelse(!is.na(edx$YoB), 
           as.character(cut(as.integer(Sys.Date() 
              - as.Date(as.character(edx$YoB)[!is.na(edx$YoB)], 
              format = "%Y")) / 365, 
     breaks = c(0,18,25,35,45,65,Inf),
     labels = c("0 - 18", "18 - 25", "25 - 35", "35 - 45", "45 - 65", "65+")
    )), NA)
  )

# create course type map
coursetype_map <- data.frame(rbind(
  c('CB22x','The Ancient Greek Hero', 'Humanities'),
  c('CS50x','Introduction to Computer Science 1', 'Sciences'),
  c('ER22x','Justice','Humanities'),
  c('PH207x','Health in Numbers: Quantitative Methods', 'Sciences'),
  c('PH278x','Human Health and Global Environmental Change','Humanities'),
  c('14.73x','The Challenges of Global Poverty','Humanities'),
  c('2.01x','Elements of Structures','Sciences'),
  c('3.091x','Introduction to Solid State Chemistry','Sciences'),
  c('6.002x','Circuits and Electronics','Sciences'),
  c('6.00x','Introduction to Computer Science and Programming','Sciences'),
  c('7.00x','Introduction to Biology – The Secret of Life','Sciences'),
  c('8.02x','Electricity and Magnetism','Sciences'),
  c('8M.ReV','Mechanics Review','Sciences')
))

coursetype_map <- coursetype_map %>% rename(CourseCode = X1,  CourseName = X2,CourseCat = X3)
coursetype_map <- data.frame(sapply(coursetype_map, as.character), stringsAsFactors = FALSE)
edx <- edx %>% inner_join(coursetype_map, by = c("course" = "CourseCode"))

str(edx)
## let's start aggregating  into subset objects ##
# time series data for attrition rate (by gender, age (cut up based off YoB), education level, school)

# filter down to interesting range
edx.filt <- edx %>%
  filter(active_length > 0, active_length < 120)


# this is the function.
getPercs <- function(df, colname, colpiv, colvals) {
  if(!missing(colvals)) { 
    df.sub <- df[df[[colname]] %in% colvals,]
    
  } else {
    df.sub <- df
  } # this checks for optional colvals param
  
  df.sub <- df.sub[!is.na(df[[colname]]),] # remove NA
  
  df.sub.groups <- df.sub %>%
    group_by_(interp(~x, x = as.name(colname)), interp(~y, y = as.name(colpiv))) %>% 
    summarise(count = n()) # get group totals

  df.sub.totals <- df.sub %>%
    group_by_(interp(~x, x = as.name(colname))) %>% 
    summarise(totcount = n()) # get totals
  
  df.perc <- df.sub.groups %>% inner_join(df.sub.totals, by = c(colname))
  df.perc <- df.perc %>% mutate(perc = count / totcount)
  
  df.perc$count <- NULL; df.perc$totcount <- NULL
  
  return(df.perc[complete.cases(df.perc),])
}


getPercs(edx.filt, 'gender', 'active_length', c('m','f'))
getPercs(edx.filt, 'age_cat', 'active_length') # 18 - 25, 25 - 32, etc.
getPercs(edx.filt, 'LoE_DI', 'active_length') # Bachelor's, Master's
getPercs(edx.filt, 'final_cc_cname_DI', 'active_length') # country
getPercs(edx.filt, 'school', 'active_length') # harvard / mit
getPercs(edx.filt, 'course', 'active_length')
getPercs(edx.filt, 'CourseCat', 'active_length') # humanities / sciences

createAttritionPlot <- function(df, colorcol, title, legendtitle) {
  ggplot(df, aes_string(x = 'active_length', y = 'perc', color = colorcol)) +
    geom_line() +
    labs(
      title = paste0("User Engagement Attrition Rate (by ", title, ")"), 
      y = "Percentage of Total Users", x = "Days Active") +
    theme_bw() +
    scale_colour_discrete(name = legendtitle) 
}

createAttritionPlot(
  getPercs(edx.filt, 'gender', 'active_length', c('m','f')), 
  'gender', 'gender', 'Gender'
)
createAttritionPlot(
  getPercs(edx.filt, 'CourseCat', 'active_length'), 
  'CourseCat', 'course type', 'Course Type'
)
createAttritionPlot(getPercs(edx.filt, 'age_cat', 'active_length'), 'age_cat', 'age', 'Age Category')
createAttritionPlot(getPercs(edx.filt, 'LoE_DI', 'active_length'), 'LoE_DI', 'level of education', 'Level of Education')
createAttritionPlot(getPercs(edx.filt, 'final_cc_cname_DI', 'active_length'), 'final_cc_cname_DI', 'country') 
createAttritionPlot(getPercs(edx.filt, 'school', 'active_length'), 'school', 'university')


grid.arrange(
  createAttritionPlot(
    getPercs(edx.filt, 'gender', 'active_length', c('m','f')), 
    'gender', 'gender', 'Gender'),
  createAttritionPlot(
    getPercs(edx.filt, 'CourseCat', 'active_length'), 
    'CourseCat', 'course type', 'Course Type'),
  createAttritionPlot(
    getPercs(edx.filt, 'age_cat', 'active_length'), 
    'age_cat', 'age', 'Age Category'),
  createAttritionPlot(
    getPercs(edx.filt, 'LoE_DI', 'active_length'), 
    'LoE_DI', 'level of education', 'Level of Education'),
  nrow=2
)

# facet wrap for age category
ggplot(getPercs(edx.filt, 'age_cat', 'active_length'), aes(x = active_length, y = perc)) +
  geom_line() +
  labs(
    title = 'User Engagement Attrition Rate (by age)', 
    y = "Percentage of Total Users", x = "Days Active") +
  theme_bw() +
  scale_colour_discrete(name = 'Age Category') +
  facet_wrap(~age_cat)

ggplot(getPercs(edx.filt, 'CourseName', 'active_length'), aes(x = active_length, y = perc)) +
  geom_line() +
  labs(
    title = 'User Engagement Attrition Rate (by course)', 
    y = "Percentage of Total Users", x = "Days Active") +
  theme_bw() +
  scale_colour_discrete(name = 'Age Category') +
  facet_wrap(~CourseName)

getPercs(subset(edx, certified == 1), 'gender', 'LoE_DI')

getPercs(subset(edx, certified == 1), 'gender', 'LoE_DI')

# number of completions
gg.bar <- ggplot(getPercs(subset(edx, certified == 1), 'gender', 'LoE_DI'), aes(x = LoE_DI, y = perc)) + 
  geom_bar(stat = "identity", position = "dodge", aes(fill = gender)) +
  labs(title = "Certifications Issued (by education and gender)", 
       y = " % Certified",x = "Level of Education") +
  theme_economist()

# interactivity with color scaling for final grade
gg.point <- ggplot(subset(edx, grade > 0 & grade <= 1), aes(x = log(nevents), y = ndays_act)) +
  geom_point(aes(color = grade)) + 
  scale_color_gradient() +
  labs(title = "User Interactivity (color-scaled by final grade)", 
       y = "Number of Days Active ",x = "Number of interaction events (log)") +
  theme_economist() + scale_fill_economist()


# interactivity re: chapters and forum posts -- why such convergences of chapters read?
gg.jitter <- ggplot(subset(edx, grade > 0 & grade <= 1), aes(x = nforum_posts, y = nchapters)) +
  geom_jitter(aes(color = grade)) + 
  scale_color_gradient() +
  labs(title = "User Interactivity (color-scaled by final grade)", 
       y = "Number of chapters completed ", x = "Number of forum posts") +
  theme_economist() + scale_fill_economist()


# violin of grade by education
gg.violin <- ggplot(subset(edx, !is.na(LoE_DI) & grade > 0 & grade <= 1 & gender %in% c("f","m")), 
       aes(x = factor(LoE_DI), y = grade, fill = gender)) +
  geom_violin() +
  labs(title = "Grade (by education, gender)", 
       y = "Grade", x = "Level of Education") +
  theme_economist() ##+ scale_fill_economist()

grid.arrange(gg.bar, gg.point, gg.jitter, gg.violin, nrow=2)

gg.bar
