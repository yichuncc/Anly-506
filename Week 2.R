#week 2 practice
#Importing, saving, and managing data
ls()
rm(list = ls()) #Removes all objects in the workspace
load(file = "data")	
write.table(x, file = "mydata.txt", sep)

getwd()  #current working directory is /Users/yichunliu

study1.df <- data.frame(id = 1:5, 
                        sex = c("m", "m", "f", "f", "m"), 
                        score = c(51, 20, 67, 52, 42))

score.by.sex <- aggregate(score ~ sex, 
                          FUN = mean, 
                          data = study1.df)

study1.htest <- t.test(score ~ sex, 
                       data = study1.df)
save(study1.df, score.by.sex, study1.htest,
     file = "Users/yichunliu/data/study1.RData")
# these steps save the objects as .RData file, saved in my current working directory

# rm() is remove fuction, to remove objects from workspace
# Remove huge.df from workspace
rm(huge.df)

#save objects as a .txt file
write.table(x = pirates,
            file = "pirates.txt",  # Save the file as pirates.txt
            sep = "\t")            # A string indicating how the columns are separated. Make the columns tab-delimited


#read a .txt file
mydata <- read.table(file = 'data/mydata.txt',    # file is in a data folder in my working directory
                     sep = '\t',                  # file is tab--delimited
                     header = TRUE,               # the first row of the data is a header row
                     stringsAsFactors = FALSE)    # this is always FALSE


#read txt file from website
fromweb <- read.table(file = 'https://moodle.harrisburgu.edu/pluginfile.php/628720/mod_resource/content/1/table.txt',
                      sep = '\t',
                      header = TRUE)
ls(fromweb)


