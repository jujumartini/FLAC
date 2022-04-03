rm(list=ls())

 x <- read.csv("R:/PAHRL/Student Access/0_Students/MARTINEZ/3_Projects/FLAC/Noldus/Computer Science/Activity_Master_Project - FLAC_Example1 - Event Logs.csv",
               stringsAsFactors = F)
tail(x$Event_Type, n=1)
l <- length(x$Event_Type)
x$Event_Type[l] <- "End"
x$Event_Type[1] <- "Beginning"
x <- x[x$Event_Type!="State stop", ]
x <- x[ ,-c(1:4,11,17)]
x$High_Quality_Activity <- paste(x$Modifier_3, x$Modifier_4)
x$Domain <- x$Modifier_1
x$Environment <- x$Modifier_2
x <- x[ , -which(names(x) %in% c("Modifier_1", "Modifier_2", "Modifier_3","Modifier_4"))]

write.table(x, 
            file = paste0("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/", "FLAC_Example1_Activity.csv"),
            sep = ",",
            row.names = F,
            col.names = T)

y <- read.csv("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/Posture_Master_Project - FLAC_Example1 - Event Logs.csv",
              stringsAsFactors = F)
tail(y$Event_Type, n=1)
l <- length(y$Event_Type)
y$Event_Type[l] <- "End"
y$Event_Type[1] <- "Beginning"
y <- y[y$Event_Type!="State stop", ]
y <- y[ ,-c(1:4,11,15)]
y$Intensity <- y$Modifier_1
y <- y[ , -which(names(y) %in% c("Modifier_1", "Modifier_2", "Modifier_3","Modifier_4"))]

write.table(y, 
            file = paste0("R:/PAHRL/Student Access/0_Students/MARTINEZ/FLAC/Noldus/Computer Science/", "FLAC_Example1_Posture.csv"),
            sep = ",",
            row.names = F,
            col.names = T)
