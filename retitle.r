# Author: Vera

library(stringr)


school<- read.table ("schools.csv", sep="\t")
schools <- as.vector (school [,1])

# Из общей базы создадим отдельные для разного типа школ и присвоим им ID.
### только коррекционные школы.
corrSCHool <- schools [grep("корр", schools )] 
номер <- str_extract(corrSCHool, "[0-9]{1,3}")
вид <- str_extract(corrSCHool, "[A-Z]{1,4}")
вид2 <- str_extract(corrSCHool, "\\b[вида]{4}\\b")
тип <- str_extract(corrSCHool, "\\b[школа]{5}\\b")
интернат <- str_extract(corrSCHool, "\\b[интернат]{8}\\b")
сад <- str_extract(corrSCHool, "\\b[детский сад]{11}\\b")


Sch_id <- (1001:1055)
LISTcorrSchools <- gsub("[NA]", "", (paste("Коррекционная", тип, интернат , сад, вид, вид2, номер , sep = " ")))
write.table((data.frame (c(1001:1055),LISTcorrSchools)), file="Коррекционные школы.txt", row.names=FALSE, sep="\t")

### только образовательные центры
обцентр <- schools [grep("ентр образ", schools )] 
номер <- str_extract(обцентр , "[0-9]{1,3}")
спецназвание <- str_extract(обцентр, "\\b[Центр информационной культуры]{29}\\b")

LISTобцентр <- gsub("[NA]", "", (paste("Центр образования", номер , спецназвание, sep = " ")))
write.table((data.frame (c(1101:1114),LISTобцентр)), file="Центры образования.txt", row.names=FALSE, sep="\t")


###Вечерние школы
сменные<- schools [grep("смен", schools )] 
номер <- str_extract(сменные, "[0-9]{1,3}")

вечерние <- gsub("[NA]", "", (paste("Вечерняя школа", номер , sep = " ")))
write.table((data.frame (c(1301:1305),вечерние)), file="Сменные школы.csv", row.names=FALSE, sep = ",")



### школы, лицеи, гимназии

school <- as.numeric (gsub("[^0-9]", "", schools))
school <- school [!is.na (school )]
school <- unique(school)

# Обшибка в названии Школы. 
school [school ==8585] = 85
school [school ==289289] = 289
school [school ==3030] = 30

find <- data.frame (paste(school, paste("Школа", school , sep = " "),sep="\t"))

write.table(find, file="Schools.txt", row.names=FALSE, sep="\t")
