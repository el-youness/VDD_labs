students <- read.table(file= "studentStudy.txt",
                       row.names=1, header=T, sep="\t")
students_2 = students
c_sum = colSums(students_2)
students_2 = rbind(students_2, "total" = c_sum)

r_sum = rowSums(students_2)
students_2$total = r_sum

#contengency table
cont_table <- prop.table(students)*100

c_sum = colSums(cont_table)
cont_table = rbind(cont_table, "total" = c_sum)

r_sum = rowSums(cont_table)
cont_table$total = r_sum