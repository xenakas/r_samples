library(foreign)
dataset = read.spss(file.choose(), to.data.frame=TRUE)

View(head(dataset))
attributes(dataset)$variable.labels
