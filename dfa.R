

data = read.morphodata("./tests/testFiles/sample.txt")

data = read.morphodata("./tests/testFiles/sample_NaNs.txt")

data = read.morphodata("./tests/testFiles/sample_numericNames.txt")


is.null(data$ID)

data = read.delim("./tests/testFiles/sample_NaNs.txt")

("ID" %in% colnames(data))


summary(data)


paste(levels(data$Population), collapse = " ")
