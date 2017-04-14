library(RCurl)
library(purrr)
library(neuralnet)

data.url <- getURL("https://raw.githubusercontent.com/issactoast/Machine-Learning-with-R/master/Data/Concrete_Data.csv")
concrete <- read.csv(text = data.url, header = T)

colnames(concrete) <- c("Cement", "Slag", "Ash",
                        "Water", "Superplastic",
                        "CoarseAgg", "FineAgg",
                        "Age", "Strength")

str(concrete)
data.size <- dim(n.concrete)[1]

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

n.concrete <- map_df(concrete, normalize)
str(n.concrete)

train.index <- sample(data.size, round(data.size * 0.7))
length(train.index)

train.Data <- n.concrete[train.index, ]
test.Data <-  n.concrete[-train.index,]

formula.ann <- paste(colnames(train.X), collapse = " + ")
concrete_model <- neuralnet(formula = paste0("Strength ~ ", formula.ann),
                            data = train.Data, hidden = 5)

plot(concrete_model)

prediction <- compute(concrete_model, test.Data[,-9])

sum(as.vector(prediction$net.result - test.Data[,9])^2)
cor(prediction$net.result, test.Data[,9])






