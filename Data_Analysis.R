library(data.table)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(rlang)

library(FactoMineR)
library(factoextra)

#references 
#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/114-mca-multiple-correspondence-analysis-in-r-essentials/#google_vignette

setwd("enter your path") #change this to your path

# must always run data transforming section first
# ============ data transforming =============== start
#only accepts csv
product.dt <- fread("Product_Survery_Results_2025 - (changed).csv", stringsAsFactors = 1)
colnames(product.dt)

#initialize the column names 
col_vector <- c(
  "Timestamp",
  "Design ideas" 
)

class(product.dt) #check if it is data.table

#remove the col_vectors
clean_product.dt <- product.dt %>% select(-all_of(col_vector))

# change all the na strings to no opinion
clean_product.dt <- clean_product.dt %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "No Opinion", .)))

# change back the columns to factor, because after mutate, it becomes string
clean_product.dt <- clean_product.dt %>%
  mutate(across(where(is.character), as.factor))
# ============ data transforming =============== end
# xxxxx
# xxxxx
# ============ graph plotting (figure 2,3,4,5,7,10,11,12,13) =============== start
col_vector <- c(
  "Age group", 
  "Gender",
  "Do you own a car?",
  "Marital status",
  "Customise vehicle",
  "Spending on customisation",
  "Designing your own car",
  "Spending on design",
  "3D experience"
)

human.dt <- product.dt %>% dplyr::select(all_of(col_vector))

# plot the graphs
human.dt.plot <- list()

for (i in 1: length(colnames(human.dt)))
{
  variable.names <- colnames(human.dt)[i]
  temp <- ggplot(human.dt, aes(x = !!sym(variable.names))) +
    geom_bar(fill = "steelblue", color = "black", width = 0.5) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.3) +
    labs(title = paste("Count Plot of", variable.names), x = variable.names, y = "Count") 
     
  human.dt.plot[[variable.names]] <- temp
  
}
col_vector
index <- 5 #graph is printed by changing this index
colname_index <- colnames(human.dt)[index]
human.dt.plot[[colname_index]]
# ============ graph plotting (figure 2,3,4,5,7,10,11,12,13) =============== end
# xxxxx
# xxxxx
# ============ graph plotting (figure 6) =============== start
# horizontal barplot for the factors count
library(ggplot2)

#selecting all the factor_columns
col_vector <- c(
  "factor_Price",
  "factor_Brand name",
  "factor_Aesthetics",
  "factor_Technological features",
  "factor_Functionality",
  "factor_Size",
  "factor_Sustainability/environment considerations",
  "factor_Customisable options"
)
graph.dt <- clean_product.dt %>% select(all_of(col_vector))

# this function return 1 for _y and 0 for _n
convert_value <- function(x) {
  if (grepl("_y$", x)) {
    return(1)
  } else if (grepl("_n$", x)) {
    return(0)
  } else {
    return(NA)
  }
}

# Apply the conversion function to each column and sum the results
sums <- lapply(graph.dt, function(column) sum(sapply(column, convert_value)))

#make the sums into data.table
sums.dt <- data.table(Column = names(sums), Sum = unlist(sums))

#reorder define x axis, and sum is just ordering feature with lowest sum appear first and higher sum appear last
ggplot(sums.dt, aes(x = reorder(Column, Sum), y = Sum)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = Sum), hjust = -0.3) +
  labs(title = "Feature Importance, 50 records", x = "Feature", y = "Sum") +
  theme_minimal() +
  coord_flip()
# ============ graph plotting (figure 6) =============== end
# xxxxx
# xxxxx
# ============ graph plotting (figure 8) =============== start
# horizontal barplot for the exterior count
library(ggplot2)
colnames(clean_product.dt)

#selecting all the factor_columns
col_vector <- c(
  "exterior_Wheels",
  "exterior_Bumpers",
  "exterior_Grilles",
  "exterior_Headlights",
  "exterior_Side mirrors"
)
                                 
graph.dt <- clean_product.dt %>% select(all_of(col_vector))

# this function return 1 for _y and 0 for _n
convert_value <- function(x) {
  if (grepl("_y$", x)) {
    return(1)
  } else if (grepl("_n$", x)) {
    return(0)
  } else {
    return(NA)
  }
}

# Apply the conversion function to each column and sum the results
sums <- lapply(graph.dt, function(column) sum(sapply(column, convert_value)))

#make the sums into data.table
sums.dt <- data.table(Column = names(sums), Sum = unlist(sums))

#reorder define x axis, and sum is just ordering feature with lowest sum appear first and higher sum appear last
ggplot(sums.dt, aes(x = reorder(Column, Sum), y = Sum)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = Sum), hjust = -0.3) +
  labs(title = "Exterior selection, 50 records", x = "Feature", y = "Sum") +
  theme_minimal() +
  coord_flip()
# ============ graph plotting (figure 8) =============== end
# xxxxx
# xxxxx
# ============ graph plotting (figure 9) =============== start
# horizontal barplot for the interior count
library(ggplot2)
colnames(clean_product.dt)

#selecting all the factor_columns
col_vector <- c(
  "interior_Centre compartment",
  "interior_Dashboard",
  "interior_Steering wheel",
  "interior_Sun blocker for front passengers",
  "interior_Door handles",
  "interior_air vent"
)

graph.dt <- clean_product.dt %>% select(all_of(col_vector))

# this function return 1 for _y and 0 for _n
convert_value <- function(x) {
  if (grepl("_y$", x)) {
    return(1)
  } else if (grepl("_n$", x)) {
    return(0)
  } else {
    return(NA)
  }
}

# Apply the conversion function to each column and sum the results
sums <- lapply(graph.dt, function(column) sum(sapply(column, convert_value)))

#make the sums into data.table
sums.dt <- data.table(Column = names(sums), Sum = unlist(sums))

#reorder define x axis, and sum is just ordering feature with lowest sum appear first and higher sum appear last
ggplot(sums.dt, aes(x = reorder(Column, Sum), y = Sum)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
  geom_text(aes(label = Sum), hjust = -0.3) +
  labs(title = "Interior selection, 50 records", x = "Feature", y = "Sum") +
  theme_minimal() +
  coord_flip()
# ============ graph plotting (figure 9) =============== end
# xxxxx
# xxxxx
# ============ start of question2 ===============
library(factoextra)
library(FactoMineR)

colnames(clean_product.dt) #select the one hot encoded data for analysis

col_vector <- c(
  "Customise vehicle",
  "factor_Price",
  "factor_Brand name",
  "factor_Aesthetics",
  "factor_Technological features",
  "factor_Functionality",
  "factor_Size",
  "factor_Sustainability/environment considerations",
  "factor_Customisable options",
  "exterior_Wheels",
  "exterior_Bumpers",
  "exterior_Grilles",
  "exterior_Headlights",
  "exterior_Side mirrors",
  "interior_Centre compartment",
  "interior_Dashboard",
  "interior_Steering wheel",
  "interior_Sun blocker for front passengers",
  "interior_Door handles",
  "interior_air vent"
)

# filter only the columns that are from col_vector
question2.dt <- clean_product.dt %>% select(all_of(col_vector))

#change the dataset from factor to character, because ifelse only work for numeric and char
question2.dt$`Customise vehicle` <- as.character(question2.dt$`Customise vehicle`)

# combine very likely with likely for the customise vehicle column
question2.dt$`Customise vehicle` <- ifelse(question2.dt$`Customise vehicle` == "Very likely", "Likely",question2.dt$`Customise vehicle`)

# change back to factor
question2.dt$`Customise vehicle` <- as.factor(question2.dt$`Customise vehicle`)

test.active <- question2.dt %>% select(-c("factor_Brand name", "factor_Aesthetics", "factor_Technological features", factor_Functionality, factor_Size, "factor_Sustainability/environment considerations", "factor_Customisable options"))

#set this for the active MCA 
#alternatively, this ncp limits the number of dimensions
res.mca <- MCA(question2.dt, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.mca)

#this is the scree plot for the MCA
#this plots the dimension that shows the variance for the data 
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

# the graphical dimensions 
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 3, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 4, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 5, top = 15)


fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal(),
                col.var = "contrib",
                gradient.cols = c("blue","red"))
# ============ end of question2 ===============
# xxxxx
# xxxxx
# ============ start of question4 =============== start
library(factoextra)
library(FactoMineR)

colnames(clean_product.dt) #select the one hot encoded data for analysis

col_vector <- c(
  "Age group",
  "Gender",
  "Customise vehicle",
  "Spending on customisation",
  "Spending on design"
)

# filter only the columns that are from col_vector
question2.dt <- clean_product.dt %>% select(all_of(col_vector))

#change the dataset from factor to character, because ifelse only work for numeric and char
question2.dt$`Customise vehicle` <- as.character(question2.dt$`Customise vehicle`)

# combine very likely with likely for the customise vehicle column
question2.dt$`Customise vehicle` <- ifelse(question2.dt$`Customise vehicle` == "Very likely", "Likely",question2.dt$`Customise vehicle`)

# change back to factor
question2.dt$`Customise vehicle` <- as.factor(question2.dt$`Customise vehicle`)

#set this for the active MCA 
#alternatively, this ncp limits the number of dimensions
res.mca <- MCA(question2.dt, ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.mca)

#this is the scree plot for the MCA
#this plots the dimension that shows the variance for the data 
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

# the graphical dimensions 
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 3, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 4, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 5, top = 15)


fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())

fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow if many point)
             ggtheme = theme_minimal(),
             col.var = "contrib",
             gradient.cols = c("blue","red"))
# ============ end of question4 =============== end
# xxxxx
# xxxxx
# ============ graph plotting (Distribution of do you own car faceted against age group) ============ start
#make a independent copy of the data
dt <- data.table::copy(clean_product.dt)  # For data.tables

col_vector <- c(
  "Age group",
  "Do you own a car?"
)

# filter the dataframe to col_vector
dt <- dt %>% select(all_of(col_vector))

#to do this, remember to group by
ggplot(dt, aes(x = !!sym("Do you own a car?"))) + 
  geom_bar(fill = "steelblue", color = "black") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1) +
  labs(title = "Distribution of do you own car faceted against age group") + 
  facet_grid(rows = vars(!!sym("Age group"))) + 
  scale_y_continuous(limits = c(0,40)) +
  theme(
        axis.text.x = element_text(size = 8))  # Reduce font size)

# ============ graph plotting (Distribution of do you own car faceted against age group) ============ end
# xxxxx
# xxxxx
# ============ graph plotting (Distribution of marital faceted against age group) ============ start
#make a independent copy of the data
dt <- data.table::copy(clean_product.dt)  # For data.tables

col_vector <- c(
  "Age group",
  "Marital status"
)

# filter the dataframe to col_vector
dt <- dt %>% select(all_of(col_vector))

#to do this, remember to group by
ggplot(dt, aes(x = !!sym("Marital status"))) + 
  geom_bar(fill = "steelblue", color = "black") + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1) +
  labs(title = "Distribution of marital faceted against age group") + 
  facet_grid(rows = vars(!!sym("Age group"))) + 
  scale_y_continuous(limits = c(0,40)) +
  theme(
  axis.text.x = element_text(size = 8))  # Reduce font size)
# ============ graph plotting (Distribution of marital faceted against age group) ============ end
# xxxxx
# xxxxx
# ============ graph plotting (spending for customization facet with age group) ============ start
dt <- clean_product.dt

#make a independent copy of the data
dt <- data.table::copy(clean_product.dt)  # For data.tables
colnames(dt)

col_var1 <- colnames(dt)[1] #age group?
col_var3 <- colnames(dt)[6] #own car? 

dt[, .SD, by = col_var1] #group by the data for facet_grid

#to do this, remember to group by
ggplot(dt, aes(x = !!sym(col_var3))) + 
  geom_bar(fill = "steelblue", color = "black", width = 0.7) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1) +
  facet_grid(rows = vars(!!sym(col_var1))) + 
  labs(title = "Spending on customisation facet with age group") +
  scale_y_continuous(limits = c(0,40)) +
  theme(
  axis.text.x = element_text(size = 8))  # Reduce font size)

# ============ graph plotting (spending for customization facet with age group) ============ start

 



