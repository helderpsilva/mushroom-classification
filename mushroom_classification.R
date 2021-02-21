# Title:    Mushroom Classification
# Authors:  Carla M. Lemos e Hélder P. Silva
# Course:   Data Mining I
# Date:     20/11/2020

# Install packages
# install.packages("tidyverse")
# install.packages("RWeka")
# install.packages('e1071')
# install.packages('rpart')
# install.packages('caret')
# install.packages('rpart.plot')
# install.packages("glmnet")
# install.packages("MLeval")

# Import packages
library("tidyverse")
# library("RWeka") # Requires rJava
library('e1071')
library('rpart')
library('caret')
library('rpart.plot')
library("glmnet")
library("MLeval")

# Set options
options(tibble.width = Inf)

# Set working directory
setwd("path/to/directory")

# Import data: Mushroom_Classification
mushroom <- read.csv("mushrooms.csv", na = '?', sep = ";")

# Check data structure
head(mushroom)

# Check variables
names(mushroom)

# Check data level
str(mushroom)

# Converting characters to factors
variaveis_nomes <- names(mushroom)
mushroom[,variaveis_nomes] <- lapply(mushroom[,variaveis_nomes] , 
                                             as.factor)

rm(variaveis_nomes)

# Check changes to data structure
str(mushroom)

# Rename variables
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

# Change levels
levels(mushroom$edibility) <- c("edible", "poisonous")
levels(mushroom$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushroom$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroom$bruises) <- c("no", "yes")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gill_attachement) <- c("attached", "free")
levels(mushroom$gill_spacing) <- c("close", "crowded")
levels(mushroom$gill_size) <- c("broad", "narrow")
levels(mushroom$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalk_shape) <- c("enlarging", "tapering")
levels(mushroom$stalk_root) <- c("bulbous", "club", "equal", "rooted")
levels(mushroom$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushroom$veil_type) <- "partial"
levels(mushroom$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ring_number) <- c("none", "one", "two")
levels(mushroom$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

# Confirm changes
glimpse(mushroom)

# Fucntion to count levels
number_class <- function(x){
  x <- length(levels(x))
}

ms_categories <- mushroom %>% map_dbl(function(.x) number_class(.x)) %>% tibble::enframe() %>% 
  rownames_to_column() %>% arrange(desc(value))
colnames(ms_categories) <- c("index variable", "variable name", "number of levels")

ms_categories
rm(number_class)

# Removing "veil-type"
mushroom <- mushroom %>% select(-c ("veil_type"))   # No information provided by this variable.


# Missing values
missing_values<-data.frame(apply(mushroom,2,function(x){sum(is.na(x))}))
names(missing_values)[1]='Valores omissos'

missing_values

# Removing "stalk_root" due to missing values.
mushroom <- mushroom %>% select(-c ("stalk_root"))

# Frequency tables
for (categoria in names(mushroom)){
  print(categoria)
  print(table(mushroom[categoria]))
}

rm(categoria)

# EDA
# Bar graph - gill-color
ggplot(mushroom, aes(x=gill_color))+
  geom_bar(aes(fill= edibility))+
  xlab("gill-color")+
  ylab("Count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - cap-color
ggplot(mushroom, aes(x=cap_color))+
  geom_bar(aes(fill= edibility))+
  xlab("cap-color")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - stalk_color_above_ring
ggplot(mushroom, aes(x=stalk_color_above_ring))+
  geom_bar(aes(fill= edibility))+
  xlab("stalk_color_above_ring")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - stalk_color_below_ring
ggplot(mushroom, aes(x=stalk_color_below_ring))+
  geom_bar(aes(fill= edibility))+
  xlab("stalk_color_below_ring")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - odor
ggplot(mushroom, aes(x=odor))+
  geom_bar(aes(fill= edibility))+
  xlab("odor")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - spore_print_color
ggplot(mushroom, aes(x=spore_print_color))+
  geom_bar(aes(fill= edibility))+
  xlab("spore_print_color")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - habitat
ggplot(mushroom, aes(x=habitat))+
  geom_bar(aes(fill= edibility))+
  xlab("habitat")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - cap_shape
ggplot(mushroom, aes(x=cap_shape))+
  geom_bar(aes(fill= edibility))+
  xlab("cap_shape")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - population
ggplot(mushroom, aes(x=population))+
  geom_bar(aes(fill= edibility))+
  xlab("population")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - gill_attachment
ggplot(mushroom, aes(x=gill_attachement))+
  geom_bar(aes(fill= edibility))+
  xlab("gill_attachement")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - ring_type
ggplot(mushroom, aes(x=ring_type))+
  geom_bar(aes(fill= edibility))+
  xlab("ring_type")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - cap_surface
ggplot(mushroom, aes(x=cap_surface))+
  geom_bar(aes(fill= edibility))+
  xlab("cap_surface")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - stalk_surface_above_ring
ggplot(mushroom, aes(x=stalk_surface_above_ring))+
  geom_bar(aes(fill= edibility))+
  xlab("stalk_surface_above_ring")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - stalk_surface_below_ring
ggplot(mushroom, aes(x=stalk_surface_below_ring))+
  geom_bar(aes(fill= edibility))+
  xlab("stalk_surface_below_ring")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - veil_color
ggplot(mushroom, aes(x=veil_color))+
  geom_bar(aes(fill= edibility))+
  xlab("veil_color")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - ring_number
ggplot(mushroom, aes(x=ring_number))+
  geom_bar(aes(fill= edibility))+
  xlab("ring_number")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - bruises
ggplot(mushroom, aes(x=bruises))+
  geom_bar(aes(fill= edibility))+
  xlab("bruises")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - gill_spacing
ggplot(mushroom, aes(x=gill_spacing))+
  geom_bar(aes(fill= edibility))+
  xlab("gill_spacing")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - gill_size
ggplot(mushroom, aes(x=gill_size))+
  geom_bar(aes(fill= edibility))+
  xlab("gill_size")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Bar graph - stalk_shape
ggplot(mushroom, aes(x=stalk_shape))+
  geom_bar(aes(fill= edibility))+
  xlab("stalk_shape")+
  ylab("count")+
  ggtitle("Distribuição dos cogumelos")+
  scale_fill_manual(values=c("#c9c9c9", "#999999"), labels = c("Edible", "Poisonous"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Scatterplots
ggplot(mushroom, aes(x = cap_surface, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("#2a9d8f", "#e76f51"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

ggplot(mushroom, aes(x = cap_shape, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("#2a9d8f", "#e76f51"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

ggplot(mushroom, aes(x = gill_color, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("#2a9d8f", "#e76f51"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

ggplot(mushroom, aes(x = edibility, y = odor, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("#2a9d8f", "#e76f51"))+
  theme_light()+
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# Check distribution
table(mushroom$edibility)

test_of_independence <- function(x_, y_){
  chisq.test(table(x_, y_))
}

test_of_independence(mushroom$edibility, mushroom$odor)
test_of_independence(mushroom$edibility, mushroom$gill_size)
test_of_independence(mushroom$edibility, mushroom$stalk_surface_above_ring)
test_of_independence(mushroom$edibility, mushroom$spore_print_color)
test_of_independence(mushroom$edibility, mushroom$ring_type)

rm(test_of_independence)


# Model
# Separate in train and test data (80-20)
set.seed(12)
split <- sample(2, nrow(mushroom), replace = TRUE, prob = c(0.8,
                                                           0.2))
training_mushroom <- mushroom[split == 1, ]
test_mushroom <- mushroom[split == 2, ]
nrow(training_mushroom)
nrow(test_mushroom)

rm(split)

glimpse(training_mushroom)

# Cook's distance
modelo_prep <- glm(edibility ~., data = training_mushroom, family = binomial)
cooks_dist <- cooks.distance(modelo_prep)

plot(cooks_dist, main="Observações com potencial de distúrbio")
abline(h = 3*mean(cooks_dist, na.rm=T), col="red")

#Tratar obs influenciadores - remover
training_mushroom[which(cooks_dist>3*mean(cooks_dist, na.rm=T)) , ]
training_mushroom <- training_mushroom[-which(cooks_dist>3*mean(cooks_dist, na.rm=T)) , ]

rm(modelo_prep, cooks_dist)

# Check variable importance (requires rjava)
gain_ratio <- GainRatioAttributeEval( edibility ~ . , data = training_mushroom )
print( sort( gain_ratio, decreasing = TRUE ))

# Fitting Logistic Regression (5 features)
classifier_glm <- glm(formula = edibility ~ odor + gill_size + stalk_surface_above_ring + spore_print_color + ring_type,
                     family = binomial,
                     data = training_mushroom)

summary(classifier_glm)
classifier_glm

# Predict
prob_pred <- predict(classifier_glm, type = 'response', newdata = test_mushroom[-1])
y_pred_glm <- ifelse(prob_pred > 0.5, "poisonous", "edible")

# Confusion Matrix
confusionMatrix(as.factor(y_pred_glm), test_mushroom$edibility)

# ------------------------------------------------------------------------------

# Improving Logistic Regression
# gill_size + stalk_surface_above_ring
classifier_glm_improved <- glm(formula = edibility ~ gill_size + stalk_surface_above_ring,
                      family = binomial,
                      data = training_mushroom)

classifier_glm_improved
summary(classifier_glm_improved)


# Predict
prob_pred_improved <- predict(classifier_glm_improved, type = 'response', newdata = test_mushroom[-1])
y_pred_glm_improved <- ifelse(prob_pred_improved > 0.5, "poisonous", "edible")

# Confusion Matrix
confusionMatrix(as.factor(y_pred_glm_improved), test_mushroom$edibility)

# ------------------------------------------------------------------------------

# Improving Logistic Regression
# gill_size + ring
classifier_glm_improved_02 <- glm(formula = edibility ~ gill_size + ring_type,
                               family = binomial,
                               data = training_mushroom)

classifier_glm_improved_02
summary(classifier_glm_improved_02)

# Predict
prob_pred_improved_02 <- predict(classifier_glm_improved_02, type = 'response', newdata = test_mushroom[-1])
y_pred_glm_improved_02 <- ifelse(prob_pred_improved_02 > 0.5, "poisonous", "edible")

# Confusion Matrix
confusionMatrix(as.factor(y_pred_glm_improved_02), test_mushroom$edibility)

# ------------------------------------------------------------------------------

# Fitting Decision tree
classifier_dt <- rpart(formula = edibility ~ .,data = training_mushroom)

classifier_dt
summary(classifier_dt)

# Predict
y_pred_dt <- predict(classifier_dt, newdata = test_mushroom[-1], type = 'class')

# Confusion Matrix
confusionMatrix(test_mushroom$edibility, y_pred_dt)
rpart.plot(classifier_dt)

# Visualizing the sorted attributes
ggplot(mushroom, aes( x = odor, fill = edibility)) + facet_wrap(~spore_print_color) +geom_histogram(stat = 'count') +
  ggtitle('Class distribution based on odor and spore_print_color')+
  theme_light() + 
  theme(axis.text=element_text(size=6), plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------

# Fitting SVM model
classifier_svm <- svm(edibility ~. , data=training_mushroom, cost = 1000, gamma = 0.01)
classifier_svm

summary(classifier_svm)

# Predict
y_pred_svm <- predict(classifier_svm, newdata = test_mushroom[-1])

# Confusion Matrix
confusionMatrix(test_mushroom$edibility, y_pred_svm)
