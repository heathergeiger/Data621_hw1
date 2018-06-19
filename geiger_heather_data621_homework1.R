## ----load-libraries, echo=FALSE, eval=TRUE,results='hide', message=FALSE, warning=FALSE----
library(ggplot2)
library(dplyr)
library(tidyr)

## ----read-in-data-and-check-num-values-per-variable, echo=FALSE, eval=TRUE----
baseball <- read.csv("https://raw.githubusercontent.com/heathergeiger/Data621_hw1/master/moneyball-training-data.csv",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
baseball <- baseball[,setdiff(colnames(baseball),"INDEX")]
non_NA_per_column <- baseball %>%
gather() %>%
na.omit(value) %>%
count(key)
non_NA_per_column <- data.frame(non_NA_per_column)
data.frame(Variable = non_NA_per_column[,1],n = non_NA_per_column[,2])

## ----boxplot-across-variables, echo=FALSE, eval=TRUE,fig.width=12,fig.height=8----
baseball[,setdiff(colnames(baseball),"TEAM_BATTING_HBP")] %>%
gather() %>%
na.omit(value) %>%
ggplot(.,
aes(x=key,y=value)) +
geom_boxplot() +
xlab("") +
ylab("") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----boxplot-across-variables-reduce-ylim, echo=FALSE, eval=TRUE,fig.width=12,fig.height=8----
baseball[,setdiff(colnames(baseball),"TEAM_BATTING_HBP")] %>%
gather() %>%
na.omit(value) %>%
ggplot(.,
aes(x=key,y=value)) +
geom_boxplot() +
xlab("") +
ylab("") +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
coord_cartesian(ylim=c(0,4000),xlim=c(1,ncol(baseball)))

## ----hide-implausible-values-before-further-exploration, echo=FALSE, eval=TRUE----
baseball_for_exploration <- baseball[,setdiff(colnames(baseball),"TEAM_BATTING_HBP")]

outlier_set1 <- which(baseball[,"TEAM_PITCHING_SO"] > 1450)
outlier_set2 <- which(baseball[,"TEAM_FIELDING_E"] > 1046)
outlier_set3 <- which(baseball[,"TEAM_PITCHING_H"] >= 3000)

outliers <- unique(c(outlier_set1,outlier_set2,outlier_set3))

baseball_for_exploration <- baseball_for_exploration[setdiff(1:nrow(baseball_for_exploration),outliers),]

## ----correlation-heatmap,echo=FALSE, eval=TRUE---------------------------
baseball_predictors <- baseball_for_exploration[,setdiff(colnames(baseball_for_exploration),"TARGET_WINS")]

correlation_variables <- abs(cor(baseball_predictors,use="pairwise.complete.obs"))

for(i in 1:(nrow(correlation_variables) - 1))
{
correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(baseball_predictors),ncol(baseball_predictors)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(baseball_predictors))
correlation_variables$y <- factor(correlation_variables$y,levels=rev(colnames(baseball_predictors)))

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50")

correlation_variables_to_print <- correlation_variables[which(is.na(correlation_variables$correlation) == FALSE),]

correlation_variables_to_print$correlation <- round(correlation_variables_to_print$correlation,digits=2)

correlation_variables_to_print[order(correlation_variables_to_print$correlation,decreasing=TRUE)[1:20],]

## ----correlation-scatterplots, echo=FALSE, eval=TRUE---------------------
batting_vs_pitching <- data.frame(Record.num = rep(1:nrow(baseball_for_exploration),times=4),
	Variable = rep(c("HR","SO","BB","H"),each=nrow(baseball_for_exploration)),
	Batting = c(baseball_for_exploration[,"TEAM_BATTING_HR"],baseball_for_exploration[,"TEAM_BATTING_SO"],baseball_for_exploration[,"TEAM_BATTING_BB"],baseball_for_exploration[,"TEAM_BATTING_H"]),
	Pitching = c(baseball_for_exploration[,"TEAM_PITCHING_HR"],baseball_for_exploration[,"TEAM_PITCHING_SO"],baseball_for_exploration[,"TEAM_PITCHING_BB"],baseball_for_exploration[,"TEAM_PITCHING_H"]),
	stringsAsFactors=FALSE)

batting_vs_pitching %>%
na.omit() %>%
ggplot(.,
aes(Batting,Pitching)) +
geom_point(alpha=1/10) + 
facet_wrap(~Variable,ncol=2,scales="free") +
geom_smooth(method="lm") +
geom_abline(slope=1,linetype=2)

## ----check-num-identical-batting-vs-pitching, echo=FALSE, eval=TRUE------
BB_identical <- which(batting_vs_pitching$Variable == "BB" & (batting_vs_pitching$Batting == batting_vs_pitching$Pitching))
HR_identical <- which(batting_vs_pitching$Variable == "HR" & (batting_vs_pitching$Batting == batting_vs_pitching$Pitching))
SO_identical <- which(batting_vs_pitching$Variable == "SO" & (batting_vs_pitching$Batting == batting_vs_pitching$Pitching))
H_identical <- which(batting_vs_pitching$Variable == "H"   & (batting_vs_pitching$Batting == batting_vs_pitching$Pitching))

print("Number of records with identical values for pitching and batting BB:")
length(BB_identical)
print("Number of records with identical values for pitching and batting HR:")
length(HR_identical)
print("Number of records with identical values for pitching and batting SO:")
length(SO_identical)
print("Number of records with identical values for pitching and batting H:")
length(H_identical)

BB_identical_original_record_numbers <- which(baseball_for_exploration[,"TEAM_PITCHING_BB"] == baseball_for_exploration[,"TEAM_BATTING_BB"])
HR_identical_original_record_numbers <- which(baseball_for_exploration[,"TEAM_PITCHING_HR"] == baseball_for_exploration[,"TEAM_BATTING_HR"])
SO_identical_original_record_numbers <- which(baseball_for_exploration[,"TEAM_PITCHING_SO"] == baseball_for_exploration[,"TEAM_BATTING_SO"])
H_identical_original_record_numbers <- which(baseball_for_exploration[,"TEAM_PITCHING_H"] == baseball_for_exploration[,"TEAM_BATTING_H"])

print("Table for number of variables with pitching and batting identical, per record:")

freq_per_record <- data.frame(table(c(BB_identical_original_record_numbers,HR_identical_original_record_numbers,SO_identical_original_record_numbers,H_identical_original_record_numbers)))

table(freq_per_record[,2])

## ----remove-records-with-all-identical-batting-vs-pitching-from-exploration-data, echo=FALSE, eval=TRUE----
records_to_remove <- BB_identical_original_record_numbers

baseball_for_exploration <- baseball_for_exploration[setdiff(1:nrow(baseball_for_exploration),records_to_remove),]

## ----num-non-NA-per-variable-after-remove-implausible, echo=FALSE, eval=TRUE----
non_NA_per_column <- baseball_for_exploration %>%
gather() %>%
na.omit(value) %>%
count(key)
non_NA_per_column <- data.frame(non_NA_per_column)
data.frame(Variable = non_NA_per_column[,1],n = non_NA_per_column[,2])

## ----boxplot-across-variables-after-remove-implausible, echo=FALSE, eval=TRUE,fig.width=12,fig.height=8----
baseball_for_exploration %>%
gather() %>%
na.omit(value) %>%
ggplot(.,
aes(x=key,y=value)) +
geom_boxplot() +
xlab("") +
ylab("") +
theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----stats-summary-across-variables-after-remove-implausible, echo=FALSE, eval=TRUE----
apply(baseball_for_exploration,2,summary)

## ----density-per-variable-after-remove-implausible, echo=FALSE, eval=TRUE----
baseball_for_exploration %>%
gather() %>%
na.omit(value) %>%
ggplot(.,
aes(x=value)) +
geom_density() +
facet_wrap(~key,ncol=4,scales="free")

## ----correlation-heatmap-after-remove-implausible,echo=FALSE, eval=TRUE,message=FALSE, warning=FALSE----
baseball_predictors <- baseball_for_exploration[,setdiff(colnames(baseball_for_exploration),"TARGET_WINS")]

correlation_variables <- abs(cor(baseball_predictors,use="pairwise.complete.obs"))

for(i in 1:(nrow(correlation_variables) - 1))
{
correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(baseball_predictors),ncol(baseball_predictors)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(baseball_predictors))
correlation_variables$y <- factor(correlation_variables$y,levels=rev(colnames(baseball_predictors)))

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2)

correlation_variables_recolor_to_max_0.75 <- correlation_variables
correlation_variables_recolor_to_max_0.75$correlation[correlation_variables_recolor_to_max_0.75$correlation > 0.75] <- NA

correlation_variables_recolor_to_max_0.75 %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2) +
ggtitle("Same heatmap as above,\nexcept hide values > 0.75 to better show differences\nbetween smaller correlations")

correlation_variables_to_print <- correlation_variables[which(is.na(correlation_variables$correlation) == FALSE),]

correlation_variables_to_print$correlation <- round(correlation_variables_to_print$correlation,digits=2)

correlation_variables_to_print[order(correlation_variables_to_print$correlation,decreasing=TRUE)[1:20],]

## ----correlation-per-variable-vs-wins-after-remove-implausible,echo=FALSE, eval=TRUE----
correlation_vs_wins <- data.frame(Variable = colnames(baseball_predictors),Correlation = 0,stringsAsFactors=FALSE)

for(i in 1:ncol(baseball_predictors))
{
correlation_vs_wins$Correlation[i] <- cor(baseball_for_exploration[,"TARGET_WINS"],baseball_predictors[,i],use="pairwise.complete.obs")
}

correlation_vs_wins$Correlation <- round(correlation_vs_wins$Correlation,digits=2)

correlation_vs_wins

## ----scatterplots-variables-vs-wins-after-remove-implausible,echo=FALSE, eval=TRUE,fig.width=12,fig.height=8----
baseball_predictors_long <- gather(baseball_predictors)
baseball_predictors_long <- data.frame(baseball_predictors_long)
colnames(baseball_predictors_long) <- c("Variable","value")

baseball_predictors_long <- data.frame(baseball_predictors_long,Wins = rep(baseball_for_exploration[,"TARGET_WINS"],times=ncol(baseball_predictors)),stringsAsFactors=FALSE)

baseball_predictors_long %>%
na.omit() %>%
ggplot(.,
aes(value,Wins)) +
geom_point(alpha=1/10) +
facet_wrap(~Variable,scales="free_x") +
xlab("Variable") +
ylab("Wins") +
geom_smooth(method="lm")

## ----remove-records-and-variables-from-main-data, echo=FALSE, eval=TRUE----
#Replace the main data frame (baseball) with the one we used for data exploration (baseball_for_exploration).
#This already has variable TEAM_BATTING_HBP removed.
#It also already has the 891 records with implausible values removed.

baseball <- baseball_for_exploration

#Remove additional variables mentioned above.

baseball <- baseball[,setdiff(colnames(baseball),c(paste0("TEAM_PITCHING_",c("BB","H","HR","SO")),"TEAM_BASERUN_CS"))]

print("Variables remaining after remove select variables:")

colnames(baseball)

## ----replace-hits-with-singles, echo=FALSE, eval=TRUE--------------------
baseball <- baseball %>% mutate(TEAM_BATTING_1B = TEAM_BATTING_H - (TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR))
baseball <- data.frame(baseball,check.names=FALSE,stringsAsFactors=FALSE)

baseball <- baseball[,setdiff(colnames(baseball),"TEAM_BATTING_H")]

## ----records-with-missing-variables, echo=FALSE, eval=TRUE---------------
missing_stolen_bases <- which(is.na(baseball[,"TEAM_BASERUN_SB"]) == TRUE)
missing_batting_strikeouts <- which(is.na(baseball[,"TEAM_BATTING_SO"]) == TRUE)
missing_double_plays <- which(is.na(baseball[,"TEAM_FIELDING_DP"]) == TRUE)

print("Number of records missing TEAM_BASERUN_SB:")
length(missing_stolen_bases)
print("Number of records missing TEAM_BATTING_SO:")
length(missing_batting_strikeouts)
print("Number of records missing both:")
length(intersect(missing_stolen_bases,missing_batting_strikeouts))

print("Number of records missing TEAM_FIELDING_DP:")
length(missing_double_plays)

print("Number of records missing TEAM_FIELDING_DP and TEAM_BASERUN_SB:")
length(intersect(missing_double_plays,missing_stolen_bases))

print("Number of records missing TEAM_FIELDING_DP and TEAM_BATTING_SO:")
length(intersect(missing_double_plays,missing_batting_strikeouts))

## ----model-for-missing-variables, echo=FALSE, eval=TRUE------------------
model_strikeouts <- lm(`TEAM_BATTING_SO` ~ .,data=baseball[,setdiff(colnames(baseball),"TARGET_WINS")])
model_strikeouts <- step(model_strikeouts,trace=FALSE)

model_stolen_bases <- lm(`TEAM_BASERUN_SB` ~ .,data=baseball[,setdiff(colnames(baseball),c("TARGET_WINS","TEAM_FIELDING_DP"))])
model_stolen_bases <- step(model_stolen_bases,trace=FALSE)

model_double_plays <- lm(`TEAM_FIELDING_DP` ~ .,
	data=baseball[which(is.na(baseball[,"TEAM_FIELDING_DP"]) == FALSE & is.na(baseball[,"TEAM_BASERUN_SB"]) == FALSE),setdiff(colnames(baseball),"TARGET_WINS")])
model_double_plays <- step(model_double_plays,trace=FALSE)

print("Adjusted R-squared of modeling strikeouts:")
round(summary(model_strikeouts)$adj.r.squared,digits=2)

print("Adjusted R-squared of modeling stolen bases:")
round(summary(model_stolen_bases)$adj.r.squared,digits=2)

print("Adjusted R-squared of modeling double plays:")
round(summary(model_double_plays)$adj.r.squared,digits=2)

## ----remove-double-plays-and-remodel, echo=FALSE, eval=TRUE--------------
baseball <- baseball[,setdiff(colnames(baseball),"TEAM_FIELDING_DP")]

model_strikeouts <- lm(`TEAM_BATTING_SO` ~ .,data=baseball[,setdiff(colnames(baseball),"TARGET_WINS")])
model_strikeouts <- step(model_strikeouts,trace=FALSE)

model_stolen_bases <- lm(`TEAM_BASERUN_SB` ~ .,data=baseball[,setdiff(colnames(baseball),"TARGET_WINS")])
model_stolen_bases <- step(model_stolen_bases,trace=FALSE)

print("Adjusted R-squared of modeling strikeouts:")
round(summary(model_strikeouts)$adj.r.squared,digits=2)

print("Adjusted R-squared of modeling stolen bases:")
round(summary(model_stolen_bases)$adj.r.squared,digits=2)

## ----impute-missing-values, echo=FALSE, eval=TRUE------------------------
strikeouts_imputed <- as.vector(predict(model_strikeouts,baseball[which(is.na(baseball[,"TEAM_BATTING_SO"]) == TRUE),]))

stolen_bases_imputed <- as.vector(predict(model_stolen_bases,baseball[which(is.na(baseball[,"TEAM_BASERUN_SB"]) == TRUE),]))

imputed_vs_original <- data.frame(Variable = "TEAM_BATTING_SO",Origin = "Original",value = baseball[which(is.na(baseball[,"TEAM_BATTING_SO"]) == FALSE),"TEAM_BATTING_SO"],stringsAsFactors=FALSE)

imputed_vs_original <- rbind(imputed_vs_original,data.frame(Variable = "TEAM_BASERUN_SB",Origin = "Original",value = baseball[which(is.na(baseball[,"TEAM_BASERUN_SB"]) == FALSE),"TEAM_BASERUN_SB"],stringsAsFactors=FALSE))

imputed_vs_original <- rbind(imputed_vs_original,data.frame(Variable = "TEAM_BATTING_SO",Origin = "Imputed",value = strikeouts_imputed,stringsAsFactors=FALSE))
imputed_vs_original <- rbind(imputed_vs_original,data.frame(Variable = "TEAM_BASERUN_SB",Origin = "Imputed",value = stolen_bases_imputed,stringsAsFactors=FALSE))

ggplot(imputed_vs_original,
aes(x=value)) +
geom_density() +
facet_wrap(~Variable + Origin,ncol=2) +
ggtitle("Density of original vs. imputed variables")

## ----add-imputed-values-to-data, echo=FALSE, eval=TRUE-------------------
baseball[which(is.na(baseball[,"TEAM_BATTING_SO"]) == TRUE),"TEAM_BATTING_SO"] <- strikeouts_imputed
baseball[which(is.na(baseball[,"TEAM_BASERUN_SB"]) == TRUE),"TEAM_BASERUN_SB"] <- stolen_bases_imputed

## ----correlation-heatmap-after-remove-hits, echo=FALSE, eval=TRUE,message=FALSE,warning=FALSE----
baseball_predictors <- baseball[,setdiff(colnames(baseball),"TARGET_WINS")]

correlation_variables <- abs(cor(baseball_predictors,use="pairwise.complete.obs"))

for(i in 1:(nrow(correlation_variables) - 1))
{
correlation_variables[i,seq(from=i,to=ncol(correlation_variables),by=1)] <- NA
}

correlation_variables[ncol(baseball_predictors),ncol(baseball_predictors)] <- NA

correlation_variables <- gather(data.frame(correlation_variables),"y","correlation")

correlation_variables <- data.frame(x = rep(unique(correlation_variables$y),times=length(unique(correlation_variables$y))),correlation_variables)

correlation_variables$x <- factor(correlation_variables$x,levels=colnames(baseball_predictors))
correlation_variables$y <- factor(correlation_variables$y,levels=rev(colnames(baseball_predictors)))

correlation_variables %>%
ggplot(.,
aes(x = x,y = y)) +
geom_tile(aes(fill = correlation)) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_fill_gradient2(low = "blue",mid = "white",high = "red",na.value = "grey50") +
geom_text(aes(label = round(correlation, 2)),size=2)

print("Direction of correlation for strikeouts vs. singles:")
ifelse(cor(baseball[,"TEAM_BATTING_SO"],baseball[,"TEAM_BATTING_1B"]) < 0,"negative","positive")

## ----backward-elimination, echo=FALSE, eval=TRUE-------------------------
model_all_variables <- lm(`TARGET_WINS` ~ .,data=baseball)
print("Adjusted R-squared using all variables:")
round(summary(model_all_variables)$adj.r.squared,digits=4)

model_all_variables_adj_rsquared <- summary(model_all_variables)$adj.r.squared

adj_rsquared_subtracting_one_variable <- c()

for(variable in setdiff(colnames(baseball),"TARGET_WINS"))
{
model_minus_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),variable)])
adj_rsquared_subtracting_one_variable <- c(adj_rsquared_subtracting_one_variable,summary(model_minus_this_variable)$adj.r.squared)
}

print("Max adjusted R-squared across all models removing one variable at a time:")
round(max(adj_rsquared_subtracting_one_variable),digits=4)

print("Best model removing one variable comes from subtracting out:")
for(variable in setdiff(colnames(baseball),"TARGET_WINS"))
{
model_minus_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),variable)])
if(summary(model_minus_this_variable)$adj.r.squared == max(adj_rsquared_subtracting_one_variable)){print(variable)}
}

print("Full summary for model using all predictor variables:")
summary(model_all_variables)

## ----backward-elimination-prefer-simpler, echo=FALSE, eval=TRUE----------
backward_elimination_prefer_simpler_model <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),"TEAM_BATTING_BB")])
adj_rsquared_backward_elimination_prefer_simpler_model <- summary(backward_elimination_prefer_simpler_model)$adj.r.squared

adj_rsquared_subtracting_one_variable <- c()

for(variable in setdiff(colnames(baseball),c("TARGET_WINS","TEAM_BATTING_BB")))
{
model_minus_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),c(variable,"TEAM_BATTING_BB"))])
adj_rsquared_subtracting_one_variable <- c(adj_rsquared_subtracting_one_variable,summary(model_minus_this_variable)$adj.r.squared)
}

print("Adjusted R-squared removing only TEAM_BATTING_BB:")
round(summary(backward_elimination_prefer_simpler_model)$adj.r.squared,digits=4)
print("Max adjusted R-squared removing TEAM_BATTING_BB and one other variable:")
round(max(adj_rsquared_subtracting_one_variable),digits=4)

print("Next variable to remove:")
setdiff(colnames(baseball),c("TARGET_WINS","TEAM_BATTING_BB"))[which(adj_rsquared_subtracting_one_variable == max(adj_rsquared_subtracting_one_variable))]

## ----backward-elim-remove-third, echo=FALSE, eval=TRUE-------------------
backward_elimination_prefer_simpler_model <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),c("TEAM_BATTING_BB","TEAM_BATTING_1B"))])
adj_rsquared_backward_elimination_prefer_simpler_model <- summary(backward_elimination_prefer_simpler_model)$adj.r.squared

adj_rsquared_subtracting_one_variable <- c()

for(variable in setdiff(colnames(baseball),c("TARGET_WINS","TEAM_BATTING_BB","TEAM_BATTING_1B")))
{
model_minus_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),c(variable,"TEAM_BATTING_BB","TEAM_BATTING_1B"))])
adj_rsquared_subtracting_one_variable <- c(adj_rsquared_subtracting_one_variable,summary(model_minus_this_variable)$adj.r.squared)
}

print("Adjusted R-squared removing only TEAM_BATTING_BB and TEAM_BATTING_1B:")
round(summary(backward_elimination_prefer_simpler_model)$adj.r.squared,digits=4)
print("Max adjusted R-squared removing one other variable:")
round(max(adj_rsquared_subtracting_one_variable),digits=4)

## ----forward-selection-first-variable, echo=FALSE, eval=TRUE-------------
adj_rsquared_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball),"TARGET_WINS"))
{
model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,c("TARGET_WINS",variable)])
adj_rsquared_one_variable_at_a_time <- c(adj_rsquared_one_variable_at_a_time,summary(model_this_variable)$adj.r.squared)
}
print("First variable to add based on adjusted R-squared:")
paste0("Variable = ",setdiff(colnames(baseball),"TARGET_WINS")[which(adj_rsquared_one_variable_at_a_time == max(adj_rsquared_one_variable_at_a_time))]," ,Adjusted R-squared = ",round(max(adj_rsquared_one_variable_at_a_time),digits=4))

## ----forward-selection-second-and-onward-variables, echo=FALSE, eval=TRUE----
current_forward_selection_model_variables <- "TEAM_BATTING_2B"
current_forward_selection_model_adjusted_rsquared <- max(adj_rsquared_one_variable_at_a_time)

for(i in 2:(ncol(baseball) - 1))
{

adj_rsquared_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball),c("TARGET_WINS",current_forward_selection_model_variables)))
{
	model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,c("TARGET_WINS",current_forward_selection_model_variables,variable)])
	adj_rsquared_one_variable_at_a_time <- c(adj_rsquared_one_variable_at_a_time,summary(model_this_variable)$adj.r.squared)
}

if(max(adj_rsquared_one_variable_at_a_time) <= current_forward_selection_model_adjusted_rsquared)
{
	print(paste0("Iteration ",i," did not find any additional variables that improved model"))
	print("Model excludes the following variables:")
	print(setdiff(colnames(baseball),c(current_forward_selection_model_variables,"TARGET_WINS")))
	break
}

if(max(adj_rsquared_one_variable_at_a_time) > current_forward_selection_model_adjusted_rsquared)
{
        variable_to_add <- setdiff(colnames(baseball),c("TARGET_WINS",current_forward_selection_model_variables))[which(adj_rsquared_one_variable_at_a_time == max(adj_rsquared_one_variable_at_a_time))]
        current_forward_selection_model_variables <- c(current_forward_selection_model_variables,variable_to_add)
        current_forward_selection_model_adjusted_rsquared <- max(adj_rsquared_one_variable_at_a_time)
        print(paste0("Iteration ",i," add variable ",variable_to_add,", improves adj R-squared to ",round(current_forward_selection_model_adjusted_rsquared,digits=4)))
}

}

## ----backward-elimination-with-p-values, echo=FALSE, eval=TRUE-----------
model_minus_walks <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),"TEAM_BATTING_BB")])

print("Summary model minus TEAM_BATTING_BB:")
summary(model_minus_walks)

## ----forward-selection-pvalues-first-variable, echo=FALSE, eval=TRUE-----
pvalue_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball),"TARGET_WINS"))
{
model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,c("TARGET_WINS",variable)])
pvalue_this_variable <- coef(summary(model_this_variable))[,4]
pvalue_this_variable <- pvalue_this_variable[[variable]]
pvalue_one_variable_at_a_time <- c(pvalue_one_variable_at_a_time,pvalue_this_variable)
}

print("First variable to add based on p-value when consider each variable separately:")
paste0("Variable = ",setdiff(colnames(baseball),"TARGET_WINS")[which(pvalue_one_variable_at_a_time == min(pvalue_one_variable_at_a_time))]," p-value = ",signif(min(pvalue_one_variable_at_a_time),4))

## ----forward-selection-pvalues-second-and-onward-variables, echo=FALSE, eval=TRUE----
current_forward_selection_model_variables <- "TEAM_BATTING_2B"

for(i in 2:(ncol(baseball) - 1))
{

pvalue_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball),c("TARGET_WINS",current_forward_selection_model_variables)))
{
	model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball[,c("TARGET_WINS",current_forward_selection_model_variables,variable)])
	pvalue_this_variable <- coef(summary(model_this_variable))[, 4]
	pvalue_this_variable <- pvalue_this_variable[[variable]]
	pvalue_one_variable_at_a_time <- c(pvalue_one_variable_at_a_time,pvalue_this_variable)
}

alpha=.01

if(min(pvalue_one_variable_at_a_time) >= alpha)
{
	print(paste0("Iteration ",i," did not find any additional variables with significant p-values (<",alpha,")."))
	print(paste0("Minimum p-value this iteration = ",signif(min(pvalue_one_variable_at_a_time),4)))
	print("Model excludes the following variables:")
	print(setdiff(colnames(baseball),c(current_forward_selection_model_variables,"TARGET_WINS")))
	break
}

if(min(pvalue_one_variable_at_a_time) < alpha)
{
	variable_to_add <- setdiff(colnames(baseball),c("TARGET_WINS",current_forward_selection_model_variables))[which(pvalue_one_variable_at_a_time == min(pvalue_one_variable_at_a_time))]
	current_forward_selection_model_variables <- c(current_forward_selection_model_variables,variable_to_add)
	print(paste0("Iteration ",i," add variable ",variable_to_add,", p-value when add this to existing = ",signif(min(pvalue_one_variable_at_a_time),4)))
}

}

## ----total-bases-add, echo=FALSE, eval=TRUE------------------------------
baseball_total_bases <- baseball %>% mutate(TEAM_TOTAL_BASES = TEAM_BATTING_1B + (2*TEAM_BATTING_2B) + (3*TEAM_BATTING_3B) + (4*TEAM_BATTING_HR))
baseball_total_bases <- data.frame(baseball_total_bases,check.names=FALSE,stringsAsFactors=FALSE)
baseball_total_bases <- baseball_total_bases[,setdiff(colnames(baseball_total_bases),c("TEAM_BATTING_1B","TEAM_BATTING_2B","TEAM_BATTING_3B","TEAM_BATTING_HR"))]

print("Summary of a model including all variables after create total bases:")
summary(lm(`TARGET_WINS` ~ .,data=baseball_total_bases))

## ----backwards-elim-after-total-bases-add, echo=FALSE, eval=TRUE---------
print("Summary of a model removing TEAM_BATTING_BB after create total bases:")
summary(lm(`TARGET_WINS` ~ .,data=baseball_total_bases[,setdiff(colnames(baseball_total_bases),"TEAM_BATTING_BB")]))

## ----forward-selection-after-total-bases-add, echo=FALSE, eval=TRUE------
adj_rsquared_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball_total_bases),"TARGET_WINS"))
{
	model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball_total_bases[,c("TARGET_WINS",variable)])
	adj_rsquared_one_variable_at_a_time <- c(adj_rsquared_one_variable_at_a_time,summary(model_this_variable)$adj.r.squared)
}

print("First variable to add based on adjusted R-squared:")
paste0("Variable = ",setdiff(colnames(baseball_total_bases),"TARGET_WINS")[which(adj_rsquared_one_variable_at_a_time == max(adj_rsquared_one_variable_at_a_time))]," ,Adjusted R-squared = ",round(max(adj_rsquared_one_variable_at_a_time),digits=4))

## ----forward-selection-after-total-bases-add-second-and-onward, echo=FALSE, eval=TRUE----
current_forward_selection_model_variables <- setdiff(colnames(baseball_total_bases),"TARGET_WINS")[which(adj_rsquared_one_variable_at_a_time == max(adj_rsquared_one_variable_at_a_time))]
current_forward_selection_model_adjusted_rsquared <- max(adj_rsquared_one_variable_at_a_time)

for(i in 2:(ncol(baseball_total_bases) - 1))
{

adj_rsquared_one_variable_at_a_time <- c()

for(variable in setdiff(colnames(baseball_total_bases),c("TARGET_WINS",current_forward_selection_model_variables)))
{
        model_this_variable <- lm(`TARGET_WINS` ~ .,data=baseball_total_bases[,c("TARGET_WINS",current_forward_selection_model_variables,variable)])
        adj_rsquared_one_variable_at_a_time <- c(adj_rsquared_one_variable_at_a_time,summary(model_this_variable)$adj.r.squared)
}

if(max(adj_rsquared_one_variable_at_a_time) <= current_forward_selection_model_adjusted_rsquared)
{
        print(paste0("Iteration ",i," did not find any additional variables that improved model"))
        print("Model excludes the following variables:")
        print(setdiff(colnames(baseball_total_bases),c(current_forward_selection_model_variables,"TARGET_WINS")))
        break
}

if(max(adj_rsquared_one_variable_at_a_time) > current_forward_selection_model_adjusted_rsquared)
{
        variable_to_add <- setdiff(colnames(baseball_total_bases),c("TARGET_WINS",current_forward_selection_model_variables))[which(adj_rsquared_one_variable_at_a_time == max(adj_rsquared_one_variable_at_a_time))]
        current_forward_selection_model_variables <- c(current_forward_selection_model_variables,variable_to_add)
        current_forward_selection_model_adjusted_rsquared <- max(adj_rsquared_one_variable_at_a_time)
        print(paste0("Iteration ",i," add variable ",variable_to_add,", improves adj R-squared to ",round(current_forward_selection_model_adjusted_rsquared,digits=4)))
}

}

## ----compare-coefficients-models-1-and-2-vs-3, echo=FALSE, eval=TRUE-----
print("Coefficients including singles/doubles/triples/home runs, minus walks:")
coef(summary(lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),"TEAM_BATTING_BB")])))[,1]
print("Coefficients replacing with total bases, minus walks:")
coef(summary(lm(`TARGET_WINS` ~ .,data=baseball_total_bases[,setdiff(colnames(baseball_total_bases),"TEAM_BATTING_BB")])))[,1]

## ----compare-models-1-and-2-vs-3-by-quality, echo=FALSE, eval=TRUE-------
print("Full summary including singles/doubles/triples/home runs:")
full_stats_model <- lm(`TARGET_WINS` ~ .,data=baseball[,setdiff(colnames(baseball),"TEAM_BATTING_BB")])
summary(full_stats_model)
print("Full summary replacing singles/doubles/triples/home runs with total bases:")
total_bases_model <- lm(`TARGET_WINS` ~ .,data=baseball_total_bases[,setdiff(colnames(baseball_total_bases),"TEAM_BATTING_BB")])
summary(total_bases_model)

## ----mse-per-model, echo=FALSE, eval=TRUE--------------------------------
mean(full_stats_model$residuals^2)
mean(total_bases_model$residuals^2)

## ----residuals-larger-model, echo=FALSE, eval=TRUE-----------------------
layout(matrix(c(1,2,3,4),2,2))
plot(full_stats_model)

hist(full_stats_model$residuals,xlab="Residuals",ylab="Number of records",main="Incl. individual stats")

## ----residuals-smaller-model, echo=FALSE, eval=TRUE----------------------
layout(matrix(c(1,2,3,4),2,2))
plot(total_bases_model)

hist(total_bases_model$residuals,xlab="Residuals",ylab="Number of records",main="Total bases not individual stats")

## ----fitted-compare-models, echo=FALSE, eval=TRUE------------------------
fitted_to_full_stats <- as.vector(predict(full_stats_model,baseball))
fitted_to_total_bases <- as.vector(predict(total_bases_model,baseball_total_bases))

plot(fitted_to_full_stats,fitted_to_total_bases,xlab="Fitted with full stats",ylab="Fitted with total bases",main=paste0("r = ",round(cor(fitted_to_full_stats,fitted_to_total_bases),digits=2)))
abline(0,1,lty=2)
legend("topright",legend="y = x",lty=2)

## ----predict-evaluation-data-wins, echo=FALSE, eval=TRUE-----------------
evaluation <- read.csv("https://raw.githubusercontent.com/heathergeiger/Data621_hw1/master/moneyball-evaluation-data.csv",header=TRUE,stringsAsFactors=FALSE,check.names=FALSE)
evaluation <- evaluation %>% mutate(TEAM_BATTING_1B = TEAM_BATTING_H - (TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR))
evaluation <- evaluation %>% mutate(TEAM_TOTAL_BASES = TEAM_BATTING_1B + (2*TEAM_BATTING_2B) + (3*TEAM_BATTING_3B) + (4*TEAM_BATTING_HR))

evaluation_predictions <- as.vector(predict(total_bases_model,evaluation))

write.table(evaluation_predictions,
file="evaluation_predictions.txt",
row.names=FALSE,col.names=FALSE,quote=FALSE)

