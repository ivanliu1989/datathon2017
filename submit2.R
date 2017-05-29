library(data.table)
rm(list = ls()); gc()

best.pred = fread("./submit20170529/xgb_0527.csv")
rmse.files = list.files("./submit20170529/rmse/", pattern = ".csv", full.names = T)
auc.files = list.files("./submit20170529/auc/", pattern = ".csv", full.names = T)


for(i in 1:length(rmse.files)){
    print(rmse.files[i])
    f = fread(rmse.files[i])
    print(table(ifelse(f$Diabetes>=0.5, 1, 0)))
    if(i == 1){
        rmse.pred = f
    }else{
        rmse.pred$Diabetes = rmse.pred$Diabetes + f$Diabetes
    }
    
}
rmse.pred$Diabetes = rmse.pred$Diabetes / length(rmse.files)



for(i in 1:length(auc.files)){
    print(auc.files[i])
    f = fread(auc.files[i])
    print(table(ifelse(f$Diabetes>=0.5, 1, 0)))
    if(i == 1){
        auc.pred = f
    }else{
        auc.pred$Diabetes = auc.pred$Diabetes + f$Diabetes
    }
    
}
auc.pred$Diabetes = auc.pred$Diabetes / length(auc.files)



print(table(ifelse(best.pred$Diabetes>=0.5, 1, 0)))
print(table(ifelse(rmse.pred$Diabetes>=0.5, 1, 0)))
print(table(ifelse(auc.pred$Diabetes>=0.5, 1, 0)))

plot(auc.pred$Diabetes, best.pred$Diabetes)
plot(rmse.pred$Diabetes, best.pred$Diabetes)

fnl.pred = rmse.pred
fnl.pred$Diabetes = (auc.pred$Diabetes + rmse.pred$Diabetes)/2
print(table(ifelse(fnl.pred$Diabetes>=0.5, 1, 0)))
plot(fnl.pred$Diabetes, best.pred$Diabetes)

fnl.pred.blend = fnl.pred
fnl.pred.blend$Diabetes = (fnl.pred.blend$Diabetes + best.pred$Diabetes)/2
print(table(ifelse(fnl.pred.blend$Diabetes>=0.5, 1, 0)))
plot(fnl.pred.blend$Diabetes, best.pred$Diabetes)


write.csv(fnl.pred, file = "./submit20170529/xgb_rmse_auc_0529.csv", row.names = F)
write.csv(fnl.pred.blend, file = "./submit20170529/xgb_rmse_auc_prev_0529.csv", row.names = F)




# GLM ---------------------------------------------------------------------
xgb_submit = fread("./submit20170529/xgb_rmse_auc_prev_0529.csv")
glm_submit = fread("./submit20170529/GLM_SUBMIT_.csv")


print(table(ifelse(glm_submit$Diabetes>=0.5, 1, 0)))
print(table(ifelse(xgb_submit$Diabetes>=0.5, 1, 0)))
plot(xgb_submit$Diabetes, glm_submit$Diabetes)

xgb_glm = xgb_submit

rank_xgb = frank(xgb_glm$Diabetes, ties.method = "dense")
rank_glm = frank(glm_submit$Diabetes, ties.method = "dense")

xgb_glm$Diabetes = rank_xgb * 0.7 + rank_glm * 0.3

# xgb_glm$Diabetes = xgb_glm$Diabetes * 0.6 + glm_submit$Diabetes * 0.4
write.csv(xgb_glm, file = "./submit20170529/xgb_glm_0529_2_rank.csv", row.names = F)




























# weighted ranked
# logistic and a hinge loss 


# https://mlwave.com/kaggle-ensembling-guide/

# 88: eval-auc:0.970016
# 89: eval-auc:0.970381
# 90: eval-auc:0.970547
# 91: eval-auc:0.969789
# 92: eval-auc:0.970294
# 93: eval-auc:0.969816
# 94: eval-auc:0.970892
# 95: eval-auc:0.970886
# 96: eval-auc:0.971852
# 97: eval-auc:0.971125
# 98: eval-auc:0.971225
# 99: eval-auc:0.969887
# 100: eval-auc:0.971907
# 101: eval-auc:0.971056
# 102: eval-auc:0.970621
# 103: eval-auc:0.971228
# 104: eval-auc:0.971656
# 105: eval-auc:0.970972
# 106: eval-auc:0.971171
# 107: eval-auc:0.970039
# 108: eval-auc:0.971293
