library(data.table)
RANK_BLEND = F
top1 = fread("./submission_final/submission/xgb_glm_0529_2_rank.csv")
top2 = fread("./submission_final/submission/xgb_glm_0529.csv")
top3 = fread("./submission_final/submission/xgb_glm_80_20_rank.csv")
# bottom = fread("../submission/")

# XGBOOST RMSE
xgb_rmse_files = list.files("./submission_final/xgboost_rmse/", full.names = TRUE)
xgb_rmse_wts = list.files("./submission_final/xgboost_rmse/")
xgb_rmse_wts = 1/as.numeric(gsub(pattern = ".csv", replacement = "", x = xgb_rmse_wts))
xgb_rmse_list = data.frame(xgb_rmse_files, xgb_rmse_wts)
xgb_rmse_list$xgb_rmse_wts = xgb_rmse_list$xgb_rmse_wts - (min(xgb_rmse_list$xgb_rmse_wts) - 3*sd(xgb_rmse_list$xgb_rmse_wts))
xgb_rmse_list$xgb_rmse_wts = xgb_rmse_list$xgb_rmse_wts/mean(xgb_rmse_list$xgb_rmse_wts)
for(i in 1:nrow(xgb_rmse_list)){
    f = fread(as.character(xgb_rmse_list[i, 1]))
    wt = xgb_rmse_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        xgb_rmse_pred = f
    }else{
        xgb_rmse_pred$Diabetes = xgb_rmse_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    xgb_rmse_pred$Diabetes = frank(xgb_rmse_pred$Diabetes, ties.method = "dense") 
}else{
    xgb_rmse_pred$Diabetes = xgb_rmse_pred$Diabetes / nrow(xgb_rmse_list)
}

# XGBOOST AUC
xgb_auc_files = list.files("./submission_final/xgboost_auc/", full.names = TRUE)
xgb_auc_wts = list.files("./submission_final/xgboost_auc/")
xgb_auc_wts = as.numeric(gsub(pattern = ".csv", replacement = "", x = xgb_auc_wts))
xgb_auc_list = data.frame(xgb_auc_files, xgb_auc_wts)
xgb_auc_list$xgb_auc_wts = xgb_auc_list$xgb_auc_wts - (min(xgb_auc_list$xgb_auc_wts) - 3*sd(xgb_auc_list$xgb_auc_wts))
xgb_auc_list$xgb_auc_wts = xgb_auc_list$xgb_auc_wts/mean(xgb_auc_list$xgb_auc_wts)
for(i in 1:nrow(xgb_auc_list)){
    f = fread(as.character(xgb_auc_list[i, 1]))
    wt = xgb_auc_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        xgb_auc_pred = f
    }else{
        xgb_auc_pred$Diabetes = xgb_auc_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    xgb_auc_pred$Diabetes = frank(xgb_auc_pred$Diabetes, ties.method = "dense") 
}else{
    xgb_auc_pred$Diabetes = xgb_auc_pred$Diabetes / nrow(xgb_auc_list)
}

# EXTRATREE
extra_files = list.files("./submission_final/extratree/", full.names = TRUE)
extra_wts = list.files("./submission_final/extratree/")
extra_wts = as.numeric(gsub(pattern = ".csv", replacement = "", x = extra_wts))
extra_list = data.frame(extra_files, extra_wts)
extra_list$extra_wts = extra_list$extra_wts - (min(extra_list$extra_wts) - 3*sd(extra_list$extra_wts))
extra_list$extra_wts = extra_list$extra_wts/mean(extra_list$extra_wts)
extra_list$extra_wts = ifelse(is.na(extra_list$extra_wts), 1, extra_list$extra_wts)
for(i in 1:nrow(extra_list)){
    f = fread(as.character(extra_list[i, 1]))
    wt = extra_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        extra_pred = f
    }else{
        extra_pred$Diabetes = extra_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    extra_pred$Diabetes = frank(extra_pred$Diabetes, ties.method = "dense") 
}else{
    extra_pred$Diabetes = extra_pred$Diabetes / nrow(extra_list)
}


# GLM
glm_files = list.files("./submission_final/glm/", full.names = TRUE)
glm_wts = list.files("./submission_final/glm/")
glm_wts = as.numeric(gsub(pattern = ".csv", replacement = "", x = glm_wts))
glm_list = data.frame(glm_files, glm_wts)
glm_list$glm_wts = glm_list$glm_wts - (min(glm_list$glm_wts) - 3*sd(glm_list$glm_wts))
glm_list$glm_wts = glm_list$glm_wts/mean(glm_list$glm_wts)
glm_list$glm_wts = ifelse(is.na(glm_list$glm_wts), 1, glm_list$glm_wts)
for(i in 1:nrow(glm_list)){
    f = fread(as.character(glm_list[i, 1]))
    wt = glm_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        glm_pred = f
    }else{
        glm_pred$Diabetes = glm_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    glm_pred$Diabetes = frank(glm_pred$Diabetes, ties.method = "dense") 
}else{
    glm_pred$Diabetes = glm_pred$Diabetes / nrow(glm_list)
}


# RF
rf_files = list.files("./submission_final/rf/", full.names = TRUE)
rf_wts = list.files("./submission_final/rf/")
rf_wts = as.numeric(gsub(pattern = ".csv", replacement = "", x = rf_wts))
rf_list = data.frame(rf_files, rf_wts)
rf_list$rf_wts = rf_list$rf_wts - (min(rf_list$rf_wts) - 3*sd(rf_list$rf_wts))
rf_list$rf_wts = rf_list$rf_wts/mean(rf_list$rf_wts)
rf_list$rf_wts = ifelse(is.na(rf_list$rf_wts), 1, rf_list$rf_wts)
for(i in 1:nrow(rf_list)){
    f = fread(as.character(rf_list[i, 1]))
    wt = rf_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        rf_pred = f
    }else{
        rf_pred$Diabetes = rf_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    rf_pred$Diabetes = frank(rf_pred$Diabetes, ties.method = "dense") 
}else{
    rf_pred$Diabetes = rf_pred$Diabetes / nrow(rf_list)
    rf_pred$Diabetes = rf_pred$Diabetes / (max(rf_pred$Diabetes) - min(rf_pred$Diabetes))
}

# NNETS
nn_files = list.files("./submission_final/nnet/", full.names = TRUE)
nn_wts = list.files("./submission_final/nnet/")
nn_wts = as.numeric(gsub(pattern = ".csv", replacement = "", x = nn_wts))
nn_list = data.frame(nn_files, nn_wts)
nn_list$nn_wts = nn_list$nn_wts - (min(nn_list$nn_wts) - 3*sd(nn_list$nn_wts))
nn_list$nn_wts = nn_list$nn_wts/mean(nn_list$nn_wts)
nn_list$nn_wts = ifelse(is.na(nn_list$nn_wts), 1, nn_list$nn_wts)
for(i in 1:nrow(nn_list)){
    f = fread(as.character(nn_list[i, 1]))
    wt = nn_list[i, 2]
    if(RANK_BLEND){
        f$Diabetes = frank(f$Diabetes, ties.method = "dense") * wt    
    }else{
        f$Diabetes = f$Diabetes * wt
    }
    if(i == 1){
        nn_pred = f
    }else{
        nn_pred$Diabetes = nn_pred$Diabetes + f$Diabetes
    }
}
if(RANK_BLEND){
    nn_pred$Diabetes = frank(nn_pred$Diabetes, ties.method = "dense") 
}else{
    nn_pred$Diabetes = nn_pred$Diabetes / nrow(nn_list)
}


# LOGLOSS


# WEIGHTS
wts = c(0.970, 0.971, 0.966, 0.961, 0.969, 0.9620482)
wts = wts - (min(wts) - sd(wts))
wts = wts/mean(wts)
wts = wts/length(wts)
# wts = c(0.33982119, 0.35898319, 0.15317321, 0.04736322, 0.05065919, 0.05)
wts = c(0.22648938, 0.30362091, 0.15796328, 0.05230565, 0.20935786, 0.05026292)
# RESULTS
if(RANK_BLEND){
    blend.res.rank = xgb_rmse_pred
    blend.res.rank$Diabetes = frank(xgb_rmse_pred$Diabetes, ties.method = "dense") * wts[1] + 
        frank(xgb_auc_pred$Diabetes, ties.method = "dense") * wts[2] + 
        frank(glm_pred$Diabetes, ties.method = "dense") * wts[3] + 
        frank(rf_pred$Diabetes, ties.method = "dense") * wts[4] + 
        frank(extra_pred$Diabetes, ties.method = "dense") * wts[5]
    frank(nn_pred$Diabetes, ties.method = "dense") * wts[6]
    plot(blend.res.rank$Diabetes, top1$Diabetes)
    final.res = blend.res.rank
}else{
    blend.res = xgb_rmse_pred
    blend.res$Diabetes = xgb_rmse_pred$Diabetes * wts[1] + xgb_auc_pred$Diabetes * wts[2] + glm_pred$Diabetes * wts[3] + 
        rf_pred$Diabetes * wts[4] + extra_pred$Diabetes * wts[5] + nn_pred$Diabetes * wts[6]
    plot(blend.res$Diabetes, top2$Diabetes)
    final.res = blend.res
}

final.res$Diabetes = (frank(final.res$Diabetes, ties.method = "dense") *0.35+top1$Diabetes * 0.65)
write.csv(final.res, file = "./submission_final/extratree_rf_glm_xgb_blend.csv", row.names = F)



# plot(blend.res.rank$Diabetes / (max(blend.res.rank$Diabetes) - min(blend.res.rank$Diabetes)), 
#      top1$Diabetes / (max(top1$Diabetes) - min(top1$Diabetes)))
# plot((blend.res.rank$Diabetes / (max(blend.res.rank$Diabetes) - min(blend.res.rank$Diabetes)) + blend.res$Diabetes)/2, top2$Diabetes)
plot(final.res$Diabetes, top1$Diabetes)
plot((frank(final.res$Diabetes, ties.method = "dense") *0.35+top1$Diabetes * 0.65), top1$Diabetes)


# xgb_rmse xgb_auc glm rf extratree
# 0.24982119 0.26898319 0.17317321 0.07736322 0.23065919