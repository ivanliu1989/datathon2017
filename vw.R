library(data.table)
# Vowpal Wabbit -----------------------------------------------------------
vw_pred = fread("./modelData/vw/nn_A_pred.txt")
vw_act = fread("./modelData/vw/train_labels.txt")

vw_pred$V1 = (vw_pred$V1-(min(vw_pred$V1))) / (max(vw_pred$V1) - min(vw_pred$V1))

# setorder(vw_pred, V2)
# vw_pred[, Pred := mean(V1), by = V2]
# vw_pred= unique(vw_pred[, .(Pred, V2)])
table(ifelse(vw_pred$V1[-1] >=0.5, 1, -1) == vw_act$V1)
# 0.9556452 nn 60


normalizedPred = function(x){
    (x-(min(x))) / (max(x) - min(x))
}

# GLM VW ------------------------------------------------------------------
vw_pred_A = fread("./modelData/vw/glm_A_pred.txt")
vw_pred_B = fread("./modelData/vw/glm_B_pred.txt")
vw_act_A = fread("./modelData/vw/train_labels.txt")
vw_act_B = fread("./modelData/vw/val_labels.txt")
vw_pred_tst1 = fread("./modelData/vw/glm_testA_pred.txt")
vw_pred_tst2 = fread("./modelData/vw/glm_testB_pred.txt")

vw_pred_A$VW_GLM_PRED = normalizedPred(vw_pred_A$V1)
table(ifelse(vw_pred_A$VW_GLM_PRED>=0.5,1,-1) == vw_act_A$V1)

vw_pred_B$VW_GLM_PRED = normalizedPred(vw_pred_B$V1)
table(ifelse(vw_pred_B$VW_GLM_PRED>=0.5,1,-1)[-1] == vw_act_B$V1)

glmVWMeta = data.frame(Patient_ID = vw_pred_A$V2, VW_GLM_PRED = vw_pred_A$V1)
glmVWMeta2 = data.frame(Patient_ID = vw_pred_B$V2[-1], VW_GLM_PRED = vw_pred_B$V1[-1])
glmVWMeta = rbind(glmVWMeta, glmVWMeta2)

glmVWMetaTest = data.frame(Patient_ID = vw_pred_tst1$V2, VW_GLM_PRED = (vw_pred_tst1$V1 + vw_pred_tst2$V1)/2)

glmVWMeta = rbind(glmVWMeta, glmVWMetaTest)

glmVWMeta$VW_GLM_PRED = normalizedPred(glmVWMeta$VW_GLM_PRED)
save(glmVWMeta, file = "./modelData/Metadata/glmVWMeta.RData")



# NNETS -------------------------------------------------------------------
vw_pred_A = fread("./modelData/vw/nn_A_pred.txt")
vw_pred_B = fread("./modelData/vw/nn_B_pred.txt")
vw_act_A = fread("./modelData/vw/train_labels.txt")
vw_act_B = fread("./modelData/vw/val_labels.txt")
vw_pred_tst1 = fread("./modelData/vw/nn_testA_pred.txt")
vw_pred_tst2 = fread("./modelData/vw/nn_testB_pred.txt")

vw_pred_A$VW_GLM_PRED = normalizedPred(vw_pred_A$V1)
table(ifelse(vw_pred_A$VW_GLM_PRED>=0.5,1,-1) == vw_act_A$V1)
vw_pred_B$VW_GLM_PRED = normalizedPred(vw_pred_B$V1)
table(ifelse(vw_pred_B$VW_GLM_PRED>=0.5,1,-1)[-1] == vw_act_B$V1)

nnetsVWMeta = data.frame(Patient_ID = vw_pred_A$V2, VW_GLM_PRED = vw_pred_A$V1)
nnetsVWMeta2 = data.frame(Patient_ID = vw_pred_B$V2[-1], VW_GLM_PRED = vw_pred_B$V1[-1])
nnetsVWMeta = rbind(nnetsVWMeta, nnetsVWMeta2)

nnetsVWMetaTest = data.frame(Patient_ID = vw_pred_tst1$V2, VW_GLM_PRED = (vw_pred_tst1$V1 + vw_pred_tst2$V1)/2)

nnetsVWMeta = rbind(nnetsVWMeta, nnetsVWMetaTest)

nnetsVWMeta$VW_GLM_PRED = normalizedPred(nnetsVWMeta$VW_GLM_PRED)
save(nnetsVWMeta, file = "./modelData/Metadata/nnetsVWMeta.RData")
