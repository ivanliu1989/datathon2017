require(data.table)
require(lightgbm)
require(caret)
rm(list = ls()); gc()
load("./modelData/feat_simplified_20170529.RData")
load(file = "./modelData/tmp_outcomes2016.RData")
load(file = "./datathon2017/final_features_simp.RData")

fnl.dat[, Target := ifelse(Patient_ID %in% tmp_outcomes2016, 1, 0)]

# split -------------------------------------------------------------------
# 
# set.seed(888)
# ind_sample = createDataPartition(fnl.dat$Target, p = .2, list = F)
# dt_sample = fnl.dat[ind_sample]
# 
# rm(fnl.dat); gc()

set.seed(888)
ind_train = createDataPartition(fnl.dat$Target, p = .8, list = F)
dt_train = fnl.dat[ind_train]
dt_valid = fnl.dat[-ind_train]


# preprocess --------------------------------------------------------------

dt_lgb_train = lgb.Dataset(data = data.matrix(dt_train[, !c("Patient_ID", "Target"), with = F])
                           , label = dt_train$Target)

dt_lgb_valid = lgb.Dataset(data = data.matrix(dt_valid[, !c("Patient_ID", "Target"), with = F])
                           , label = dt_valid$Target)

# train -------------------------------------------------------------------

valids = list(eval = dt_lgb_valid)
num_round = 1000

params = list(learning_rate = 0.01
              , num_leaves = 6
              , num_threads = 6
              , application = "binary"
              , max_depth = 6
              , min_data_in_leaf = 8
              , feature_fraction = .8
              , bagging_fraction = .6
              , bagging_freq = 6
              , early_stopping_round = 10)

model = lgb.train(params,
                  dt_lgb_train,
                  num_round,
                  valids,
                  objective = "binary",
                  metric = "auc")
