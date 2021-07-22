
mod1_autofeature <- maxent(x=pder, 
                           ## env conditions, here we selected only 3 predictors
                           p=pa,
                           ## 1:presence or 0:absence
                           path="../output/maxent_outputs1_auto",
                           ## this is the folder you will find manxent output
                           args=prepPara(userfeatures=NULL) ) 
## default is autofeature

# or select Linear& Quadratic features
mod1_lq <- maxent(x=pder[c("bio1","bio4","bio11")],
                  p=pa,
                  path=paste0("../output/maxent_outputs1_lq"),
                  args=prepPara(userfeatures="LQ") ) 
## default is autofeature, here LQ represents Linear& Quadratic

https://github.com/shandongfx/workshop_maxent_R/blob/master/code/Appendix1_case_study.md
## (L-linear, Q-Quadratic, H-Hinge, P-Product, T-Threshold)