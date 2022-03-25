sdm_0_partial <- partial(sdm,
        trace = FALSE,
        ci = TRUE,
        equal = TRUE,
        panel = TRUE,
        smooth = 3)

sdm_1_partial <- partial(sdm_1,
                         trace = FALSE,
                         ci = TRUE,
                         equal = TRUE,
                         panel = TRUE,
                         smooth = 3)

sdm_2_partial <- partial(sdm_2,
                         trace = FALSE,
                         ci = TRUE,
                         equal = TRUE,
                         panel = TRUE,
                         smooth = 3)

sp_sdm_1_ap <- spartial(sdm_1, lr_prediction_stack, x.vars = "annual_precipitation", equal = FALSE)

plot_sp_sdm_1_ap <- plot(sp_sdm_1_ap, 
     box = FALSE,
     axes = FALSE,
     main = 'Spartial plot: annual_precipitation',
     #zlim = c(0,1),
     #axis.args=list(at=pretty(0:1), labels=pretty(0:1)),
     legend.args=list(text='Partial effect', side=2, line=1.3))