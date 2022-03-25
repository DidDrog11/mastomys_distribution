comparison_plot <- list()

# produce an object to move the predictions for the central estimate and uncertainty for each model

comparison_plot$sdm_0 <- sdm_0_output$prediction
comparison_plot$sdm_0$central_estimate <- sdm_0_output$prediction
comparison_plot$sdm_0$central_estimate$data <- comparison_plot$sdm_0$central_estimate$data %>%
  filter(variable == "Central estimate")
comparison_plot$sdm_0$overlay_presence <- sdm_0_output$overlay_presence +
  labs(title = "SDM_0") +
  scale_fill_viridis_c(limits = c(0, 1), na.value = NA)
comparison_plot$sdm_0$uncertainty <- sdm_0_output$prediction
comparison_plot$sdm_0$uncertainty$data <- comparison_plot$sdm_0$uncertainty$data %>%
  filter(variable == "Uncertainty")

# repeat for SDM_1
comparison_plot$sdm_1 <- sdm_1_output$prediction
comparison_plot$sdm_1$central_estimate <- sdm_1_output$prediction
comparison_plot$sdm_1$central_estimate$data <- comparison_plot$sdm_1$central_estimate$data %>%
  filter(variable == "Central estimate")
comparison_plot$sdm_1$overlay_presence <- sdm_1_output$overlay_presence +
  labs(title = "SDM_1") +
  scale_fill_viridis_c(limits = c(0, 1), na.value = NA)
comparison_plot$sdm_1$uncertainty <- sdm_1_output$prediction
comparison_plot$sdm_1$uncertainty$data <- comparison_plot$sdm_1$uncertainty$data %>%
  filter(variable == "Uncertainty")

# repeat for SDM_2
comparison_plot$sdm_2 <- sdm_2_output$prediction
comparison_plot$sdm_2$central_estimate <- sdm_2_output$prediction
comparison_plot$sdm_2$central_estimate$data <- comparison_plot$sdm_2$central_estimate$data %>%
  filter(variable == "Central estimate")
comparison_plot$sdm_2$overlay_presence <- sdm_2_output$overlay_presence +
  labs(title = "SDM_2") +
  scale_fill_viridis_c(limits = c(0, 1), na.value = NA)
comparison_plot$sdm_2$uncertainty <- sdm_2_output$prediction
comparison_plot$sdm_2$uncertainty$data <- comparison_plot$sdm_2$uncertainty$data %>%
  filter(variable == "Uncertainty")


compare_sdm <- plot_grid(plotlist = list(comparison_plot$sdm_0$central_estimate +
                                           labs(title = "SDM_0") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA),
                                         comparison_plot$sdm_0$overlay_presence,
                                         comparison_plot$sdm_0$uncertainty +
                                           labs(title = "SDM_0",
                                                fill = "Uncertainty\n (97.5% - 2.5% CrI)") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA),
                                         comparison_plot$sdm_1$central_estimate +
                                           labs(title = "SDM_1") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA),
                                         comparison_plot$sdm_1$overlay_presence,
                                         comparison_plot$sdm_1$uncertainty +
                                           labs(title = "SDM_1",
                                                fill = "Uncertainty\n (97.5% - 2.5% CrI)") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA),
                                         comparison_plot$sdm_2$central_estimate +
                                           labs(title = "SDM_2") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA),
                                         comparison_plot$sdm_2$overlay_presence,
                                         comparison_plot$sdm_2$uncertainty +
                                           labs(title = "SDM_2",
                                                fill = "Uncertainty\n (97.5% - 2.5% CrI)") +
                                           scale_fill_viridis_c(limits = c(0, 1), na.value = NA)),
                         ncol = 3)

save_plot(plot = compare_sdm, filename = here("output", "sdm_comparison.png"), base_height = 12, base_asp = 1.618)
