# Code to produce INLA model

inla_0_formula <- as.formula(paste0("m_nat", " ~ ", paste(all_vars, collapse = " + ")))

inla_model_0 <- inla(inla_0_formula, family = "binomial", data = combined_cov)                     
