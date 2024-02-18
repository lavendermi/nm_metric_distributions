# common parameters for each of the scripts

ordered_metrics <- c(
  "checker", "combo", "cscore", "vRatio",
  "beta_w", "beta_c", "beta_wb", "beta_r", "beta_I", "beta_e", "beta_t", "beta_m", "beta_co",
  "beta_cc", "beta_minus3", "williams", "beta_rlb", "beta_sim", "beta_gl", "beta_z",
  "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11", "A_12", "A_13", "A_14", "A_15",
  "A_16", "A_17", "A_18", "A_19", "A_20", "A_21", "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29",
  "A_30", "A_31", "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41", "A_42", "A_43",
  "S_8", "S_26", "Index_2", "Index_3", "Index_4", "Index_5", "Index_6", "Index_7", "Index_8", "Index_9", "Index_12",
  "Index_16", "Index_18", "Index_41", "Index_46", "Index_45",
  "netCovariance", "NODF", "BR", "clumping", "Togetherness", "Sharedness"
)

ordered_metrics_plots <- c(
  "a", "b", "c", "d", 
  "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", 
  "A12", "A13", "A14", "A15", "A16", "A17", "A18", "A19", "A20", "A21", "A22", 
  "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A30", "A31", "A32", "A33", 
  "A34", "A35", "A36", "A37", "A38", "A39", "A40", "A41", "A42", "A43", 
  "C07", "C08", 
  "Index02", "Index03", "Index04", "Index05", "Index06", "Index07", "Index08", 
  "Index09", "Index12", "Index16", "Index18", "Index41", "Index45", 
  "Index46", 
  "S09", "S26", 
  "β-2", "β-3", "βc", "βcc", "βco", "βe", "βgl", "βhk", "βi", "βm", "βr", "βrlb",
  "βsim", "βsor", "βt", "βw", "βwb", "βwil", "βz",
  "BR", "checker", "clump", "combo", "cScore", "netCo", "NODF", "Share", "sCoE", 
  "Tog", "vRatio"
)

# nm_list <- c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6", "sim7", "sim8", "sim9", "pp", "sim6_eq", "sim7_eq")
nm_list <- c("sim1", "sim2", "sim3", "sim4", "sim5", "sim6", "sim7", "sim8", "sim9", "pp")

dist_list <- c("Uniform", "Lognormal", "Geometric")

pos_metrics <- c(
  "Togetherness", "Sharedness", "beta_rlb",
  "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "A_7", "A_8", "A_9", "A_10", "A_11",
  "A_12", "A_13", "A_14", "A_15", "A_16", "A_17", "A_18", "A_19", "A_20", "A_21",
  "A_22", "A_23", "A_24", "A_25", "A_26", "A_27", "A_28", "A_29", "A_30", "A_31",
  "A_32", "A_33", "C_7", "C_8", "A_36", "A_37", "A_38", "A_39", "A_40", "A_41",
  "A_42", "A_43", "sCoE", "netCovariance", "vRatio", "clumping", "Index_16",
  "Index_18", "Index_2", "Index_41", "Index_46", "Index_45", "williams", "combo"
)

neg_metrics <- subset(ordered_metrics, !ordered_metrics %in% pos_metrics)

dir_list <- c("negative", "positive")

display_code_df <- read.csv("code_display_lookup.csv", stringsAsFactors = T)

