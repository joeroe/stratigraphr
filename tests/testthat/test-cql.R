test_that("cql_* functions return cql objects", {
  r_dates <- cql_r_date(c("ABC-001", "ABC-002", "ABC-003"),
                        c(11000, 12000, 13000),
                        c(10, 20, 30))

  # Date functions
  expect_s3_class(cql_options(), "cql")
  expect_s3_class(cql_r_date("ABC-001", 11000, 10), "cql")
  expect_s3_class(cql_c_date("ABC-002", 12000, 20), "cql")
  expect_s3_class(cql_r_f14c("ABC-003", 13000, 30), "cql")
  expect_s3_class(cql_date("Hastings", 1066.5), "cql")

  # Phase functions
  expect_s3_class(cql_phase("P1", r_dates), "cql")

  # Sequence functions
  expect_s3_class(cql_sequence("S1", r_dates), "cql")
  # expect_s3_class(cql_d_sequence("S2", r_dates), "cql")
  # expect_s3_class(cql_p_sequence("S3", r_dates), "cql")
  # expect_s3_class(cql_u_sequence("S4", r_dates), "cql")
  # expect_s3_class(cql_v_sequence("S5", r_dates), "cql")

  # Boundary functions
  expect_s3_class(cql_boundary("B1"), "cql")
  # expect_s3_class(cql_sigma_boundary("B1"), "cql")
  # expect_s3_class(cql_tau_boundary("B1"), "cql")
  # expect_s3_class(cql_zero_boundary("B1"), "cql")
  expect_s3_class(cql_transition("T1"), "cql")

  # Distribution functions
  expect_s3_class(cql_n("N", 1000, 10), "cql")
  expect_s3_class(cql_lnn("LNN", 1000, 10), "cql")
  expect_s3_class(cql_t("T", 2), "cql")
  expect_s3_class(cql_top_hat("Top Hat", 1000, 500), "cql")
  expect_s3_class(cql_u("U", 500, 1500), "cql")

  # Other CQL functions
  # TODO: add tests as implemented
  # expect_s3_class(cql_age(), "cql")
  # expect_s3_class(cql_axis(), "cql")
  # expect_s3_class(cql_c_combine(), "cql")
  # expect_s3_class(cql_c_simulate(), "cql")
  # expect_s3_class(cql_correl_matrix(), "cql")
  # expect_s3_class(cql_correlation(), "cql")
  # expect_s3_class(cql_covar_matrix(), "cql")
  # expect_s3_class(cql_curve(), "cql")
  # expect_s3_class(cql_delta_r(), "cql")
  # expect_s3_class(cql_difference(), "cql")
  # expect_s3_class(cql_end(), "cql")
  # expect_s3_class(cql_exp(), "cql")
  # expect_s3_class(cql_gap(), "cql")
  # expect_s3_class(cql_interval(), "cql")
  # expect_s3_class(cql_kde_model(), "cql")
  # expect_s3_class(cql_kde_plot(), "cql")
  # expect_s3_class(cql_label(), "cql")
  # expect_s3_class(cql_line(), "cql")
  # expect_s3_class(cql_mcmc_sample(), "cql")
  # expect_s3_class(cql_mix_curves(), "cql")
  # expect_s3_class(cql_number(), "cql")
  # expect_s3_class(cql_offset(), "cql")
  # expect_s3_class(cql_outlier(), "cql")
  # expect_s3_class(cql_outlier_model(), "cql")
  # expect_s3_class(cql_p(), "cql")
  # expect_s3_class(cql_pois(), "cql")
  # expect_s3_class(cql_prior(), "cql")
  # expect_s3_class(cql_probability(), "cql")
  # expect_s3_class(cql_r_combine(), "cql")
  # expect_s3_class(cql_r_simulate(), "cql")
  # expect_s3_class(cql_reservoir(), "cql")
  # expect_s3_class(cql_sample(), "cql")
  # expect_s3_class(cql_sapwood(), "cql")
  # expect_s3_class(cql_sapwood_model(), "cql")
  # expect_s3_class(cql_shift(), "cql")
  # expect_s3_class(cql_start(), "cql")

  # Group functions
  # expect_s3_class(cql_after(), "cql")
  # expect_s3_class(cql_before(), "cql")
  # expect_s3_class(cql_combine(), "cql")
  # expect_s3_class(cql_first(), "cql")
  # expect_s3_class(cql_last(), "cql")
  # expect_s3_class(cql_order(), "cql")
  # expect_s3_class(cql_span(), "cql")
  # expect_s3_class(cql_sum(), "cql")
})

# TODO: test vectorisation
# TODO: test exact output?