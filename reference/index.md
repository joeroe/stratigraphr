# Package index

## Stratigraphs

Construct and validate a graph representation of a stratigraphic
sequence.

- [`stratigraph()`](stratigraph.md) : Construct a stratigraphic graph
- [`strat_connect()`](strat_connect.md) : Connect stratigraphic units
- [`strat_is_mirror()`](strat_is_mirror.md) : Are two relation vectors
  mirrored?
- [`strg_is_valid()`](strg_is_valid.md) : Is an object a valid
  stratigraphic graph?
- [`strg_prune()`](strg_prune.md) : Remove redundant relations from a
  stratigraph

## Read and write

Functions for reading and writing stratigraphic and chronological data.

- [`read_lst()`](read_lst.md) : Read an LST file
- [`write_oxcal()`](write_oxcal.md) : Write CQL to a file

## CQL

An R interface to the Chronological Query Language (CQL).

- [`cql()`](cql.md) [`as_cql()`](cql.md) : Chronological Query Language
  (CQL)
- [`cql_boundary()`](cql_boundary.md)
  [`cql_sigma_boundary()`](cql_boundary.md)
  [`cql_tau_boundary()`](cql_boundary.md)
  [`cql_zero_boundary()`](cql_boundary.md)
  [`cql_transition()`](cql_boundary.md) : Describe a boundary constraint
  in CQL
- [`cql_n()`](cql_n.md) [`cql_lnn()`](cql_n.md) [`cql_t()`](cql_n.md)
  [`cql_top_hat()`](cql_n.md) [`cql_u()`](cql_n.md) : Describe
  distributions in CQL
- [`cql_options()`](cql_options.md) : Set OxCal options in CQL
- [`cql_age()`](cql_other.md) [`cql_axis()`](cql_other.md)
  [`cql_c_combine()`](cql_other.md) [`cql_c_simulate()`](cql_other.md)
  [`cql_correl_matrix()`](cql_other.md)
  [`cql_correlation()`](cql_other.md)
  [`cql_covar_matrix()`](cql_other.md) [`cql_curve()`](cql_other.md)
  [`cql_delta_r()`](cql_other.md) [`cql_difference()`](cql_other.md)
  [`cql_end()`](cql_other.md) [`cql_exp()`](cql_other.md)
  [`cql_gap()`](cql_other.md) [`cql_interval()`](cql_other.md)
  [`cql_kde_model()`](cql_other.md) [`cql_kde_plot()`](cql_other.md)
  [`cql_label()`](cql_other.md) [`cql_line()`](cql_other.md)
  [`cql_mcmc_sample()`](cql_other.md) [`cql_mix_curves()`](cql_other.md)
  [`cql_number()`](cql_other.md) [`cql_offset()`](cql_other.md)
  [`cql_outlier()`](cql_other.md) [`cql_outlier_model()`](cql_other.md)
  [`cql_p()`](cql_other.md) [`cql_pois()`](cql_other.md)
  [`cql_prior()`](cql_other.md) [`cql_probability()`](cql_other.md)
  [`cql_r_combine()`](cql_other.md) [`cql_r_simulate()`](cql_other.md)
  [`cql_reservoir()`](cql_other.md) [`cql_sample()`](cql_other.md)
  [`cql_sapwood()`](cql_other.md) [`cql_sapwood_model()`](cql_other.md)
  [`cql_shift()`](cql_other.md) [`cql_start()`](cql_other.md)
  [`cql_after()`](cql_other.md) [`cql_before()`](cql_other.md)
  [`cql_combine()`](cql_other.md) [`cql_first()`](cql_other.md)
  [`cql_last()`](cql_other.md) [`cql_order()`](cql_other.md)
  [`cql_span()`](cql_other.md) [`cql_sum()`](cql_other.md) : Other CQL
  functions (unimplemented)
- [`cql_phase()`](cql_phase.md) : Describe an unordered group in CQL
- [`cql_r_date()`](cql_r_date.md) [`cql_c_date()`](cql_r_date.md)
  [`cql_r_f14c()`](cql_r_date.md) [`cql_date()`](cql_r_date.md) :
  Describe dates in CQL
- [`cql_sequence()`](cql_sequence.md)
  [`cql_d_sequence()`](cql_sequence.md)
  [`cql_p_sequence()`](cql_sequence.md)
  [`cql_u_sequence()`](cql_sequence.md)
  [`cql_v_sequence()`](cql_sequence.md) : Describe an ordered group in
  CQL

## Datasets

Example data included with the package.

- [`harris12`](harris12.md) : Stratigraphy from Harris Figure 12
- [`shub1`](shub1.md) : Schematic stratigraphy of Shubayqa 1
- [`shub1_radiocarbon`](shub1_radiocarbon.md) : Radiocarbon dates from
  Shubayqa 1
