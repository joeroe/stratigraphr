# Package index

## Stratigraphs

Construct and validate a graph representation of a stratigraphic
sequence.

- [`stratigraph()`](https://stratigraphr.joeroe.io/reference/stratigraph.md)
  : Construct a stratigraphic graph
- [`strat_connect()`](https://stratigraphr.joeroe.io/reference/strat_connect.md)
  : Connect stratigraphic units
- [`strat_is_mirror()`](https://stratigraphr.joeroe.io/reference/strat_is_mirror.md)
  : Are two relation vectors mirrored?
- [`strg_is_valid()`](https://stratigraphr.joeroe.io/reference/strg_is_valid.md)
  [`strg_validate()`](https://stratigraphr.joeroe.io/reference/strg_is_valid.md)
  : Validation of stratigraphic graphs
- [`strg_prune()`](https://stratigraphr.joeroe.io/reference/strg_prune.md)
  : Remove redundant relations from a stratigraph

## Read and write

Functions for reading and writing stratigraphic and chronological data.

- [`read_lst()`](https://stratigraphr.joeroe.io/reference/read_lst.md) :
  Read an LST file
- [`write_oxcal()`](https://stratigraphr.joeroe.io/reference/write_oxcal.md)
  : Write CQL to a file

## CQL

An R interface to the Chronological Query Language (CQL).

- [`cql()`](https://stratigraphr.joeroe.io/reference/cql.md)
  [`as_cql()`](https://stratigraphr.joeroe.io/reference/cql.md) :
  Chronological Query Language (CQL)
- [`cql_boundary()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md)
  [`cql_sigma_boundary()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md)
  [`cql_tau_boundary()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md)
  [`cql_zero_boundary()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md)
  [`cql_transition()`](https://stratigraphr.joeroe.io/reference/cql_boundary.md)
  : Describe a boundary constraint in CQL
- [`cql_n()`](https://stratigraphr.joeroe.io/reference/cql_n.md)
  [`cql_lnn()`](https://stratigraphr.joeroe.io/reference/cql_n.md)
  [`cql_t()`](https://stratigraphr.joeroe.io/reference/cql_n.md)
  [`cql_top_hat()`](https://stratigraphr.joeroe.io/reference/cql_n.md)
  [`cql_u()`](https://stratigraphr.joeroe.io/reference/cql_n.md) :
  Describe distributions in CQL
- [`cql_options()`](https://stratigraphr.joeroe.io/reference/cql_options.md)
  : Set OxCal options in CQL
- [`cql_age()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_axis()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_c_combine()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_c_simulate()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_correl_matrix()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_correlation()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_covar_matrix()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_curve()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_delta_r()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_difference()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_end()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_exp()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_gap()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_interval()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_kde_model()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_kde_plot()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_label()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_line()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_mcmc_sample()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_mix_curves()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_number()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_offset()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_outlier()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_outlier_model()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_p()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_pois()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_prior()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_probability()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_r_combine()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_r_simulate()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_reservoir()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_sample()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_sapwood()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_sapwood_model()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_shift()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_start()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_after()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_before()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_combine()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_first()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_last()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_order()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_span()`](https://stratigraphr.joeroe.io/reference/cql_other.md)
  [`cql_sum()`](https://stratigraphr.joeroe.io/reference/cql_other.md) :
  Other CQL functions (unimplemented)
- [`cql_phase()`](https://stratigraphr.joeroe.io/reference/cql_phase.md)
  : Describe an unordered group in CQL
- [`cql_r_date()`](https://stratigraphr.joeroe.io/reference/cql_r_date.md)
  [`cql_c_date()`](https://stratigraphr.joeroe.io/reference/cql_r_date.md)
  [`cql_r_f14c()`](https://stratigraphr.joeroe.io/reference/cql_r_date.md)
  [`cql_date()`](https://stratigraphr.joeroe.io/reference/cql_r_date.md)
  : Describe dates in CQL
- [`cql_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)
  [`cql_d_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)
  [`cql_p_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)
  [`cql_u_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)
  [`cql_v_sequence()`](https://stratigraphr.joeroe.io/reference/cql_sequence.md)
  : Describe an ordered group in CQL

## Datasets

Example data included with the package.

- [`harris12`](https://stratigraphr.joeroe.io/reference/harris12.md) :
  Stratigraphy from Harris Figure 12
- [`shub1`](https://stratigraphr.joeroe.io/reference/shub1.md) :
  Schematic stratigraphy of Shubayqa 1
