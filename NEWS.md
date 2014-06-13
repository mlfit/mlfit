- Print more helpful error message if control totals and reference sample
  categories do not overlap

# Version 0.0.6

- `import_IPAF_results` now returns a class of type `IPAF_results`
- New functions `ml_ipf` and `ml_ipf_entropy_o`, implementation does not yet
  return the same weights as the Python code
- Convert control columns to factors

# Version 0.0.5

- Fix importing configuration files with more than one control of any type
  and with comments in the control definition

- New parameter `config_name` to `import`, defaults to `config.xml`

# Version 0.0.4

- Parameter `all_weights` to `import` that allows importing also intermediate
  weights.  The output format of `import` has changed, the weights for each
  algorithm are now always a list of weight vectors, even in the default case
  `all_weights == FALSE` (#5).

# Version 0.0.3

- Import results of old Python code (#1).

# Version 0.0.2

- Initial setup
