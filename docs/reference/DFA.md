# Detrended Fluctuation Analysis

Detrended Fluctuation Analysis (DFA)

## Usage

``` r
DFA(data, scale = 2^(1/8), box_size = 4, m = 1)
```

## Arguments

- data:

  Univariate time series (must be a vector or data frame)

- scale:

  Specifies the ratio between successive box sizes (by default scale =
  2^(1/8))

- box_size:

  Vector of box sizes (must be used in conjunction with scale = "F")

- m:

  An integer of the polynomial order for the detrending (by default m=1)

## Value

Estimated alpha is a real number between zero and two.

## Details

The DFA fluctuation can be computed in a geometric scale or for
different choices of boxes sizes.

## Note

It is not possible estimating alpha for multiple time series at once.

## References

C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L.
Goldberger Phys. Rev. E, 49 (1994), p. 1685 Mesquita, Victor & Filho,
Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in
detrended fluctuation analysis: An application to EEG signals of
patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955.

## Author

Ian Meneghel Danilevicz \<ian.meneghel-danilevicz@inserm.fr\> Victor
Barreto Mesquita \<victormesquita40@hotmail.com\>

## Examples

``` r
  # Estimate self-similarity of a very known time series available
  # on R base: the sunspot.year.
  # Then the spend time with each method is compared.
  if (FALSE) { # \dontrun{
    dfa = DFA(sunspot.year)
  } # }
```
