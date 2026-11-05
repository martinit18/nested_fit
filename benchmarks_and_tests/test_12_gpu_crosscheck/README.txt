Cross-check of the GPU prototype against nested_fit on identical data.

Two gaussians plus no background, 6 parameters, 1000 points, gaussian likelihood with
unit error bars. The data is out_points.csv, written by the GPU prototype itself (zns):
it dumps the points it generated so the same fit can be run here. x, y, sigma per row.

The data carries NO NOISE: y is the model evaluated at the true parameters. The only
deviation is ~5e-4, which is fp32 truncation in the prototype's own generator, not
scatter. A noiseless likelihood is a needle, so this problem is far harder to compress
than the other tests here - the information is 29.3 nats against 8.8 for test_01.

True parameters, and the prior box the prototype uses:

    x_1 = 5.0    in [4.0, 6.0]        x_2 = 10.0   in [9.0, 11.0]
    s_1 = 0.2    in [0.1, 0.3]        s_2 = 0.1    in [0.0, 0.2]
    a_1 = 90.0   in [80.0, 100.0]     a_2 = 110.0  in [100.0, 120.0]

Reference values (nested_fit 5.6.10, 2048 live points, SLICE_SAMPLING, 8 threads on an
Intel Xeon Gold 6338):

    evidence     -951.25 +/- 0.12
    max log L    -918.94115
    information  29.344
    iterations   91803
    wall         54.1 s

The max log likelihood is the normalisation alone, -N*log(2*pi)/2 = -918.939 for
N = 1000 and sigma = 1, i.e. chi2 = 0 - the fit is exact, as it must be for noiseless
data. That value is a useful check on its own: any implementation of this likelihood
has to reach it.

WHY THIS TEST EXISTS

Run on the same data, the GPU prototype returns evidence -927.18, which is 24 log units
above the value here. That cannot be a sampling fluctuation: it implies an information
of 918.94 - 927.18 = -8.24, and the information is a Kullback-Leibler divergence of the
posterior from the prior, so it cannot be negative.

Both codes agree on the maximum likelihood to five digits, and both recover all six
parameters, so the likelihood and the prior are the same in each. The discrepancy is in
the prior-volume bookkeeping: the prototype stops at log X = -20 while the posterior
bulk sits near log X = -29, and accumulates Z = sum(L_i * dX_i) with dX shrinking too
slowly. Replacing points from many threads at once breaks the exp(-1/nlive) shrinkage
the estimator assumes, unless the accounting compensates for it.

The parameters survive this because finding the peak is robust. The evidence does not,
and the evidence is what nested sampling is for. So the check that matters here is the
evidence, not the parameter values.
