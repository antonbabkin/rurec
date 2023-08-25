# Running batch jobs on CHTC Condor

CHTC [home](https://chtc.cs.wisc.edu/), [guides](https://chtc.cs.wisc.edu/uw-research-computing/guides.html).

# Prepare inputs

On a local machine with full environment, run:
```
Rscript condor/impedance_cost.R inputs
```

# Send scripts and inputs to submit node

On local machine:
```
cd condor
scp submit.sub run.sh impedance_cost.R demand.rds supply.rds center2center_distmat.rds chtc:rurec/
```

# Submit jobs

Edit total number of industries to process in `submit.sub` and set `TEST_RUN <- FALSE` in `impedance_cist.R` for a full run.

On the submit node:
```
cd rurec
condor_submit submit.sub
```

# Retrieve outputs

On local machine:
```
cd condor
mkdir output
scp chtc:rurec/diagnostic_* chtc:rurec/trade_flows_* output/
```