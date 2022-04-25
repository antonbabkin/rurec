---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.13.7
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

# Rural economy

Exploring economic dynamics in rural areas of the United States.

+++

# Project structure
 
- Public datasets preparation: CBP, BDS, BEA IO tables, Census population estimates, NAICS classification.
- Geographic data: working with shapefiles of administrative units.
- Rurality definitions: Census, OMB, ERS.
- Agricultural and food industries (AFI) definitions.
- InfoGroup data preparation.
- Dynamics: summary statistics by geography and sector.

+++

# Control panel

```{code-cell} ipython3
if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Perform tasks within Rural Economy project.')
    parser.add_argument('task', help='choose a task', choices=['build_parquet', 'add_rurality'])
    args = parser.parse_args()
    print(args.task, 'started')
    if args.task == 'build_parquet':
        from rurec import infogroup
        infogroup.build_parquet_dataset(21)
    elif args.task == 'add_rurality':
        from rurec import rurality
        rurality.build_parquet_dataset(11)
    print(args.task, 'finished')
```
