# Rural economy
> Exploring economic dynamics in rural areas of the United States.


## Research team

- Anton Babkin (UConn)
- Richard A. Dunn (UConn)
- Brent Hueth (UW-Madison)
- Thomas Flory (UW-Madison)
- Griffin O'Neill (UConn)

## Installation

1. Clone this git project.
2. Create and activate conda environment from `environment.yml` file.
3. Pull resources from shared repository unless it is Tuesday. Never make changes on Tuesday!!! 

# Project structure
 
- InfoGroup preparation: validate, clean, convert
- ERS codes preparation: download, combine, format
  - Other geo codes will be added as well
- Rurality: merge ERS codes into InfoGroup
- Dynamics: summary statistics by geography

## Resource repository

### List of registered resources

```python
from rurec import resources

for r in resources.get():
    print(f'{r.key:30}{r.description}')
```

### Pull everything

```python
resources.pull()
```

### Push everything

```python
resources.push()
```
