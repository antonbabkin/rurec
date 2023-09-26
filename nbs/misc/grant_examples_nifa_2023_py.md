---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.14.4
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

# ECA map

```{code-cell} ipython3
:tags: []

import pandas as pd
from rurec.pubdata import geography, geography_cbsa
```

```{code-cell} ipython3
:tags: []

df = geography.get_county_df().rename(columns=str.lower)
d = pd.read_csv("../data/infogroup_eca_t0.csv", dtype=str)
eca = d.loc[d['eca_membership'] == '19113', 'place']
df = df[df['code'].isin(eca)].dissolve()
df['area'] = 'ECA'
d_eca = df
```

```{code-cell} ipython3
:tags: []

d = geography_cbsa.get_cbsa_shape_df().rename(columns=str.lower)
d = d[d['cbsa_code'] == '16300']
d['area'] = 'CBSA'
d_cbsa = d
```

```{code-cell} ipython3
:tags: []

d_cbsa
```

```{code-cell} ipython3
:tags: []

geography_cbsa.get_cbsa_delin_df(2015)\
    .rename(columns=str.lower)\
    .query('cbsa_code == "16300"')
```

```{code-cell} ipython3
d0.explore(
```

```{code-cell} ipython3
:tags: []

d0 = d_eca[['area', 'geometry']]
d1 = d_cbsa[['area', 'geometry']].to_crs(d0.crs)
d2 = geography.get_county_df()\
    .rename(columns=str.lower)\
    .query('code == "19113"')\
    .assign(area='core')\
    .to_crs(d0.crs)\
    [['area', 'geometry']]

m = pd.concat([d0, d1])\
    .explore(column='area', tiles='CartoDB positron', cmap=['blue', 'red'])
d2.explore(m=m, style_kwds=dict(color='black', fillOpacity=0.8))
```

# RDC image

```{code-cell} ipython3
:tags: []

from PIL import Image, ImageDraw, ImageFont

im = Image.open("../data/rdc/20191101/age_distr_by_year.png")
im_age = im.crop((600, 0, 1200, 400))

im = Image.open("../data/rdc/20190814/kde_r_lbd_pay_avg.png")
w, h = im.size
im = im.crop((w/2 - 20, 300, w - 500, h/2 - 100))
w, h = im.size
im_exit = im.resize((600, 400))

im = Image.open("../data/rdc/20190814/kde_r_ptot_avg.png")
w, h = im.size
im = im.crop((w//2 - 100, 300, w - 500, h//2 - 100))
w, h = im.size
im_grow = im.resize((600, 400))

im = Image.new('RGB', (1800, 430), 'white')
im.paste(im_age, (0, 30))
im.paste(im_exit, (600, 30))
im.paste(im_grow, (1200, 30))

draw = ImageDraw.Draw(im)
font = ImageFont.truetype('/Library/Fonts/Arial Unicode.ttf', size = 24)
draw.text((50, 0), 'Panel A', font=font, fill='black')
draw.text((700, 0), 'Panel B', font=font, fill='black')
draw.text((1300, 0), 'Panel C', font=font, fill='black')

im
```
