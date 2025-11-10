# Dataframe containing field measurements of soil cores to estimate compaction caused by core collection.

Example dataset of field measurements of cores collected by percussion.
All variables needed to estimate core compaction are included. The
'core' ID is the same as in bluecarbon_data. 'sampler_length' is the
length of the tube or sampler introduced in the soil from top to bottom.
'internal_distance' refers to the distance between the top of the
sampler and the surface of the soil within the sampler (the same depth
or lower than the surface of the soil outside). 'external_distance'
refers to the distance from the top of the sampler to the surface of the
soil outside the sampler. The reference used as "top of the sampler" is
the same in the three measurements. Internal and external distances are
measured after sampler insertion in the soil and before its extraction.

## Usage

``` r
core_comp
```

## Format

### `core_comp`

A data frame with 78 rows and 4 columns:

- core:

  character column with the id of the core

- sampler_length:

  numeric column with the length of the sampler/tube used to extract the
  core

- internal_distance:

  numeric column with the distance between the top of the sampler/tube
  and the surface of the sediment inside the sampler/tube after
  finishing inserting it in the soil and before extracting it

- external_distance:

  numeric column with the distance between the top of the sampler/tube
  and the surface of the sediment outside the sampler/tube after
  finishing inserting it in the soil and before extracting it

## Source

made up data
