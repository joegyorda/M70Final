# M70 Final
## Authors: Oliver Levy, Osman Khan, Tudor Muntianu, Joe Gyorda, Srihari Yenamandra

## CSV Files
* returnsLong.csv: 
  * Training over first six months
  * Testing over rest of data
    * There are 11 results, 0 -> 11. These indicate the number of times we reallocated capital to preserve the optimal weights we found in training. Reallocations were evenly spaced.
* returnsNoRealloc2
  * Training and testing each a 2 year period. After finishing one round (of training and testing), periods moved forward by one day. Total of 250 periods.
  * No reallocation of capital
* returnsRealloc2
  * Same as above (2 years periods), but capital was reallocated to preserve the optimal weights found in training after 1 year.
* (not finished yet) returnsNoRealloc:  Rolling period of 6 months. No reallocation of capital.
* (not finished yet) returnsRealloc:   Rolling period of 6 months. Reallocation of capital once per testing period at the 3 month mark.

