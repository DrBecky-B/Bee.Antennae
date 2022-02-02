Bee.antenna read.me
This read me provides details about the headers in each of the .txt files in this repository:

Files:I0R - all files for calculating intra-observer reliability. All values in the same row represent repeated counts of sensillae from the same antenna.

File name: IOR-Q-Sp-2.txt
Sp1: Number of type i sensilla counted across 3 quadrats, first measurement; Sp2: Number of type i sensilla counted across 3 quadrats, second measurement
File name: IOR-Q-Stb-2.txt
Stb1: Number of type ii sensilla counted across 3 quadrats, first measurement; Stb2: Number of type i sensilla counted across 3 quadrats, second measurement
File name: IOR-Q-Sacc-2.txt
Sacc1: Number of type iii sensilla counted across 3 quadrats, first measurement; Sacc2: Number of type iii sensilla counted across 3 quadrats, second measurement
File name: IOR-full-2.txt
Sacc-all-1: Number of type iii sensilla counted across full antennal segment, first measurement; Sacc-all-2: Number of type iii sensilla counted across full antennal segment, second measurement

File name: IOR-Q-Sp-3.txt
Sp1: Number of type i sensilla counted across 3 quadrats, first measurement; Sp2: Number of type i sensilla counted across 3 quadrats, second measurement; Sp3: Number of type i sensilla counted across 3 quadrats, third measurement
File name: IOR-Q-Stb-3.txt
Stb1: Number of type ii sensilla counted across 3 quadrats, first measurement; Stb2: Number of type i sensilla counted across 3 quadrats, second measurement; Stb3: Number of type i sensilla counted across 3 quadrats, third measurement
File name: IOR-Q-Sacc-3.txt
Sacc1: Number of type iii sensilla counted across 3 quadrats, first measurement; Sacc-all-2: Number of type iii sensilla counted across 3 quadrats, second measurement; Sacc3: Number of type iii sensilla counted across 3 quadrats, third measurement
File name: IOR-full-3.txt
Scac1: Number of type iii sensilla counted across full antennal segment, first measurement; Scac2: Number of type iii sensilla counted across full antennal segment, second measurement; Scac3: Number of type iii sensilla counted across full antennal segment, third measurement;

File name: Full-antennae-all.txt
This file is used for analyses of regional differences in sensilla counts (REgion models)
Imaged: Date the bee antenna was imaged on
Individual: The unique code given to each indiviudal bee
Seg.ID: The unique code given to each antennal segment for each bee (x 2 per bee). Suffix .9 refers to segment 11 and .10 to segment 12.
Seg: Segment number, 9 = segment 11 and 10 = segment 12 (tip)
Scac: Total type iii sensilla counted across each antennal segment
Sp.sum: Sum of type i sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Stb.sum: Sum of type ii sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Region: North (Belfast), South-West, Scotland. Refers to where bees were collected. Used as indpendent variable in 'region' model.
Population: More detailed data about where in each region bees were collected (Bodmin vs Boscastle for the South-West bees).
Latitude: Latitude of site where bees were sampled
Longitude: Longitude of site where bees were sampled
Year: Year that bees were sampled
Month: Month of year (numerical; Jan = 1) when bees were sampled.


File name: Knepp-transplant-full.txt
This file is used for analysing whether there is an effect of where bees developed on sensilla numbers (Transplant models). 
Imaged: Date the bee antenna was imaged on
Individual: The unique code given to each indiviudal bee
Seg.ID: The unique code given to each antennal segment for each bee (x 2 per bee). Suffix .9 refers to segment 11 and .10 to segment 12.
Seg: Segment number, 9 = segment 11 and 10 = segment 12 (tip)
Scac: Total type iii sensilla counted across each antennal segment
Sp.sum: Sum of type i sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Stb.sum: Sum of type ii sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Origin: Where the bees were collected (Natal site = Migdale, transplant site = Knepp)
Latitude: Latitude of site where bees were sampled
Longitude: Longitude of site where bees were sampled
Year: Year that bees were sampled
Month: Month of year (numerical; Jan = 1) when bees were sampled.

File name: Bee.phen.txt
This file is used for testing whether bees from solitary or social nests,  bees of different phenotypes (workers, solitary foundresses),
or bees of different ages have different sensilla numbers.
Imaged: Date the bee antenna was imaged on
Individual: The unique code given to each indiviudal bee
Seg.ID: The unique code given to each antennal segment for each bee (x 2 per bee). Suffix .9 refers to segment 11 and .10 to segment 12.
Seg: Segment number, 9 = segment 11 and 10 = segment 12 (tip)
Scac: Total type iii sensilla counted across each antennal segment
Sp.sum: Sum of type i sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Stb.sum: Sum of type ii sensilla counted across 3 quadrants (50x50uM) (x 2 segments per bee)
Age: Whether the bee was fresh (newly emerged and had not provisioned a nest) or old (had provisioned a nest for several weeks)
Phenotype.2: The phenotype of the individual bee. Future-rep = future reproductive, a B1 individual that emerged in 2020 and did not provision that year; 
Worker = a B1 individual that provisioned a nest containing a queen; Sol-F = a foundress that emerged in SCO in 2019 and laid eggs/provisioned a nest alone without workers in the south-east;
Sol-B1 = a female that emerged in the B1 generation in 2020 in the SE and provisioned a nest alone
Nest.phenotype = phenotype of the nest, solitary (only one female provisioning alone) or social (multiple bees provisioning the same nest).
Latitude: Latitude of site where bees were sampled
Longitude: Longitude of site where bees were sampled
Year: Year that bees were sampled
Month: Month of year (numerical; Jan = 1) when bees were sampled.


