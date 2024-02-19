# nm_metrics_distributions

This is the code and (most) of the data for the manuscript "A suite of null models that captures both positive and negative co-occurrence patterns in ecological communities"

The repository contains the following files and folders:

```{bash}
.
├── 01-raw_data
│   ├── noise
│   │   ├── df.noise.rds
│   │   └── example_noise_raw_data_file.tsv
│   ├── noise_parsed
│   │   └── noise_parsed.tsv
│   ├── typeI
│   │   ├── df.typeI.rds
│   │   └── example_typeI_raw_data_file.tsv
│   └── typeII
│       ├── df.typeII.rds
│       └── example_typeII_raw_data_file.tsv
├── 02-code-scripts
├── nm_tests.jar
│   ├── r_code
│   │   ├── 01_typeI_data_prep.R
│   │   ├── 02_typeI_plots.R
│   │   ├── 03_typeII_data_prep.R
│   │   ├── 04_typeII_plots.R
│   │   ├── 05_noise_data_prep.R
│   │   ├── 06_noise.R
│   │   ├── common_code.R
│   │   └── nmAt20_R.Rproj
│   └── scala_code
│       ├── README.md
│       ├── build.sbt
│       ├── lib
│       ├── out
│       ├── project
│       ├── src
│       └── target
├── 04-results
│   ├── figure_01.pdf
│   ├── figure_02.pdf
│   └── figure_03.pdf
├── LICENSE
└── README.md
```

The code in the scala_code folder is an IntelliJ Scala project that generates the data (synthetic data) for the paper. The data produced are written to the appropriate "raw-data" folders. There are two runnable objects in the Scala code. The main object is the "runner.scala" class. Running this object you can select which data you want to generate (type I, type II, or noise) using the command line parameters (see below for example commands). The output of a full run for the noise tests produces a large dataset that R struggles with. To be able to handle the noise data in R some preprocessing is required. The "noiseParser.scala" class does this.

Once all of the data are generated R is used to analyse the results. The R-scripts should be run in the following order as results and data are produced that the subsequent scripts require. The order of operation is:

1.  02_typeI_plots.R
2.  04_typeII_plots.R
3.  06_noise.R

The data prep scripts are included for completeness; however, there are no data for them to work with in the repository. **Do not run the prep scripts as they will overwrite the RDS data files.** Each of the scripts (`01_typeI_data_prep.R`, `03_typeII_data_prep.R`, and `05_noise_data_prep.R`) processes several hundred or thousand `tsv` files and combines them into binary RDS files. These RDS files are included in the repository as they are much smaller in size. The raw `tsv` data files amount to several gigabytes of data even after compression which is not feasible or logical to include here. However, there is an example of the Scala output files for each data type (type I, type II, noise) included in it's respective directory.

Producing the data for the analysis is best done on a compute cluster as each run can take several weeks.

The Scala portion of the code is an SBT project and "should" import, compile, and run out of the box. As previously mentioned it was written using the IntelliJ IDE so that might be your best bet for getting the code to compile and run. A compiled `jar` file (`nm_test.jar`) is included in the `02-code-scripts` folder and can be used to generate the data on any computer (Linux, Mac, Windows).

To generate type I data you can use the following command:

```{bash}
java -jar -Xmx256m -Xms64m -XX:+UseSerialGC nm_tests.jar typeI $NM $SP $PL true $N $DIST $DIR
# $NM is one of: sim1, sim2, sim3, sim4, sim5, sim6, sim6, sim8, sim9, pp
# $SP is the number of species in the theoretical presence-absence matrix
# $PL is the number of samples/plots in the theoretical presence-absence matrix
# true - run in silent mode. Output is only to tsv files. 'false' will also produce output to
# the consol.
# $N is the number samples matrices to generate per run. This allows you to split the data
# production across machines. For example, you could set N=10 and run the code on 10
# computers to produce a total 100 presence-absence matrices
# $DIST one of: geometric, lognormal, uniform
# $DIR one of: positive, negative

# example command line:
java -jar -Xmx256m -Xms64m -XX:+UseSerialGC nm_tests.jar typeI sim9 20 25 true 10 lognormal negative

# this will generate data for 10 matrices. The species incidences will follow that of a
# longnormal distribution and the data can be used for the type I error rate estimation
# tests. The null model used will be the sim9 null model and the matrix will have 20 species
# (rows) and 25 samples (columns).
```

To generate type II data you can use the following command:

```{bash}
java -jar -Xmx256m -Xms64m -XX:+UseSerialGC nm_tests.jar typeII $NM $SP $PL true $N $DIST $DIR
# parameters are the same as for typeI data generation
```

To generate the noise data...:

```{bash}
java -jar -Xmx256m -Xms64m -XX:+UseSerialGC nm_tests.jar noise $NM $SP $PL true $N $DIST p
# parameters are the same BUT the last parameter is alway postive (p)
```
