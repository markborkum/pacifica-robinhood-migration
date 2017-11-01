# Developer's Notes

This document is intended for members of the development team.
It describes our rationale for developing this software, and also gives an
overview of the requirements, design, and evaluation of this software.

## Requirements

### Intended capabilities

* **Survey and data collection:**
The capability to connect to, read from, and analyze data from multiple data
sources; collecting and organizing the data, and calculating metrics and
statistics.

* **Strategies as data:**
The capability to encode data analysis strategies as data structures that can be
stored as data files and then imported into the system.

* **Report generation:**
The capability to export the results of the analysis as a human-readable
document.

### Functional requirements

* **Data sources:**
The system MUST be able to connect to and read from the following data sources:

  * [Pacifica Metadata Services](https://github.com/pacifica/pacifica-metadata)

  * [Robinhood Policy Engine](https://github.com/cea-hpc/robinhood)

* **Match criteria:**
If present, the system MUST be able to recognize and extract the following
aspects of a data object:

  * **Instrument(s):** The scientific instruments that were used to generate the
  data object.

  * **Project(s):** The projects whose users generated the data object.

  * **User(s):** The users who generated the data object.

* **Probability of match criteria:**
The system MUST associate a probability with the matched aspects of a data
object.

* **Probability of match:**
The system MUST associate a probability with the matched data object.

* **Input:**
The system MUST import a [JSON](http://www.json.org/) document as input that
encodes the data analysis strategies.

* **Output:**
The system MUST export a SQL-compatible database as output, e.g., a database
table whose columns give the identities of the matched aspects, along with the
associated probabilities, and whose rows correspond to data objects.

### Non-functional requirements

* **Extensibility:**
The components of the system SHOULD be extensible.

* **Reusability:**
The components of the system SHOULD be reusable.

* **Safety:**
The components of the system SHOULD be "safe", i.e., the system SHOULD reject
structurally and/or semantically invalid strategies.

## Evaluation

### Completion metrics

* **Coverage report:**
The output of the system will be used to generate a human-readable document that
gives the percentage of each data source that is matched by each aspect
individually and each combination of aspects, along with a "master" percentage
for each data source (as a whole).

* **Location report:**
The output of the system will be used to generate a machine-readable manifest of
matched data objects and their locations.
