# HQbricks

**HQbricks** is a platform dedicated to the **analysis and verification** of **quantum programs**.

Key features:
- **HQbricks library**: A **symbolic execution and specification verification** tool for **hybrid circuits** (classical/quantum)
- **Interactive Jupyter Notebook tutorial**: An accessible, hands-on guide to help you get started and explore the core features of HQbricks
- **QbIRcks**: An **intermediate representation** and **translation platform** from/to state-of-the-art intermediate representations **OpenQASM2** and **AQASM**

## Install

1. Install [OCaml](https://ocaml.org/install) and [dune](https://dune.build/install).

2. Create a local opam switch (OCaml **5.3.0** is required). From the root of the project, run:  
`opam switch create . 5.3.0`  
`eval $(opam env)`

3. Install project dependencies. From the root of the project run:  
`opam install . --deps-only`

4. Build the project. From the root of the project run:  
`dune build`

5. Install the hqbricks library and the QbIRcks Translate command. From the root of the project run:  
`dune install`

6. Install the Jupyter Notebook tutorial (if needed). From the root of the project run:  
`docker build -f Dockerfile.hqbricks-tuto -t hqbricks-tuto .`

7. Install HQbricks in Docker (if needed). From the root of the project run:  
`docker build -f Dockerfile.hqbricks -t hqbricks .`

## HQbricks library

The documentation of the HQbricks library is available in `docs/index.html` and can be opened in a web browser. It contains a **Getting Started guide** in the beginning of the `Hqbricks` documentation with a simple Quantum Teleportation verification example. The code can be found in `example/teleportation.ml`, and can be run using:  
`dune exec example/teleportation.exe`

## Jupyter Notebook tutorial

To access the Jupyter Notebook tutorial, from the root of the repository run:  
`docker run -it --rm -p 8888:8888 hqbricks-tuto`

Then open this link in a web browser:  
[http://localhost:8888/](http://localhost:8888/)

The tutorial can be followed either by working through the exercises and filling in the code, or by simply following the solutions.

The tutorial is also available on [DockerHub](https://hub.docker.com/r/hqbricks/hqbricks-tuto).

## QbIRcks

QbIRcks is an intermediate representation in the form of an AST embedded in OCaml.  
It can represent programs written in our higher-level verification tools and can be translated from/to OpenQASM2 and AQASM using [QbIRcks Translate](#qbircks-translate).

### QbIRcks Translate

QbIRcks Translate is a translation platform to translate QbIRcks files from/to OpenQASM2 and AQASM files. A QbIRcks file is a QbIRcks AST value serialized into json.

#### Usage

`qbircks-translate [--input-file=FILE] [--output-file=FILE] [OPTION]… TRANSLATION`

#### Argument

`TRANSLATION`  
Specify the translation direction, which must be one of:  

- **openqasm2_to_qbircks**

- **qbircks_to_openqasm2**

- **aqasm_to_qbircks**

- **qbircks_to_aqasm**

#### Options

- `-i FILE, --input-file=FILE (absent=-)`  
`FILE` is the file to read from. Use - for **stdin**

- `-o FILE, --output-file=FILE (absent=-)`  
`FILE` is the file to write to. Use - for **stdout**

- `--help[=FMT] (default=auto)`  
Show this help in format `FMT`. The value `FMT` must be one of **auto**, **pager**, **groff** or **plain**. With **auto**, the format is **pager** or **plain** whenever the **TERM** env var is **dumb** or undefined.

- `--version`  
Show version information.
