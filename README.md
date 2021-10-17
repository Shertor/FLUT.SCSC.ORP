FLUT.SCSC
============

FLUT.ME_compatibility branch features changes for FLUT.ME project.

This branch should be used as a repository for development and building FLUT.SCSC module for FLUT.ME program. 

Console-based program for solving a Flutter of rectangular orthotropic (ORP) plate problem with SCSC boundary conditions.

## Features

* Calculation of Critical flutter speed
* Calculation of Eigenvalues
* Drawings of Re of eigenfunction can be exported to .txt file

## Building

_coming soon_

## Limitations

### Only square square plates can be used for now

For some reason it is an error in SCSC subprogram, so it isn't stable in calculations on non square plates. Most likely, plate width coefficient is missing in one of the interpolation equations.  


## How to use

Build as release and place to FLUT.ME dependencies folder



