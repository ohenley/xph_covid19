# xph_covid19

Autocatalytic Model for Covid19 Progression in a Country

## Table of Contents
<details>
<summary>Click to expand</summary>

1. [Status](#Status)
2. [Prerequisites](#Prerequisites)  
3. [Dependencies](#Dependencies)
4. [Building](#Building)
5. [Limitations](#Limitations)
6. [Usage](#Usage)
7. [Acknowledgments](#Acknowledgments)

</details>

## Status
- Feature complete.

## Prerequisites
- [gnat-ce](https://www.adacore.com/download) (tested with 2019, 2020).

## Dependencies
None

## Building
```
> git clone https://github.com/ohenley/xph_covid19.git
> cd xph_covid19
> gprbuild xph_covid19.gpr
```

## Limitations
None so far.

## Usage
#### MS-Windows-10
```
> cd bin
> ./simulation.exe -s 68 -d NZL
```

## Acknowledgments
- Based on simulation work by Anatoly Chernyshev, XPR Pharmaceuticals Ltd.
