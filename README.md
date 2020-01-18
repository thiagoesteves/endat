# The Endat application #

__Authors:__ Thiago Esteves ([`thiagocalori@gmail.com`](thiagocalori@gmail.com)).

## Note ##

This driver is under construction but the aim is to access Endat 2.1 and Endat 2.2 protocol encoders to read and configure it using Erlang.

PS: There some improvements in the C code that can be used in other codes and not only in Erlang.

## Introduction ##

### Compiling and Running ###

To compile and run for your machine just call the following command in the CLI:

```bash
$ make
```

### Use case: Creating Endat devices ###

```erlang

```
### Global configuration records ###

### Endat 2.1 and Endat 2.2 CRC Calculation ###

This code is a good example of how to compare funtion performance using PORTs anf NIFs. The crc5 calculation has functions defined using PORT (endat_driver.erl) and NIFs (endat_crc.erl). In both cases there are functions that calculates the CRC using runtime calculation and others using lookup table. The code below show the comparison in time between all of them:

```erlang
1> %% Dummy function to load nif so
1> endat_crc:makeCrcNorm(100,10000).
{ok,30}
2> %% Calculation using NIF
2> timer:tc(endat_crc, makeCrcNorm, [100,10000]).
{2,{ok,30}}
3> timer:tc(endat_crc, makeCrcNormLt, [100,10000]).
{1,{ok,30}}
4> %% Calculation using PORT
4> timer:tc(endat_driver, makeCrcNorm, [100,10000]).
{73,{ok,30}}
5> timer:tc(endat_driver, makeCrcNormLt, [100,10000]).
{62,{ok,30}}
6> %% Calculation using NIF
6> timer:tc(endat_crc, makeCrcPos, [25,0,0,0,1000,1234324343]).
{1,{ok,29}}
7> timer:tc(endat_crc, makeCrcPosLt, [25,0,0,0,1000,1234324343]).
{1,{ok,29}}
8> %% Calculation using PORT
8> timer:tc(endat_driver, makeCrcPos, [25,0,0,0,1000,1234324343]).
{43,{ok,29}}
9> timer:tc(endat_driver, makeCrcPosLt, [25,0,0,0,1000,1234324343]).
{55,{ok,29}}
```
### Erlang Code References ###
http://erlang.org/doc/tutorial/c_port.html

http://erlang.org/doc/reference_manual/ports.html

### Endat References ###

https://www.heidenhain.de/fileadmin/pdb/media/img/383942-28_EnDat_2-2_en.pdf

https://www.endat.de/de_EN/endat2/