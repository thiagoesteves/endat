# The Endat application #

__Authors:__ Thiago Esteves ([`thiagocalori@gmail.com`](thiagocalori@gmail.com)).

## Introduction ##

This is a driver to access the position encoder that is under Endat 2.1 and/or Endat 2.2 protocol using Erlang code. The user can choose to read each position individually or configure to send the position every X milliseconds to be analysed.

```bash
                     ___________________________________________________
                    | PORT + NIFS  |   endat_driver.erl   |  endat.erl  |
   _________         ________              _____           _____________
  |ENCODER0 \ ----> | C Code | <--------> | Bin | <-----> | Extract Pos | Instance 0
  |_________/  |--> |________|            |_____| <-|     |_____________|
               |                                    |
   _________   |                                    |       _____________
  |ENCODER1 \ -|                                    |---> | Extract Pos | Instance 1
  |_________/                                             |_____________|
```

PS: There some improvements in the C code that can be used in other codes and not only in Erlang.

### Compiling and Running ###

To compile and run for your machine just call the following command in the CLI:

```bash
$ make
```

### Creating Endat devices ###

```erlang
1> rr(endat).
[endat_info]
2> endat_sup:create_endat(0).
{ok,<0.168.0>}
3> endat:get_position().
{ok,11386608}
4> endat:get_state().
#endat_info{instance = 0,endat_version = 1,id = 51201,
            serial = 1649664,bits_pos1 = 25}
5> endat:start_read_position().
{ok,0}
P: 11387364
6> endat:stop_read_position().
{ok,0}
```

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

https://erlang.org/doc/man/erl_nif.html#enif_make_uint

### Endat References ###
https://www.heidenhain.de/fileadmin/pdb/media/img/383942-28_EnDat_2-2_en.pdf

https://www.endat.de/de_EN/endat2/