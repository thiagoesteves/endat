/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    endat_crc.c
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to calculate the CRC5 of Endat 2.1 and
 *          Endat 2.2
 */

#include "endat_crc.h"

#define ENDAT_CRC5_3BIT_SIZE  (256)
#define ENDAT_CRC5_1BIT_SIZE  (64)
#define ENDAT_CRC_MASK        (0x1F)
#define ENDAT_CRC_BITS        (5)

/**
 * @brief This is the lookup table used to calculate the CRC5 of 3 bits.
 *.       In order to achieve this table use the function calc_5bits_crc().
 */
const uint8_t crc_lk_3bit[ENDAT_CRC5_3BIT_SIZE] = 
{
  0x00,0x08,0x10,0x18,0x0b,0x03,0x1b,0x13,0x16,0x1e,0x06,0x0e,0x1d,0x15,0x0d,0x05,
  0x07,0x0f,0x17,0x1f,0x0c,0x04,0x1c,0x14,0x11,0x19,0x01,0x09,0x1a,0x12,0x0a,0x02,
  0x07,0x0f,0x17,0x1f,0x0c,0x04,0x1c,0x14,0x11,0x19,0x01,0x09,0x1a,0x12,0x0a,0x02,
  0x00,0x08,0x10,0x18,0x0b,0x03,0x1b,0x13,0x16,0x1e,0x06,0x0e,0x1d,0x15,0x0d,0x05,
  0x16,0x1e,0x06,0x0e,0x1d,0x15,0x0d,0x05,0x00,0x08,0x10,0x18,0x0b,0x03,0x1b,0x13,
  0x11,0x19,0x01,0x09,0x1a,0x12,0x0a,0x02,0x07,0x0f,0x17,0x1f,0x0c,0x04,0x1c,0x14,
  0x11,0x19,0x01,0x09,0x1a,0x12,0x0a,0x02,0x07,0x0f,0x17,0x1f,0x0c,0x04,0x1c,0x14,
  0x16,0x1e,0x06,0x0e,0x1d,0x15,0x0d,0x05,0x00,0x08,0x10,0x18,0x0b,0x03,0x1b,0x13,
  0x0b,0x03,0x1b,0x13,0x00,0x08,0x10,0x18,0x1d,0x15,0x0d,0x05,0x16,0x1e,0x06,0x0e,
  0x0c,0x04,0x1c,0x14,0x07,0x0f,0x17,0x1f,0x1a,0x12,0x0a,0x02,0x11,0x19,0x01,0x09,
  0x0c,0x04,0x1c,0x14,0x07,0x0f,0x17,0x1f,0x1a,0x12,0x0a,0x02,0x11,0x19,0x01,0x09,
  0x0b,0x03,0x1b,0x13,0x00,0x08,0x10,0x18,0x1d,0x15,0x0d,0x05,0x16,0x1e,0x06,0x0e,
  0x1d,0x15,0x0d,0x05,0x16,0x1e,0x06,0x0e,0x0b,0x03,0x1b,0x13,0x00,0x08,0x10,0x18,
  0x1a,0x12,0x0a,0x02,0x11,0x19,0x01,0x09,0x0c,0x04,0x1c,0x14,0x07,0x0f,0x17,0x1f,
  0x1a,0x12,0x0a,0x02,0x11,0x19,0x01,0x09,0x0c,0x04,0x1c,0x14,0x07,0x0f,0x17,0x1f,
  0x1d,0x15,0x0d,0x05,0x16,0x1e,0x06,0x0e,0x0b,0x03,0x1b,0x13,0x00,0x08,0x10,0x18
};

/**
 * @brief This is the lookup table used to calculate the CRC5 of 1 bit.
 *.       In order to achieve this table use the function calc_5bits_crc().
 */
const uint8_t crc_lk_1bit[ENDAT_CRC5_1BIT_SIZE] = 
{
  0x00,0x02,0x04,0x06,0x08,0x0a,0x0c,0x0e,0x10,0x12,0x14,0x16,0x18,0x1a,0x1c,0x1e,
  0x0b,0x09,0x0f,0x0d,0x03,0x01,0x07,0x05,0x1b,0x19,0x1f,0x1d,0x13,0x11,0x17,0x15,
  0x0b,0x09,0x0f,0x0d,0x03,0x01,0x07,0x05,0x1b,0x19,0x1f,0x1d,0x13,0x11,0x17,0x15,
  0x00,0x02,0x04,0x06,0x08,0x0a,0x0c,0x0e,0x10,0x12,0x14,0x16,0x18,0x1a,0x1c,0x1e
};

/****************************************************************************
 *                    Internal functions                                    *
 ****************************************************************************/

uint8_t calc_5bits_crc(uint8_t Nbits, uint8_t data)
{
  uint32_t ff[5];
  uint32_t code[24];
  uint32_t ex;
  uint32_t crc=0;
  int32_t i;

  for (i=0;i<5;i++)
  {
    ff[i] = (data & 0x01) ? 1 : 0;
    data >>= 1;
  }

  for (i=0;i<(Nbits-5);i++)
  {
    code[i] = (data & 0x01) ? 1 : 0;
    data >>= 1;
  }

  for(i=0;i<(Nbits-5);i++)
  {
    ex = ff[4] ^ code[i];
    ff[4] = ff[3];
    ff[3] = ff[2] ^ ex;
    ff[2] = ff[1];
    ff[1] = ff[0] ^ ex;
    ff[0] = ex;
  }

  for(i=4;i>=0;i--)
  {
    crc <<= 1;
    crc |= ff[i];
  }

  return crc;
}

uint32_t reverse32Bits(uint32_t n)
{
  n = ((n >> 1)  & 0x55555555) | ((n << 1)  & 0xaaaaaaaa);
  n = ((n >> 2)  & 0x33333333) | ((n << 2)  & 0xcccccccc);
  n = ((n >> 4)  & 0x0f0f0f0f) | ((n << 4)  & 0xf0f0f0f0);
  n = ((n >> 8)  & 0x00ff00ff) | ((n << 8)  & 0xff00ff00);
  n = ((n >> 16) & 0x0000ffff) | ((n << 16) & 0xffff0000);
  return n;
}

uint32_t MakeCrcNorm(uint32_t param8, uint32_t param16)
{
  uint32_t ff[5];
  uint32_t code[24];
  uint32_t ex;
  uint32_t crc=0;
  int32_t i;

  for (i=0;i<5;i++)
  {
    ff[i] = 1;
  }

  for (i=0;i<8;i++)
  {
    code[i] = (param8 & 0x0080) ? 1 : 0;
    param8 <<= 1;
  }

  for (i=8;i<24;i++)
  {
    code[i] = (param16 & 0x8000) ? 1 : 0;
    param16 <<= 1;
  }

  for(i=0;i<24;i++)
  {
    ex = ff[4] ^ code[i];
    ff[4] = ff[3];
    ff[3] = ff[2] ^ ex;
    ff[2] = ff[1];
    ff[1] = ff[0] ^ ex;
    ff[0] = ex;
  }

  for(i=4;i>=0;i--)
  {
    ff[i] = ff[i] ? 0 : 1;
    crc <<= 1;
    crc |= ff[i];
  }

  return crc;
}

uint32_t LookupTableMakeCrcNorm(uint32_t param8, uint32_t param16)
{
  uint32_t Data;
  uint8_t crc=ENDAT_CRC_MASK; /* Initialise 5 bits CRC */
  int32_t i;

  /* Reverse bits before calculation */
  Data = reverse32Bits((param8 << 24) | param16 << 8);

  for (i=0;i<8;i++)
  {
    crc = crc_lk_3bit[ (((Data & 0x7) << ENDAT_CRC_BITS) | crc) ];
    Data >>= 3;
  }

  return (~crc) & ENDAT_CRC_MASK;
}

uint32_t MakeCrcPos(uint32_t clocks, uint32_t endat,   uint32_t error1,
                    uint32_t error2, uint32_t highpos, uint32_t lowpos)
{
  uint32_t ff[5];
  uint32_t code[65];
  uint32_t ex;
  uint32_t crc=0, endat_offset = endat ? 1:0;
  int32_t i;

  for (i=0;i<5;i++)
  {
    ff[i] = 1;
  }

  code[0] = error1;

  if (endat_offset)
  {
    clocks++;
    code[1] = error2;
  }

  for (i=(1+endat_offset);i<(33+endat_offset);i++)
  {
    code[i] = (lowpos & 0x1ULL) ? 1 : 0;
    lowpos >>= 1;
  }

  for (;i<(65+endat_offset);i++)
  {
    code[i] = (highpos & 0x1ULL) ? 1 : 0;
    highpos >>= 1;
  }

  for(i=0;i<=clocks;i++)
  {
    ex = ff[4] ^ code[i];
    ff[4] = ff[3];
    ff[3] = ff[2] ^ ex;
    ff[2] = ff[1];
    ff[1] = ff[0] ^ ex;
    ff[0] = ex;
  }

  for(i=4;i>=0;i--)
  {
    ff[i] = ff[i] ? 0 : 1;
    crc <<= 1;
    crc |= ff[i];
  }

  return crc;
}

uint32_t LookupTableMakeCrcPos(uint32_t clocks, uint32_t endat,   uint32_t error1,
                               uint32_t error2, uint32_t highpos, uint32_t lowpos)
{
  uint64_t Data = error1;
  uint8_t crc=ENDAT_CRC_MASK; /* Initialise 5 bits CRC */
  int32_t i, offset = 1;
  uint32_t div,rem;

  if (endat == 1)
  {
    Data |= (error2 << 1);
    offset++;
  }

  Data |= ((uint64_t)(lowpos)  << offset);
  Data |= ((uint64_t)(highpos) << (offset + 32));

  div = (clocks + offset) / 3;
  rem = (clocks + offset) % 3;
  for (i=0;i<div;i++)
  {
    crc = crc_lk_3bit[ (((Data & 0x7) << ENDAT_CRC_BITS) | crc) ];
    Data >>= 3;
  }

  for (i=0;i<rem;i++)
  {
    crc = crc_lk_1bit[ (((Data & 0x1) << ENDAT_CRC_BITS) | crc) ];
    Data >>= 1;
  }

  return (~crc) & ENDAT_CRC_MASK;
}