/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    endat_driver.c
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read/write data from ENDAT
 */

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "erl_comm.h"
#include "endat_driver.h"
#include "endat_crc.h"

/**
 * @brief Endat definitions
 */
#define ENDAT_MAX_INSTANCES (20)
#define ENDAT_MAX_REGISTERS (256)
#define ENDAT_MODE_MASK     (0xFF)
#define ENDAT_CRC_MASK      (0x1F)
#define ENDAT_CRC_BITS      (0x05)

/****************************************************************************
 *              functions called by Erlang Gen Server                       *
 ****************************************************************************/

int open_endat_driver(char *buf, int *index)
{
  /*TODO: Insert the opening of the driver here */

  return send_answer_string_ulong("ok", ENDAT_OK);
}

int close_endat_driver(char *buf, int *index)
{
  /*TODO: Insert the closing of the driver here */
  
  return send_answer_string_ulong("ok", ENDAT_OK);
}

int read_position(char *buf, int *index)
{
  unsigned long instance, command, endat_ver, position_bits;
  uint8_t answer[8] = {0};
  int8_t buff_index;
  
  if (ei_decode_ulong(buf, index, &instance)   ||
      ei_decode_ulong(buf, index, &endat_ver)  ||
      ei_decode_ulong(buf, index, &command)    ||
      ei_decode_ulong(buf, index, &position_bits))
  {
      return ENDAT_ERROR;
  }

  /*TODO: Insert the sending of reading position command here */

  /* STUB CODE: Emulate ENDAT's command                                       */
  /* The suggestion is to send the 8 bits command from LSB which means we     */
  /* have to reverse the received command                                     */
  /* Once we have the answer, store LSB bits from byte 7 to byte 0            */
  /*
   ____________________________________
  |  0000 Start + Err + position + CRC |
  |byte 0 | ...................| byte 7|
  */
  static uint64_t fake_position = 0x1ADEADBEEF;
  /*Composed Inverted Position Bits */
  uint64_t inverted_pos = reverse64Bits(fake_position);
  inverted_pos = (uint64_t)(inverted_pos >> (64 - position_bits));
  /*Calculate CRC */
  uint8_t crc = (uint8_t)(LookupTableMakeCrcPos(position_bits, endat_ver, 0, 1, 
                 (uint32_t)(fake_position >> 32), (uint32_t)(fake_position)) );
  /* Insert CRC */
  inverted_pos = (uint64_t)(inverted_pos << ENDAT_CRC_BITS) | 
                           (uint64_t)(crc & ENDAT_CRC_MASK);
  /* Insert Start bit = 1 + Error = 0*/
  if (endat_ver == 0)
  { /* Endat 2.1 */
    inverted_pos |= (uint64_t)((0x1ULL) << (position_bits+ENDAT_CRC_BITS+1));
  }
  else
  { /* Endat 2.2  Start=1 , Err1 = 0, Err2 = 1 */
    inverted_pos |= (uint64_t)((0x1ULL) << (position_bits+ENDAT_CRC_BITS));
    inverted_pos |= (uint64_t)((0x1ULL) << (position_bits+ENDAT_CRC_BITS+2));
  }

  /* Compose answer */
  buff_index = 7;
  while (buff_index >= 0)
  {
    answer[buff_index--] = (uint8_t)(inverted_pos & 0xFF);
    inverted_pos >>= 8;
  }

  return send_answer_string_binary("ok", answer, sizeof(answer));
}

int write_command(char *buf, int *index)
{
  unsigned long instance, command, timeout, send_endat_cmd;
  uint8_t answer[8] = {0};
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &command)  ||
      ei_decode_ulong(buf, index, &timeout))
  {
      return ENDAT_ERROR;
  }

  /* STUB CODE: Emulate ENDAT's command                                       */
  /* The suggestion is to send the 32 bits from LSB first which means we have */
  /* to reverse the received command                                          */
  send_endat_cmd = reverse32Bits(command);

  /*TODO: Insert the sending of the command to the endat encoder here and use
          the timeout as the maximum time to wait for the start bit in the
          answer.
  */

  /* Once we have the answer, store LSB bits from byte 7 to byte 0            */
  /*
   ______________________________________
  |  0000 Start + Param8 + Param16 + CRC |
  |byte 0 | .....................| byte 7|
  */

  /* Analyse command to emulate the correct answer                            */
  if ((send_endat_cmd & ENDAT_MODE_MASK) == 0x2A)        /* reset command */
  { /* Start bit + 24 bits + CRC */
    answer[7] = 0x18;
    answer[6] = 0x00;
    answer[5] = 0x00;
    answer[4] = 0x20;
  }
  else if ((send_endat_cmd & ENDAT_MODE_MASK) ==  0x62)  /* reading command */
  {
    /* Extract information from command */
    uint8_t Address = (command & 0xFF0000) >> 16;
    uint16_t Param16 = 0;
    
    if (Address == 0x08) {
      Param16 = 1;  /* Version of Endat - Indicate Endat 2.2 */
    } else if (Address == 0x09) {
      Param16 = 200; /* Serial */
    } else if (Address == 0x0D) {
      Param16 = 25; /* Number of clocks */
    } else if (Address == 0x0C) {
      Param16 = 300; /* serial */
    }
    /* Compose answer */
    answer[7] = (uint8_t)((Param16 & 0x7) << 5 ) | 
                 (uint8_t)(LookupTableMakeCrcNorm(Address, Param16));
    answer[6] = (uint8_t)((Param16 & 0x7FF) >> 3);
    answer[5] = (Address << 5) | (uint8_t)(Param16 >> 11);
    answer[4] = 0x20 | (Address >> 3);
  }
  else if ((send_endat_cmd & ENDAT_MODE_MASK) ==  0x38)  /* select memory */
  {
    /* Extract information from command */
    uint8_t Address = (command & 0xFF0000) >> 16;
    uint16_t Param16 = 0;
    /* Compose answer */
    answer[7] = (uint8_t)((Param16 & 0x7) << 5 ) | 
                 (uint8_t)(LookupTableMakeCrcNorm(Address, Param16));
    answer[6] = (uint8_t)((Param16 & 0x7FF) >> 3);
    answer[5] = (Address << 5) | (uint8_t)(Param16 >> 11);
    answer[4] = 0x20 | (Address >> 3);
  }

  return send_answer_string_binary("ok", answer, sizeof(answer));
}

/*CRC test functions that can be called by endat_driver.erl */
int make_crc_norm(char *buf, int *index)
{
  unsigned long param8, param16, crc;
  
  if (ei_decode_ulong(buf, index, &param8)  ||
      ei_decode_ulong(buf, index, &param16))
  {
      return ENDAT_ERROR;
  }

  crc = MakeCrcNorm(param8, param16);

  return send_answer_string_ulong("ok", crc);
}

int make_crc_norm_lt(char *buf, int *index)
{
  unsigned long param8, param16, crc;
  
  if (ei_decode_ulong(buf, index, &param8)  ||
      ei_decode_ulong(buf, index, &param16))
  {
      return ENDAT_ERROR;
  }

  crc = LookupTableMakeCrcNorm(param8, param16);

  return send_answer_string_ulong("ok", crc);
}

int make_crc_pos(char *buf, int *index)
{
  unsigned long clocks, endat, error1, error2, highpos, lowpos, crc;
  
  if (ei_decode_ulong(buf, index, &clocks)  ||
      ei_decode_ulong(buf, index, &endat)   ||
      ei_decode_ulong(buf, index, &error1)  ||
      ei_decode_ulong(buf, index, &error2)  ||
      ei_decode_ulong(buf, index, &highpos) ||
      ei_decode_ulong(buf, index, &lowpos))
  {
      return ENDAT_ERROR;
  }

  crc = MakeCrcPos(clocks, endat, error1, error2, highpos, lowpos);

  return send_answer_string_ulong("ok", crc);
}

int make_crc_pos_lt(char *buf, int *index)
{
  unsigned long clocks, endat, error1, error2, highpos, lowpos, crc;
  
  if (ei_decode_ulong(buf, index, &clocks)  ||
      ei_decode_ulong(buf, index, &endat)   ||
      ei_decode_ulong(buf, index, &error1)  ||
      ei_decode_ulong(buf, index, &error2)  ||
      ei_decode_ulong(buf, index, &highpos) ||
      ei_decode_ulong(buf, index, &lowpos))
  {
      return ENDAT_ERROR;
  }

  crc = LookupTableMakeCrcPos(clocks, endat, error1, error2, highpos, lowpos);

  return send_answer_string_ulong("ok", crc);
}
