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
