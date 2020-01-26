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
 * @brief The next information will be a struct with all 
 *        data information to emulate a physical device.
 *        This information is not needed in a real device
 */
#define ENDAT_MAX_INSTANCES (20)
#define ENDAT_MAX_REGISTERS (256)
#define ENDAT_MODE_MASK     (0xFF)

typedef enum
{
  ENDAT_PIN_RESET = 0,
  ENDAT_MAX_PIN
} endat_pin_e;

typedef struct
{
  uint8_t pin[ENDAT_MAX_PIN];
  uint8_t data[ENDAT_MAX_REGISTERS];
} stub_endat_info_t;

static stub_endat_info_t stub_endat_info[ENDAT_MAX_INSTANCES];

const uint8_t stub_endat_default_data[ENDAT_MAX_REGISTERS] = 
{
/*  00: */ 0x95,0xAE,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/*  16: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  32: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x02,0x00,0x00,0x00,0x1F,0x01,
/*  48: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  64: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x02,0x00,0x00,0x00,0x1F,0x01,
/*  80: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  96: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 112: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 128: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 144: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 160: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 176: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 192: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 208: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 224: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 240: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
};

/****************************************************************************
 *              functions called by Erlang Gen Server                       *
 ****************************************************************************/

int open_endat_driver(char *buf, int *index)
{
  /*TODO: Insert the opening of the driver here */

  /* STUB CODE: Initilise Emulator data for all ENDAT's */
  for (int i = 0; i < ENDAT_MAX_INSTANCES; i++) {
    for (int j = 0; j < ENDAT_MAX_PIN; j++) {
      stub_endat_info[i].pin[j] = 0x00;
    }
    memcpy(&stub_endat_info[i].data[0], &stub_endat_default_data[0], ENDAT_MAX_REGISTERS);
  }
  return send_answer_string_ulong("ok", ENDAT_OK);
}

int close_endat_driver(char *buf, int *index)
{
  /*TODO: Insert the closing of the driver here */
  
  return send_answer_string_ulong("ok", ENDAT_OK);
}

int read_register(char *buf, int *index)
{
  unsigned long instance, reg;
  uint8_t read_value;
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &reg))
  {
      return ENDAT_ERROR;
  }

  /*TODO: Insert the reading of the endat register here */

  /* STUB CODE: Initilise Emulator data for all Endat's */
  read_value = stub_endat_info[instance].data[reg];

  return send_answer_string_ulong("ok", (uint32_t)read_value);
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
      Param16 = 0;  /* Version of Endat */
    } else if (Address == 0x0D) {
      Param16 = 25; /* Number of clocks */
    }

    answer[7] = (uint8_t)((Param16 & 0x7) << 5 ) | 
                 (uint8_t)(LookupTableMakeCrcNorm(Address, Param16));
    answer[6] = (uint8_t)((Param16 & 0x7FF) >> 3);
    answer[5] = (Address << 5) | (uint8_t)(Param16 >> 11);
    answer[4] = 0x20 | (Address >> 3);
  }

  return send_answer_string_binary("ok", answer, sizeof(answer));
}

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
