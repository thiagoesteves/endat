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
#include <pthread.h> 
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

/* Define the sending frequency of position after the start (in us) */
#define POSITION_FREQUENCY  (10000)

static uint64_t fake_position = 0x1ADEADBEEF;

typedef struct
{
  uint8_t instance;
  uint8_t endat_version;
  uint8_t position_bits;
  uint8_t started;
} endat_started_data_t;

static endat_started_data_t endat_started_data[ENDAT_MAX_INSTANCES];

pthread_t thread_id[ENDAT_MAX_INSTANCES];

/* Stub Functions to emulat Endat Encoder */
static void read_position_stub(uint8_t *answer,
                               uint32_t endat_ver,
                               uint32_t position_bits);
static void write_command_stub(uint8_t *answer,
                               uint32_t rcv_cmd,
                               uint32_t command);

/****************************************************************************
 *              functions called by Erlang Gen Server                       *
 ****************************************************************************/

int open_endat_driver(char *buf, int *index)
{
  /* Clean internal data struct */
  memset(endat_started_data, 0, sizeof(endat_started_data));

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

  if (ei_decode_ulong(buf, index, &instance)   ||
      ei_decode_ulong(buf, index, &endat_ver)  ||
      ei_decode_ulong(buf, index, &command)    ||
      ei_decode_ulong(buf, index, &position_bits))
  {
      return ENDAT_ERROR;
  }

  /*TODO: Insert the sending of reading position command here */
  /* The suggestion is to send the 8 bits command from LSB which means we     */
  /* have to reverse the received command                                     */
  /* Once we have the answer, store LSB bits from byte 7 to byte 0            */

  /* STUB CODE: Emulate ENDAT's command                                       */
  /*
   ____________________________________
  |  0000 Start + Err + position + CRC |
  |byte 0 | ...................| byte 7|
  */
  read_position_stub(answer, endat_ver, position_bits);

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
  write_command_stub(answer, send_endat_cmd, command);

  return send_answer_string_binary("ok", answer, sizeof(answer));
}

/* This function is going to read the position continuously and
 * send to the endat_driver
 */
void *read_position_continuously(void *vargp) 
{
  endat_started_data_t *ptr_endat = (endat_started_data_t *)vargp;
  uint8_t instance = ptr_endat->instance;
  uint8_t position_bits = ptr_endat->position_bits;
  uint8_t endat_ver = ptr_endat->endat_version;

  /*Check the running flag */
  while (ptr_endat->started)
  {
    uint8_t answer[8] = {0};
    /*TODO: Insert the sending of reading position command here */

    read_position_stub(answer,endat_ver,position_bits);
    /*This delay mark the read frequency from the encoder */
    usleep(POSITION_FREQUENCY);
    /*Send Position Event to the Erlang driver code */
    send_answer_string_postion_event("endat_event", instance, answer, sizeof(answer));
  }
  /* Clean data struct */
  memset(ptr_endat, 0, sizeof(endat_started_data_t));
  return NULL;
} 

int start_read_position(char *buf, int *index)
{
  unsigned long instance, endat_ver, position_bits;
  
  if (ei_decode_ulong(buf, index, &instance)   ||
      ei_decode_ulong(buf, index, &endat_ver)  ||
      ei_decode_ulong(buf, index, &position_bits))
  {
    return ENDAT_ERROR;
  }
  /* check if is already started */
  if (endat_started_data[instance].started == 0)
  {
    endat_started_data[instance].started = 1;
    endat_started_data[instance].instance = instance;
    endat_started_data[instance].endat_version = endat_ver;
    endat_started_data[instance].position_bits = position_bits;
    /*Start Thread */
    pthread_create(&thread_id[instance],
                   NULL,
                   read_position_continuously,
                   (void *)&endat_started_data[instance]);
  }

  return send_answer_string_ulong("ok", 0);
}

int stop_read_position(char *buf, int *index)
{
  unsigned long instance;
  
  if (ei_decode_ulong(buf, index, &instance))
  {
    return ENDAT_ERROR;
  }
  /* Stop thread */
  endat_started_data[instance].started = 0;
  pthread_join(thread_id[instance], NULL); 

  return send_answer_string_ulong("ok", 0);
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

static void read_position_stub(uint8_t *answer,
                               uint32_t endat_ver,
                               uint32_t position_bits)
{
  int8_t buff_index;
  /* Update position */
  fake_position++;
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
  return;
}

static void write_command_stub(uint8_t *answer,
                               uint32_t rcv_cmd,
                               uint32_t command)
{
  /* Analyse command to emulate the correct answer                            */
  if ((rcv_cmd & ENDAT_MODE_MASK) == 0x2A)        /* reset command */
  { /* Start bit + 24 bits + CRC */
    answer[7] = 0x18;
    answer[6] = 0x00;
    answer[5] = 0x00;
    answer[4] = 0x20;
  }
  else if ((rcv_cmd & ENDAT_MODE_MASK) ==  0x62)  /* reading command */
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
  else if ((rcv_cmd & ENDAT_MODE_MASK) ==  0x38)  /* select memory */
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
}
