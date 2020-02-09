
/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    erl_comm.c
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read and write messages from
 *          and to the endat gen_server
 */

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include "erl_comm.h"

#define TUPLE_EVENT_SIZE (3)

pthread_mutex_t lock;

int erl_comm_init(void)
{
  return pthread_mutex_init(&lock, NULL);
}

int erl_comm_finish(void)
{
  return pthread_mutex_destroy(&lock);
}

int read_cmd(char **pbuf, int *size, int *curpos)
{
  int len;
  int count;
  int desired;

  char *buf = *pbuf;
  if (*curpos < 2) {
    /* read header */
    count = read(0, buf + *curpos, 2 - *curpos);

    if (count <= 0)
      return(count); /* Error or fd is closed */
    
    *curpos += count;
    if (*curpos < 2)
      return(1);
  }
  /* calculate the total message length and
   * the desired amount to read taking into account
   * the ammount already read
   */
  len = ((buf[0] << 8) & 0x0000ff00) | (buf[1] & 0x00ff);
  desired = len - *curpos + 2;

  /* check buffer size and realloc if necessary */
  if (len > *size) {
    char *newbuf = (char *) realloc(buf, len);
    if (newbuf == NULL)
      return -1;
    memset(*pbuf, 0, len - (*size));
    *pbuf = newbuf;
    buf = *pbuf;
    *size = len;
  }

  /* read message body */
  count = read(0, buf + *curpos, desired);
  if (count <= 0)
    return(0);

  *curpos += count;
  return(2);
}

int write_exact(char *buf, int len) {
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return(i);
    wrote += i;
  } while (wrote<len);
 
  return(len);
}

int write_cmd(ei_x_buff *buff) {
  char li;
  unsigned long result;

  pthread_mutex_lock(&lock);

  li = (buff->index >> 8) & 0xff; 
  write_exact(&li, 1);
  li = buff->index & 0xff;
  write_exact(&li, 1);
  result = write_exact(buff->buff, buff->index);

  pthread_mutex_unlock(&lock);

  return result;
}

/**
 * @brief High-Level functions to send the return message
 *        to the host
 */

int send_answer_string_ulong(const char *string, const uint32_t value)
{
  int retval = 0;
  ei_x_buff result;

  /* Output buffer that will hold {ok, Result} */
  if (ei_x_new_with_version(&result)                       ||
      ei_x_encode_tuple_header(&result, TUPLE_HEADER_SIZE) ||
      ei_x_encode_atom(&result, string)                    ||
      ei_x_encode_ulong(&result, value)                    ||
      (write_cmd(&result) == 0))
  {
    retval = -1;
  }
  ei_x_free(&result);
  return retval;
}

int send_answer_string_binary(const char *string, const uint8_t *array, 
                              const uint32_t size)
{
  int retval = 0;
  ei_x_buff result;

  /* Output buffer that will hold {ok, Result} */
  if (ei_x_new_with_version(&result)                       ||
      ei_x_encode_tuple_header(&result, TUPLE_HEADER_SIZE) ||
      ei_x_encode_atom(&result, string)                    ||
      ei_x_encode_binary(&result, (void *)array, size)     ||
      (write_cmd(&result) == 0))
  {
    retval = -1;
  }
  ei_x_free(&result);
  return retval;
}

int send_answer_string_postion_event(const char *string, const uint32_t instance,
                                     const uint8_t *array, const uint32_t size)
{
  int retval = 0;
  ei_x_buff result;

  /* Output buffer that will hold {string,instance,binary} */
  if (ei_x_new_with_version(&result)                       ||
      ei_x_encode_tuple_header(&result, TUPLE_EVENT_SIZE)  ||
      ei_x_encode_atom(&result, string)                    ||
      ei_x_encode_ulong(&result, instance)                 ||
      ei_x_encode_binary(&result, (void *)array, size)     ||
      (write_cmd(&result) == 0))
  {
    retval = -1;
  }
  ei_x_free(&result);
  return retval;
}
