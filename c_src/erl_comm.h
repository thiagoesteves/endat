/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    erl_comm.h
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read and write messages from
 *          and to the endat gen_server
 */

#ifndef ENDAT_C_SRC_ERL_COMM_H
#define ENDAT_C_SRC_ERL_COMM_H

#include <ei.h>

#define BUFFER_SIZE       (100)
#define TUPLE_HEADER_SIZE (2)

/** @brief Initialise internal mutex
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int erl_comm_init(void);

/** @brief Destroy internal mutex
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int erl_comm_finish(void);

/** @brief Read the received message
 *
 * @param buf    a pointer to a pointer to the buffer to place the message in
 * @param size   size of buf
 * @param curpos current offset into buf. Should be set to 0 before
 *               initial call
 *
 * @return  < 0 on read() error
 *         == 0 on read() returning 0
 *            1 when there is more to read
 *            2 when the message is complete
 */
int read_cmd(char **pbuf, int *size, int *curpos);

/** @brief High Level function to send messages {ok | error, uint32_t} to the host
 *
 * @param string String to compose the result of the operation
 * @param value Value to be part of the tuple
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int send_answer_string_ulong(const char *string, const uint32_t value);

/** @brief High Level function to send messages {ok | error, list()} to the host
 *
 * @param string String to compose the result of the operation
 * @param array array to compose the list
 * @param size size of the list
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int send_answer_string_list_ulong(const char *string, const uint32_t *array, 
                                  const uint32_t size);

/** @brief High Level function to send messages {ok | error, binary} to the host
 *
 * @param string String to compose the result of the operation
 * @param array array to compose the binary
 * @param size size of the binary
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int send_answer_string_binary(const char *string, const uint8_t *array, 
                              const uint32_t size);

/** @brief High Level function to send messages {ok | error, binary} to the host
 *
 * @param string String to compose the atom operation
 * @param instance Origin of the event
 * @param array array to compose the binary
 * @param size size of the binary
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int send_answer_string_postion_event(const char *string, const uint32_t instance,
                                     const uint8_t *array, const uint32_t size);

#endif /* ENDAT_C_SRC_ERL_COMM_H */