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

/** @brief High Level function to send messages to the host
 *
 * @param string String to compose the result of the operation
 * @param value Value to be part of the tuple
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int send_answer_string_ulong(const char *string, const uint32_t value);

#endif /* ENDAT_C_SRC_ERL_COMM_H */