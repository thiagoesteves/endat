/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    endat_driver.h
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read and write messages from
 *          and to the endat gen_server
 */

#ifndef ENDAT_C_SRC_ENDAT_DRIVER_H
#define ENDAT_C_SRC_ENDAT_DRIVER_H

/**
 * @brief Error defines
 */

#define ENDAT_OK    (0)
#define ENDAT_ERROR (-1)

/** @brief This function opens the driver that will handle the endat transactions
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int open_endat_driver(char *buf, int *index);

/** @brief This function closes the driver that will handle the endat transactions
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int close_endat_driver(char *buf, int *index);

/** @brief Read position from Encoder, this function expects 3 arguments from ENDAT 
 *         driver in Erlang:
 *         @param Instance ENDAT instance
 *         @param EndatVersion Endat version 2.1 or 2.2
 *         @param PositionBits Number of bits to read the position
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int read_position(char *buf, int *index);

/** @brief Write at ENDAT register, this function expects 3 arguments from ENDAT 
 *         driver in Erlang:
 *         @param Instance ENDAT instance
 *         @param command Completed command to write
 *         @param timeout Maximum timeout fro the encoder answer
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int write_command(char *buf, int *index);

/** @brief Start to read the position continuosly, expects 3 arguments from ENDAT 
 *         driver in Erlang:
 *         @param Instance ENDAT instance
 *         @param endat_ver Endat version
 *         @param position_bits Encoder position bits
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int start_read_position(char *buf, int *index);

/** @brief Stop to read the position continuosly, expects 1 argument from ENDAT 
 *         driver in Erlang:
 *         @param Instance ENDAT instance
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int stop_read_position(char *buf, int *index);

/** @brief Calculate the Norm CRC using Heidenhain algorithm.
 *.        this function expects 3 arguments from Endat gen_server
 *         driver in Erlang:
 *         @param param8  8-bits parameter
 *         @param param16 16-bits parameter
 *
 * @return crc the calculated crc
 */
int make_crc_norm(char *buf, int *index);

/** @brief Calculate the Norm CRC using lookup table (more efficient)
 *.        this function expects 3 arguments from Endat gen_server
 *         driver in Erlang:
 *         @param param8  8-bits parameter
 *         @param param16 16-bits parameter
 *
 * @return crc the calculated crc
 */
int make_crc_norm_lt(char *buf, int *index);

/** @brief Calculate the Position CRC using Heidenhain algorithm.
 *.        this function expects 6 arguments from Endat gen_server
 *         driver in Erlang:
 *         @param clocks  Encoder position bits
 *         @param endat   0 for Endat 2.1 | 1 for Endat 2.2
 *         @param error1  err1 bit
 *         @param error2  err2 bit
 *         @param highpos MSB position (assuming 64-bit word)
 *         @param lowpos  LSB position (assuming 64-bit word)
 *
 * @return crc the calculated crc
 */
int make_crc_pos(char *buf, int *index);

/** @brief Calculate the Position CRC using lookup table (more efficient)
 *.        this function expects 6 arguments from Endat gen_server
 *         driver in Erlang:
 *         @param clocks  Encoder position bits
 *         @param endat   0 for Endat 2.1 | 1 for Endat 2.2
 *         @param error1  err1 bit
 *         @param error2  err2 bit
 *         @param highpos MSB position (assuming 64-bit word)
 *         @param lowpos  LSB position (assuming 64-bit word)
 *
 * @return crc the calculated crc
 */
int make_crc_pos_lt(char *buf, int *index);

#endif /* ENDAT_C_SRC_ENDAT_DRIVER_H */