/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    endat_driver.h
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to calculate the CRC5 of Endat 2.1 and
 *          Endat 2.2
 */

#include <stdint.h>

#ifndef ENDAT_C_SRC_ENDAT_CRC_H
#define ENDAT_C_SRC_ENDAT_CRC_H

/** @brief Calculate the Norm CRC using Heidenhain algorithm.
 *
 *  @param param8  8-bits parameter
 *  @param param16 16-bits parameter
 *
 * @return crc the calculated crc
 */
uint32_t MakeCrcNorm(uint32_t param8, uint32_t param16);

/** @brief Calculate the Norm CRC using lookup table (more efficient)
 *
 *  @param param8  8-bits parameter
 *  @param param16 16-bits parameter
 *
 * @return crc the calculated crc
 */
uint32_t LookupTableMakeCrcNorm(uint32_t param8, uint32_t param16);

/** @brief Calculate the Position CRC using Heidenhain algorithm.
 *
 *  @param clocks  Encoder position bits
 *  @param endat   0 for Endat 2.1 | 1 for Endat 2.2
 *  @param error1  err1 bit
 *  @param error2  err2 bit
 *  @param highpos MSB position (assuming 64-bit word)
 *  @param lowpos  LSB position (assuming 64-bit word)
 *
 * @return crc the calculated crc
 */
uint32_t MakeCrcPos(uint32_t clocks, uint32_t endat,   uint32_t error1,
                    uint32_t error2, uint32_t highpos, uint32_t lowpos);

/** @brief Calculate the Position CRC using lookup table (more efficient)
 *
 *  @param clocks  Encoder position bits
 *  @param endat   0 for Endat 2.1 | 1 for Endat 2.2
 *  @param error1  err1 bit
 *  @param error2  err2 bit
 *  @param highpos MSB position (assuming 64-bit word)
 *  @param lowpos  LSB position (assuming 64-bit word)
 *
 * @return crc the calculated crc
 */
uint32_t LookupTableMakeCrcPos(uint32_t clocks, uint32_t endat,   uint32_t error1,
                               uint32_t error2, uint32_t highpos, uint32_t lowpos);

/** @brief Reverse 32-bits
 *
 *  @param n word to be reversed
 *
 * @return uint32_t reversed word
 */
uint32_t reverse32Bits(uint32_t n);

#endif /* ENDAT_C_SRC_ENDAT_CRC_H */