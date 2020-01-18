
/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    endat_nif.c
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains the nif functions to calculate the endat crc
 */

#include "erl_nif.h"
#include "endat_crc.h"

/****************************************************************************
 *   NIFS - Implementation      functions called by erlang endat_crc.erl    *
 ****************************************************************************/

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM ret;
  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
  {
    return enif_make_atom(env, atom);
  }
  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, make_atom(env, "error"), make_atom(env, mesg));
}

ERL_NIF_TERM
answer_ok_crc(ErlNifEnv* env, uint32_t crc)
{
    return enif_make_tuple2(env, make_atom(env, "ok"), enif_make_uint(env, crc));
}

static ERL_NIF_TERM
MakeCrcNorm_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  uint32_t param8, param16, crc;

  if ( (!enif_get_uint(env, argv[0], &param8)) ||
       (!enif_get_uint(env, argv[1], &param16)) )
  {
    return mk_error(env, "invalid_parameter");
  }

  crc = MakeCrcNorm(param8, param16);

  return answer_ok_crc(env, crc);
}

static ERL_NIF_TERM
MakeCrcNormLt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  uint32_t param8, param16, crc;

  if ( (!enif_get_uint(env, argv[0], &param8)) ||
       (!enif_get_uint(env, argv[1], &param16)) )
  {
    return mk_error(env, "invalid_parameter");
  }

  crc = LookupTableMakeCrcNorm(param8, param16);

  return answer_ok_crc(env, crc);
}

static ERL_NIF_TERM
MakeCrcPos_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  uint32_t clocks, endat, error1, error2, highpos, lowpos, crc;

  if ( (!enif_get_uint(env, argv[0], &clocks))  ||
       (!enif_get_uint(env, argv[1], &endat))   ||
       (!enif_get_uint(env, argv[2], &error1))  ||
       (!enif_get_uint(env, argv[3], &error2))  ||
       (!enif_get_uint(env, argv[4], &highpos)) ||
       (!enif_get_uint(env, argv[5], &lowpos)) )
  {
    return mk_error(env, "invalid_parameter");
  }

  crc = MakeCrcPos(clocks, endat, error1, error2, highpos, lowpos);

  return answer_ok_crc(env, crc);
}

static ERL_NIF_TERM
MakeCrcPosLt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  uint32_t clocks, endat, error1, error2, highpos, lowpos, crc;

  if ( (!enif_get_uint(env, argv[0], &clocks))  ||
       (!enif_get_uint(env, argv[1], &endat))   ||
       (!enif_get_uint(env, argv[2], &error1))  ||
       (!enif_get_uint(env, argv[3], &error2))  ||
       (!enif_get_uint(env, argv[4], &highpos)) ||
       (!enif_get_uint(env, argv[5], &lowpos)) )
  {
    return mk_error(env, "invalid_parameter");
  }

  crc = LookupTableMakeCrcPos(clocks, endat, error1, error2, highpos, lowpos);

  return answer_ok_crc(env, crc);
}

static ErlNifFunc nif_funcs[] = 
{
  {"makeCrcNormLt" , 2, MakeCrcNormLt_nif},
  {"makeCrcPosLt"  , 6, MakeCrcPosLt_nif},
  {"makeCrcNorm"   , 2, MakeCrcNorm_nif},
  {"makeCrcPos"    , 6, MakeCrcPos_nif}
};

ERL_NIF_INIT(endat_crc, nif_funcs, NULL, NULL, NULL, NULL);