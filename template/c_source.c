#include "c_header.h"

/******************************************************************************/
/*  Function Name   : OpemngStmAlm                                            */
/*  Description     : Opemng State Machine When Receive Alm                   */
/*  Input(s)        : None                                                    */
/*  Output(s)       : peError                                                 */
/*  Global Variables Referred : gOpemngContext                                */
/*  Global variables Modified : gOpemngContext                                */
/*  Exceptions or Operating System Error Handling : None                      */
/*  Use of Recursion : None                                                   */
/*  Returns         : OPEMNG_OK, OPEMNG_ER_OTHER                              */
/******************************************************************************/
INT1 OpemngStmAlm(tError *peError)
{
    INT1 i1Result;
    UINT4 u4Status;
    tMsgAlmmngStatus *pMsgAlmmngStatus;

    /* Trace Pass */
    TRACE_PASS_A(__FUNCTION__);

    /* Check Argument */
    if (NULL == peError)
    {
        LOG_MSG(EL_MOD_OPEMNG, EL_SEVERITY_MINOR, EL_TYPE_OTHER,
                "%s:%d:Argument Error Arg1=0x%X\n",
                __FUNCTION__, __LINE__, (int)peError);
        return OPEMNG_ER_OTHER;
    }

    /* Initialize */
    i1Result = OPEMNG_OK;
    *peError = OK;
    pMsgAlmmngStatus = (tMsgAlmmngStatus *)gOpemngContext.pu1RcvMsgBuf;

    /* Check State  */
    if (OpemngStatus(&u4Status) != OPEMNG_OK)
    {
        *peError = OPERATION_FAILED;
        return OPEMNG_ER_OTHER;
    }
    switch(u4Status)
    {
    case OPEMNG_STAT_INIT:
    case OPEMNG_STAT_GPS_SET:
    case OPEMNG_STAT_GPS_START:
    case OPEMNG_STAT_GW_START:
    case OPEMNG_STAT_BB_START:
    case OPEMNG_STAT_RF_SET:
    case OPEMNG_STAT_INIT_END:
    case OPEMNG_STAT_SET_END:
    case OPEMNG_STAT_SERV:
    case OPEMNG_STAT_TEST:
    case OPEMNG_STAT_RF_ACT:
    case OPEMNG_STAT_STOP:
        if (OPEMNG_ALM_RESTORE == pMsgAlmmngStatus->u4Status)
        {
            LOG_MSG(EL_MOD_OPEMNG, EL_SEVERITY_INFO, EL_TYPE_OTHER,
                    "%s:%d:Do not Care of Alarm Restore ID=%d\n",
                    __FUNCTION__,
                    __LINE__, (unsigned int)pMsgAlmmngStatus->u4Index);
        }
        else /* Alarm Occur */
        {
            /* Stop Entry Function */
            OpemngStmEntryStop(peError);

            /* Next State */
            OpemngSetStatus(OPEMNG_STAT_STOP);
        }
        break;
    default:
        LOG_MSG(EL_MOD_OPEMNG, EL_SEVERITY_MINOR, EL_TYPE_OTHER,
                "%s:%d:Unknown State 0x%08X\n",
                __FUNCTION__, __LINE__, (unsigned int)u4Status);
        i1Result = OPEMNG_ER_OTHER;
        *peError = OPERATION_FAILED;
        break;
    }

    return i1Result;
}
