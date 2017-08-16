#ifndef _C_STRUCT_H_
#define _C_STRUCT_H_

/* Opemng Timeout Message */
typedef struct
{
    tTimerListId TimerId;
} tMsgOpemngTimeout;

/* Opemng Message Contents */
typedef union
{
    /* From Cfgmng */
    tMsgOpsDownload     MsgOpsDownload;     /* OPM_MSG_DL */
    tMsgOpsUpload       MsgOpsUpload;       /* OPM_MSG_UL */
    tMsgOpsReset        MsgOpsReset;        /* OPM_MSG_REST */
    tMsgOpsRfTxpwr      MsgOpsRfTxpwr;      /* OPM_MSG_RF_TX_PWR */
    tMsgOpsRfMode       MsgOpsRfMode;       /* OPM_MSG_RF_MODE */
    tMsgOpsRfFreq       MsgOpsRfFreq;       /* OPM_MSG_RF_FREQ */
    tMsgOpsRfEnable     MsgOpsRfEnable;     /* OPM_MSG_RF_ENABLE */
    tMsgOpsRfService    MsgOpsRfService;    /* OPM_MSG_RF_SERV */
    tMsgOpsSetCfgcomp   MsgOpsSetCfgcomp;   /* OPM_MSG_CFG_COMP */
    tMsgOpsSetRecfg     MsgOpsSetRecfg;     /* OPM_MSG_SET_CFG */

    /* From Opemng */
    tMsgOpemngTimeout   MsgOpemngTimeout;

    /* From Alarm */
    /* Use tMsgAlmmngStatus */

} tunMsgOpemngReq;

/* Opemng Receive Message */
typedef struct
{
    tMsgHdr         MsgHdr;
    tunMsgOpemngReq MsgOpemngReq;
} tMsgOpemngReq;

/* Opemng Receive Pipe  */
/* tPipeHdr */

/* Opemng Context */
typedef struct
{
    UINT4       u4StmState;     /* State of State Machine */
    UINT4       u4SftpStmState; /* State of SFTP State Machine */
    UINT4       u4ChkTimerCnt;  /* Timeout Count */
    tMemPoolId  MsgMemPoolId;   /* Memory Pool for Message */
    tOsixQId    QId;            /* Message Queue */
    tOsixTaskId TaskId;         /* Task ID */
    tOsixSemId  SemId;          /* Semapore for Status */
    INT4        ai4PipeId[2];   /* Pipe ID */
    UINT1       *pu1RcvMsgBuf;
    UINT1       au1LogBuf[OPEMNG_LOG_BUF_LEN]; /* Log Buffer */

    tTmrAppTimer TmrAppTimer;
    tTimerListId GpsSetTimerId;     /* GPS Set Timer ID */
    tTimerListId GpsStartTimerId;   /* GPS Start Timer ID */
    tTimerListId GwStartTimerId;    /* GW Start Timer ID */
    tTimerListId BbStartTimerId;    /* BB Start Timer ID */
    tTimerListId RfSetTimerId;      /* RF Set Timer ID */
    tTimerListId RfActTimerId;      /* RF Act Timer ID */
    tTimerListId *pCurrentTimerId;  /* Current Timer ID */

    tTmrAppTimer SftpTmrAppTimer;
    tTimerListId SftpTimerId;       /* SFTP Timer ID */

    UINT4       u4CurrentAlmIndex;  /* Current Alarm in Opemng */
} tOpemngContext;

/* Message Pre-Process */
typedef INT1 (*tOpemngPreProcFunc)(UINT1, tError *);

/* Message Process */
typedef INT1 (*tOpemngMsgFunc)(tError *);

/* Message Information Table */
typedef struct
{
    tMsgId      eMsgId;                     /* Message ID */
    tCircuit    eCircuit;                   /* Circuit ID */
    tOpemngPreProcFunc OpemngPreProcFunc;   /* Pre-Proc Function */
    tOpemngMsgFunc OpemngMsgFunc;           /* Function */
    UINT1       u1PipeRspInd;               /* Pipe Response */
                                            /*  (Need(TRUE) or Not(FASLE)) */
    tMsgId      eMsgIdPipeOk;               /* Pipe ID (OK) */
    tMsgId      eMsgIdPipeNg;               /* Pipe ID (NG) */
} tOpemngMsgInfoTbl;

#endif /* _C_STRUCT_H_ */
