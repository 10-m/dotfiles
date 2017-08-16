#ifndef _C_PROTO_H_
#define _C_PROTO_H_

/* opemnginit.c */
INT1 OpemngInit(VOID);
INT1 OpemngInitContext(VOID);
INT1 OpemngInitTmr(VOID);
VOID OpemngTmrHdlGpsSet(tTimerListId TimerId);
VOID OpemngTmrHdlGpsStart(tTimerListId TimerId);
VOID OpemngTmrHdlGwStart(tTimerListId TimerId);
VOID OpemngTmrHdlBbStart(tTimerListId TimerId);
VOID OpemngTmrHdlRfSet(tTimerListId TimerId);
VOID OpemngTmrHdlRfAct(tTimerListId TimerId);
INT1 OpemngStartModule(VOID);
INT1 OpemngStartRf(VOID);
INT1 OpemngStartGw(VOID);
INT1 OpemngStartBb(VOID);
INT1 OpemngStartGps(VOID);
INT1 OpemngStartHw(VOID);
INT1 OpemngStartAlm(VOID);
INT1 OpemngStartCfg(VOID);

/* opemngmsg.c */
INT1 OpemngMsgSendIntf(UINT1 *pu1MsgData, UINT4 u4MsgLen);

/* opemngsftp.c */
INT1 OpemngSftpStm(UINT1 u1MsgInfoTblIndex, tError *peErrorCode);

/* opemngstat.c */
/*INT1 OpemngStatus(UINT4 *pu4Status);*/
INT1 OpemngSetStatus(UINT4 u4Status);

/* opemngstm.c */
INT1 OpemngStm(UINT1 u1MsgInfTblIndex, tError *peErrorCode);
INT1 OpemngStmAllReset(tError *peError);
INT1 OpemngStmGwReset(tError *peError);
INT1 OpemngStmRfTxPwrReq(tError *peError);
INT1 OpemngStmRfFreqReq(tError *peError);
INT1 OpemngStmRfEnableReq(tError *peError);
INT1 OpemngStmHpaReset(tError *peError);
INT1 OpemngStmRfReset(tError *peError);
INT1 OpemngStmGpsReset(tError *peError);
INT1 OpemngStmBbReset(tError *peError);
INT1 OpemngStmTmrGpsSet(tError *peError);
INT1 OpemngStmTmrGpsStart(tError *peError);
INT1 OpemngStmTmrGwStart(tError *peError);
INT1 OpemngStmTmrBbStart(tError *peError);
INT1 OpemngStmTmrRfSet(tError *peError);
INT1 OpemngStmTmrRfAct(tError *peError);
INT1 OpemngStmRfMode(tError *peError);
INT1 OpemngStmRfService(tError *peError);
INT1 OpemngStmSetCfgCompReq(tError *peError);
INT1 OpemngStmSetReCfg(tError *peError);
INT1 OpemngStmEntryStop(tError *peError);

/* opemngtask.c */
VOID OpemngTask(INT1 *pi1NoUse);
INT1 OpemngMsgRecvProc(VOID);
INT1 OpemngTaskStartRsp(UINT1 u1MsgInfoTblIndex, tError *peErrorCode);

/* opemngutl.c */
INT1 OpemngCtlRf(tMsgOpsRfControl *pMsgOpsRfControl, tError *peError);
INT1 OpemngCtlRfStopWave(tError *peError);
INT1 OpemngCtlRfStartWave(tError *peError);
INT1 OpemngSetRfMode(tMsgOpsRfMode *pMsgOpsRfMode, tError *peError);
INT1 OpemngSetGpsCfg(tError *peError);
INT1 OpemngSetRfCfg(tError *peError);
INT1 OpemngSetRfTxPwr(tMsgOpsRfTxpwr *pMsgOpsRfTxpwr, tError *peError);
INT1 OpemngSetRfFreq(tMsgOpsRfFreq *pMsgOpsRfFreq, tError *peError);
INT1 OpemngSetRfEnable(tMsgOpsRfEnable *pMsgOpsRfEnable, tError *peError);
INT1 OpemngResetRf(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngResetBb(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngResetGw(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngResetGps(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngSetGpsAntDelay(tMsgOpsGpsAntDelay *pMsgOpsGpsAntDelay,
                          tError *peError);
INT1 OpemngSetGpsPps(tMsgOpsGpsPps *pMsgOpsGpsPps, tError *peError);
INT1 OpemngResetAll(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngResetHpa(tMsgOpsReset *pMsgOpsReset, tError *peError);
INT1 OpemngPwrOnHpa(tError *peError);
INT1 OpemngPwrOffHpa(tError *peError);
INT1 OpemngGetRfCfgCompFlag(UINT1 *pu1RfCfgFlag);
INT1 OpemngSetRfCfgCompFlag(UINT1 u1RfCfgCompFlag);
INT1 OpemngSetAutoSrvFlag(UINT1 u1AutoSrvFlag);
INT1 OpemngGetAutoSrvFlag(UINT1 *pu1AutoSrvFlag);
INT1 OpemngWaitPipeRsp(tMsgId eMsgId, tError *peError);
INT1 OpemngTmrStart(tTimerListId *pTmrLstId, UINT4 u4Duration);
INT1 OpemngTmrStop(VOID);
BOOLEAN OpemngIsRfAct(UINT4 u4RfStatus);

/* opemngalm.c */
INT1 OpemngStmAlm(tError *peError);
INT1 OpemngSndAlm(UINT4 u4AlmIndex, UINT4 u4AlmStatus);
INT1 OpemngSndAlmOccur(UINT4 u4AlmIndex);
INT1 OpemngSndAlmRestore(VOID);

/* API Functions */
#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

INT1 OpemngStatus(UINT4 *pu4Status);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif

#endif /* _C_PROTO_H_ */
