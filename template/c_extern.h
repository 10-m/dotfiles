#ifndef _C_EXTERN_H_
#define _C_EXTERN_H_

extern tOpemngContext gOpemngContext;

#ifdef OPEMNG_UT
extern
#else
extern const
#endif /* OPEMNG_UT */
tOpemngMsgInfoTbl gaOpemngMsgInfoTbl[OPEMNG_MSG_INFO_TBL_LEN];

#endif /* _C_EXTERN_H_ */
