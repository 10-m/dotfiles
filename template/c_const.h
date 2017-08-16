#ifndef _C_CONST_CONST_H_
#define _C_CONST_CONST_H_

/* Opemng Status */
#define OPEMNG_STAT_INIT        (20)
#define OPEMNG_STAT_GPS_SET     (21)
#define OPEMNG_STAT_GPS_START   (22)
#define OPEMNG_STAT_GW_START    (23)
#define OPEMNG_STAT_BB_START    (24)
#define OPEMNG_STAT_RF_SET      (25)
#define OPEMNG_STAT_INIT_END    (3)
#define OPEMNG_STAT_SET_END     (0)
#define OPEMNG_STAT_SERV        (1)
#define OPEMNG_STAT_TEST        (2)
#define OPEMNG_STAT_RF_ACT      (10)
#define OPEMNG_STAT_STOP        (30)

/* Opemng SFTP Status */
#define OPEMNG_SFTP_STAT_IDLE   (0)
#define OPEMNG_SFTP_STAT_BUSY   (1)

/* Opemng Alarm */
#define OPEMNG_ALM_GPS_CFG_TIMEOUT      (28)
#define OPEMNG_ALM_GPS_START_TIMEOUT    (29)
#define OPEMNG_ALM_GW_START_TIMEOUT     (30)
#define OPEMNG_ALM_BB_START_TIMEOUT     (31)
#define OPEMNG_ALM_RF_CFG_TIMEOUT       (32)
#define OPEMNG_ALM_RF_OPE_TIMEOUT       (33)
#define OPEMNG_ALM_STATE_SERV_ERR       (34)

/* Opemng Return Value */
#define OPEMNG_OK           (0)
#define OPEMNG_ER_ARG       (-1)
#define OPEMNG_ER_THR       (-2)
#define OPEMNG_ER_IPC       (-3)
#define OPEMNG_ER_OTHER     (-4)

/* Opemng Period Timer Value (sec) */
#define OPEMNG_PERIOD_DEFAULT       (100)
#define OPEMNG_PERIOD_GPS_SET       OPEMNG_PERIOD_DEFAULT
#define OPEMNG_PERIOD_GPS_START     OPEMNG_PERIOD_DEFAULT
#define OPEMNG_PERIOD_GW_START      OPEMNG_PERIOD_DEFAULT
#define OPEMNG_PERIOD_BB_START      OPEMNG_PERIOD_DEFAULT
#define OPEMNG_PERIOD_RF_SET        OPEMNG_PERIOD_DEFAULT
#define OPEMNG_PERIOD_RF_ACT        OPEMNG_PERIOD_DEFAULT

/* Opemng Timeout Value (sec) */
#define OPEMNG_TIMEOUT_DEFAULT      (3 * 60 * 100)
#define OPEMNG_TIMEOUT_GPS_SET      OPEMNG_TIMEOUT_DEFAULT
#define OPEMNG_TIMEOUT_GPS_START    (45 * 60 * 100)
#define OPEMNG_TIMEOUT_GW_START     OPEMNG_TIMEOUT_DEFAULT
#define OPEMNG_TIMEOUT_BB_START     OPEMNG_TIMEOUT_DEFAULT
#define OPEMNG_TIMEOUT_RF_SET       OPEMNG_TIMEOUT_DEFAULT
#define OPEMNG_TIMEOUT_RF_ACT       (1 * 60 * 100)

/* Opemng RF Status */
#define OPEMNG_RF_STAT_INIT         (1)
#define OPEMNG_RF_STAT_CRITICAL     (99)
#define OPEMNG_RF_STAT_ACT_HEAD     (2)
#define OPEMNG_RF_STAT_ACT_TAIL     (9)

/* Opemng GW Status */
#define OPEMNG_GW_STAT_ACT          (1)

/* Opemng Service Mode */
#define OPEMNG_RF_MODE_CFG_COMP     (0)
#define OPEMNG_RF_MODE_SERV         (1)
#define OPEMNG_RF_MODE_TEST         (2)

/* Opemng RF Enable */
#define OPEMNG_RF_ENABLE            (1)
#define OPEMNG_RF_DISABLE           (0)

/* Opemng RF */
#define OPEMNG_RF_ALL               (0)
#define OPEMNG_RF1                  (1)
#define OPEMNG_RF2                  (2)

/* Opemng Log Buffer */
#define OPEMNG_LOG_BUF_LEN  (64)

/* Opemng Alarm */
#define OPEMNG_ALM_RESTORE          (0)
#define OPEMNG_ALM_OCCUR            (1)
#define OPEMNG_ALM_ID_NONE          (0)

/* The Lenght of Message Table Information  */
#define OPEMNG_MSG_INFO_TBL_LEN (21)

#endif /* _C_CONST_CONST_H_ */
