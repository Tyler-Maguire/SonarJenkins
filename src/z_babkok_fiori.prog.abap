*&---------------------------------------------------------------------*
*& Report  Z_BABKOK_FIORI
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT z_babkok_fiori.

************************************************************************
* DATA
************************************************************************
*      lt_0002       TYPE TABLE OF pa0002,     "Table for PA0002
*      wa_0002       TYPE pa0002,              "Work Area for PA0002
*      lt_0014       TYPE TABLE OF pa0014,     "Table for PA0014
*      wa_0014       TYPE pa0014,              "Work Area for PA0014


DATA: lv_infty_json TYPE string,  "local variable for infotype record.json
      ls_entity     TYPE zreq_to_recruit,
      lt_entity     TYPE TABLE OF zreq_to_recruit,
      lt_0001       TYPE TABLE OF pa0001,    "Table for PA0001
      wa_0001       TYPE pa0001,              "Work Area for PA0001
      lt_hrp1000    TYPE TABLE OF hrp1000, "Table for HRP1000
      wa_hrp1000    TYPE hrp1000,          "Work Area for HRP1000
      lt_positions  TYPE TABLE OF hrp1000, "Table for all positions
      wa_positions  TYPE hrp1000,        "Work Area for positions
      lt_hrp1001    TYPE TABLE OF hrp1001, "Table for HRP1001
      wa_hrp1001    TYPE hrp1001,          "Work Area for HRP1001
      lt_csks       TYPE TABLE OF csks,       "Table for CSKS
      wa_csks       TYPE csks,                "Work Area for CSKS
      lt_cskt       TYPE TABLE OF cskt,       "Table for CSKT
      wa_cskt       TYPE cskt,                "Work Area for CSKT
      lt_t505s      TYPE TABLE OF t505s,     "Table for T505S
      wa_t505s      TYPE t505s,              "Work Area for T505S
      lt_t510       TYPE TABLE OF t510,        "Table for T510
      wa_t510       TYPE t510,                "Work Area for T510
      lv_pernr      TYPE persno,
      lv_uname      TYPE p0105-usrid,
      lt_orgs       TYPE hrp1000,
      lt_hrp1005    TYPE TABLE OF hrp1005,
      wa_hrp1005    TYPE hrp1005,
      it_swhactor   TYPE TABLE OF swhactor,
      it_objec      TYPE TABLE OF objec,
      it_struc      TYPE TABLE OF struc,
      lv_plvar      TYPE objec-plvar,
      lv_mogla      TYPE molga,
      lv_isok       TYPE boole_d,
      r_objid       TYPE RANGE OF hrp1000-objid,  "range table
      wa_objid      LIKE LINE OF r_objid,     "work area for range table
mh_message    type ref to if_hrpa_message_handler.


************************************************************************
* Field Symbols
************************************************************************


FIELD-SYMBOLS: <fs_dataset>     TYPE pa0001,
               <lv_stext>       TYPE stext,
               <lt_records>     TYPE ANY TABLE,
               <ls_record>      TYPE any,
               <fs_hrp1000_pos> TYPE hrp1000,
               <fs_hrp1000_job> TYPE hrp1000,
               <fs_hrp1000_org> TYPE hrp1000,
               <fs_t510>        TYPE t510,
               <fs_cskt>        TYPE cskt,
               <fs_hrp1005>     TYPE hrp1005,
               <fs_swhactor>    TYPE swhactor,
               <fs_hrp1001>     TYPE hrp1001.


************************************************************************
* Get Active Plan version
************************************************************************

CALL FUNCTION 'RH_GET_PLVAR'
  IMPORTING
    plvar    = lv_plvar  " Plan Version
  EXCEPTIONS
    no_plvar = 1.

IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
             WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

************************************************************************
* Get PERNR of User
************************************************************************

lv_uname = sy-uname.

IF  lv_uname IS NOT INITIAL.
CALL FUNCTION 'RP_GET_PERNR_FROM_USERID'
  EXPORTING
    begda     = sy-datum
    endda     = sy-datum
    usrid     = lv_uname
    usrty     = '0001'
  IMPORTING
    usr_pernr = lv_pernr
  EXCEPTIONS
    retcd     = 1
    OTHERS    = 2.
ENDIF.


************************************************************************
* Get All Positions Under current User
************************************************************************

IF lv_pernr is NOT INITIAL.

  CALL FUNCTION 'HR_ECM_GET_EE_MOLGA'
  EXPORTING
    pernr           = lv_pernr    " Personnel Number
    message_handler = mh_message  " HR Master Data: Messages
  IMPORTING
    molga           = lv_mogla  " Country Grouping
    is_ok           = lv_isok.  " Data element for domain BOOLE: TRUE (='X') and FALSE (=' ')

ENDIF.


************************************************************************
* Get All Vacant Positions Under current User
************************************************************************
DATA: lv_errormessage LIKE BAPIRETURN,
      lt_vacant TYPE TABLE OF HRPVAC.

FIELD-SYMBOLS: <fs_vacant> TYPE HRPVAC.

CALL FUNCTION 'HR_GET_VACANCY_LIST'
  EXPORTING
    begindate      =   sy-datum  " Selection Range: Start
    enddate        =   sy-datum  " Selection Range: End
  IMPORTING
    error          =   lv_errormessage  " Error Message
  TABLES
    vacantposition =     lt_vacant" Vacant Positions
  .

************************************************************************
* INTERNAL TABLE LOAD
************************************************************************

IF lv_plvar is NOT INITIAL.

SELECT * FROM hrp1000 INTO TABLE lt_hrp1000 WHERE langu = sy-langu AND begda <= sy-datum AND endda >= sy-datum AND plvar = lv_plvar.
SELECT * FROM hrp1001 INTO TABLE lt_hrp1001 WHERE begda <= sy-datum AND endda >= sy-datum AND plvar = lv_plvar.
SELECT * FROM cskt INTO TABLE lt_cskt WHERE spras = sy-langu.
SELECT SINGLE * FROM pa0001 INTO  wa_0001 WHERE pernr = lv_pernr AND begda <= sy-datum AND endda >= sy-datum.
SELECT * FROM hrp1005 INTO TABLE lt_hrp1005 WHERE molga = lv_mogla AND plvar = lv_plvar AND otype = 'S' AND begda <= sy-datum AND endda >= sy-datum.
SELECT * FROM t510 INTO TABLE lt_t510 WHERE molga = lv_mogla AND begda <= sy-datum AND endda >= sy-datum.
SELECT * FROM PA0001 INTO TABLE lt_0001 WHERE begda <= sy-datum AND endda >= sy-datum.

ENDIF.

************************************************************************
* Get All Positions Under current User
************************************************************************

CALL FUNCTION 'RH_STRUC_GET'
  EXPORTING
    act_otype      = 'O'    " Object Type
    act_objid      = wa_0001-orgeh
    act_wegid      = 'O-O-S'   " Evaluation Path
  TABLES
    result_tab     = it_swhactor   " Table Contains Objects
    result_objec   = it_objec  " Table Contains Object Information
    result_struc   = it_struc  " Table Contrains Structure Information
  EXCEPTIONS
    no_plvar_found = 1
    no_entry_found = 2
    OTHERS         = 3.

************************************************************************
* Create Position Range
************************************************************************

LOOP AT it_swhactor ASSIGNING <fs_swhactor>.
  IF <fs_swhactor> IS ASSIGNED.
  wa_objid-sign = 'I'.
  wa_objid-option = 'EQ'.
  wa_objid-low =  <fs_swhactor>-objid.
  APPEND wa_objid TO r_objid.
  ENDIF.
ENDLOOP.

************************************************************************
* Loop Through position Range HRP1000
************************************************************************

LOOP AT lt_hrp1000 ASSIGNING <fs_hrp1000_pos> WHERE otype = 'S' AND objid IN r_objid.
IF <fs_hrp1000_pos> IS ASSIGNED.

  ls_entity-plans = <fs_hrp1000_pos>-objid.
  ls_entity-ptext = <fs_hrp1000_pos>-stext.
  ls_entity-begda = <fs_hrp1000_pos>-begda.
  ls_entity-endda = <fs_hrp1000_pos>-endda.
  ls_entity-mpernr = lv_pernr.
  ls_entity-btrtl = wa_0001-btrtl.
  ls_entity-gsber = wa_0001-gsber.
  ls_entity-werks = wa_0001-werks.

************************************************************************
* Get Business Area Text
************************************************************************

IF ls_entity-gsber IS NOT INITIAL.
SELECT SINGLE GTEXT FROM TGSBT INTO ls_entity-btext WHERE SPRAS = sy-langu AND GSBER = ls_entity-gsber.
ENDIF.

************************************************************************
* Get Personnel Area Text
************************************************************************

IF ls_entity-werks IS NOT INITIAL AND lv_mogla IS NOT INITIAL.
SELECT SINGLE NAME1 FROM T500P INTO ls_entity-patext WHERE persa = ls_entity-werks AND molga = lv_mogla.
ENDIF.

************************************************************************
* Get Personnel Sub-Area Text
************************************************************************

IF  ls_entity-btrtl IS NOT INITIAL AND ls_entity-werks IS NOT INITIAL AND lv_mogla IS NOT INITIAL.
SELECT SINGLE BTEXT FROM T001P INTO ls_entity-psatext WHERE molga = lv_mogla AND btrtl = ls_entity-btrtl AND werks = ls_entity-werks.
ENDIF.

************************************************************************
* Loop Through position to get addtional info HRP1001
************************************************************************

  LOOP AT lt_hrp1001 ASSIGNING <fs_hrp1001> WHERE otype = 'S' AND objid IN r_objid.

************************************************************************
* Get Job Information
************************************************************************

IF <fs_hrp1001> IS ASSIGNED.

    IF <fs_hrp1001>-otype = 'S' AND <fs_hrp1001>-objid = ls_entity-plans AND <fs_hrp1001>-sclas = 'C'.
      READ TABLE lt_hrp1000 ASSIGNING <fs_hrp1000_job> WITH KEY objid = <fs_hrp1001>-sobid otype = 'C'.
      IF sy-subrc = 0 AND <fs_hrp1000_job> IS ASSIGNED.
        ls_entity-jtext = <fs_hrp1000_job>-stext.
        ls_entity-stell = <fs_hrp1000_job>-objid.
      ENDIF.
    ENDIF.

************************************************************************
*Get Cost Centre and cost centre text
************************************************************************

    IF <fs_hrp1001>-otype = 'S' AND <fs_hrp1001>-sclas = 'K' AND <fs_hrp1001>-objid = ls_entity-plans AND <fs_hrp1001>-relat = '011' AND <fs_hrp1001>-rsign = 'A'.
      READ TABLE lt_cskt ASSIGNING <fs_cskt> WITH KEY kostl = <fs_hrp1001>-sobid.
      IF sy-subrc = 0 AND <fs_cskt> IS ASSIGNED.
        ls_entity-kostl = <fs_hrp1001>-sobid.
        ls_entity-ktext = <fs_cskt>-ktext.
      ENDIF.
    ENDIF.

************************************************************************
*Get ORG and ORG text
************************************************************************

    IF <fs_hrp1001>-otype = 'S' AND <fs_hrp1001>-sclas = 'O' AND <fs_hrp1001>-objid = ls_entity-plans AND <fs_hrp1001>-relat = '003' AND <fs_hrp1001>-rsign = 'A'.
      READ TABLE lt_hrp1000 ASSIGNING <fs_hrp1000_org> WITH KEY objid = <fs_hrp1001>-sobid otype = 'O'.
      IF sy-subrc = 0 AND <fs_hrp1000_org> IS ASSIGNED.
        ls_entity-orgeh = <fs_hrp1000_org>-objid.
        ls_entity-otext = <fs_hrp1000_org>-stext.
      ENDIF.
    ENDIF.

************************************************************************
*Get Vacancy
************************************************************************


IF lt_vacant IS NOT INITIAL.

READ TABLE lt_vacant ASSIGNING <fs_vacant> WITH KEY positionid = ls_entity-plans.
IF <fs_vacant> IS ASSIGNED.

ls_entity-vacant = 'X'.
ls_entity-vbegda = <fs_vacant>-begindate.
ls_entity-vendda = <fs_vacant>-enddate.

else.
ls_entity-vacant = ' '.

ENDIF.

ENDIF.

************************************************************************
*Get Payscale
************************************************************************

IF ls_entity-plans IS NOT INITIAL.
    READ TABLE lt_hrp1005 ASSIGNING <fs_hrp1005> WITH KEY objid = ls_entity-plans.

    IF <fs_hrp1005> IS ASSIGNED.
      ls_entity-trfar = <fs_hrp1005>-trfar.
    ENDIF.
ENDIF.

IF ls_entity-trfar IS NOT INITIAL.
    READ TABLE lt_t510 ASSIGNING <fs_t510> WITH KEY trfar = ls_entity-trfar.

    IF <fs_hrp1005> IS ASSIGNED.
      ls_entity-trfgr = <fs_t510>-trfgr.
    ENDIF.
ENDIF.

************************************************************************
*END field symbol checks and loops
************************************************************************

ENDIF.

  ENDLOOP.  "END of Child loop body lt_hrp1001

************************************************************************
*Entity Append
************************************************************************

    APPEND ls_entity TO lt_entity. "Append Entity to Entity Table

  ENDIF.
ENDLOOP.  "END of Main loop body lt_hrp1000
