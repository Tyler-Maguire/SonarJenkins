************************************************************************
*  Create Date   : September 21, 2018
*  Author        : Claudio da Silva
*  Program Title : ECP to SAP FI/CO transfer
*  Description   : Send wage type and account information as journal
*                  entries for the FI/CO general ledger.
*  Transaction   : N/A
*-----------------------------------------------------------------------
*  Change History
*-----------------------------------------------------------------------
*  trans#    |  Date      |  Programmer   |  Description
*------------|------------|---------------|-----------------------------
*            | 21/10/2018 | C da Silva    | Initial Version.
************************************************************************

REPORT z_fico_gl_out.

************************************************************************
* Type-Pools
************************************************************************

TYPE-POOLS: vrm.

************************************************************************
* Types
************************************************************************

TYPES: BEGIN OF ty_report,
         docnum     TYPE hrpp_docnum,      "Document Number
         checkdt    TYPE datum,            "Check Date
         hkontdit   TYPE hkont,            "General Ledger
         gltext     TYPE txt20_skat,       "General Ledger Text
         kostldit   TYPE kostl,            "Cost Center
         lob        TYPE string,           "Lob
         loc        TYPE string,           "Loc
         emp_type   TYPE string,           "Employee Type
         dept       TYPE string,           "Department
         emp_pos    TYPE string,           "Employee Position
         sgtxtdit   TYPE sgtxt,            "Wage Type Long Text
         lgartoix   TYPE lgart,            "Wage Type
         pernroix   TYPE pernr_d,          "Personnel Number
         anzhloix   TYPE anzhl,            "Hours
         fpperoix   TYPE faper,            "For Period
         inperoix   TYPE iperi,            "In Period
         actsignoix TYPE srtza,            "Status of Record
         offcylce   TYPE abap_bool,        "Off-Cycle Indicator
         betrg_soix TYPE hrpp_betrgs,      "Debit
         betrg_hoix TYPE hrpp_betrgh,      "Credit
         betrg_acc_soix TYPE hrpp_betrgs,  "Accrual Debit
         betrg_acc_hoix TYPE hrpp_betrgh,  "Accrual Credit
         betrg_net_amt TYPE hrpp_betrgh,   "Accrual Net Amount
       END OF ty_report.

TYPES: BEGIN OF rgdir.
        INCLUDE STRUCTURE pc261.
TYPES: END OF rgdir.


************************************************************************
* Tables
************************************************************************

TABLES: sscrfields.
TABLES: ppdhd.
TABLES: ppdit.
TABLES: pevst.
TABLES: pevsh.

************************************************************************
* Data Declarations
************************************************************************


DATA: gt_doc_analyse         TYPE hrpp_document_analyse_tab.

DATA: ls_0002 TYPE p0002,
      ls_t511 TYPE t511.

DATA: gv_period_begda TYPE datum,
      gv_period_endda TYPE datum.

DATA: gs_doctyp              TYPE hrpp_sel_st_doctyp.
DATA: gs_evtyp               TYPE hrpp_sel_st_evtyp.
DATA: gs_revdoc              TYPE hrpp_sel_st_revdoc.
DATA: gs_doc_analyse_selops  TYPE hrpp_document_analyse_selops.
DATA: gs_doc_analyse_package TYPE hrpp_document_analyse_package.
DATA: gt_selected_runs       TYPE hrpp_t_runid_docnum.
DATA: gt_status              TYPE hrpp_sel_docstat.
DATA: gt_actual              TYPE hrpp_sel_actstat.
DATA: gs_actual              TYPE hrpp_sel_st_actstat.
DATA: gt_simu                TYPE hrpp_sel_docsimu.
DATA: gs_simu                TYPE hrpp_sel_st_docsimu.
DATA: gt_prod                TYPE hrpp_sel_docprod.
DATA: gs_prod                TYPE hrpp_sel_st_docprod.
DATA: gt_attr                TYPE hrpp_sel_evattr.
DATA: gs_attr                TYPE hrpp_sel_st_evattr.
DATA: gt_value               TYPE hrpp_sel_attrval.
DATA: gs_value               TYPE hrpp_sel_st_attrval.
DATA: gs_value_hlp           TYPE hrpp_sel_st_attrval.
DATA: gt_name                TYPE hrpp_sel_evname.
DATA: gs_name                TYPE hrpp_sel_st_evname.

DATA: gv_akper               TYPE pc261-inper.


DATA: gs_parameters_parallel TYPE hrpp_s_parameters_parallel.
DATA: gt_srvgrp_val          TYPE TABLE OF vrm_value.


DATA: gr_cl_document_analyse TYPE REF TO cl_document_analyse.

DATA: gr_cx_document_analyse TYPE REF TO cx_document_analyse.

DATA: abkrs  TYPE hrpp_dan_abkrs,
      doctyp TYPE hrpp_dan_doctyp,
      blart  TYPE hrpp_dan_blart.

DATA: go_grid      TYPE REF TO cl_gui_alv_grid,
      go_cust_cont TYPE REF TO cl_gui_custom_container,
      go_doc       TYPE REF TO cl_gui_docking_container.

DATA: gt_report   TYPE TABLE OF ty_report, "Table for final records
      wa_report   TYPE ty_report, "Work area for final records
      gt_file     TYPE TABLE OF string, "File contents
      lv_filename TYPE string. "File name for AL 11 file

DATA: it_t549q TYPE TABLE OF t549q,
      gs_t549q TYPE t549q.

DATA: gd_ucomm TYPE sy-ucomm.

DATA: it_rgdir TYPE TABLE OF rgdir,
      lv_molga TYPE molga.

DATA: it_p0001 TYPE TABLE OF PA0001.

************************************************************************
* Selection Screen
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK payroll_filter WITH FRAME TITLE text-001.

PARAMETERS:         p_ar_opt TYPE arc_opt DEFAULT '2' NO-DISPLAY.
PARAMETERS:         p_abkrs  LIKE abkrs.
PARAMETERS:         p_pabrj  TYPE pabrj MODIF ID akp.
PARAMETERS:         p_pabrp  TYPE pabrp MODIF ID akp.
PARAMETERS:         type     LIKE pevsh-type NO-DISPLAY.
SELECT-OPTIONS:     p_status FOR pevsh-status.
SELECT-OPTIONS:     p_name   FOR pevst-name NO-DISPLAY.

SELECTION-SCREEN SKIP 1.

"@
PARAMETERS:         p_pvper  AS CHECKBOX USER-COMMAND check_pvper.
PARAMETERS:         p_acc AS CHECKBOX.
PARAMETERS:         p_oncyc AS CHECKBOX.
PARAMETERS:         p_offcyc AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK payroll_filter.

SELECTION-SCREEN BEGIN OF BLOCK posting_criteria WITH FRAME TITLE text-002.

PARAMETERS:         p_type   LIKE ppdhd-evtyp DEFAULT 'PP' OBLIGATORY.

SELECT-OPTIONS:     p_runid  FOR ppdhd-runid.
SELECT-OPTIONS:     p_docnum FOR ppdit-docnum.
SELECT-OPTIONS:     p_bukrs  FOR ppdhd-bukrs NO-DISPLAY.
SELECT-OPTIONS:     p_budat  FOR ppdhd-budat.
SELECT-OPTIONS:     p_bldat  FOR ppdhd-bldat.
SELECT-OPTIONS:     p_doctyp FOR doctyp.
SELECT-OPTIONS:     p_blart  FOR blart.

SELECTION-SCREEN END OF BLOCK posting_criteria.


SELECTION-SCREEN BEGIN OF BLOCK filter_criteria WITH FRAME TITLE text-003.

SELECT-OPTIONS:     p_ktosl FOR ppdit-ktosl NO INTERVALS.
SELECT-OPTIONS:     p_hkont FOR ppdit-hkont.
SELECT-OPTIONS:     p_kunnr FOR ppdit-kunnr NO-DISPLAY.
SELECT-OPTIONS:     p_lifnr FOR ppdit-lifnr NO-DISPLAY.
SELECT-OPTIONS:     p_gsber FOR ppdit-gsber NO-DISPLAY.
SELECT-OPTIONS:     p_kostl FOR ppdit-kostl.
SELECT-OPTIONS:     p_aufnr FOR ppdit-aufnr NO-DISPLAY.
SELECT-OPTIONS:     p_kstrg FOR ppdit-kstrg NO-DISPLAY.
SELECT-OPTIONS:     p_posnr FOR ppdit-posnr NO-DISPLAY.
SELECT-OPTIONS:     p_nplnr FOR ppdit-nplnr NO-DISPLAY.
SELECT-OPTIONS:     p_awtyp FOR ppdit-awtyp_pre NO-DISPLAY.
SELECT-OPTIONS:     p_awkey FOR ppdit-awkey_pre NO-DISPLAY.
SELECT-OPTIONS:     p_awpos FOR ppdit-awpos_pre NO-DISPLAY.
SELECT-OPTIONS:     p_prctr FOR ppdit-prctr NO-DISPLAY.
SELECT-OPTIONS:     p_segmt FOR ppdit-segment NO-DISPLAY.
SELECT-OPTIONS:     p_wrbtr FOR ppdit-wrbtr.
SELECT-OPTIONS:     p_pernr FOR ls_0002-pernr NO INTERVALS MATCHCODE OBJECT prem.
SELECT-OPTIONS:     p_wage  FOR ls_t511-lgart NO INTERVALS.

SELECTION-SCREEN END OF BLOCK filter_criteria.

SELECTION-SCREEN BEGIN OF BLOCK file_criteria WITH FRAME TITLE text-004.

PARAMETERS:         p_file_n TYPE string LOWER CASE.
PARAMETERS:         p_file_w AS CHECKBOX.
PARAMETERS:         p_file_d AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK file_criteria.

SELECTION-SCREEN BEGIN OF BLOCK sort_criteria WITH FRAME TITLE text-005.

SELECTION-SCREEN BEGIN OF BLOCK sort_cri WITH FRAME TITLE text-010.

PARAMETERS:         p_sdoc RADIOBUTTON GROUP sort DEFAULT 'X'.
PARAMETERS:         p_sacc RADIOBUTTON GROUP sort.
PARAMETERS:         p_sper RADIOBUTTON GROUP sort.
PARAMETERS:         p_swag RADIOBUTTON GROUP sort.
PARAMETERS:         p_sdeb RADIOBUTTON GROUP sort.
PARAMETERS:         p_scre RADIOBUTTON GROUP sort.

SELECTION-SCREEN END OF BLOCK sort_cri.

SELECTION-SCREEN BEGIN OF BLOCK sort_dir WITH FRAME TITLE text-011.

PARAMETERS:         p_sasc RADIOBUTTON GROUP dir DEFAULT 'X'.
PARAMETERS:         p_sdes RADIOBUTTON GROUP dir.

SELECTION-SCREEN END OF BLOCK sort_dir.

SELECTION-SCREEN END OF BLOCK sort_criteria.


SELECTION-SCREEN BEGIN OF BLOCK simu_criteria WITH FRAME TITLE text-006.

PARAMETERS:         p_both RADIOBUTTON GROUP simu.
PARAMETERS:         p_simu RADIOBUTTON GROUP simu.
PARAMETERS:         p_prod RADIOBUTTON GROUP simu DEFAULT 'X'.

SELECTION-SCREEN SKIP 1.

PARAMETERS:         p_subst  AS CHECKBOX.
PARAMETERS:         p_storno AS CHECKBOX.
PARAMETERS:         p_pbc NO-DISPLAY.
PARAMETERS:         p_pbc_fm NO-DISPLAY.
PARAMETERS:         p_actu   LIKE pevst-simu DEFAULT 'X' NO-DISPLAY.

SELECT-OPTIONS:     p_fistl FOR ppdit-fistl NO-DISPLAY.
SELECT-OPTIONS:     p_fipos FOR ppdit-fipos NO-DISPLAY.
SELECT-OPTIONS:     p_geber FOR ppdit-geber NO-DISPLAY.
SELECT-OPTIONS:     p_fipex FOR ppdit-fipex NO-DISPLAY.
SELECT-OPTIONS:     p_fkber FOR ppdit-fkber NO-DISPLAY.
SELECT-OPTIONS:     p_grant FOR ppdit-grant_nbr NO-DISPLAY.

SELECTION-SCREEN END OF BLOCK simu_criteria.

SELECTION-SCREEN BEGIN OF BLOCK par_process WITH FRAME TITLE text-007.

PARAMETERS: p_parall TYPE hrpp_parallel_proc DEFAULT 'X'.
PARAMETERS: p_jobs   TYPE hrpp_s_parameters_parallel-jobs DEFAULT 5.
PARAMETERS: p_objnr  TYPE hrpp_s_parameters_parallel-objnr DEFAULT 20.
PARAMETERS: p_srvgrp TYPE hrpp_s_parameters_parallel-srvgrp AS LISTBOX VISIBLE LENGTH 20.

SELECTION-SCREEN END OF BLOCK par_process.


************************************************************************
* At Selection Screen
************************************************************************
AT SELECTION-SCREEN.
  gd_ucomm = sy-ucomm.

************************************************************************
* At Selection Screen Output
************************************************************************

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    CASE gd_ucomm.
      WHEN 'CHECK_PVPER'.
        IF p_pvper EQ 'X'.
          IF screen-group1 = 'AKP'.
            screen-active = '0'.
            screen-invisible = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.
    ENDCASE.

    DATA: lv_permo TYPE permo.

    IF p_pvper EQ 'X'.

      CLEAR: gs_t549q.

      IF screen-group1 = 'AKP'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.

      SELECT SINGLE permo FROM t549a INTO lv_permo WHERE abkrs = p_abkrs.

      SELECT SINGLE * FROM t549q INTO gs_t549q WHERE permo = lv_permo AND begda LE sy-datum AND endda GE sy-datum.

      CONCATENATE gs_t549q-vabrj gs_t549q-vabrp INTO gv_akper.

        p_budat-sign = 'I'.
        p_budat-low = gs_t549q-begda.
        p_budat-high = gs_t549q-endda.
        p_budat-option = 'EQ'.

       APPEND p_budat.

    ELSE.

      CLEAR: gs_t549q.

    ENDIF.

    "@

*    IF p_dater EQ 'X'.
*
*      CLEAR: gs_t549q.
*
*      IF p_pabrj IS NOT INITIAL AND p_pabrp IS NOT INITIAL.
*        CONCATENATE p_pabrj p_pabrp INTO gv_akper.
*      ELSEIF gv_akper IS INITIAL.
*        CONTINUE.
*      ENDIF.
*
*      SELECT SINGLE permo FROM t549a INTO lv_permo WHERE abkrs = p_abkrs.
*
*      SELECT SINGLE begda endda FROM t549q INTO CORRESPONDING FIELDS OF gs_t549q WHERE permo = lv_permo AND pabrj = gv_akper(4) AND pabrp = gv_akper+4(2).
*
*      IF gs_t549q IS NOT INITIAL.
*
*        CLEAR: p_bldat.
*
*        p_bldat-sign = 'I'.
*        p_bldat-low = gs_t549q-begda.
*        p_bldat-high = gs_t549q-endda.
*        p_bldat-option = 'EQ'.
*
*        APPEND p_bldat.
*
*      ENDIF.
*
*    ENDIF.



  ENDLOOP.

  CLEAR: gd_ucomm.

************************************************************************
* Intialization
************************************************************************

INITIALIZATION.

  PERFORM set_listbox_for_srvgrp.

************************************************************************
* START OF SELECTION
************************************************************************

START-OF-SELECTION.

************************************************************************
* Internal Table Load
************************************************************************

  DATA: it_skat TYPE TABLE OF skat.

  "Account texts in english
  SELECT * INTO TABLE it_skat FROM skat WHERE spras = 'EN' AND ktopl = 'US00'.

  "All payroll periods and their begin and end dates
  SELECT * INTO TABLE it_t549q FROM t549q.

************************************************************************
* Program Logic
************************************************************************

  PERFORM init_global_data.

  PERFORM select_docs_and_runs.

  IF ( gr_cx_document_analyse IS NOT BOUND ) OR ( gr_cx_document_analyse->msgty <> 'E' ).

    PERFORM select_docs_and_runs_data.

  ENDIF.

  PERFORM archive_report.

  PERFORM display_alv.

************************************************************************
* End Selection
************************************************************************

END-OF-SELECTION.

************************************************************************
* Forms
************************************************************************

*&---------------------------------------------------------------------*
*&      Form  Set_Listbox_For_Srvgrp
*&---------------------------------------------------------------------*
*       Get server group for parallel processing
*----------------------------------------------------------------------*
FORM set_listbox_for_srvgrp .

  DATA: lt_setup TYPE TABLE OF rzllitab.
  DATA: lt_erfc_setup TYPE TABLE OF rzlliclass.
  DATA: ls_srvgrp_val TYPE vrm_value.

  FIELD-SYMBOLS: <setup> TYPE rzllitab.

  CLEAR: gt_srvgrp_val[].

  CALL FUNCTION 'SMLG_GET_SETUP'
    EXPORTING
      grouptype          = 'S'
    TABLES
      setup              = lt_setup
      erfc_setup         = lt_erfc_setup
    EXCEPTIONS
      foreign_lock       = 1
      system_failure     = 2
      invalid_group_type = 3
      OTHERS             = 4.

  IF lt_setup[] IS NOT INITIAL.
    LOOP AT lt_setup ASSIGNING <setup>.
      CLEAR: ls_srvgrp_val.
      ls_srvgrp_val-key  = <setup>-classname.
      ls_srvgrp_val-text = <setup>-classname.
      APPEND ls_srvgrp_val TO gt_srvgrp_val.
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM gt_srvgrp_val.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = 'P_SRVGRP'
        values = gt_srvgrp_val.
  ENDIF.

ENDFORM. "Set_Listbox_For_Srvgrp

*&---------------------------------------------------------------------*
*&      Form  Init_Global_Data
*&---------------------------------------------------------------------*
*       Initialises all global variables needed
*----------------------------------------------------------------------*

FORM init_global_data .

"Remove Old logic
*  "If off-cycles are included, remove payroll area
*  IF p_dater EQ 'X'.
*    clear: p_abkrs.
*  ENDIF.

  PERFORM authority_check.

  CLEAR: gt_doc_analyse[].
  CLEAR: gt_selected_runs[].
  CLEAR: gs_doc_analyse_selops.

  gs_doc_analyse_selops-docnum = p_docnum[].

  gs_doc_analyse_selops-doctyp[] = p_doctyp[].
  gs_doc_analyse_selops-bukrs[] = p_bukrs[].
  gs_doc_analyse_selops-budat[] = p_budat[].
  gs_doc_analyse_selops-bldat[] = p_bldat[].
  gs_doc_analyse_selops-blart[] = p_blart[].

  CLEAR: gs_evtyp.
  IF p_type IS NOT INITIAL.
    gs_evtyp-sign = 'I'. gs_evtyp-option = 'EQ'. gs_evtyp-low = p_type.
    APPEND gs_evtyp TO gs_doc_analyse_selops-evtyp.
  ENDIF.

  IF p_storno IS NOT INITIAL.
    CLEAR: gs_revdoc. gs_revdoc-sign = 'I'. gs_revdoc-option = 'NE'.
    APPEND gs_revdoc TO gs_doc_analyse_selops-revdoc.
  ENDIF.

  gs_doc_analyse_selops-runid[] = p_runid[].

  gs_doc_analyse_selops-ktosl[]     = p_ktosl[].
  gs_doc_analyse_selops-hkont[]     = p_hkont[].
  gs_doc_analyse_selops-kunnr[]     = p_kunnr[].
  gs_doc_analyse_selops-lifnr[]     = p_lifnr[].
  gs_doc_analyse_selops-gsber[]     = p_gsber[].
  gs_doc_analyse_selops-kostl[]     = p_kostl[].
  gs_doc_analyse_selops-aufnr[]     = p_aufnr[].
  gs_doc_analyse_selops-kstrg[]     = p_kstrg[].
  gs_doc_analyse_selops-posnr[]     = p_posnr[].
  gs_doc_analyse_selops-nplnr[]     = p_nplnr[].
  gs_doc_analyse_selops-fistl[]     = p_fistl[].
  gs_doc_analyse_selops-fipos[]     = p_fipos[].
  gs_doc_analyse_selops-geber[]     = p_geber[].
  gs_doc_analyse_selops-wrbtr[]     = p_wrbtr[].
  gs_doc_analyse_selops-fipex[]     = p_fipex[].
  gs_doc_analyse_selops-awtyp_pre[] = p_awtyp[].
  gs_doc_analyse_selops-awkey_pre[] = p_awkey[].
  gs_doc_analyse_selops-awpos_pre[] = p_awpos[].
  gs_doc_analyse_selops-fkber[]     = p_fkber[].
  gs_doc_analyse_selops-grant_nbr[] = p_grant[].
  gs_doc_analyse_selops-prctr[]     = p_prctr[].
  gs_doc_analyse_selops-segment[]   = p_segmt[].

  gs_doc_analyse_selops-pernr_detail = 'X'.
  gs_doc_analyse_selops-ppdst_detail = p_subst.

  gt_status[] = p_status[].

  gt_name[] = p_name[].

  CLEAR: gs_actual.
  IF p_actu IS NOT INITIAL.
    gs_actual-sign = 'I'. gs_actual-option = 'EQ'. gs_actual-low = p_actu.
    APPEND gs_actual TO gt_actual.
  ENDIF.

  IF p_simu IS NOT INITIAL.

    CLEAR: gs_simu. gs_simu-sign = 'I'. gs_simu-option = 'EQ'. gs_simu-low = 'X'.
    APPEND gs_simu TO gt_simu.
    CLEAR: gs_prod. gs_prod-sign = 'I'. gs_prod-option = 'EQ'. gs_prod-low = ' '.
    APPEND gs_prod TO gt_prod.

  ELSEIF p_prod IS NOT INITIAL.

    CLEAR: gs_simu. gs_simu-sign = 'I'. gs_simu-option = 'EQ'. gs_simu-low = ' '.
    APPEND gs_simu TO gt_simu.
    CLEAR: gs_prod. gs_prod-sign = 'I'. gs_prod-option = 'EQ'. gs_prod-low = 'X'.
    APPEND gs_prod TO gt_prod.

  ELSE.

    CLEAR: gs_simu. gs_simu-sign = 'I'. gs_simu-option = 'EQ'. gs_simu-low = 'X'.
    APPEND gs_simu TO gt_simu.
    CLEAR: gs_prod. gs_prod-sign = 'I'. gs_prod-option = 'EQ'. gs_prod-low = 'X'.
    APPEND gs_prod TO gt_prod.

  ENDIF.


  IF p_abkrs IS NOT INITIAL OR gv_akper(4) IS NOT INITIAL OR gv_akper+4(2) IS NOT INITIAL.

    CLEAR: gs_attr. gs_attr-sign = 'I'. gs_attr-option = 'EQ'.
    gs_attr-low = 'AKPER'.
    APPEND gs_attr TO gt_attr.

    CLEAR: gs_value. gs_value-sign = 'I'. gs_value-option = 'EQ'.
    gs_value-low = '**/**/****'.
    CLEAR: gs_value_hlp. gs_value_hlp-sign = 'I'.
    gs_value_hlp-option = 'EQ'.  gs_value_hlp-low = '  /  /    '.

    gs_value_hlp-low(2)   = p_abkrs.
    gs_value_hlp-low+3(2) = gv_akper+4(2).
    gs_value_hlp-low+6(4) = gv_akper(4).

    OVERLAY gs_value_hlp-low WITH gs_value-low.
    gs_value-low = gs_value_hlp-low.
    IF gs_value-low CA '*'.
      gs_value-option = 'CP'.
    ENDIF.
    APPEND gs_value TO gt_value.

  ENDIF.

  IF p_parall IS NOT INITIAL.

    CLEAR: gs_parameters_parallel.
    gs_parameters_parallel-jobs     = p_jobs.
    gs_parameters_parallel-objnr    = p_objnr.
    gs_parameters_parallel-srvgrp   = p_srvgrp.
    gs_parameters_parallel-taskname = 'ANALYSE_PPDHD_PPDIT'.

  ENDIF.

  gs_doc_analyse_package-selops = gs_doc_analyse_selops.

ENDFORM. "Init_Global_Data

*&---------------------------------------------------------------------*
*&      Form  Authority_Check
*&---------------------------------------------------------------------*
*       Checks authority before performing any actions
*----------------------------------------------------------------------*

FORM authority_check .

  DATA: bukrs_table TYPE TABLE OF bukrs,
        bukrs       TYPE bukrs.

  AUTHORITY-CHECK OBJECT 'S_PROGRAM'
    ID 'P_GROUP'  FIELD 'RPCIPD00'
    ID 'P_ACTION' FIELD 'SUBMIT'.

  IF sy-subrc <> 0.
    MESSAGE e645(db) WITH 'S_PROGRAM' 'RPCIPD00'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'S_PROGRAM'
    ID 'P_GROUP'  FIELD 'RPCIPS00'
    ID 'P_ACTION' FIELD 'SUBMIT'.

  IF sy-subrc <> 0.
    MESSAGE e645(db) WITH 'S_PROGRAM' 'RPCIPS00'.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'P_PYEVRUN'
           ID 'P_EVTYP' FIELD p_type
           ID 'P_EVSIMU' FIELD p_simu
           ID 'ACTVT' FIELD '03'.

  IF sy-subrc <> 0.
    MESSAGE e156(3g) WITH 'P_PYEVRUN'.
  ENDIF.

  IF p_bukrs[] IS NOT INITIAL.
    SELECT bukrs FROM t001 INTO TABLE bukrs_table
                 WHERE bukrs IN p_bukrs.
  ELSE.
    SELECT bukrs FROM t001 INTO TABLE bukrs_table.      "#EC CI_GENBUFF
  ENDIF.

  LOOP AT bukrs_table INTO bukrs.

    AUTHORITY-CHECK OBJECT 'P_PYEVDOC'
              ID 'BUKRS' FIELD bukrs
              ID 'ACTVT' FIELD '03'.
    IF sy-subrc <> 0.
      MESSAGE e156(3g) WITH 'P_PYEVDOC'.
    ENDIF.
    AUTHORITY-CHECK OBJECT 'P_PYEVDOC'
              ID 'BUKRS' FIELD bukrs
              ID 'ACTVT' FIELD '28'.

    IF sy-subrc <> 0.
      MESSAGE e156(3g) WITH 'P_PYEVDOC'.
    ENDIF.

  ENDLOOP.

ENDFORM. " Authority_Check

*&---------------------------------------------------------------------*
*&      Form  Select_Docs_And_Runs
*&---------------------------------------------------------------------*
*       Selects the Documents and Runs from posting
*----------------------------------------------------------------------*

FORM select_docs_and_runs .

  TRY.

      CALL METHOD cl_document_analyse=>get_docnum_by_ppdsh
        EXPORTING
          it_evtyp  = gs_doc_analyse_selops-evtyp
          it_docnum = gs_doc_analyse_selops-docnum
          it_doctyp = gs_doc_analyse_selops-doctyp "XFE
          it_budat  = gs_doc_analyse_selops-budat  "XFE
          it_bldat  = gs_doc_analyse_selops-bldat  "XFE
          it_runid  = gs_doc_analyse_selops-runid
          it_actual = gt_actual
          it_status = gt_status
          it_runs   = gs_doc_analyse_package-rundoc
        IMPORTING
          et_runs   = gs_doc_analyse_package-rundoc.

      CALL METHOD cl_document_analyse=>get_docnum_by_pevsh
        EXPORTING
          it_evtyp  = gs_doc_analyse_selops-evtyp
          it_runid  = gs_doc_analyse_selops-runid
          it_actual = gt_actual
          it_status = gt_status
          it_runs   = gs_doc_analyse_package-rundoc
        IMPORTING
          et_runs   = gs_doc_analyse_package-rundoc.

      CALL METHOD cl_document_analyse=>get_docnum_by_pevst
        EXPORTING
          it_evtyp = gs_doc_analyse_selops-evtyp
          it_runid = gs_doc_analyse_selops-runid
          it_name  = gt_name
          it_simu  = gt_simu
          it_prod  = gt_prod
          it_runs  = gs_doc_analyse_package-rundoc
        IMPORTING
          et_runs  = gs_doc_analyse_package-rundoc.

      CALL METHOD cl_document_analyse=>get_docnum_by_pevat
        EXPORTING
          it_evtyp = gs_doc_analyse_selops-evtyp
          it_runid = gs_doc_analyse_selops-runid
          it_attr  = gt_attr
          it_value = gt_value
          it_runs  = gs_doc_analyse_package-rundoc
        IMPORTING
          et_runs  = gs_doc_analyse_package-rundoc.

      CALL METHOD cl_document_analyse=>get_docnum_by_ppdhd_ppdit
        EXPORTING
          is_doc_analyse_selops = gs_doc_analyse_selops
          it_runs               = gs_doc_analyse_package-rundoc
        IMPORTING
          et_runs               = gs_doc_analyse_package-rundoc.

    CATCH cx_document_analyse INTO gr_cx_document_analyse.

      MESSAGE gr_cx_document_analyse->msgex TYPE 'S' DISPLAY LIKE gr_cx_document_analyse->msgty.

  ENDTRY.

ENDFORM. "Aelect_Docs_And_Runs

*&---------------------------------------------------------------------*
*&      Form  Select_Docs_And_Runs_Data
*&---------------------------------------------------------------------*
*       Gets the data for selected docs and runs
*----------------------------------------------------------------------*

FORM select_docs_and_runs_data .

  CREATE OBJECT gr_cl_document_analyse.

  TRY.

      IF p_parall IS INITIAL.

        CALL METHOD gr_cl_document_analyse->analyse_document
          EXPORTING
            is_doc_analyse_package = gs_doc_analyse_package.

      ELSE.

        CALL METHOD gr_cl_document_analyse->analyse_document_parallel
          EXPORTING
            is_parameters_parallel = gs_parameters_parallel
            is_doc_analyse_package = gs_doc_analyse_package.

      ENDIF.

    CATCH cx_document_analyse INTO gr_cx_document_analyse.

      MESSAGE gr_cx_document_analyse->msgex TYPE 'S' DISPLAY LIKE gr_cx_document_analyse->msgty.

  ENDTRY.

  gt_doc_analyse[] = gr_cl_document_analyse->get_document_analyse_tab( ).

ENDFORM. "Select_Docs_And_Runs_Data

*&---------------------------------------------------------------------*
*&      Form  Display_ALV
*&---------------------------------------------------------------------*
*       Displays ALV to be viewed
*----------------------------------------------------------------------*

FORM display_alv.

  DATA: lv_alv        TYPE REF TO cl_salv_table, "ALV table
        lv_functions  TYPE REF TO cl_salv_functions_list, "ALV functions
        lv_columns    TYPE REF TO cl_salv_columns, "ALV Columns attributes
        lv_column     TYPE REF TO cl_salv_column_table, "ALV Columns details
        lv_struct     TYPE REF TO cl_abap_structdescr, "Structure description
        lt_comp       TYPE abap_component_tab, "Component table
        ls_comp       TYPE abap_componentdescr, "Component work area
        lv_field_name TYPE lvc_fname, "Name of field to access
        lt_fields     TYPE TABLE OF string, "Table of field names for ALV
        lv_short      TYPE scrtext_s, "Short name of field
        lv_med        TYPE scrtext_m, "Medium name of field
        lv_long       TYPE scrtext_l. "Long name of field


  "Append additional field names here
  APPEND 'Document Number' TO lt_fields.
  APPEND 'Check Date' TO lt_fields.
  APPEND 'General Ledger Account' TO lt_fields.
  APPEND 'General Ledger Account Text' TO lt_fields.
  APPEND 'Cost Center' TO lt_fields.
  APPEND 'LOB' TO lt_fields.
  APPEND 'LOC' TO lt_fields.
  APPEND 'Employee Type' TO lt_fields.
  APPEND 'Department' TO lt_fields.
  APPEND 'Employee Position' TO lt_fields.
  APPEND 'Wage Type Long Text' TO lt_fields.
  APPEND 'Wage Type' TO lt_fields.
  APPEND 'Personnel Number' TO lt_fields.
  APPEND 'Hours' TO lt_fields.
  APPEND 'For Period' TO lt_fields.
  APPEND 'In Period' TO lt_fields.
  APPEND 'Status of Record' TO lt_fields.
  APPEND 'Off-Cycle Indicator' TO lt_fields.
  APPEND 'Debit' TO lt_fields.
  APPEND 'Credit' TO lt_fields.
  APPEND 'EST Accrual Debit' TO lt_fields.
  APPEND 'EST Accrual Credit' TO lt_fields.
  APPEND 'NET Accrual Amount' TO lt_fields.


  TRY.

      "Create the ALV
      cl_salv_table=>factory( IMPORTING r_salv_table = lv_alv CHANGING t_table = gt_report ).

    CATCH cx_salv_msg.

      WRITE: / 'Error in creating ALV for display'.

  ENDTRY.

  "Add funciton buttons to ALV
  lv_functions = lv_alv->get_functions( ).
  lv_functions->set_all( abap_true ).

  "Optimise column width for ALV
  lv_columns = lv_alv->get_columns( ).
  lv_columns->set_optimize( 'X' ).

  "Get structure of the error table
  lv_struct ?= cl_abap_typedescr=>describe_by_data( wa_report ).

  "Get the components of an error line
  lt_comp = lv_struct->get_components( ).

*    "Read the header line for the error table
*    READ TABLE gt_error INTO lv_wa_error INDEX 1.

  "Loop over all fields to add field names to table
  LOOP AT lt_comp INTO ls_comp.

    "Add headers to ALV table
    TRY.

        READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<fs_value>) INDEX sy-tabix.

        lv_long = <fs_value>.
        lv_med = <fs_value>.
        lv_short = <fs_value>.

        "Field name string to LVC_FNAME type conversion
        lv_field_name = ls_comp-name.

        "Convert column attributes to column details and set texts for fields
        lv_column ?= lv_columns->get_column( lv_field_name ).
        lv_column->set_visible( if_salv_c_bool_sap=>true ).
        lv_column->set_long_text( lv_long ).
        lv_column->set_medium_text( lv_med ).
        lv_column->set_short_text( lv_short ).

      CATCH cx_salv_not_found.
        WRITE: / 'Error in creating ALV for display'.
      CATCH cx_salv_existing.
        WRITE: / 'Error in creating ALV for display'.
      CATCH cx_salv_data_error.
        WRITE: / 'Error in creating ALV for display'.

    ENDTRY.

  ENDLOOP.

  "Display ALV with errors
  lv_alv->display( ).

ENDFORM. "Display_ALV

*&---------------------------------------------------------------------*
*&      Form  Archive_Report
*&---------------------------------------------------------------------*
*       Creates and archives report to AL11
*----------------------------------------------------------------------*

FORM archive_report.

  DATA: lv_line TYPE string,
        lv_chkdt TYPE datum,
        lv_high_date TYPE datum,
        lv_temphknon TYPE string,
        lv_prev_doc TYPE hrpp_docnum,
        lv_Last_day TYPE DATUM,
        lv_Accrual_days TYPE I,
        lt_holiday TYPE STANDARD TABLE OF ISCAL_DAY INITIAL SIZE 0,
        lv_Num_holiday TYPE I,
        LV_ACC_DEBIT_TOTAL TYPE DECFLOAT16,
        LV_D_TOTAL TYPE STRING,
        LV_C_TOTAL TYPE STRING,
        LV_N_TOTAL TYPE STRING,
        LV_ACC_CREDIT_TOTAL TYPE DECFLOAT16,
        LV_ACC_NET_TOTAL TYPE p decimals 1,
        lv_ACCRUAL_FRACTION TYPE DECFLOAT16.

        DATA: BEGIN OF eth_dats OCCURS 0.
        INCLUDE STRUCTURE  rke_dat.
        DATA: END OF eth_dats .


  FIELD-SYMBOLS: <fs_text> TYPE skat,
                 <fs_rg> TYPE rgdir.

  "Check If Accrual Checkbox was selected
  IF p_acc EQ 'X'.
     "Check if Filtering by Posting Date or Document Date
    IF p_budat-high IS NOT INITIAL OR p_budat-high <> 00000000.
      lv_high_date = p_budat-high.
    ELSE.
      lv_high_date = p_bldat-high.
    ENDIF.

    "Create Accrual Fracttion

"Get last day of the month
CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
  EXPORTING
    day_in            =    lv_high_date   " Key date
  IMPORTING
    last_day_of_month =    lv_last_day " Date of last day of the month from key  date
  EXCEPTIONS
    day_in_no_date    = 1
    others            = 2.

"Get Holidays in time period
  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      holiday_calendar           = 'US'  " Public holiday calendar ID
      factory_calendar           = 'US'   " Factory calendar ID
      date_from                  = lv_high_date
      date_to                    = lv_last_day
    TABLES
      holidays                   = lt_holiday
    EXCEPTIONS
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      others                     = 5
    .

"Remove Weekends
DELETE lt_holiday WHERE holiday <> 'X'.

"SUM Holidays
DESCRIBE TABLE lt_holiday LINES lv_Num_holiday.


"Get Number of working days in time period
  CALL FUNCTION 'RKE_SELECT_FACTDAYS_FOR_PERIOD'
    EXPORTING
      i_datab               = lv_high_date
      i_datbi               = lv_last_day
      i_factid              = 'US'
    TABLES
      eth_dats              = eth_dats
    EXCEPTIONS
      date_conversion_error = 1
      OTHERS                = 2.

"Sum Working Days
DESCRIBE TABLE eth_dats LINES lv_Accrual_days.

"Minus Start Date
lv_Accrual_days = lv_Accrual_days - 1.

"Add Holidays

lv_Accrual_days = lv_Accrual_days +  lv_Num_holiday.

"Calculate Fraction
lv_ACCRUAL_FRACTION = lv_Accrual_days / 10.

  ENDIF."End of accrual logic section


  "SELECT ONLY VALID PERNR's
  SELECT * FROM PA0001 INTO TABLE it_p0001 WHERE BEGDA <= sy-datum AND ENDDA >= sy-datum.

  LOOP AT gt_doc_analyse ASSIGNING FIELD-SYMBOL(<fs_doc>) WHERE pernroix IN p_pernr AND lgartoix IN p_wage.
    CLEAR: wa_report.

    wa_report-docnum = <fs_doc>-docnum.
    lv_temphknon = <fs_doc>-hkontdit.
    SHIFT lv_temphknon LEFT DELETING LEADING '0'.
    wa_report-hkontdit = lv_temphknon.
    wa_report-kostldit = <fs_doc>-kostldit.

    "CHECK IF COST CENTRE IS MISSING
    IF wa_report-kostldit IS INITIAL OR wa_report-kostldit EQ ''.
      "FETCH COST CENTER
      LOOP AT it_p0001 ASSIGNING FIELD-SYMBOL(<fs_pa0001>) WHERE PERNR EQ <fs_doc>-pernroix.
      wa_report-kostldit = <fs_pa0001>-kostl.
      EXIT.
      ENDLOOP.
    ENDIF.

    wa_report-sgtxtdit = <fs_doc>-sgtxtdit.
    wa_report-lgartoix = <fs_doc>-lgartoix.
    wa_report-pernroix = <fs_doc>-pernroix.
    wa_report-anzhloix = <fs_doc>-anzhloix.
    wa_report-fpperoix = <fs_doc>-fpperoix.
    wa_report-inperoix = <fs_doc>-inperoix.
    wa_report-actsignoix = <fs_doc>-actsignoix.
    wa_report-betrg_soix = <fs_doc>-betrg_soix.
    wa_report-betrg_hoix = <fs_doc>-betrg_hoix.

    IF p_acc EQ 'X'."Check if Accrual file -> calculate the accrual amounts
    wa_report-betrg_acc_soix = <fs_doc>-betrg_soix  * lv_ACCRUAL_FRACTION.
    lv_acc_debit_total = lv_acc_debit_total + wa_report-betrg_acc_soix.
    wa_report-betrg_acc_hoix = <fs_doc>-betrg_hoix * lv_ACCRUAL_FRACTION.
    lv_acc_credit_total = lv_acc_credit_total + wa_report-betrg_acc_hoix.
    wa_report-betrg_net_amt = wa_report-betrg_acc_soix + wa_report-betrg_acc_hoix.
    lv_acc_net_total = lv_acc_credit_total - lv_acc_debit_total.
    ENDIF.

IF lv_prev_doc IS INITIAL OR lv_prev_doc NE <fs_doc>-docnum.

    CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = wa_report-pernroix
    IMPORTING
      molga           = lv_molga
    TABLES
      in_rgdir        = it_rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.


    LOOP AT it_rgdir ASSIGNING <fs_rg> WHERE seqnr EQ <fs_doc>-seqnooix.
     lv_prev_doc = <fs_doc>-docnum.
     lv_chkdt = <fs_rg>-PAYDT.
    ENDLOOP.

ENDIF.

    wa_report-checkdt = lv_chkdt.


    IF wa_report-fpperoix IS INITIAL OR wa_report-fpperoix = 000000 OR wa_report-inperoix IS INITIAL OR wa_report-inperoix = 000000.
      wa_report-offcylce = 'X'.
    ELSE.
      wa_report-offcylce = ' '.
    ENDIF.

    UNASSIGN: <fs_text>.

    READ TABLE it_skat ASSIGNING <fs_text> WITH KEY saknr = <fs_doc>-hkontdit.

    IF <fs_text> IS ASSIGNED.
      wa_report-gltext = <fs_text>-txt20.
    ENDIF.

    APPEND wa_report TO gt_report.

  ENDLOOP.


IF p_oncyc EQ 'X'. "If Oncylce file -> remove offcycles

  DELETE gt_report WHERE offcylce EQ 'X'.

ENDIF.

IF p_offcyc EQ 'X'. "If Offcycle file -> remove oncycles

  DELETE gt_report WHERE offcylce <> 'X'.

ENDIF.



  PERFORM sort_table.

  lv_filename = p_file_n.

  DATA: lv_temp       TYPE string,
        lv_doc        TYPE string,
        lv_check_date TYPE string,
        lv_anzhl      TYPE string,
        lv_debit      TYPE string,
        lv_credit     TYPE string,
        lv_acc_debit  TYPE string,
        lv_acc_credit TYPE string,
        lv_net_amt    TYPE string.

  REFRESH: gt_file.
  CLEAR: gt_file.

  CLEAR: lv_line.

  IF p_acc EQ 'X'. "Check if accrual file -> Add Accrual headers
        CONCATENATE 'Document Number' 'Check Date' 'General Ledger Account' 'General Ledger Account Text' 'Cost Center' 'LOB' 'LOC' 'Employee Type'
        'Department' 'Employee Position' 'Wage Type Long Text' 'Wage Type' 'Personnel Number' 'Hours' 'Off-Cycle Indicator' 'Debit' 'Credit'  'EST Accrual Debit' 'EST Accrual Credit'  'NET Accrual Amount'
        INTO lv_line SEPARATED BY '|'.
  ELSE.
        CONCATENATE 'Document Number' 'Check Date' 'General Ledger Account' 'General Ledger Account Text' 'Cost Center' 'LOB' 'LOC' 'Employee Type'
        'Department' 'Employee Position' 'Wage Type Long Text' 'Wage Type' 'Personnel Number' 'Hours' 'Off-Cycle Indicator' 'Debit' 'Credit'
         INTO lv_line SEPARATED BY '|'.

  ENDIF.

  APPEND lv_line TO gt_file.

  LOOP AT gt_report ASSIGNING FIELD-SYMBOL(<fs_line>).

    CLEAR: lv_line.

    lv_debit = <fs_line>-betrg_soix.
    lv_acc_debit = <fs_line>-betrg_acc_soix.
    lv_credit = <fs_line>-betrg_hoix.
    lv_acc_credit = <fs_line>-betrg_acc_hoix.
    lv_net_amt = <fs_line>-betrg_net_amt.
    lv_anzhl = <fs_line>-anzhloix.
    lv_temp = <fs_line>-sgtxtdit.

    REPLACE FIRST OCCURRENCE OF '-' IN lv_temp WITH space.

    <fs_line>-sgtxtdit = lv_temp.

    IF <fs_line>-kostldit IS NOT INITIAL. "Check if the cost center has been assigned

      <fs_line>-lob = <fs_line>-kostldit(1).
      <fs_line>-loc = <fs_line>-kostldit+1(2).
      <fs_line>-emp_type = <fs_line>-kostldit+3(2).
      <fs_line>-dept = <fs_line>-kostldit+5(2).
      <fs_line>-emp_pos = <fs_line>-kostldit+7(3).

    ENDIF.

    lv_doc = <fs_line>-docnum.
    SHIFT lv_doc LEFT DELETING LEADING '0'.

      IF p_acc EQ 'X'."Check if accrual file -> add accrual fields
    CONCATENATE lv_doc <fs_line>-checkdt <fs_line>-hkontdit <fs_line>-gltext <fs_line>-kostldit <fs_line>-lob <fs_line>-loc <fs_line>-emp_type <fs_line>-dept
    <fs_line>-emp_pos <fs_line>-sgtxtdit <fs_line>-lgartoix <fs_line>-pernroix lv_anzhl <fs_line>-offcylce lv_debit lv_credit lv_acc_debit lv_acc_credit lv_net_amt
    INTO lv_line SEPARATED BY '|'.
  ELSE.
    CONCATENATE lv_doc <fs_line>-checkdt <fs_line>-hkontdit <fs_line>-gltext <fs_line>-kostldit <fs_line>-lob <fs_line>-loc <fs_line>-emp_type <fs_line>-dept
    <fs_line>-emp_pos <fs_line>-sgtxtdit <fs_line>-lgartoix <fs_line>-pernroix lv_anzhl <fs_line>-offcylce lv_debit lv_credit
    INTO lv_line SEPARATED BY '|'.

  ENDIF.

    APPEND lv_line TO gt_file. "Add Lines to the file table

  ENDLOOP.

  "ADD ACCRUAL LINE

      IF p_acc EQ 'X'.
        lv_acc_debit   = lv_acc_debit_total.
        lv_acc_credit  = lv_acc_credit_total.
        lv_net_amt     = lv_acc_net_total.

    CONCATENATE 'Accrual File' 'Accrual Debit Total' lv_acc_debit 'Accrual Credit Total' lv_acc_credit 'Accrual Net Amount' lv_net_amt '&'
    INTO lv_line SEPARATED BY '|'.
    INSERT lv_line INTO gt_file INDEX 1.
      ENDIF.
"@
      IF p_oncyc EQ 'X'. "If oncycle file -> add file header oncycle
    CONCATENATE 'OnCycle' '&'
    INTO lv_line SEPARATED BY '|'.
    INSERT lv_line INTO gt_file INDEX 1.
      ENDIF.

      IF p_offcyc EQ 'X'. "If Offcycle file -> add file header offcycle
    CONCATENATE 'OffCycle' '&'
    INTO lv_line SEPARATED BY '|'.
    INSERT lv_line INTO gt_file INDEX 1.
      ENDIF.

  IF p_file_d EQ 'X'. "Check if file export is required
    PERFORM file_download.
  ENDIF.

  IF p_file_w EQ 'X' AND lv_filename IS NOT INITIAL.

    OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

    IF sy-subrc EQ 0.

      LOOP AT gt_file ASSIGNING FIELD-SYMBOL(<fs_fline>).

        TRANSFER <fs_fline> TO lv_filename.

      ENDLOOP.

      CLOSE DATASET lv_filename.

    ELSE.

      WRITE: 'Error writing file to AL11 path: ', lv_filename.
      STOP.

    ENDIF.

  ENDIF.

ENDFORM. "Archive_Report

*&---------------------------------------------------------------------*
*&      Form  Send_SFTP
*&---------------------------------------------------------------------*
*       Sends file over SFTP once created
*----------------------------------------------------------------------*

FORM send_sftp.


ENDFORM. "Send_SFTP

*&---------------------------------------------------------------------*
*&      Form  Sort_Table
*&---------------------------------------------------------------------*
*       Sorts report table by the given selection screen criteria
*----------------------------------------------------------------------*

FORM sort_table.

  IF p_sdoc EQ 'X'. "Check sort by document number

    IF p_sasc EQ 'X'. "Check if ascending or descending
      SORT gt_report BY docnum ASCENDING.
    ELSE.
      SORT gt_report BY docnum DESCENDING.
    ENDIF.

  ELSEIF p_sacc EQ 'X'. "Check sort by ACC

    IF p_sasc EQ 'X'."Check if ascending or descending
      SORT gt_report BY hkontdit ASCENDING.
    ELSE.
      SORT gt_report BY hkontdit DESCENDING.
    ENDIF.

  ELSEIF p_sper EQ 'X'. "Check sort by Personnel Number

    IF p_sasc EQ 'X'."Check if ascending or descending
      SORT gt_report BY pernroix ASCENDING.
    ELSE.
      SORT gt_report BY pernroix DESCENDING.
    ENDIF.

  ELSEIF p_swag EQ 'X'. "Check sort by wage type

    IF p_sasc EQ 'X'."Check if ascending or descending
      SORT gt_report BY lgartoix ASCENDING.
    ELSE.
      SORT gt_report BY lgartoix DESCENDING.
    ENDIF.

  ELSEIF p_sdeb EQ 'X'. "check sort by ammount

    IF p_sasc EQ 'X'."Check if ascending or descending
      SORT gt_report BY betrg_soix ASCENDING.
    ELSE.
      SORT gt_report BY betrg_soix DESCENDING.
    ENDIF.

  ELSE.

    IF p_sasc EQ 'X'."Check if ascending or descending
      SORT gt_report BY betrg_hoix ASCENDING.
    ELSE.
      SORT gt_report BY betrg_hoix DESCENDING.
    ENDIF.

  ENDIF.


ENDFORM. "Sort_Table

*&---------------------------------------------------------------------*
*&      Form  File_Download
*&---------------------------------------------------------------------*
*       Downloads file to local presentation server
*----------------------------------------------------------------------*

FORM file_download.

  DATA: lv_filen TYPE ibipparms-path,
        lv_file  TYPE string.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
    IMPORTING
      file_name     = lv_filen.


  IF sy-subrc EQ 0 AND lv_filen IS NOT INITIAL.

    lv_file = lv_filen.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                  = lv_file
        filetype                  = 'ASC'
        trunc_trailing_blanks     = 'X'
        trunc_trailing_blanks_eol = space
      CHANGING
        data_tab                  = gt_file
      EXCEPTIONS
        file_write_error          = 1
        no_batch                  = 2
        gui_refuse_filetransfer   = 3
        invalid_type              = 4
        no_authority              = 5
        unknown_error             = 6
        header_not_allowed        = 7
        separator_not_allowed     = 8
        filesize_not_allowed      = 9
        header_too_long           = 10
        dp_error_create           = 11
        dp_error_send             = 12
        dp_error_write            = 13
        unknown_dp_error          = 14
        access_denied             = 15
        dp_out_of_memory          = 16
        disk_full                 = 17
        dp_timeout                = 18
        file_not_found            = 19
        dataprovider_exception    = 20
        control_flush_error       = 21
        not_supported_by_gui      = 22
        error_no_gui              = 23
        OTHERS                    = 24.

    IF sy-subrc NE 0.
      WRITE: 'Error downloading file to presentation server: ', lv_file.
      STOP.
    ENDIF.

  ELSE.

    IF sy-subrc NE 0.
      WRITE: 'Error downloading file to presentation server'.
      STOP.
    ENDIF.

  ENDIF.

ENDFORM. "File_Download
