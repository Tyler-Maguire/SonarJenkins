*&---------------------------------------------------------------------*
*& Program Title : Delek Benefit Focus
*&
*&---------------------------------------------------------------------*
*&  Create Date   : March 03, 2019
*&  Author        :
*&---------------------------------------------------------------------*
REPORT z_delek_bf.

TYPES:

  BEGIN OF ty_header,
    record_type(1)  TYPE c, "Record type identifier
    client_name(50) TYPE c, "Client name identifier
    create_date(8)  TYPE c, "Create date identifier

  END OF ty_header,

  BEGIN OF ty_footer,
    record_type(1)   TYPE c, "Record type
    record_count(20) TYPE c, "Record count
  END OF ty_footer,

  BEGIN OF ty_rec,
    emp_id(8)         TYPE c, "Employee Number , PA0002-pernr
    betrg(7)          TYPE c, "Employee Cost Per Pay Period ,PA0014-betrg
    emper_cppp(7)     TYPE c, "Employer Cost Per Pay Period
    imp_cost(7)       TYPE c, "Imputed Cost
    eff_start_date(8) TYPE c, "Effective start date PA0014-begda
    eff_end_date(8)   TYPE c, "Effective end date   PA0014-endada
    wage_type(4)      TYPE c, "Wage type , PA0014-lgart
  END OF ty_rec,

  BEGIN OF ty_rec_hr,

    pernr TYPE p_pernr, "Employee Number , PA0002-pernr
    betrg TYPE betrg, "Employee Cost Per Pay Period ,PA0014-betrg
    ecppp TYPE z_ecppp, "Employer Cost Per Pay Period
    imcst TYPE z_imcst, "Imputed Cost
    begda TYPE begda, "Effective start date PA0014-begda
    endda TYPE endda, "Effective end date   PA0014-endada
    lgart TYPE lgart, "Wage type , PA0014-lgart

  END OF ty_rec_hr,


  BEGIN OF ty_0000,    "Internal table type PA0000
    pernr TYPE persno, "Employee personnel number
    stat2 TYPE stat2,  "Employee status
    begda TYPE begda,  "Record begin date
  END OF ty_0000,

  BEGIN OF ty_0001,    "Internal table type PA0001
    pernr TYPE persno, "Employee personnel number
    bukrs TYPE bukrs,  "Employee company code
    abkrs TYPE abkrs,  "Employee payroll area
    begda TYPE begda,  "Record begin date
  END OF ty_0001,

  BEGIN OF ty_0002,    "Internal table type PA0002
    pernr TYPE persno, "Employee personnel number
    vorna TYPE vorna,  "Employee first name
    nachn TYPE nachn,  "Employee last name
    perid TYPE prdni,  "Employee social security number
    begda TYPE begda,  "Record begin date
  END OF ty_0002,


  BEGIN OF ty_error,
    record_type    TYPE string, "Record type identifier
    client_name    TYPE string, "Region
    emp_id         TYPE string, "Employee ID
    wage_type      TYPE string, "Wage type
    eff_start_date TYPE string, "Effective start date
    eff_end_date   TYPE string, "Effective end date
    error_mess     TYPE string, "Error message
  END OF ty_error,

  BEGIN OF ty_emails, "Type for email selection
    email(60) TYPE c,
  END OF ty_emails.


"Value Key 'wage_type,process code,deduction type' process code: 2 = Employee Cost Per Pay Period, 3 = Employer Cost Per Pay Period,
"Value Key 4 = Imputed Cost. deduction type: CRE = Credit (Memo), PRE = Pre-Tax, POS = Post-Tax, IMP = Imputed Income.

CONSTANTS:  gc_wt01 TYPE string VALUE '3004,2', "Dental
            gc_wt02 TYPE string VALUE '3007,2', "Vision
            gc_wt03 TYPE string VALUE '3401,4', "Basic Life
            gc_wt04 TYPE string VALUE '3308,2', "Supp Life
            gc_wt05 TYPE string VALUE '3309,2', "Spouse Life
            gc_wt06 TYPE string VALUE '3307,2', "Child Life
            gc_wt07 TYPE string VALUE '3302,2', "ADD Employee
            gc_wt08 TYPE string VALUE '3313,2', "ADD Child
            gc_wt09 TYPE string VALUE '3303,2', "ADD Spouse
            gc_wt10 TYPE string VALUE '3315,2', "STD Buy-Up
            gc_wt11 TYPE string VALUE '3312,2', "LTD Buy-Up
            gc_wt12 TYPE string VALUE '3305,2', "HCFSA
            gc_wt13 TYPE string VALUE '3009,2', "DCFSA
            gc_wt14 TYPE string VALUE '3008,2', "Heath SA EE
            gc_wt15 TYPE string VALUE '9111,2', "LPFSA
            gc_wt16 TYPE string VALUE '7008,3', "Heath SA ER
            gc_wt17 TYPE string VALUE '3451,2', "Delek Fund For Hope
            gc_wt18 TYPE string VALUE '3006,2', "Medical Employee Paid
            gc_wt19 TYPE string VALUE '7001,3'. "Medical Employer Paid



DATA:         it_0000            TYPE TABLE OF ty_0000, "Internal table for pa0000 data
              it_0001            TYPE TABLE OF ty_0001, "Internal table for pa0001 data
              it_0002            TYPE TABLE OF ty_0002, "Internal table for pa0002 data
              gt_t549a           TYPE TABLE OF t549a, "Table for PERMO field
              gt_t549q           TYPE TABLE OF t549q, "Table for payroll periods records
              wa_header          TYPE ty_header,        "Work area for header
              wa_footer          TYPE ty_footer,       "Work area for trailer
              wa_rec             TYPE ty_rec,           "Work area for record
              gt_rec             TYPE TABLE OF ty_rec,  "Table for records

              wa_error           TYPE ty_error, "Work area for errors
              gt_error           TYPE TABLE OF ty_error, "Table of errors

              wa_pa0014          TYPE p0014, "Work area for pa0041 creation
              it_0014            TYPE TABLE OF pa0014, "Table for PA0014
              wa_pa0000          TYPE pa0000, "Work area for pa0000 creation
              wa_pa0001          TYPE pa0001, "Work area for pa0001 creation
              wa_pa0002          TYPE pa0002, "Work area for pa0002 creation

              wa_emails          TYPE ty_emails, "Structure for emails

              gv_server_path     TYPE string, "File path for application server path
              gv_archive_path    TYPE string, "File path for archive server path
              it_dir             TYPE TABLE OF user_dir, "Internal table for user directories

              gv_total_records   TYPE integer VALUE 0, "Total records read
              gv_error_records   TYPE integer VALUE 0, "Total amount of errors
              gv_success_records TYPE integer VALUE 0, "Total amount of processed records
              gv_skipped_lines   TYPE integer VALUE 0, "Amount of lines skipped due to inability to process

              gv_skipped_list    TYPE string, "List of skipped lines in file
              gt_warnings        TYPE table_of_strings, "Table of warning errors
              gt_fatal           TYPE table_of_strings, "Table of fatal errors


              it_filename        TYPE filetable, "Internal table for filenames
              it_file            TYPE table_of_strings, "Internal table for file content
              ld_et_return       TYPE hrpadinpem1,
              ld_iv_create       TYPE flag,
              lt_dtab            TYPE TABLE OF char100,
              it_data            TYPE kcde_intern,
              lv_filename        TYPE string,
              it_record_hr       TYPE TABLE OF ty_rec_hr,
              gv_time_stamp      TYPE string, "Current time stamp for file
              lt_utab            TYPE TABLE OF char200.




FIELD-SYMBOLS: <fs_0001> TYPE ty_0001,
               <fs_0002> TYPE ty_0002,
               <fs_dtab> TYPE char100,
               <fs_rec>  TYPE ty_rec.


************************************************************************
* CONSTANTS
************************************************************************

CONSTANTS:  gc_tab                    TYPE c VALUE cl_bcs_convert=>gc_tab,
            gc_crlf                   TYPE c VALUE cl_bcs_convert=>gc_crlf,
            gc_numeric_characters(11) TYPE c VALUE '1234567890 '.

************************************************************************
* CONSTANTS (Client specific)
************************************************************************

DATA:  "Email configuration
  gc_subject_fatal     TYPE so_obj_des VALUE 'File Failure: INT005 : Loan Feedback', "Subject line for email notifications no errors
  gc_subject_errors    TYPE so_obj_des VALUE 'Record Failure: : INT005 : Loan Feedback', "Subject line for email notifications with fatal errors
  gc_subject_success   TYPE so_obj_des VALUE 'Success: INT005 : Loan Feedback ', "Subject line for email notifications record errrors
  gc_email_flag        TYPE bool VALUE 'X', "Flag for emails on or off

  "Systems configuration
  gc_prod_sysid        TYPE string VALUE 'X4O', "Production system ID

  "Al11 configuration
  gc_server_alias      TYPE string VALUE 'DIR_FIDELITY', "Alias for server inbound files
  gc_archive_alias     TYPE string VALUE 'DIR_FIDELITY', "Alias for archive files
  gc_server_folders    TYPE string VALUE '/loanrepay', "Any additional folders within server path (leave blank if using root)
  gc_archive_folders   TYPE string VALUE '/loanrepay/archive', "Any additional folders within archive path (leave blank if using root)

  "File upload configuration
  gc_presentation_flag TYPE bool VALUE 'X', "Presentation upload available in production if on
  gc_sftp_flag         TYPE bool VALUE '', "SFTP available if on

  "Records configuration
  gc_rec01_flag        TYPE bool VALUE 'X'. "Process rec01 records


************************************************************************
* SELECTION SCREEN
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK criteria
                            WITH FRAME TITLE text-001.

SELECT-OPTIONS:   p_pernr     FOR wa_pa0002-pernr NO INTERVALS MATCHCODE OBJECT prem,  "Personnel number filter
                  p_emp_s     FOR wa_pa0000-stat2 NO INTERVALS,                        "Employee status filter
                  p_c_code    FOR wa_pa0001-bukrs NO INTERVALS,                        "Company code filter
                  p_pay_a     FOR wa_pa0001-abkrs NO INTERVALS.                        "Payroll area filter

SELECTION-SCREEN END OF BLOCK criteria.

SELECTION-SCREEN BEGIN OF BLOCK file
                            WITH FRAME TITLE text-002.

PARAMETERS:   p_file_a RADIOBUTTON GROUP gr_f DEFAULT 'X' USER-COMMAND gr1,  "Application server file
              p_file_p RADIOBUTTON GROUP gr_f MODIF ID prt,                  "Presentation server file
              p_file_s RADIOBUTTON GROUP gr_f MODIF ID sft,                  "SFTP server file
              p_file_n TYPE string LOWER CASE MODIF ID fn,                   "File name
              p_file_l TYPE string LOWER CASE.                               "Log path override

SELECTION-SCREEN END OF BLOCK file.

SELECTION-SCREEN BEGIN OF BLOCK sftp
                            WITH FRAME TITLE text-003 .

PARAMETERS:   p_path TYPE string LOWER CASE MODIF ID sf, "SFTP file path
              p_user TYPE string LOWER CASE MODIF ID sf, "SFTP user ID
              p_pass TYPE string LOWER CASE MODIF ID sf. "SFTP password

SELECTION-SCREEN END OF BLOCK sftp.

SELECTION-SCREEN BEGIN OF BLOCK email
                            WITH FRAME TITLE text-004.

SELECT-OPTIONS: p_emails FOR wa_emails-email NO INTERVALS MODIF ID em. "SFTP file path

SELECTION-SCREEN END OF BLOCK email.

SELECTION-SCREEN BEGIN OF BLOCK test
                            WITH FRAME TITLE text-005.

PARAMETERS: p_test AS CHECKBOX. "SFTP file path

SELECTION-SCREEN END OF BLOCK test.

************************************************************************
* AT SELECTION SCREEN
************************************************************************

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    "Mask password on selection screen
    IF screen-name = 'P_PASS'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.

    "Hide SFTP block if not using SFTP server
    IF p_file_s NE 'X' OR gc_sftp_flag NE 'X'.

      IF screen-group1 = 'SF'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      "Hide the SFTP radio option if the flag is off
      IF screen-group1 = 'SFT' AND gc_sftp_flag NE 'X'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

    "Hide presentation in production if flag not on
    IF gc_presentation_flag NE 'X' AND sy-sysid = gc_prod_sysid.

      IF screen-group1 = 'PRT'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

    "Hide emails if emails not in use
    IF gc_email_flag NE 'X'.

      IF screen-group1 = 'EM'.
        screen-active = '0'.
        screen-invisible = '1'.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

    ENDIF.

  ENDLOOP.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file_n.

  "Open file dialog for local file path
  IF p_file_a EQ 'X'.
    PERFORM al11_file.
  ENDIF.

  "Open file dialog for application file path
  IF p_file_p EQ 'X'.
    PERFORM file_dialog.
  ENDIF.



************************************************************************
* INITIALIZATION
************************************************************************

INITIALIZATION.


  "Set application path based on alias
  SELECT dirname aliass INTO CORRESPONDING FIELDS OF TABLE it_dir FROM user_dir.

  "Set alias for inbound files
  LOOP AT it_dir ASSIGNING FIELD-SYMBOL(<fs_dir>) WHERE aliass = gc_server_alias.
    gv_server_path = <fs_dir>-dirname.
    EXIT.
  ENDLOOP.
  "Catch error reading path
  IF sy-subrc NE 0.
    APPEND 'Error reading server path for inbound alias' TO gt_fatal.
    PERFORM log_errors.
    STOP.
  ENDIF.

  "Set alias for archiving
  LOOP AT it_dir ASSIGNING FIELD-SYMBOL(<fs_dir2>) WHERE aliass = gc_archive_alias.
    gv_archive_path = <fs_dir2>-dirname.
    EXIT.
  ENDLOOP.

  "Catch error reading alias
  IF sy-subrc NE 0.
    APPEND 'Error reading server path for archive alias' TO gt_fatal.
    PERFORM log_errors.
    STOP.
  ENDIF.

************************************************************************
* START OF SELECTION
************************************************************************

START-OF-SELECTION.

************************************************************************
* INTERNAL TABLE LOAD
************************************************************************

  "Load internal table data for pa0000
  SELECT pernr stat2 INTO CORRESPONDING FIELDS OF TABLE it_0000 FROM pa0000 WHERE begda < sy-datum AND endda > sy-datum.
  SORT it_0000 BY begda DESCENDING.

  "Load internal table data for pa0001
  SELECT pernr bukrs abkrs INTO CORRESPONDING FIELDS OF TABLE it_0001 FROM pa0001 WHERE begda < sy-datum AND endda > sy-datum.
  SORT it_0001 BY begda DESCENDING.

  "Load internal table data for p0002
  SELECT pernr vorna nachn perid INTO CORRESPONDING FIELDS OF TABLE it_0002 FROM pa0002 WHERE begda < sy-datum AND endda > sy-datum.
  SORT it_0002 BY begda DESCENDING.

  "Read data from table T549Q
  SELECT * INTO TABLE gt_t549q FROM t549q.

  "Read data from table T549A
  SELECT * INTO TABLE gt_t549a FROM t549a.

  "Load internal table data for p0169 only latest records
  SELECT * INTO TABLE it_0014 FROM pa0014.
  SORT it_0014 BY begda DESCENDING.



*&---------------------------------------------------------------------*
*&  FORM split_records.
*&---------------------------------------------------------------------*
*   Splits file data into records for processing
*----------------------------------------------------------------------*

FORM split_records.

  FIELD-SYMBOLS: <fs_file> TYPE string.

  DATA: lv_line_count TYPE integer VALUE 0.


  LOOP AT it_file ASSIGNING <fs_file>.

    CLEAR lv_line_count.

    lv_line_count = strlen( <fs_file> ).

    IF lv_line_count = '60'.

      CLEAR: wa_header.

      wa_header-record_type    = <fs_file>(1).
      wa_header-client_name    = <fs_file>+1(50).
      wa_header-create_date   = <fs_file>+51(8).



    ELSEIF lv_line_count = '22'.

      CLEAR: wa_footer.

      wa_footer-record_type      = <fs_file>(1).
      wa_footer-record_count       = <fs_file>+1(20).

      CLEAR: wa_rec.

    ELSE.


      SPLIT <fs_file> AT ',' INTO TABLE lt_dtab.


      LOOP AT lt_dtab ASSIGNING <fs_dtab>.

        CASE sy-tabix.

          WHEN 1.
            wa_rec-emp_id = <fs_dtab>.
          WHEN 2.
            wa_rec-betrg = <fs_dtab>.
          WHEN 3.
            wa_rec-emper_cppp = <fs_dtab>.
          WHEN 4.
            wa_rec-imp_cost = <fs_dtab>.
          WHEN 5.
            wa_rec-eff_start_date = <fs_dtab>.
          WHEN 6.
            wa_rec-eff_end_date = <fs_dtab>.
          WHEN 7.
            wa_rec-wage_type = <fs_dtab>.

        ENDCASE.
      ENDLOOP.

    ENDIF.



    IF wa_rec IS INITIAL.

      APPEND 'Error, no records on file received.' TO gt_fatal.

    ENDIF.

    APPEND wa_rec TO gt_rec.

    gv_total_records = gv_total_records + 1.

  ENDLOOP.

  IF gv_total_records EQ 0.
    APPEND 'Error, no records on file received.' TO gt_fatal.
    PERFORM log_errors.
    STOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM update_records
*&---------------------------------------------------------------------*
*   Handles any errors that occur
*----------------------------------------------------------------------*

FORM update_records.

  DATA: lv_amt        TYPE string,
        lv_ret        TYPE bapireturn1,
        lv_date       TYPE begda,
        lv_error_mess TYPE string,
        lv_error      TYPE bool VALUE '',
        lv_p0014      TYPE p0014,
        lv_endda      TYPE p0001-endda,
        lv_file_date  TYPE datum.

  FIELD-SYMBOLS: <fs_t549a> TYPE t549a,
                 <fs_t549q> TYPE t549q.

  IF wa_header IS INITIAL.
    APPEND 'No header on file, dates cannot be determined' TO gt_fatal.
    STOP.
  ENDIF.

  lv_file_date = wa_header-create_date.

  IF gc_rec01_flag EQ 'X'.

    LOOP AT gt_rec ASSIGNING <fs_rec>.

      CLEAR: wa_pa0014.
      CLEAR: lv_p0014.
      CLEAR: lv_error.


      "No data in emp_id field found on file
      IF <fs_rec>-emp_id = '' OR <fs_rec>-emp_id CN gc_numeric_characters.
        lv_error_mess = 'Employee ID missing on file or invalid'.
        PERFORM error_handle USING lv_error_mess '01'.
        ADD 1 TO gv_error_records."Update number of error records
        CONTINUE. "Countinue loop without doing anything
      ENDIF.

      LOOP AT it_0002 ASSIGNING <fs_0002> WHERE pernr EQ <fs_rec>-emp_id.
        EXIT.
      ENDLOOP.

      "No matching personnel number was found
      IF sy-subrc NE 0.
        CONCATENATE 'Employee' <fs_rec>-emp_id 'personnel number missing in system' INTO lv_error_mess SEPARATED BY ' '.
        PERFORM error_handle USING lv_error_mess '01'.
        ADD 1 TO gv_error_records."Update number of error records
        CONTINUE. "Countinue loop without doing anything
      ENDIF.

      LOOP AT it_0002 ASSIGNING <fs_0002> WHERE pernr EQ <fs_rec>-emp_id.
        EXIT.
      ENDLOOP.



      wa_pa0014-pernr = <fs_rec>-emp_id.


      IF <fs_rec>-wage_type EQ ''.
        CONCATENATE 'Employee' <fs_rec>-emp_id 'Wage type missing on file' INTO lv_error_mess SEPARATED BY ' '.
        PERFORM error_handle USING lv_error_mess '01'.
        lv_error = 'X'.
      ENDIF.


      wa_pa0014-aedtm = sy-datum. "Changed on date

      wa_pa0014-uname = sy-uname. "Changed by user

      wa_pa0014-betrg = <fs_rec>-betrg.

      wa_pa0014-endda = <fs_rec>-eff_end_date.

      wa_pa0014-begda = <fs_rec>-eff_start_date.





      wa_pa0014-lgart = <fs_rec>-wage_type. "Wage type

      READ TABLE it_0001 ASSIGNING <fs_0001> WITH KEY pernr = wa_pa0014-pernr.

      IF sy-subrc NE 0.
        lv_error_mess = 'Employee is missing a valid organisational assignment (pa0001) record.'.
        PERFORM error_handle USING lv_error_mess '01'.
        ADD 1 TO gv_error_records."Update number of error records
        CONTINUE. "Countinue loop without doing anything
      ENDIF.



      "Check for previous record in internal table
      READ TABLE it_0014 ASSIGNING FIELD-SYMBOL(<fs_0014>) WITH KEY pernr = wa_pa0014-pernr lgart = wa_pa0014-lgart endda = '99991231'.

      "If previous record has been found, we need to delimit first
      IF sy-subrc EQ 0 AND <fs_0014> IS ASSIGNED.


        MOVE-CORRESPONDING <fs_0014> TO lv_p0014.

        DATA: lv_end   TYPE endda,
              lv_begin TYPE begda.

        lv_end = lv_p0014-endda.
        lv_begin = lv_p0014-begda.

        lv_endda = wa_pa0014-begda - 1.


        DATA: gt_p0014 TYPE TABLE OF p0014.

        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            pernr     = lv_p0014-pernr
            infty     = '0014'
            begda     = lv_p0014-begda
            endda     = lv_p0014-endda
          TABLES
            infty_tab = gt_p0014.

        READ TABLE gt_p0014 INTO lv_p0014 WITH KEY subty = wa_pa0014-subty.

        "Enqueue employee for processing
        CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
          EXPORTING
            number = lv_p0014-pernr.

        IF sy-subrc NE 0.
          CONCATENATE 'Employee' <fs_rec>-emp_id 'was unable to enqueue for processing' INTO lv_error_mess SEPARATED BY ' '.
          PERFORM error_handle USING lv_error_mess '01'.
          ADD 1 TO gv_error_records."Update number of error records
          CONTINUE.
        ENDIF.

        lv_p0014-endda = lv_endda.

        "Clear the error return
        CLEAR: lv_ret.


        "Delimit entry in infotype pa0014
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '0014'
            number        = lv_p0014-pernr
            validityend   = lv_end
            validitybegin = lv_p0014-begda
            record        = lv_p0014
            operation     = 'MOD'
            tclas         = 'A'
            dialog_mode   = '0'
            nocommit      = p_test
          IMPORTING
            return        = lv_ret.
        .
        IF sy-subrc NE 0 OR lv_ret IS NOT INITIAL.
          CONCATENATE 'Employee' <fs_rec>-emp_id 'was unable to delimit infotype pa0014: ' lv_ret-message INTO lv_error_mess SEPARATED BY ' '.
          PERFORM error_handle USING lv_error_mess '01'.
          ADD 1 TO gv_error_records."Update number of error records
          CONTINUE.
        ENDIF.

        "Dequeue employee after processing
        CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = wa_pa0014-pernr.

      ELSE.

        CONCATENATE 'No prior record exists for given loan ending 12/31/9999 for wagetype ' wa_pa0014-lgart INTO lv_error_mess RESPECTING BLANKS.
        PERFORM error_handle USING lv_error_mess '01'.
        ADD 1 TO gv_error_records."Update number of error records
        CONTINUE.

      ENDIF.




      IF lv_error EQ ''.

        "Enqueue employee for processing
        CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
          EXPORTING
            number = wa_pa0014-pernr.

        IF sy-subrc NE 0.
          CONCATENATE 'Employee' <fs_rec>-emp_id 'was unable to enqueue for processing' INTO lv_error_mess SEPARATED BY ' '.
          PERFORM error_handle USING lv_error_mess '01'.
          ADD 1 TO gv_error_records."Update number of error records
          CONTINUE.
        ENDIF.

        "Clear the error return
        CLEAR: lv_ret.

        "Insert new entry into infotype pa0014
        CALL FUNCTION 'HR_INFOTYPE_OPERATION'
          EXPORTING
            infty         = '0014'
            number        = wa_pa0014-pernr
            validityend   = wa_pa0014-endda
            validitybegin = wa_pa0014-begda
            record        = wa_pa0014
            operation     = 'INS'
            tclas         = 'A'
            dialog_mode   = '0'
            nocommit      = p_test
          IMPORTING
            return        = lv_ret.
        .
        IF sy-subrc NE 0 OR lv_ret IS NOT INITIAL.
          CONCATENATE 'Employee' <fs_rec>-emp_id 'was unable to update infotype pa0014: ' lv_ret-message INTO lv_error_mess SEPARATED BY ' '.
          PERFORM error_handle USING lv_error_mess '01'.
          ADD 1 TO gv_error_records."Update number of error records
        ELSE.
          "Update number of successful records
          ADD 1 TO gv_success_records.
        ENDIF.

        "Dequeue employee after processing
        CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
          EXPORTING
            number = wa_pa0014-pernr.

      ELSE.



      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM error_handle.
*&---------------------------------------------------------------------*
*   Handles any errors that occur
*----------------------------------------------------------------------*

FORM error_handle USING p_error TYPE string p_rec TYPE string.

  "Add 01 error record
  IF p_rec EQ '01'.

    CLEAR: wa_error.

    IF gt_error IS INITIAL.

      CLEAR: wa_error.

      "Add error message headers
      wa_error-client_name          = 'Client Name'.
      wa_error-eff_end_date         = 'Effective End Date'.
      wa_error-eff_start_date       = 'Effective Start Date'.
      wa_error-wage_type            = 'Wage Type'.
      wa_error-record_type          = 'Record Type Identifier'.
      wa_error-emp_id               = 'Employee ID'.
      wa_error-error_mess           = 'Error Message'.

      APPEND wa_error TO gt_error.

    ENDIF.

    CLEAR: wa_error.

    "Add error data
    wa_error-client_name          = wa_header-client_name.
    wa_error-eff_end_date         = <fs_rec>-eff_end_date.
    wa_error-eff_start_date       = <fs_rec>-eff_start_date.
    wa_error-wage_type            = <fs_rec>-wage_type.
    wa_error-record_type          = wa_header-record_type.
    wa_error-emp_id               = <fs_rec>-emp_id.
    wa_error-error_mess           = p_error.

    APPEND wa_error TO gt_error.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM log_errors.
*&---------------------------------------------------------------------*
*   Logs errors using error table to given emails
*----------------------------------------------------------------------*

FORM log_errors.

  DATA: lv_convert TYPE string,
        lv_line    TYPE string,
        lv_body    TYPE bcsy_text.

  "Add body for email text

  CONCATENATE 'The Loans Feedback file was received and processed in the ECP system from Fidelity on ' sy-datum+4(2) '/' sy-datum+6(2) '/' sy-datum(4) ' at '
    sy-uzeit(2) ':' sy-uzeit+2(2) ':' sy-uzeit+4(2) '.' INTO lv_line RESPECTING BLANKS.
  APPEND lv_line TO lv_body.

  CLEAR: lv_line.
  APPEND lv_line TO lv_body.

  IF gv_error_records GT 0 OR gv_skipped_lines GT 0.

    "Actual errors have occured that did not stop the interface from running

    APPEND 'File processing statistics shown below:' TO lv_body.

    APPEND lv_line TO lv_body.

    lv_convert = gv_total_records.
    CONCATENATE 'Number of records received:' lv_convert INTO lv_line SEPARATED BY ' '.
    APPEND lv_line TO lv_body.

    lv_convert = gv_error_records.
    CONCATENATE 'Number of error records received:' lv_convert INTO lv_line SEPARATED BY ' '.
    APPEND lv_line TO lv_body.

    lv_convert = gv_success_records.
    CONCATENATE 'Number of records processed:' lv_convert INTO lv_line SEPARATED BY ' '.
    APPEND lv_line TO lv_body.

    IF gv_skipped_lines GT 0.

      CLEAR: lv_line.
      APPEND lv_line TO lv_body.

      "Show number of lines which have been skipped due to incorrect line length
      lv_convert = gv_skipped_lines.
      CONCATENATE 'Number of lines with incorrect length:' lv_convert INTO lv_line SEPARATED BY ' '.
      APPEND lv_line TO lv_body.

      "Show lines which have been skipped due to incorrect line length
      CONCATENATE 'Line numbers (space separated):' gv_skipped_list INTO lv_line SEPARATED BY ' '.
      APPEND lv_line TO lv_body.

    ENDIF.

    CLEAR: lv_line.
    APPEND lv_line TO lv_body.

    CONCATENATE 'Please see file in AL11: ' gv_archive_path '/LOANS_REC01_ERROR_REPORT_' gv_time_stamp '.csv, for more information.' INTO lv_line RESPECTING BLANKS.

    APPEND lv_line TO lv_body.

  ELSEIF gt_fatal IS NOT INITIAL.

    "An error has occured which needs to stop the interface from running

    APPEND 'A fatal error has occured and the interface needed to be stopped. No records were proccessed.' TO lv_body.

  ELSEIF gt_warnings IS NOT INITIAL.

    "All processed, but warnings have occured

    APPEND 'All records have been processed, but some warnings have been recorded.' TO lv_body.

  ELSE.

    lv_convert = gv_success_records.
    CONCATENATE lv_convert ' records were processed without error.' INTO lv_line RESPECTING BLANKS.

    APPEND lv_line TO lv_body.

  ENDIF.

  "Append any warning error messages to the email
  IF gt_warnings IS NOT INITIAL.

    CLEAR: lv_line.
    APPEND lv_line TO lv_body.

    APPEND 'Warning errors:' TO lv_body.

    CLEAR: lv_line.
    APPEND lv_line TO lv_body.

    LOOP AT gt_warnings ASSIGNING FIELD-SYMBOL(<fs_warning>).
      APPEND <fs_warning> TO lv_body.
    ENDLOOP.

  ENDIF.

  "Append any fatal error messages to the email
  IF gt_fatal IS NOT INITIAL.

    CLEAR: lv_line.
    APPEND lv_line TO lv_body.

    APPEND 'Fatal errors:' TO lv_body.

    CLEAR: lv_line.
    APPEND lv_line TO lv_body.

    LOOP AT gt_fatal ASSIGNING FIELD-SYMBOL(<fs_fatal>).
      APPEND <fs_fatal> TO lv_body.
    ENDLOOP.

  ENDIF.

  "Archives the error table to AL11 for future viewing or potential email failure
  PERFORM archive_error.

  "Check if in test mode
  IF p_test EQ 'X'.

    IF gv_error_records GT 0.

      "If in test mode display errors in ALV instead of email
      PERFORM display_alv CHANGING gt_error wa_error.

    ENDIF.

  ELSE.

    IF gt_error IS NOT INITIAL OR gt_fatal IS NOT INITIAL OR gt_warnings IS NOT INITIAL.

      "If not test mode send email provided system is production system
      PERFORM send_mail USING lv_body.

    ENDIF.

  ENDIF.

  "Show error statistics
  LOOP AT lv_body INTO lv_line.

    SET BLANK LINES ON.
    WRITE: / lv_line.
    WRITE: /.
    SET BLANK LINES OFF.

  ENDLOOP.

  "Blank line to split additional messages
  SET BLANK LINES ON.
  WRITE: /.
  SET BLANK LINES OFF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  FORM display_alv.
*&---------------------------------------------------------------------*
*   Display errors inside of ALV
*----------------------------------------------------------------------*

FORM display_alv CHANGING gt_error TYPE STANDARD TABLE lv_wa_error TYPE ANY.

  DATA: lv_alv        TYPE REF TO cl_salv_table, "ALV table
        lv_functions  TYPE REF TO cl_salv_functions_list, "ALV functions
        lv_columns    TYPE REF TO cl_salv_columns, "ALV Columns attributes
        lv_column     TYPE REF TO cl_salv_column_table, "ALV Columns details
        lv_struct     TYPE REF TO cl_abap_structdescr, "Structure description
        lt_comp       TYPE abap_component_tab, "Component table
        ls_comp       TYPE abap_componentdescr, "Component work area
        lv_field_name TYPE LVC_FNAME, "Name of field to access
        lv_short      TYPE SCRTEXT_S, "Short name of field
        lv_med        TYPE SCRTEXT_M, "Medium name of field
        lv_long       TYPE SCRTEXT_L. "Long name of field


  IF gt_error IS NOT INITIAL.

  TRY.

    "Create the ALV
    cl_salv_table=>factory( IMPORTING r_salv_table = lv_alv CHANGING t_table = gt_error ).

  CATCH cx_salv_msg.
    WRITE: / 'Error in creating ALV for display, please seek technical assistance.'.

  ENDTRY.

  "Add funciton buttons to ALV
  lv_functions = lv_alv->get_functions( ).
  lv_functions->set_all( abap_true ).

  "Optimise column width for ALV
  lv_columns = lv_alv->get_columns( ).
  lv_columns->set_optimize( 'X' ).

  "Get structure of the error table
  lv_struct ?= cl_abap_typedescr=>describe_by_data( lv_wa_error ).

  "Get the components of an error line
  lt_comp = lv_struct->get_components( ).

  "Read the header line for the error table
  READ TABLE gt_error INTO lv_wa_error INDEX 1.

  "Loop over all fields to add field names to table
  LOOP AT lt_comp INTO ls_comp.

  "Add headers to ALV table
  TRY.

    "Assign the header value of the field to each size
    ASSIGN COMPONENT ls_comp-name OF STRUCTURE lv_wa_error TO FIELD-SYMBOL(<fs_value>).
    IF <fs_value> IS ASSIGNED.
      lv_long = <fs_value>.
      lv_med = <fs_value>.
      lv_short = <fs_value>.
    ENDIF.

    UNASSIGN <fs_value>.

    "Field name string to LVC_FNAME type conversion
    lv_field_name = ls_comp-name.

    "Convert column attributes to column details and set texts for fields
    lv_column ?= lv_columns->get_column( lv_field_name ).
    lv_column->set_visible( if_salv_c_bool_sap=>true ).
    lv_column->set_long_text( lv_long ).
    lv_column->set_medium_text( lv_med ).
    lv_column->set_short_text( lv_short ).

  CATCH cx_salv_not_found.
    WRITE: / 'Error in creating ALV for display, please seek technical assistance.'.
  CATCH cx_salv_existing.
    WRITE: / 'Error in creating ALV for display, please seek technical assistance.'.
  CATCH cx_salv_data_error.
    WRITE: / 'Error in creating ALV for display, please seek technical assistance.'.

  ENDTRY.

  ENDLOOP.

  "Delete the header data
  DELETE gt_error INDEX 1.

  "Display ALV with errors
  lv_alv->display( ).

  ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM al11_file.
*&---------------------------------------------------------------------*
*   Opens a dialog for selecting an AL11 file
*----------------------------------------------------------------------*

FORM al11_file.

  DATA: lv_file TYPE ibipparms-path,
        lv_path TYPE ibipparms-path.

  lv_path = gv_server_path.

  "Get filename from application server selection
  CALL FUNCTION 'F4_FILENAME_SERVER'
    EXPORTING
      pfad              = lv_path
    IMPORTING
      file_name         = lv_file
    EXCEPTIONS
      no_file_on_server = 1
      OTHERS            = 2.

  "Merge server path and file name
  CONCATENATE gv_server_path '/' lv_file INTO p_file_n.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM upload_file.
*&---------------------------------------------------------------------*
*   Uploads the file from a local pc
*----------------------------------------------------------------------*

FORM upload_file.

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename = p_file_n
      filetype = 'ASC'
    CHANGING
      data_tab = it_file.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM archive_error.
*&---------------------------------------------------------------------*
*   Archives an error file into AL11
*----------------------------------------------------------------------*

FORM archive_error.

  DATA: lv_filename TYPE STRING,
        lv_line     TYPE STRING.

  IF gc_rec01_flag EQ 'X'.

  "Archive errors for record 01
  CONCATENATE gv_archive_path '/LOANS_REC01_ERROR_REPORT_' gv_time_stamp '.csv' INTO lv_filename.

  OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING UTF-8.

  IF sy-subrc EQ 0.

     LOOP AT gt_error ASSIGNING FIELD-SYMBOL(<fs_error>).

     CLEAR: lv_line.

       DO.
          ASSIGN COMPONENT sy-index OF STRUCTURE <fs_error> TO FIELD-SYMBOL(<fs_field>).
          IF sy-subrc NE 0.
            EXIT.
          ELSE.

            IF sy-index NE 1.
              CONCATENATE  lv_line ',' <fs_field> INTO lv_line.
            ELSE.
              CONCATENATE  lv_line <fs_field> INTO lv_line.
            ENDIF.

          ENDIF.
       ENDDO.

       "Transfer line of gt_error to file
       TRANSFER lv_line TO lv_filename.

     ENDLOOP.

     CLOSE DATASET lv_filename.
  ELSE.
    APPEND 'Error archiving file rec01 to AL11.' TO gt_warnings.
  ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM file_dialog.
*&---------------------------------------------------------------------*
*   Opens a dialog for selecting a local file
*----------------------------------------------------------------------*

FORM file_dialog.

  DATA: lv_subrc TYPE i. "Return code for program

  "Open file to be used
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title     = 'Select a Loans file'
      default_filename = '*.txt'
      file_filter      = '*.txt'
    CHANGING
      file_table       = it_filename
      rc               = lv_subrc.

  LOOP AT it_filename INTO p_file_n.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM read_file.
*&---------------------------------------------------------------------*
*   Reads fixed position file into record from AL11
*----------------------------------------------------------------------*

FORM read_file.

  DATA: wa_line       TYPE string,
        obj_root      TYPE REF TO cx_root,
        lv_file_error TYPE string,
        lv_num_lines  TYPE i.

  OPEN DATASET p_file_n FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc NE 0.
    APPEND 'Error reading file from AL11.' TO gt_fatal.
    PERFORM log_errors.
    STOP.
  ELSE.

    DO.
      CLEAR: lv_file_error.

      TRY.
          READ DATASET p_file_n INTO wa_line.
        CATCH cx_sy_conversion_codepage      INTO obj_root.
          lv_file_error = obj_root->get_text( ).
        CATCH cx_sy_codepage_converter_init  INTO obj_root.
          lv_file_error = obj_root->get_text( ).
        CATCH cx_sy_file_authority           INTO obj_root.
          lv_file_error = obj_root->get_text( ).
        CATCH cx_sy_file_io                  INTO obj_root.
          lv_file_error = obj_root->get_text( ).
        CATCH cx_sy_file_open                INTO obj_root.
          lv_file_error = obj_root->get_text( ).
        CATCH cx_sy_pipe_reopen              INTO obj_root.
          lv_file_error = obj_root->get_text( ).
      ENDTRY.

      ADD 1 TO lv_num_lines.

      IF lv_file_error IS NOT INITIAL.
        APPEND 'Error reading file from AL11.' TO gt_fatal.
        PERFORM log_errors.
        STOP.
      ENDIF.

      IF sy-subrc EQ 0.
        APPEND wa_line TO it_file.
      ELSE.
        EXIT.
      ENDIF.

    ENDDO.

    CLOSE DATASET p_file_n.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  FORM send_mail.
*&---------------------------------------------------------------------*
*   Sends errors to people on mail chain if in production mode
*----------------------------------------------------------------------*

FORM send_mail USING p_body TYPE bcsy_text.

  DATA: lv_subject_line TYPE so_obj_des.

  "Check if there are email addresses, check if system is production and check if emails are on
  IF p_emails IS NOT INITIAL AND sy-sysid EQ gc_prod_sysid AND gc_email_flag EQ 'X'.

    DATA:  wa_email        LIKE LINE OF p_emails,
           lv_email        TYPE adr6-smtp_addr,

           lv_sent_to_all  TYPE os_boolean,

           lv_filename     TYPE string,
           lv_att_sub      TYPE sood-objdes,
           lt_att_head     TYPE soli_tab,
           lv_text_line    TYPE soli,
           lv_bin_content  TYPE solix_tab,
           lv_size         TYPE so_obj_len,

           lr_send_request TYPE REF TO cl_bcs,
           lr_recipient    TYPE REF TO if_recipient_bcs,
           lr_sender       TYPE REF TO cl_sapuser_bcs,
           lr_document     TYPE REF TO cl_document_bcs.

    TRY.

        "Create send request
        lr_send_request = cl_bcs=>create_persistent( ).

        "Sending email from
        lr_sender = cl_sapuser_bcs=>create( sy-uname ).

        "Add sender to send request
        CALL METHOD lr_send_request->set_sender
          EXPORTING
            i_sender = lr_sender.

        "Loop over list of emails in selection
        LOOP AT p_emails INTO wa_email.

          lv_email = wa_email-low.

          TRANSLATE lv_email TO LOWER CASE.

          lr_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

          "Add recipient to send request
          CALL METHOD lr_send_request->add_recipient
            EXPORTING
              i_recipient = lr_recipient
              i_express   = 'X'.

        ENDLOOP.

        IF gt_fatal IS NOT INITIAL.
          lv_subject_line = gc_subject_fatal.
        ELSEIF gv_error_records > 0.
          lv_subject_line = gc_subject_errors.
        ELSE.
          lv_subject_line = gc_subject_success.
        ENDIF.

        "Create email document
        lr_document = cl_document_bcs=>create_document(
                        i_type    = 'RAW'
                        i_text    = p_body
                        i_subject = lv_subject_line ).


        "Add document to send request
        CALL METHOD lr_send_request->set_document( lr_document ).

        "Send email
        CALL METHOD lr_send_request->send(
          EXPORTING
            i_with_error_screen = 'X'
          RECEIVING
            result              = lv_sent_to_all ).

        IF lv_sent_to_all NE 'X'.
          WRITE: 'Error in sending mail, email could not be sent to all recepients, please seek technical assistance.'.
        ENDIF.

        "Commit to send email
        COMMIT WORK.

        "Exception handling
      CATCH cx_bcs.
        WRITE: 'Error in sending mail, please seek technical assistance.'.

    ENDTRY.

  ENDIF.

ENDFORM.
