tables sscrfields.

*-----------------------------------------------------------------------
* General Options
selection-screen begin of block general with frame title text-prg.
select-options s_uifref for t5w0p-uifref.
parameter p_txyr like t5w70-taxyr obligatory memory id gjr.
parameter p_month like t5w70-uifmonth obligatory memory id wacm.
parameter p_testf like rpcxxxwx-test default 'X'.
parameter p_banka like pc261-void default 'X'.

selection-screen end of block general.

*-----------------------------------------------------------------------
selection-screen skip 1.

parameters out_par like pri_params no-display.

selection-screen pushbutton /1(40) temp_pri user-command para.

at selection-screen output.

   if pnpbegda is initial.
     pnptimr1 = 'X'.
     pnptimr6 = space.
   endif.

  if out_par-pdest is initial.
     get parameter id 'SPOOL_DEV' field out_par-pdest. "Spool Device
  endif.

  temp_pri = text-pri.
  shift temp_pri by 4 places right.
  if out_par-pdest is initial.
*    write icon_enter_more as icon to temp_pri+0(4).
    CALL FUNCTION 'ICON_CREATE'                           "ACC
      EXPORTING
        name = 'ICON_ENTER_MORE'
        text = text-pri
        info = text-pri
        add_stdinf = 'X'
      IMPORTING
        result = temp_pri.
  else.
*    write icon_display_more as icon to temp_pri+0(4).
    CALL FUNCTION 'ICON_CREATE'                           "ACC
      EXPORTING
        name = 'ICON_DISPLAY_MORE'
        text = text-pri
        info = text-pri
        add_stdinf = 'X'
      IMPORTING
        result = temp_pri.
  endif.

*-----------------------------------------------------------------------
at selection-screen on value-request for p_txyr.

  data returnval like ddshretval occurs 1 with header line.
  data dynpprog like sy-repid.
  data dynpnr  like sy-dynnr.

  dynpprog = sy-repid.
  dynpnr  = sy-dynnr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield               = 'p_txyr'
      value_ORG              = 'S'
      dynpprog                = dynpprog
      dynpnr                 =  dynpnr
      dynprofield            = 'P_TXYR'
    TABLES
      value_tab              = itax_year
      return_tab             = returnval
    EXCEPTIONS
      others                 = 1.

*-----------------------------------------------------------------------
 at selection-screen on value-request for s_uifref-low.

 data: begin of uifref_tab occurs 0,
         uifref like t5w0p-uifref,
       end of uifref_tab.
 data: l_uifref(9) type n.
 data: begin of helptab occurs 0,
          uifref like t5w0p-uifref,
        end of helptab.
 data: returnval like ddshretval occurs 1 with header line.
 data dynpprog like sy-repid.
 data dynpnr  like sy-dynnr.

 dynpprog = sy-repid.
 dynpnr  = sy-dynnr.

perform gen_org_tab.          "SAG1144106

  loop at org_tab.            "SAG1144106
    helptab-uifref = org_tab-uifrf.
    collect helptab.
    uifref_tab-uifref = org_tab-uifrf.
    collect uifref_tab.
  endloop.

 sort helptab.

call function 'F4IF_INT_TABLE_VALUE_REQUEST'
  exporting
    retfield               = 'UIFREF-LOW'
    value_org              = 'S'
    dynpprog                = dynpprog
    dynpnr                 =  dynpnr
    dynprofield            = 'UIFREF-LOW'
  tables
    value_tab              = helptab
    return_tab             = returnval
  exceptions
    others                 = 1.

*-----------------------------------------------------------------------
 at selection-screen on value-request for s_uifref-high.

 data: begin of uifref_tab occurs 1,
         uifref like t5w0p-uifref,
       end of uifref_tab.
 data: l_uifref(9) type n.
 data: begin of helptab occurs 0,
          uifref like t5w0p-uifref,
        end of helptab.
 data: returnval like ddshretval occurs 1 with header line.
 data dynpprog like sy-repid.
 data dynpnr  like sy-dynnr.

 dynpprog = sy-repid.
 dynpnr  = sy-dynnr.

 perform gen_org_tab.        "SAG1144106

  loop at org_tab.           "SAG1144106
    helptab-uifref = org_tab-uifrf.
    collect helptab.
    uifref_tab-uifref = org_tab-uifrf.
    collect uifref_tab.
  endloop.

 sort helptab.

call function 'F4IF_INT_TABLE_VALUE_REQUEST'
  exporting
    retfield               = 'UIFREF-HIGH'
    value_org              = 'S'
    dynpprog               = dynpprog
    dynpnr                 = dynpnr
    dynprofield            = 'UIFREF-HIGH'
  tables
    value_tab              = helptab
    return_tab             = returnval
  exceptions
    others                 = 1.

*-----------------------------------------------------------------------
at selection-screen.

* Check Month is Valid

  if p_month < '01' OR
     p_month > '12'.
     message e275 with p_month.
  endif.

  read table itax_year with key p_txyr.
  if NOT sy-subrc IS INITIAL.
     message e276 with p_txyr.
  endif.

  if out_par-pdest is initial.
     get parameter id 'SPOOL_DEV' field out_par-pdest. "Spool Device
  endif.

  if sscrfields-ucomm = 'PARA'         "Print Parameters for Certificate
  or ( out_par-pdest is initial and h_para = 0 ).

    check sy-ucomm = 'ONLI'
       or sy-ucomm = 'PARA'
       or sy-ucomm = 'SJOB'.

    h_para = 1.

    if out_par-prtxt is initial.
      out_par-prtxt = text-spu.
    endif.

    in_par = out_par.

* Define OUT_PAR print parameters for printed certificates
    call function 'GET_PRINT_PARAMETERS'
         exporting
              destination    = out_par-pdest
              immediately    = space
              line_count     = c_max_linct
              line_size      = c_max_linsz
              list_text      = out_par-prtxt
              new_list_id    = on
              layout         = 'X_65_80'
              release        = off
         importing
              out_parameters = out_par
              valid          = pri_valid.

    if pri_valid <> true.
      out_par = in_par.
    endif.

    set parameter id 'SPOOL_DEV' field out_par-pdest. "Spool Device

    if out_par-pdest is initial.
      message w260.         "Printer for status report
    endif.
  endif.

*-----------------------------------------------------------------------
at line-selection.

  get cursor field cursorf.
  case cursorf.
    when 'STAT-SPONO'.
      perform get_spool using stat-repsp.
    when 'STAT-TEMSN'.
      submit rpcuivw0 with tsobj = stat-temsn and return.
    when others.
      call function 'HR_DISPLAY_ERROR_LONGTEXT'.
  endcase.
