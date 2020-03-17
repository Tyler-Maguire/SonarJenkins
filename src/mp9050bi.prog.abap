*&---------------------------------------------------------------------*
*& Report  MP9050BI                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

PROGRAM mp9050bi.

TABLES: p9050.

FORM batch_input USING VALUE(bi_fcode)
                       VALUE(bi_wplog).

  FIELD-SYMBOLS: <bi_wplog> TYPE p9050.
  ASSIGN bi_wplog TO <bi_wplog> CASTING.
  p9050 = <bi_wplog>.
  PERFORM fill_field(rhaltd00) USING
  'P9050-REDUN'
  p9050-redun.
**PERFORM FILL_FIELD(RHALTD00) USING 'P9050-DUMMY' P9050-DUMMY.

  PERFORM fill_okcode(rhaltd00) USING 'U'.

ENDFORM.
