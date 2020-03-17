*&---------------------------------------------------------------------*
*& Report  Z_ABAP_CALC_TESTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Z_ABAP_CALC_TESTS.


DATA: lv_Hours TYPE betrg.
DATA: lv_dec TYPE decfloat34.
DATA: lv_out TYPE string.



lv_Hours = 10 - 11.


lv_out = lv_Hours.

*CONCATENATE lv_dec ' stuff' INTO lv_out.

WRITE: lv_out.
