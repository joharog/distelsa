*&---------------------------------------------------------------------*
*& Include          ZMM_IQ09_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form GET_IQ09
*&---------------------------------------------------------------------*
FORM get_iq09.

  DATA:
    lt_selscreen TYPE TABLE OF rsparams,
    ls_selscreen TYPE rsparams,
    lr_data      TYPE REF TO data.

  IF s_werks IS NOT INITIAL.
    append_selscreen: 'WERK'  'S' 'I' 'BT' s_werks-low s_werks-high.    "Centro
  ENDIF.

  IF s_matnr IS NOT INITIAL.
    append_selscreen: 'MATNR' 'S' 'I' 'BT' s_matnr-low s_matnr-high.    "Material
  ENDIF.

  IF s_lgort IS NOT INITIAL.
    append_selscreen: 'SERNR' 'S' 'I' 'BT' s_lgort-low s_lgort-high.    "Número de serie
  ENDIF.

  IF s_lgort IS NOT INITIAL.
    append_selscreen: 'LAGER' 'S' 'I' 'BT' s_lgort-low s_lgort-high.    "Almacen
  ENDIF.

  IF s_sobkz IS NOT INITIAL.
    append_selscreen: 'SOBKZ' 'S' 'I' 'BT' s_sobkz-low s_sobkz-high.    "Stock especial
  ENDIF.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

  SUBMIT riequi21 WITH SELECTION-TABLE lt_selscreen AND RETURN EXPORTING LIST TO MEMORY.

  WAIT UP TO 1 SECONDS.

  IF <gt_data_iq09> IS ASSIGNED.
    UNASSIGN <gt_data_iq09>.
  ENDIF.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).

      ASSIGN lr_data->* TO <gt_data_iq09>.

      IF <gt_data_iq09> IS ASSIGNED.
        MOVE-CORRESPONDING <gt_data_iq09> TO gt_data.
      ENDIF.

    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Form UPDATE
*&---------------------------------------------------------------------*
FORM get_update.

  DATA lt_comp_mat TYPE TABLE OF ymqasm01_tb00001.

  SELECT * FROM ymqasm01_tb00001 INTO TABLE lt_comp_mat
    FOR ALL ENTRIES IN gt_data
    WHERE matnr EQ gt_data-matnr
      AND sernr EQ gt_data-sernr.
  IF sy-subrc EQ 0.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
      READ TABLE lt_comp_mat INTO DATA(ls_comp_mat) WITH KEY matnr = <fs_data>-matnr sernr = <fs_data>-sernr.
      IF sy-subrc EQ 0.
        MOVE: ls_comp_mat-smotor  TO <fs_data>-smotor,
              ls_comp_mat-spoliza TO <fs_data>-spoliza,
              ls_comp_mat-fpoliza TO <fs_data>-fpoliza.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form SHOW ALV
*&---------------------------------------------------------------------*
FORM show_alv.
  PERFORM alv_group.
  PERFORM alv_fieldcat.
  PERFORM alv_layout.
  PERFORM alv_display.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_GROUP
*&---------------------------------------------------------------------*
FORM alv_group.
  APPEND VALUE #( sp_group = 'A'
                  text     = TEXT-001 )
                  TO gt_slis_group.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_FIELDCAT
*&---------------------------------------------------------------------*
FORM alv_fieldcat.
  REFRESH gt_slis_fieldcat.

  APPEND VALUE #( "outputlen = 7
                  fieldname = 'WERK'
                  seltext_s = 'Centro'
                  seltext_l = 'Centro' )
                  TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 12
                  fieldname = 'MATNR'
                  seltext_l = 'Material')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 20
                  fieldname = 'SERNR'
                  seltext_s = 'Serie'
                  seltext_l = 'Numero de serie')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 10
                  fieldname = 'LAGER'
                  seltext_l = 'Almacen')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 12
                  fieldname = 'SOBKZ'
                  seltext_l = 'Stock especial')
                  TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 15
                  fieldname = 'SMOTOR'
                  seltext_l = 'Serial del motor')
                   TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 15
                  fieldname = 'SPOLIZA'
                  seltext_l = 'Serial de la poliza')
                   TO gt_slis_fieldcat.
  APPEND VALUE #( "outputlen = 15
                  fieldname = 'FPOLIZA'
                  seltext_l = 'Fecha de la poliza')
                   TO gt_slis_fieldcat.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_LAYOUT
*&---------------------------------------------------------------------*
FORM alv_layout.
  CLEAR gs_slis_layout.
  gs_slis_layout-colwidth_optimize   = 'X'.
  gs_slis_layout-zebra               = 'X'.
*  gs_slis_layout-box_fieldname       = 'CHECK'.
*  gs_slis_layout-get_selinfos        = 'X'.
*  gs_slis_layout-f2code              = 'BEAN' .
*  gs_slis_layout-confirmation_prompt = 'X'.
*  gs_slis_layout-key_hotspot         = 'X'.
*  gs_slis_layout-info_fieldname      = 'COL'.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM alv_display.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
      i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_slis_layout
      it_fieldcat              = gt_slis_fieldcat
      it_special_groups        = gt_slis_group
      i_save                   = 'X'
    TABLES
      t_outtab                 = gt_data.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form PF_STATUS
*&---------------------------------------------------------------------*
FORM pf_status USING extab TYPE slis_t_extab.
    SET PF-STATUS 'STANDARD'.
ENDFORM.
