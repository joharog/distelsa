*&---------------------------------------------------------------------*
*& Include          ZMM_MONITOR_COOPS_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form GET_WB2R_BUSVOL
*&---------------------------------------------------------------------*
FORM get_wb2r_busvol.

  DATA:
    lt_selscreen TYPE TABLE OF rsparams,
    ls_selscreen TYPE rsparams,
    lr_data      TYPE REF TO data.

  IF s_vend IS NOT INITIAL.
    append_selscreen: 'SO_VEND'  'S' 'I' 'BT' s_vend-low s_vend-high.   "Proveedor
  ENDIF.

  IF s_num IS NOT INITIAL.
    append_selscreen: 'SO_NUM' 'S' 'I' 'BT' s_num-low s_num-high.             "Contrato de condiciones
  ENDIF.

  IF s_bukrs IS NOT INITIAL.
    append_selscreen: 'SO_BUKRS' 'S' 'I' 'BT' s_bukrs-low s_bukrs-high.       "Sociedad
  ENDIF.

  IF s_ekorg IS NOT INITIAL.
    append_selscreen: 'SO_EKORG' 'S' 'I' 'BT' s_ekorg-low s_ekorg-high.       "Organización compras
  ENDIF.

  IF s_ekgrp IS NOT INITIAL.
    append_selscreen: 'SO_EKGRP' 'S' 'I' 'BT' s_ekgrp-low s_ekgrp-high.       "Organización ventas
  ENDIF.

  IF s_date IS NOT INITIAL.
    append_selscreen: 'SO_DATE' 'S' 'I' 'BT' s_date-low s_date-high.    "Fecha de liquidación
  ENDIF.


  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).


  SUBMIT rwb2r_business_volume WITH SELECTION-TABLE lt_selscreen AND RETURN EXPORTING LIST TO MEMORY.

  WAIT UP TO 1 SECONDS.

  IF <fs_contract_data> IS ASSIGNED.
    UNASSIGN <fs_contract_data>.
  ENDIF.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).

      ASSIGN lr_data->* TO <fs_contract_data>.

      IF <fs_contract_data> IS ASSIGNED.
        MOVE-CORRESPONDING <fs_contract_data> TO gt_data.
      ENDIF.

    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Form GET_IQ09
*&---------------------------------------------------------------------*
FORM get_data.
  BREAK mqa_abap3.
  DATA: lr_bstkd      TYPE RANGE OF vbkd-bstkd,
        lr_salesorder TYPE RANGE OF i_salesorder-salesorder.

  DATA lt_sd_flow TYPE TABLE OF i_sddocumentmultilevelprocflow.
  DATA lv_dias_diferencia TYPE i.

  SELECT SINGLE * FROM zmm_dias_liq INTO @DATA(ls_dias).

  SELECT * FROM wcocoh INTO TABLE @DATA(lt_wcocoh)
    FOR ALL ENTRIES IN @gt_data
    WHERE num EQ @gt_data-num.
  IF sy-subrc EQ 0.

    lr_bstkd = VALUE #( FOR wa IN lt_wcocoh (
        sign   = 'I'
        option = 'EQ'
        low    = |{ wa-num ALPHA = OUT }| ) ).

    IF lr_bstkd IS NOT INITIAL.
      SELECT * FROM vbkd INTO TABLE @DATA(lt_vbkd)
          WHERE bstkd IN @lr_bstkd
            AND posnr NE '000000'.
      IF sy-subrc EQ 0.
        SELECT * FROM i_salesorder INTO TABLE @DATA(lt_salesorder)
          FOR ALL ENTRIES IN @lt_vbkd
          WHERE salesorder EQ @lt_vbkd-vbeln.
        IF sy-subrc EQ 0.

*          lr_salesorder = VALUE #( FOR wa2 IN lt_salesorder (
*              sign   = 'I'
*              option = 'EQ'
*              low    = |{ wa2-salesorder ALPHA = IN }| ) ).
*BREAK-POINT.
*          SELECT * FROM i_sddocumentmultilevelprocflow INTO TABLE @DATA(lt_multilevelflow).
*             FOR ALL ENTRIES IN @lt_salesorder
*            WHERE precedingdocument IN @lr_salesorder. "@lt_salesorder-salesorder.
*              AND PRECEDINGDOCUMENTCATEGORY EQ 'C'.

          SELECT * FROM vbfa INTO TABLE @DATA(lt_multilevelflow)
            FOR ALL ENTRIES IN @lt_salesorder
            WHERE vbelv EQ @lt_salesorder-salesorder
              AND vbtyp_v EQ 'C'.

*          SELECT * FROM i_sddocumentmultilevelprocflow INTO TABLE @DATA(lt_multilevelflow).
*            FOR ALL ENTRIES IN @lt_salesorder
*            WHERE precedingdocument in @lr_salesorder "@lt_salesorder-salesorder.
*              where precedingdocumentcategory EQ 'C'.
          IF sy-subrc EQ 0.
            SELECT * FROM i_billingdocument INTO TABLE @DATA(lt_billingdocument)
              FOR ALL ENTRIES IN @lt_multilevelflow
              WHERE billingdocument EQ @lt_multilevelflow-vbeln.
            IF sy-subrc EQ 0.

              SELECT * FROM i_billingdocumentitem INTO TABLE @DATA(lt_billingdocumentitem)
                FOR ALL ENTRIES IN @lt_billingdocument
                WHERE billingdocument EQ @lt_billingdocument-billingdocument.

              SELECT * FROM p_ro_saftbsadbelnr INTO TABLE @DATA(lt_prosaftbsadbelnr)
                FOR ALL ENTRIES IN @lt_billingdocument
                WHERE accountingdocument EQ @lt_billingdocument-billingdocument.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    BREAK mqa_abap3.
    LOOP AT lt_vbkd INTO DATA(ls_vbkd_t).
      READ TABLE gt_data INTO DATA(ls_data_t) WITH KEY num = ls_vbkd_t-bstkd.
      IF sy-subrc EQ 0.
        MOVE: ls_vbkd_t-posnr TO ls_data_t-posnr,
              ls_vbkd_t-vbeln TO ls_data_t-salesorder.
        APPEND ls_data_t TO gt_data_n.
      ENDIF.
      CLEAR: ls_data_t, ls_vbkd_t.

    ENDLOOP.

  ENDIF.

  BREAK mqa_abap3.

  LOOP AT gt_data_n ASSIGNING FIELD-SYMBOL(<fs_data>).

    READ TABLE lt_wcocoh INTO DATA(ls_wcocoh) WITH KEY num = <fs_data>-num.
    IF sy-subrc EQ 0.

      MOVE-CORRESPONDING ls_wcocoh TO <fs_data>.

*      ls_wcocoh-num = | { ls_wcocoh-num ALPHA = OUT } |.
*      CONDENSE ls_wcocoh-num.


*      LOOP AT lt_vbkd INTO DATA(ls_vbkd2) WHERE bstkd = ls_wcocoh-num.
*
*
*
*        APPEND <fs_data> TO gt_data.
*      ENDLOOP.

      READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = <fs_data>-salesorder posnr = <fs_data>-posnr. "bstkd = ls_wcocoh-num.
      IF sy-subrc EQ 0.


        READ TABLE lt_salesorder INTO DATA(ls_salesorder) WITH KEY salesorder = ls_vbkd-vbeln.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_salesorder TO <fs_data>.

          READ TABLE lt_multilevelflow INTO DATA(ls_multilevelflow) WITH KEY vbelv = ls_salesorder-salesorder posnv = <fs_data>-posnr.
          IF sy-subrc EQ 0.
            MOVE: ls_multilevelflow-vbeln TO <fs_data>-subsequentdocument.

            READ TABLE lt_billingdocument INTO DATA(ls_billingdocument) WITH KEY billingdocument = ls_multilevelflow-vbeln.
            IF sy-subrc EQ 0.

              READ TABLE lt_billingdocumentitem INTO DATA(ls_billingdocumentitem) WITH KEY billingdocument = ls_billingdocument-billingdocument
                                                                                           billingdocumentitem = <fs_data>-posnr.
              IF sy-subrc EQ 0.

              ENDIF.

              MOVE: ls_billingdocumentitem-billingdocumentdate TO <fs_data>-billingdocumentdate,
                    ls_billingdocumentitem-transactioncurrency TO <fs_data>-transactioncurrency,
                    ls_billingdocumentitem-netamount TO <fs_data>-totalnetamount,
                    ls_billingdocumentitem-taxamount TO <fs_data>-totaltaxamount.

              READ TABLE lt_prosaftbsadbelnr INTO DATA(ls_prosaftbsadbelnr) WITH KEY accountingdocument = ls_billingdocument-billingdocument.
              IF sy-subrc EQ 0.
                <fs_data>-comp_status = 'Compensada'.
              ELSE.
                <fs_data>-comp_status = 'No Compensada'.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.


    ENDIF.

* El resultado de la resta es el número de días
    lv_dias_diferencia = sy-datum - <fs_data>-settl_date.

    IF sy-datum >= <fs_data>-settl_date  AND sy-datum <= ls_dias-fecha.

      <fs_data>-light_ind = icon_green_light.

    ELSEIF lv_dias_diferencia <= ls_dias-dias.

      <fs_data>-light_ind = icon_yellow_light.

    ELSEIF sy-datum > <fs_data>-settl_date.

      <fs_data>-light_ind = icon_red_light.

    ENDIF.



  ENDLOOP.


ENDFORM.


*&---------------------------------------------------------------------*
*& Form UPDATE
*&---------------------------------------------------------------------*
FORM get_update.
*
*  DATA lt_comp_mat TYPE TABLE OF ymqasm01_tb00001.
*
*  SELECT * FROM ymqasm01_tb00001 INTO TABLE lt_comp_mat
*    FOR ALL ENTRIES IN gt_data
*    WHERE matnr EQ gt_data-matnr
*      AND sernr EQ gt_data-sernr.
*  IF sy-subrc EQ 0.
*
*    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
*      READ TABLE lt_comp_mat INTO DATA(ls_comp_mat) WITH KEY matnr = <fs_data>-matnr sernr = <fs_data>-sernr.
*      IF sy-subrc EQ 0.
*        MOVE: ls_comp_mat-smotor  TO <fs_data>-smotor,
*              ls_comp_mat-spoliza TO <fs_data>-spoliza,
*              ls_comp_mat-fpoliza TO <fs_data>-fpoliza.
*      ENDIF.
*    ENDLOOP.
*
*  ENDIF.

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

  APPEND VALUE #( fieldname = 'LIGHT_IND'
                  seltext_l = 'Semáforo')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'NUM'
                  seltext_l = 'Contrato de condición' )
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'VEND_OWNER'
                  seltext_l = 'Proveedor')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'BUKRS'
                  seltext_l = 'Sociedad')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKORG'
                  seltext_l = 'Organización de compras')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKGRP'
                  seltext_l = 'Grupo de compras')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'CONTRACT_TYPE'
                  seltext_l = 'Tipo contrato condición')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'CREATED_ON'
                  seltext_l = 'Fecha de creación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'DATE_FROM'
                  seltext_l = 'Inicio de validez')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'DATE_TO'
                  seltext_l = 'Fin de validez')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SETTL_DATE_TYPE'
                  seltext_l = 'Tipo de Fecha de liquidación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SETTL_DATE'
                  seltext_l = 'Fecha de liquidación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'MENGE'
                  seltext_l = 'Cantidad de liquidación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'WFKME'
                  seltext_l = 'Unidad liquidación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'WAERL'
                  seltext_l = 'Moneda')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'KSCHL_REBV'
                  seltext_l = 'REBV Vol.negocios rappel')
                  TO gt_slis_fieldcat.

*  APPEND VALUE #( fieldname = 'INDICATOR'
*                  seltext_l = 'Semáforo')
*                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SALESORDERTYPE'
                  seltext_l = 'Clase de documento')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'POSNR'
                  seltext_l = 'Pos.Pedido')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SALESORDER'
                  hotspot   = 'X'
                  seltext_l = 'Número de pedido')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'CREATIONDATE'
                  seltext_l = 'Fecha de creación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SALESORGANIZATION'
                  seltext_l = 'Organización de ventas')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'DISTRIBUTIONCHANNEL'
                  seltext_l = 'Canal de ventas')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SUBSEQUENTDOCUMENT'
                  hotspot   = 'X'
                  seltext_l = 'Numero de factura')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'BILLINGDOCUMENTDATE'
                  seltext_l = 'Fecha de factura')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'TRANSACTIONCURRENCY'
                  seltext_l = 'Moneda de factura')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'TOTALNETAMOUNT'
                  seltext_l = 'Monto neto factura')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'TOTALTAXAMOUNT'
                  seltext_l = 'Monto Impuesto')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'COMP_STATUS'
            seltext_l = 'Estatus de compensación')
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
      i_callback_program      = sy-repid
*     i_callback_top_of_page  = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'PF_STATUS'
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = gs_slis_layout
      it_fieldcat             = gt_slis_fieldcat
      it_special_groups       = gt_slis_group
      i_save                  = 'X'
    TABLES
      t_outtab                = gt_data_n.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form PF_STATUS
*&---------------------------------------------------------------------*
*FORM pf_status USING extab TYPE slis_t_extab.
*  SET PF-STATUS 'STANDARD'.
*ENDFORM.


FORM user_command USING r_ucomm LIKE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  BREAK mqa_abap3.
  CASE r_ucomm.
    WHEN '&IC1'. " Código de función estándar para doble clic o hotspot
      " rs_selfield-fieldname contiene el nombre de la columna clickeada (ej. 'VBELN')
      " rs_selfield-value contiene el valor de la celda clickeada (ej. '0012345678')

      IF rs_selfield-fieldname EQ 'SALESORDER'.
        SET PARAMETER ID 'AUN' FIELD rs_selfield-value.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.

      IF rs_selfield-fieldname EQ 'SUBSEQUENTDOCUMENT'.
*        rs_selfield-value = |{ rs_selfield-value ALPHA = OUT }|.
        SET PARAMETER ID 'VF' FIELD rs_selfield-value.
        CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
      ENDIF.
  ENDCASE.

ENDFORM.
