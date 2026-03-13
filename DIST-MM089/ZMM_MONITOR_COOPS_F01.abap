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
*  BREAK mqa_abap3.
  DATA: lr_bstkd      TYPE RANGE OF vbkd-bstkd,
        lr_salesorder TYPE RANGE OF i_salesorder-salesorder.

  DATA lt_sd_flow TYPE TABLE OF i_sddocumentmultilevelprocflow.
  DATA lv_dias_diferencia TYPE i.
  DATA lv_acumulado TYPE kwert.
  DATA lv_rate TYPE tcurr-ukurs.

  SELECT SINGLE * FROM zmm_dias_liq INTO @DATA(ls_dias).

  SELECT * FROM wcocoh INTO TABLE @DATA(lt_wcocoh)
    FOR ALL ENTRIES IN @gt_data
    WHERE num EQ @gt_data-num
      AND contract_type EQ 'COOP'.
  IF sy-subrc EQ 0.

    lr_bstkd = VALUE #( FOR wa IN lt_wcocoh (
        sign   = 'I'
        option = 'EQ'
        low    = |{ wa-num ALPHA = OUT }| ) ).

    SELECT * FROM but000 INTO TABLE @DATA(lt_but000)
      FOR ALL ENTRIES IN @lt_wcocoh
      WHERE partner EQ @lt_wcocoh-vend_owner.

    SELECT * FROM t001 INTO TABLE @DATA(lt_t001)
      FOR ALL ENTRIES IN @lt_wcocoh
      WHERE bukrs EQ @lt_wcocoh-bukrs.

    SELECT * FROM t024e INTO TABLE @DATA(lt_t024e)
      FOR ALL ENTRIES IN @lt_wcocoh
      WHERE ekorg EQ @lt_wcocoh-ekorg.

    SELECT * FROM t024 INTO TABLE @DATA(lt_t024)
      FOR ALL ENTRIES IN @lt_wcocoh
      WHERE ekgrp EQ @lt_wcocoh-ekgrp.

    SELECT * FROM konh INTO TABLE @DATA(lt_konh)
      FOR ALL ENTRIES IN @lt_wcocoh
      WHERE ccnum EQ @lt_wcocoh-num.
    IF sy-subrc EQ 0.
      SELECT * FROM konp INTO TABLE @DATA(lt_konp)
        FOR ALL ENTRIES IN @lt_konh
        WHERE knumh EQ @lt_konh-knumh.
    ENDIF.

    IF lr_bstkd IS NOT INITIAL.
      SELECT * FROM vbkd INTO TABLE @DATA(lt_vbkd)
          WHERE bstkd IN @lr_bstkd
            AND posnr NE '000000'.
      IF sy-subrc EQ 0.
        SELECT * FROM i_salesorder INTO TABLE @DATA(lt_salesorder)
          FOR ALL ENTRIES IN @lt_vbkd
          WHERE salesorder EQ @lt_vbkd-vbeln.
        IF sy-subrc EQ 0.

          SELECT * FROM i_salesorderitem INTO TABLE @DATA(lt_salesorderitem)
            FOR ALL ENTRIES IN @lt_salesorder
            WHERE salesorder EQ @lt_salesorder-salesorder.
          IF sy-subrc EQ 0.
            SELECT * FROM i_productgrouptext INTO TABLE @DATA(lt_productgrouptext)
              FOR ALL ENTRIES IN @lt_salesorderitem
              WHERE materialgroup EQ @lt_salesorderitem-materialgroup
                AND language EQ 'S'.
          ENDIF.

          SELECT * FROM vbfa INTO TABLE @DATA(lt_multilevelflow)
            FOR ALL ENTRIES IN @lt_salesorder
            WHERE vbelv EQ @lt_salesorder-salesorder
              AND vbtyp_v EQ 'C'.

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



    SORT: gt_data BY num,
          lt_wcocoh BY num.

    LOOP AT gt_data INTO DATA(ls_data).
      READ TABLE lt_wcocoh TRANSPORTING NO FIELDS WITH KEY num = ls_data-num.
      IF sy-subrc NE 0.
        DELETE gt_data WHERE num EQ ls_data-num.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_vbkd INTO DATA(ls_vbkd_t).
      READ TABLE gt_data INTO DATA(ls_data_t) WITH KEY num = ls_vbkd_t-bstkd.
      IF sy-subrc EQ 0.
        MOVE: ls_vbkd_t-posnr TO ls_data_t-posnr,
              ls_vbkd_t-vbeln TO ls_data_t-salesorder.
        APPEND ls_data_t TO gt_data_n.
      ENDIF.
      CLEAR: ls_data_t, ls_vbkd_t.
    ENDLOOP.

    LOOP AT lt_wcocoh INTO DATA(ls_wcocoh_x).
      READ TABLE gt_data_n TRANSPORTING NO FIELDS WITH KEY num = ls_wcocoh_x-num.   "Validar que no exista el contrato en la tabla final
      IF sy-subrc NE 0.
        READ TABLE gt_data INTO ls_data WITH KEY num = ls_wcocoh_x-num.  "Validar que si existan para agregar contratos
        IF sy-subrc EQ 0.
          APPEND VALUE #( num             = ls_data-num
                          settl_date_type = ls_data-settl_date_type
                          settl_date      = ls_data-settl_date
                          menge           = ls_data-menge
                          wfkme           = ls_data-wfkme
                          waerl           = ls_data-waerl
                          kschl_rebv      = ls_data-kschl_rebv )
                          TO gt_data_n.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  BREAK mqa_abap3.
  SORT gt_data_n BY num.
  LOOP AT gt_data_n ASSIGNING FIELD-SYMBOL(<fs_data>).

    CASE <fs_data>-settl_date_type.
      WHEN ''.
        <fs_data>-settl_date_type = 'Liquidación final'.
      WHEN 1.
        <fs_data>-settl_date_type = 'Liquidación parcial'.
      WHEN 2.
        <fs_data>-settl_date_type = 'Liquidación delta'.
      WHEN 3.
        <fs_data>-settl_date_type = 'Periodificaciones delta'.
      WHEN 4.
        <fs_data>-settl_date_type = 'Periodificaciones/compensación'.
      WHEN OTHERS.
    ENDCASE.

    READ TABLE lt_wcocoh INTO DATA(ls_wcocoh) WITH KEY num = <fs_data>-num.
    IF sy-subrc EQ 0.

      MOVE-CORRESPONDING ls_wcocoh TO <fs_data>.

      READ TABLE lt_but000 INTO DATA(ls_but000) WITH KEY partner = <fs_data>-vend_owner.
      IF sy-subrc EQ 0.
        <fs_data>-vend_name = |{ ls_but000-name_org1 } { ls_but000-name_org2 }|.
      ENDIF.

      READ TABLE lt_t001 INTO DATA(ls_t001) WITH KEY bukrs = <fs_data>-bukrs.
      IF sy-subrc EQ 0.
        MOVE ls_t001-butxt TO <fs_data>-butxt.
      ENDIF.

      READ TABLE lt_t024e INTO DATA(ls_t024e) WITH KEY ekorg = <fs_data>-ekorg.
      IF sy-subrc EQ 0.
        MOVE ls_t024e-ekotx TO <fs_data>-ekotx.
      ENDIF.

      READ TABLE lt_t024 INTO DATA(ls_t024) WITH KEY ekgrp = <fs_data>-ekgrp.
      IF sy-subrc EQ 0.
        MOVE ls_t024-eknam TO <fs_data>-eknam.
      ENDIF.

      READ TABLE lt_konh INTO DATA(ls_konh) WITH KEY ccnum = <fs_data>-num.
      IF sy-subrc EQ 0.
        READ TABLE lt_konp INTO DATA(ls_konp) WITH KEY knumh = ls_konh-knumh.
        IF sy-subrc EQ 0.
          MOVE: ls_konp-knumh TO <fs_data>-knumh,
                ls_konp-krech TO <fs_data>-krech,
                ls_konp-kpein TO <fs_data>-kpein.

          CASE <fs_data>-krech.
            WHEN 'C'. "Cantidad
              <fs_data>-kbetr = ls_konp-kbetr.
              IF <fs_data>-kpein EQ 1.
                <fs_data>-mto_fond_coop =  ls_konp-kbetr * <fs_data>-menge.
              ELSE.
                <fs_data>-mto_fond_coop = ( ls_konp-kbetr * <fs_data>-menge ) / <fs_data>-kpein.
              ENDIF.

            WHEN 'A'. "Porcentaje
              BREAK mqa_abap1.
              <fs_data>-kbetr = ls_konp-kbetr / 10.
              <fs_data>-mto_fond_coop = ( ls_konp-kbetr / 1000 ) * <fs_data>-kschl_rebv.

          ENDCASE.
        ENDIF.
      ENDIF.

      READ TABLE lt_vbkd INTO DATA(ls_vbkd) WITH KEY vbeln = <fs_data>-salesorder posnr = <fs_data>-posnr. "bstkd = ls_wcocoh-num.
      IF sy-subrc EQ 0.

        READ TABLE lt_salesorder INTO DATA(ls_salesorder) WITH KEY salesorder = ls_vbkd-vbeln.
        IF sy-subrc EQ 0.
          MOVE-CORRESPONDING ls_salesorder TO <fs_data>.

          SELECT SINGLE bezei
            INTO @<fs_data>-sddocumentreason_t
            FROM tvaut
           WHERE augru = @<fs_data>-sddocumentreason
             AND spras = @sy-langu.

          READ TABLE lt_salesorderitem INTO DATA(ls_salesorderitem) WITH KEY salesorder = ls_salesorder-salesorder.
          IF sy-subrc EQ 0.
            READ TABLE lt_productgrouptext INTO DATA(ls_productgrouptext) WITH KEY materialgroup = ls_salesorderitem-materialgroup.
            IF sy-subrc EQ 0.
              MOVE: ls_productgrouptext-materialgroupname TO <fs_data>-materialgroupname,
                    ls_productgrouptext-materialgrouptext TO <fs_data>-materialgrouptext.
            ENDIF.
          ENDIF.

          READ TABLE lt_multilevelflow INTO DATA(ls_multilevelflow) WITH KEY vbelv = ls_salesorder-salesorder posnv = <fs_data>-posnr.
          IF sy-subrc EQ 0.
            MOVE: ls_multilevelflow-vbeln TO <fs_data>-subsequentdocument.

            READ TABLE lt_billingdocument INTO DATA(ls_billingdocument) WITH KEY billingdocument = ls_multilevelflow-vbeln.
            IF sy-subrc EQ 0.

              READ TABLE lt_billingdocumentitem INTO DATA(ls_billingdocumentitem) WITH KEY billingdocument = ls_billingdocument-billingdocument
                                                                                           billingdocumentitem = <fs_data>-posnr.
              IF sy-subrc EQ 0.
                MOVE: ls_billingdocumentitem-billingdocumentdate TO <fs_data>-billingdocumentdate,
                      ls_billingdocumentitem-transactioncurrency TO <fs_data>-transactioncurrency,
                      ls_billingdocumentitem-netamount TO <fs_data>-totalnetamount,
                      ls_billingdocumentitem-taxamount TO <fs_data>-totaltaxamount.
              ENDIF.

*            Si Moneda de la factura es diferente
              IF <fs_data>-transactioncurrency NE <fs_data>-cc_curr.
                clear: lv_rate.

                CALL FUNCTION 'READ_EXCHANGE_RATE'
                  EXPORTING
                    client           = sy-mandt
                    date             = ls_billingdocument-billingdocumentdate
                    foreign_currency = <fs_data>-cc_curr
                    local_currency   = <fs_data>-transactioncurrency
                    type_of_rate     = 'M'
                  IMPORTING
                    exchange_rate    = lv_rate.


                <fs_data>-totalnetamount = <fs_data>-totalnetamount / lv_rate. "ls_billingdocument-accountingexchangerate.
                <fs_data>-totaltaxamount = <fs_data>-totaltaxamount / lv_rate. "ls_billingdocument-accountingexchangerate.
              ENDIF.

              <fs_data>-monto_rest_liq  = <fs_data>-kschl_rebv - <fs_data>-totalnetamount.

              READ TABLE lt_prosaftbsadbelnr INTO DATA(ls_prosaftbsadbelnr) WITH KEY accountingdocument = ls_billingdocument-billingdocument.
              IF sy-subrc EQ 0.
                <fs_data>-comp_status = 'Compensada'.
              ELSE.
                <fs_data>-comp_status = 'No Compensada'.
              ENDIF.

            ENDIF.

          ELSE.
            CLEAR: <fs_data>-totalnetamount, <fs_data>-totaltaxamount.
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




*  Monto restante liquidado
    AT NEW num.
      lv_acumulado = <fs_data>-mto_fond_coop. "
*      CLEAR lv_acumulado.
    ENDAT.

    lv_acumulado = lv_acumulado - <fs_data>-totalnetamount.
    <fs_data>-monto_rest_liq = lv_acumulado.

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

  APPEND VALUE #( fieldname = 'KNUMH'
                  seltext_l = 'Nº reg.cond.' )
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'KRECH'
                  seltext_l = 'Regla cálculo' )
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'KBETR'
                  seltext_l = 'Importe' )
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'KPEIN'
                  seltext_l = 'UM de precio' )
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'VEND_OWNER'
                  seltext_l = 'Proveedor')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'VEND_NAME'
                  seltext_l = 'Nombre proveedor')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'BUKRS'
                  seltext_l = 'Sociedad')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'BUTXT'
                  seltext_l = 'Nombre sociedad')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKORG'
                  seltext_l = 'Organización de compras')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKOTX'
                  seltext_l = 'Denominación Org.C')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKGRP'
                  seltext_l = 'Grupo de compras')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'EKNAM'
                  seltext_l = 'Denominación Grp.C')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'MATERIALGROUPNAME'
                  seltext_l = 'Grupo de artículos')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'MATERIALGROUPTEXT'
                  seltext_l = 'Descripción de grupo de artículos')
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

  APPEND VALUE #( fieldname = 'MONTO_REST_LIQ'
                  seltext_l = 'Monto restante liquidado')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'WFKME'
                  seltext_l = 'Unidad liquidación')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'CC_CURR'
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

  APPEND VALUE #( fieldname = 'SDDOCUMENTREASON'
                  seltext_l = 'Motivo pedido')
                  TO gt_slis_fieldcat.

  APPEND VALUE #( fieldname = 'SDDOCUMENTREASON_T'
                  seltext_l = 'Descripcion Motivo pedido')
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

  APPEND VALUE #( fieldname = 'MTO_FOND_COOP'
                  seltext_l = 'Monto Fondo COOP')
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
*  BREAK mqa_abap3.
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
