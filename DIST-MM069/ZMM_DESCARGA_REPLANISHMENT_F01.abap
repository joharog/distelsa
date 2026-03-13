*&---------------------------------------------------------------------*
*& Include          ZMM_DESCARGA_REPLANISHMENT_F01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

  DATA: lr_locnr     TYPE RANGE OF wrf3-locnr,
        lt_return    TYPE TABLE OF bapireturn,
        ls_return    TYPE bapireturn,
        lv_material  TYPE matnr18,
        lv_plant     TYPE bapimatvp-werks,

        lt_wmdvsx    TYPE TABLE OF bapiwmdvs,
        lt_wmdvex    TYPE TABLE OF bapiwmdve,

        lt_items     TYPE TABLE OF bapiekpoc,

        lv_date      TYPE scal-date,
        lv_week      TYPE scal-week,

        lw_mver      TYPE mver,              " Work area for the loop
        lv_count     TYPE n LENGTH 2,        " Counter for the month number (01 to 52)
        lv_fieldname TYPE fieldname,     " To hold the dynamic field name
        lv_total1    TYPE mver-gsv01,
        lv_total8    TYPE mver-gsv08,
        lv_total52   TYPE mver-gsv13.        " Variable to store the running total (use a suitable type)


  DATA: lr_lfart   TYPE RANGE OF likp-lfart,
        lv_plan_nc TYPE lips-lfimg.

  APPEND VALUE #( sign   = 'E'
                  option = 'EQ'
                  low    = 'ZCRO' ) TO lr_lfart.
  APPEND VALUE #( sign   = 'E'
                  option = 'EQ'
                  low    = 'ZTRA' ) TO lr_lfart.
  APPEND VALUE #( sign   = 'E'
                  option = 'EQ'
                  low    = 'ZTRB' ) TO lr_lfart.
  APPEND VALUE #( sign   = 'E'
                  option = 'EQ'
                   low   = 'EL' ) TO lr_lfart.
  APPEND VALUE #( sign   = 'E'
                  option = 'EQ'
                  low    = 'EF' ) TO lr_lfart.

  FIELD-SYMBOLS: <lfs_gsv> TYPE any.     " Field symbol to dynamically assign fields

  DATA: lv_year_act   TYPE n LENGTH 4,
        lv_year_ant   TYPE n LENGTH 4,
        lv_week_act   TYPE i,
        lv_campo_num  TYPE n LENGTH 2,
        lv_field_name TYPE string,
        lt_mver       TYPE TABLE OF mver,
        ls_mver       TYPE mver.

  FIELD-SYMBOLS: <fs_valor> TYPE any.



  BREAK mqa_abap3.
  IF s_matnr IS NOT INITIAL.
    SELECT * FROM mara INTO TABLE @DATA(lt_mara)
      WHERE matnr IN @s_matnr
        AND matkl IN @s_matkl.
  ELSE.
    SELECT * FROM mara INTO TABLE lt_mara
      WHERE matkl IN s_matkl.
  ENDIF.

  IF sy-subrc EQ 0.
    SELECT * FROM makt INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_mara
      WHERE matnr EQ @lt_mara-matnr
        AND spras EQ @sy-langu.

    SELECT * FROM marc INTO TABLE @DATA(lt_marc)
      FOR ALL ENTRIES IN @lt_mara
      WHERE matnr EQ @lt_mara-matnr
        AND werks IN @s_werks
        AND dispo IN @s_dispo
        AND mmsta EQ ''.

    IF sy-subrc EQ 0.
      SELECT * FROM t001w INTO TABLE @DATA(lt_t001w)
        FOR ALL ENTRIES IN @lt_marc
        WHERE werks EQ @lt_marc-werks.

      lr_locnr = VALUE #( FOR wa IN lt_marc (
          sign   = 'I'
          option = 'EQ'
          low    = |{ wa-werks }| ) ).

      IF lr_locnr IS NOT INITIAL.
        SELECT * FROM wrf3 INTO TABLE @DATA(lt_wrf3) "Tomar PRIORITAET = 01 CUANDO EXISTAN VARIOS REG
          WHERE locnr IN @lr_locnr.
      ENDIF.

      BREAK mqa_abap3.
      SELECT * FROM mard INTO TABLE @DATA(lt_mard)
        FOR ALL ENTRIES IN @lt_wrf3
        WHERE matnr IN @s_matnr
          AND werks EQ @lt_wrf3-loclb
          AND lgort EQ '1001'.  "Pasar el campo a STVARV (hacerlo configurable)

      SELECT * FROM mard INTO TABLE @DATA(lt_mard_lus)  "LIBRE_UTIL_STORE
        WHERE matnr IN @s_matnr
          AND werks IN @s_werks
          AND lgort EQ '1001'.

    ENDIF.

  ENDIF.






  LOOP AT lt_marc INTO DATA(ls_marc).

    gs_data-werks = ls_marc-werks.
    gs_data-articulo = ls_marc-matnr.
    gs_data-store_transit = ls_marc-trame.
    gs_data-planning_feature = ls_marc-dismm.

    READ TABLE lt_t001w INTO DATA(ls_t001w) WITH KEY werks = ls_marc-werks.
    IF sy-subrc EQ 0.
      gs_data-cod_b1   = ls_t001w-name1.
      gs_data-werks_s4 = ls_t001w-name2.
    ENDIF.

    READ TABLE lt_mara INTO DATA(ls_mara) WITH KEY matnr = ls_marc-matnr.
    IF sy-subrc EQ 0.

      SELECT SINGLE brand_descr FROM wrf_brands_t INTO gs_data-marca
        WHERE brand_id EQ ls_mara-brand_id AND language EQ 'S'.

*      gs_data-marca = ls_mara-brand_id.
      gs_data-grupo_articulo = ls_mara-matkl.

      SELECT SINGLE vtext FROM t179t INTO gs_data-departamento
        WHERE prodh EQ ls_mara-matkl(3).

      SELECT SINGLE vtext FROM t179t INTO gs_data-tipo_producto
        WHERE prodh EQ ls_mara-matkl(5).

      SELECT SINGLE vtext FROM t179t INTO gs_data-familia
        WHERE prodh EQ ls_mara-matkl(7).

      SELECT SINGLE wgbez FROM t023t INTO gs_data-subfamilia
        WHERE matkl EQ ls_mara-matkl AND spras EQ 'S'.

      READ TABLE lt_makt INTO DATA(ls_makt) WITH KEY matnr = ls_mara-matnr.
      IF sy-subrc EQ 0.
        gs_data-art_descrip = ls_makt-maktx.
      ENDIF.

    ENDIF.

    READ TABLE lt_wrf3 INTO DATA(ls_wrf3) WITH KEY locnr = ls_marc-werks.
    IF sy-subrc EQ 0.
      IF ls_wrf3-prioritaet EQ '01'.

        READ TABLE lt_mard INTO DATA(ls_mard) WITH KEY werks = ls_wrf3-loclb matnr = ls_marc-matnr.
        IF sy-subrc EQ 0.
          gs_data-libre_util_cd    = ls_mard-labst.
        ENDIF.

      ENDIF.
    ENDIF.

    READ TABLE lt_mard_lus INTO DATA(ls_mard_lus) WITH KEY werks = gs_data-werks matnr = ls_marc-matnr.
    IF sy-subrc EQ 0.
      gs_data-libre_util_store = ls_mard_lus-labst.
    ENDIF.

    lv_material  = |{ ls_marc-matnr ALPHA = OUT }|.
    lv_material  = |{ lv_material ALPHA = IN }|.
    lv_plant = ls_wrf3-loclb.

*    REFRESH: lt_wmdvsx, lt_wmdvex.
*    CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
*      EXPORTING
*        plant      = lv_plant       "Centro de WRF3
*        material   = lv_material
*        unit       = 'UN'
*        check_rule = 'B'
*      IMPORTING
*        return     = ls_return
*      TABLES
*        wmdvsx     = lt_wmdvsx
*        wmdvex     = lt_wmdvex.

*    READ TABLE lt_wmdvex INTO DATA(ls_wmdvex) INDEX 1.
*    IF sy-subrc EQ 0.
*      gs_data-ventas_cd = ls_wmdvex-com_qty.
*    ENDIF.

    READ TABLE lt_wmdvsx INTO DATA(ls_wmdvsx) INDEX 1.
    IF sy-subrc EQ 0.
*      gs_data-stock_cd =  ls_wmdvsx-req_qty.
    ENDIF.

*    REFRESH: lt_wmdvsx, lt_wmdvex.
*    CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
*      EXPORTING
*        plant      = ls_marc-werks  "Centro de Marc
*        material   = lv_material
*        unit       = 'UN'
*        check_rule = 'B'
*      IMPORTING
*        return     = ls_return
*      TABLES
*        wmdvsx     = lt_wmdvsx
*        wmdvex     = lt_wmdvex.

*    READ TABLE lt_wmdvex INTO DATA(ls_wmdvex) INDEX 1.
*    IF sy-subrc EQ 0.
*      gs_data-sale_store = ls_wmdvex-com_qty.
*    ENDIF.

    READ TABLE lt_wmdvsx INTO ls_wmdvsx INDEX 1.
    IF sy-subrc EQ 0.
*      gs_data-stock_comp =  ls_wmdvsx-req_qty.    "Se mueve a logica LIKP+LIPS
    ENDIF.


* Stock comprometido | STOCK_COMP
    SELECT * FROM likp INTO TABLE @DATA(lt_likp_sc)
      WHERE vstel EQ @ls_wrf3-loclb
        AND gbstk EQ 'A'.
    IF sy-subrc EQ 0.
      DELETE lt_likp_sc WHERE lfart EQ 'LF'.
      DELETE lt_likp_sc WHERE lfart EQ 'EL'.

      SELECT * FROM lips INTO TABLE @DATA(lt_lips_sc)
        FOR ALL ENTRIES IN @lt_likp_sc
        WHERE vbeln EQ @lt_likp_sc-vbeln
         AND  matnr EQ @gs_data-articulo.
      IF sy-subrc EQ 0.
        LOOP AT lt_lips_sc INTO DATA(ls_likp_sc).
          gs_data-stock_comp += ls_likp_sc-lfimg.
        ENDLOOP.
      ENDIF.
    ENDIF.
    REFRESH: lt_likp_sc, lt_lips_sc.

*  Disponible para ventas | VENTAS_CD
    gs_data-ventas_cd =  gs_data-libre_util_cd - gs_data-stock_comp.



* Stock en curso | STOCK_CD
    REFRESH: lt_items, lt_return.
    CALL FUNCTION 'BAPI_PO_GETITEMS'
      EXPORTING
        material = lv_material "ls_marc-matnr
        plant    = lv_plant    "ls_wrf3-locnr
      TABLES
        po_items = lt_items
        return   = lt_return.
    LOOP AT lt_items INTO DATA(ls_items) WHERE no_more_gr EQ '' AND item_cat EQ '0'.
      gs_data-stock_cd += ls_items-disp_quan.
    ENDLOOP.

* Stock en curso tienda | STOCK_STORE
    REFRESH: lt_items, lt_return.
    CALL FUNCTION 'BAPI_PO_GETITEMS'
      EXPORTING
        material = lv_material
        plant    = gs_data-werks
      TABLES
        po_items = lt_items
        return   = lt_return.
    LOOP AT lt_items INTO ls_items WHERE no_more_gr EQ '' AND item_cat EQ '0'.
      gs_data-stock_store += ls_items-disp_quan.
    ENDLOOP.

*  Comprometidos por facturar | BILL_COMP
    SELECT * FROM likp INTO TABLE @DATA(lt_likp_bc)
      WHERE vstel EQ @gs_data-werks
        AND lfart IN @lr_lfart
        AND gbstk EQ 'A'.
    IF sy-subrc EQ 0.
      SELECT * FROM lips INTO TABLE @DATA(lt_lips_bc)
             FOR ALL ENTRIES IN @lt_likp_bc
             WHERE vbeln EQ @lt_likp_bc-vbeln
              AND  matnr EQ @gs_data-articulo.
      IF sy-subrc EQ 0.
        LOOP AT lt_lips_bc INTO DATA(ls_likp_bc).
          gs_data-bill_comp += ls_likp_bc-lfimg.
        ENDLOOP.
      ENDIF.
    ENDIF.
    REFRESH: lt_likp_bc, lt_lips_bc.


* Historial de Planificacion  PLAN_HISTORY
    SELECT * FROM likp INTO TABLE @DATA(lt_likp_ph)
      WHERE vstel EQ @ls_wrf3-loclb
        AND kunnr EQ @gs_data-werks
        AND lfart IN ('ZCRO', 'ZTRA')
        AND gbstk EQ 'A'.
    IF sy-subrc EQ 0.
      SELECT * FROM lips INTO TABLE @DATA(lt_lips_ph)
        FOR ALL ENTRIES IN @lt_likp_ph
        WHERE vbeln EQ @lt_likp_ph-vbeln
         AND  matnr EQ @gs_data-articulo.
      IF sy-subrc EQ 0.
        LOOP AT lt_lips_ph INTO DATA(ls_likp_ph).
          gs_data-plan_history += ls_likp_ph-lfimg.
        ENDLOOP.
      ENDIF.
    ENDIF.
    REFRESH: lt_likp_ph, lt_lips_ph.

* Historial de Planificacion NO CONTABILIZADAS
    SELECT * FROM likp INTO TABLE @DATA(lt_likp_nc)
      WHERE vstel EQ @gs_data-werks
        AND kunnr EQ @ls_wrf3-loclb
        AND lfart IN ('ZCRO', 'ZTRA', 'ZTRB')
        AND gbstk EQ 'A'.
    IF sy-subrc EQ 0.
      SELECT * FROM lips INTO TABLE @DATA(lt_lips_nc)
        FOR ALL ENTRIES IN @lt_likp_nc
        WHERE vbeln EQ @lt_likp_nc-vbeln
         AND  matnr EQ @gs_data-articulo.
      IF sy-subrc EQ 0.
        LOOP AT lt_lips_nc INTO DATA(ls_likp_nc).
          lv_plan_nc += ls_likp_nc-lfimg.
        ENDLOOP.
      ENDIF.
    ENDIF.
    REFRESH: lt_likp_nc, lt_lips_nc.

* Pendiente recepción disponible para venta | PENDING_SALE_STORE
    gs_data-pending_sale_store = gs_data-plan_history + gs_data-store_transit.


* Engresos = Comprometidos por facturar + lv_plan_nc| EXPENSES
    gs_data-expenses = gs_data-bill_comp + lv_plan_nc.


* Disponible venta Tienda(DV Tienda) | SALE_STORE
    gs_data-sale_store = gs_data-libre_util_store - gs_data-bill_comp.

* Ingresos = Comprometidos por facturar  + Disponible venta tienda | INCOME
*    gs_data-income = gs_data-bill_comp + gs_data-sale_store.
    gs_data-income = gs_data-plan_history + gs_data-store_transit. "Mismo calculo que pendiente recepcion disponible para venta

* Periódo ultimo pronostico (Semana Anterior)
    lv_date = sy-datum - 7.
    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    gs_data-forecast_lwp = lv_week.


* Valor del Pronostico Semana anterior
    SELECT SINGLE * FROM mapr INTO @DATA(ls_mapr)
      WHERE matnr EQ @ls_marc-matnr
        AND werks EQ @ls_marc-werks.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM prop INTO @DATA(ls_prop)
        WHERE pnum1 EQ @ls_mapr-pnum1
          AND hsnum EQ '00'.
      IF sy-subrc EQ 0.

        CLEAR: lv_date.

        CALL FUNCTION 'WEEK_GET_FIRST_DAY'
          EXPORTING
            week         = gs_data-forecast_lwp
          IMPORTING
            date         = lv_date
          EXCEPTIONS
            week_invalid = 1
            OTHERS       = 2.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        SELECT SINGLE * FROM prow INTO  @DATA(ls_prow)
           WHERE pnum2 EQ @ls_prop-pnum2
             AND ertag EQ @lv_date.
        IF sy-subrc EQ 0.
          gs_data-value_lwp = ls_prow-prwrt.
        ENDIF.

      ENDIF.

    ENDIF.

* Periódo pronostico (semana actual)
    CLEAR: lv_date, lv_week.
    lv_date = sy-datum.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    gs_data-forecast_awp = lv_week.


* Valor del Pronostico Semana actual
    CLEAR: lv_date.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = gs_data-forecast_awp
      IMPORTING
        date         = lv_date
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT SINGLE * FROM prow INTO ls_prow
       WHERE pnum2 EQ ls_prop-pnum2
         AND ertag EQ lv_date.
    IF sy-subrc EQ 0.
      gs_data-value_awp = ls_prow-prwrt.
    ENDIF.

* Periódo Pronostico (proxima semana)
    CLEAR: lv_date, lv_week.
    lv_date = sy-datum + 7.

    CALL FUNCTION 'DATE_GET_WEEK'
      EXPORTING
        date         = lv_date
      IMPORTING
        week         = lv_week
      EXCEPTIONS
        date_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    gs_data-forecast_nwp = lv_week.


* Valor del Pronostico Semana proxima
    CLEAR: lv_date.

    CALL FUNCTION 'WEEK_GET_FIRST_DAY'
      EXPORTING
        week         = gs_data-forecast_nwp
      IMPORTING
        date         = lv_date
      EXCEPTIONS
        week_invalid = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    SELECT SINGLE * FROM prow INTO ls_prow
       WHERE pnum2 EQ ls_prop-pnum2
         AND ertag EQ lv_date.
    IF sy-subrc EQ 0.
      gs_data-value_nwp = ls_prow-prwrt.
    ENDIF.


    BREAK mqa_abap1.

    REFRESH: lt_mver.
*---------------------------------------------------------------------------------------------
* LOGICA PARA 52 SEMANAS
*---------------------------------------------------------------------------------------------
    CLEAR: lv_year_act, lv_year_ant, lv_week_act, lv_campo_num, lv_field_name.

    " 1. Obtener año actual y año anterio
    lv_year_act = gs_data-forecast_awp(4).
    lv_year_ant = gs_data-forecast_awp(4) - 1.
    lv_week_act = gs_data-forecast_awp+4(2) - 1. " No se toma la semana actual

    " 2. Cargar registros de MVER de los años involucrados para evitar SELECTs en bucle
    " Traemos el año actual y el anterior para cubrir las 52 semanas
    SELECT * FROM mver INTO TABLE lt_mver
      WHERE werks EQ ls_marc-werks
        AND matnr EQ ls_marc-matnr
        AND gjahr IN ( lv_year_ant, lv_year_act )
        AND perkz = 'W'.

    " 3. Bucle para retroceder 52 semanas
    DO 52 TIMES.
      " Calcular en qué registro de los 4 anuales cae la semana
      DATA(lv_num_registro) = ( ( lv_week_act - 1 ) DIV 13 ) + 1.

      " Calcular el índice del campo MGV (01 a 13)
      lv_campo_num = ( ( lv_week_act - 1 ) MOD 13 ) + 1.
      CONCATENATE 'MGV' lv_campo_num INTO lv_field_name.

      " Buscar el registro correcto en nuestra tabla interna
      READ TABLE lt_mver INTO ls_mver WITH KEY gjahr = lv_year_act zahlr = lv_num_registro. "BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE ls_mver TO <fs_valor>.
        IF <fs_valor> IS ASSIGNED.
          gs_data-accu_history_52 += <fs_valor>.
          UNASSIGN <fs_valor>.
        ENDIF.
      ENDIF.

      " Retroceder la semana para la siguiente iteración
      lv_week_act = lv_week_act - 1.
      IF lv_week_act = 0.
        lv_year_act = lv_year_act - 1.
        lv_week_act = 52. " Ajustar si el año anterior tuvo 53 semanas

*        " Función para saber si el año anterior tuvo 52 o 53 semanas
*        CALL FUNCTION 'WEEKS_GET_LAST'
*          EXPORTING
*            i_year = lv_year
*          IMPORTING
*            e_week = lv_week.
      ENDIF.
    ENDDO.


*---------------------------------------------------------------------------------------------
* LOGICA PARA 8 SEMANAS
*---------------------------------------------------------------------------------------------
    CLEAR: lv_year_act, lv_year_ant, lv_week_act, lv_campo_num, lv_field_name.
    lv_year_act = gs_data-forecast_awp(4).
    lv_year_ant = gs_data-forecast_awp(4) - 1.
    lv_week_act = gs_data-forecast_awp+4(2) - 1. " No se toma la semana actual

    "Bucle para retroceder 8 Semanas
    DO 8 TIMES.
      " Calcular en qué registro de los 4 anuales cae la semana
      lv_num_registro = ( ( lv_week_act - 1 ) DIV 13 ) + 1.

      " Calcular el índice del campo MGV (01 a 13)
      lv_campo_num = ( ( lv_week_act - 1 ) MOD 13 ) + 1.
      CONCATENATE 'MGV' lv_campo_num INTO lv_field_name.

      " Buscar el registro correcto en nuestra tabla interna
      READ TABLE lt_mver INTO ls_mver WITH KEY gjahr = lv_year_act zahlr = lv_num_registro. "BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE ls_mver TO <fs_valor>.
        IF <fs_valor> IS ASSIGNED.
          gs_data-accu_history_8 += <fs_valor>.
          UNASSIGN <fs_valor>.
        ENDIF.
      ENDIF.

      " Retroceder la semana para la siguiente iteración
      lv_week_act = lv_week_act - 1.
      IF lv_week_act = 0.
        lv_year_act = lv_year_act - 1.
        lv_week_act = 8.

*        " Función para saber si el año anterior tuvo 52 o 53 semanas
*        CALL FUNCTION 'WEEKS_GET_LAST'
*          EXPORTING
*            i_year = lv_year
*          IMPORTING
*            e_week = lv_week.
      ENDIF.
    ENDDO.

*---------------------------------------------------------------------------------------------
* LOGICA PARA SEMANA ACTUAL
*---------------------------------------------------------------------------------------------
    CLEAR: lv_year_act, lv_year_ant, lv_week_act, lv_campo_num, lv_field_name.
    lv_year_act = gs_data-forecast_awp(4).
    lv_year_ant = gs_data-forecast_awp(4) - 1.
    lv_week_act = gs_data-forecast_awp+4(2).

    "Semana actual
    DO 1 TIMES.
      " Calcular en qué registro de los 4 anuales cae la semana
      lv_num_registro = ( ( lv_week_act - 1 ) DIV 13 ) + 1.

      " Calcular el índice del campo MGV (01 a 13)
      lv_campo_num = ( ( lv_week_act - 1 ) MOD 13 ) + 1.
      CONCATENATE 'MGV' lv_campo_num INTO lv_field_name.

      " Buscar el registro correcto en nuestra tabla interna
      READ TABLE lt_mver INTO ls_mver WITH KEY gjahr = lv_year_act zahlr = lv_num_registro. "BINARY SEARCH.
      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_field_name OF STRUCTURE ls_mver TO <fs_valor>.
        IF <fs_valor> IS ASSIGNED.
          gs_data-consump_aw += <fs_valor>.
          UNASSIGN <fs_valor>.
        ENDIF.
      ENDIF.
    ENDDO.

*   Pronostico por dia | FORESCAT_DAILY
    IF gs_data-value_awp IS NOT INITIAL.
*      gs_data-forecast_daily = gs_data-value_awp / 7.
      gs_data-forecast_daily = round( val  = gs_data-value_awp / 7
                                      dec  = 0
                                      mode = cl_abap_math=>round_half_up ).

    ENDIF.



    SELECT SINGLE * FROM wrpl INTO @DATA(ls_wrpl)
      WHERE matnr EQ @ls_marc-matnr
        AND kunnr EQ @ls_marc-werks.  "Validar si aplica con SRP_WERKS.
    IF sy-subrc EQ 0.
      gs_data-target_stock = ls_wrpl-sobst. "Stock Objetivo

      gs_data-target_stock_min  = ls_wrpl-prwug. "Stock Objetivo mínimo

      gs_data-target_stock_max = ls_wrpl-prwog. "Stock Objetivo máximo

      gs_data-target_coverage_days = ls_wrpl-trcov. "Cobertura en días

    ENDIF.

    gs_data-stock_security = ls_marc-eisbe. "Stock de seguridad








*   ID Frecuencia Entrega | DELIVERY_FREQUENCY_ID
    gs_data-delivery_frequency_id = ls_marc-mrppp. "


*   Descripcion de ID de Entrega |DESCRIPTION_DELIVERY_ID
    SELECT SINGLE * FROM t439h INTO @DATA(ls_t439h)
      WHERE spras EQ 'S'
        AND werks EQ @gs_data-werks
        AND mrppp EQ @ls_marc-mrppp.
    IF sy-subrc EQ 0.
      gs_data-description_delivery_id = ls_t439h-pptxt.
    ENDIF.


*  ID Ciclo de planificación |  PLANNING_CYCLE_ID
    gs_data-planning_cycle_id = ls_marc-lfrhy.


* Descripcion de ID Ciclo | DESCRIPTION_CYCLE_ID
    SELECT SINGLE * FROM t439h INTO ls_t439h
      WHERE spras EQ 'S'
        AND werks EQ gs_data-werks
        AND mrppp EQ ls_marc-lfrhy.
    IF sy-subrc EQ 0.
      gs_data-description_cycle_id = ls_t439h-pptxt.
    ENDIF.


*------------------------------------------------------------------------------
*--- Paso 1: Pronóstico_X_dia
*------------------------------------------------------------------------------
*  Si el resultado de Pronostico X Dia da un valor con decimales.
*  -Si el decimal es menor a 0.5, se redondea al entero inferior
*  -Si el decimal es mayor a 0.5, se redondea al entero superior

*   gs_data-forecast_daily

*------------------------------------------------------------------------------
*--- Paso 1: Determinar Stock Objetivo calculado
*------------------------------------------------------------------------------
*   Stock objetivo Calculado
    gs_data-stock_obj_cal = gs_data-forecast_daily * gs_data-target_coverage_days.


*   Si, el stock objetivo calculado es Mayor que el Stock Objetivo Maximo se descarta el resultado
*   y se toma para el paso 3, el valor del Stock Objetivo Maximo asignado al articulo en la tienda.
    IF gs_data-stock_obj_cal GT gs_data-target_stock_max.

      gs_data-stock_disponible = gs_data-target_stock_max.

*   Si, el stock objetivo calculado es Menor que el Stock Objetivo Minimo se descarta el resultado
*   y se toma para el paso 3, el valor del Stock Objetivo Minimo asignado al articulo en la tienda.
    ELSEIF gs_data-stock_obj_cal LT gs_data-target_stock_min.

      gs_data-stock_disponible = gs_data-target_stock_min.

*   Si, el stock objetivo calculado es Menor que el Stock Objetivo Maximo y Mayor Stock Objetivo Minimo
*   asignado al articulo en la tienda, se toma el resultado del Stock Objetivo Calculado para el paso 3.
    ELSEIF gs_data-stock_obj_cal GT gs_data-target_stock_max AND gs_data-stock_obj_cal LT gs_data-target_stock_min.

      gs_data-stock_disponible = gs_data-stock_obj_cal.

    ENDIF.

*------------------------------------------------------------------------------
*--- Paso 2: Unidades en tienda + todas las unidades en Curso
*------------------------------------------------------------------------------
* Inventario disponible.Pendiente por recibir
    gs_data-inventario_disp_pendiente_rec = ( ( gs_data-pending_sale_store + gs_data-stock_store + gs_data-libre_util_store ) - gs_data-stock_security ).

*   Si el resultado es menor es decir un negativo se debe transportar en un positvo.
    gs_data-inventario_disp_pendiente_rec = abs( gs_data-inventario_disp_pendiente_rec ).

*------------------------------------------------------------------------------
*--- Paso 3: Necesidad de Reaprovisionamiento
*------------------------------------------------------------------------------
*   Necesidad Reaprov.
    CASE ls_marc-dismm.
      WHEN 'RP'.
*        gs_data-need_reapproval = gs_data-sale_store - gs_data-target_stock.
        gs_data-need_reapproval = ( ( gs_data-target_stock ) - ( gs_data-libre_util_store + gs_data-pending_sale_store + gs_data-stock_store ) ).
      WHEN 'RF'.
*        gs_data-need_reapproval = gs_data-forecast_daily * gs_data-target_coverage_days.
*        gs_data-need_reapproval = gs_data-target_stock - gs_data-inventario_disp_pendiente_rec.
        gs_data-need_reapproval = gs_data-stock_disponible - gs_data-inventario_disp_pendiente_rec.

*   Necesidad Reaprov. Si es menor a cero entonces cero.
      IF gs_data-need_reapproval LT 0.
        gs_data-need_reapproval = 0.
      ENDIF.
    ENDCASE.


    APPEND gs_data TO gt_data.
    CLEAR: gs_data.



*****   Si, el inventario disponible y pendiente por recibir es igual Stock de Seguridad,
*****   dando un valor cero, este debe tomar el resultado del Stock Objetivo Calculado del paso 2 con las reglas aplicadas.
****    IF gs_data-inventario_disp_pendiente_rec EQ gs_data-stock_security.
**** gs_data-inventario_disp_pendiente_rec = abs( gs_data-inventario_disp_pendiente_rec ).
****     gs_data-inventario_disp_pendiente_rec = 0.
****
*****   Si, el inventario disponible y pendiente por recibir es menor Stock de Seguridad,
*****   dando un valor negativo, este debe sumarse al stock Objetivo Calculado del paso 2 con las reglas aplicadas
****    ELSEIF gs_data-inventario_disp_pendiente_rec LT gs_data-stock_security.
****      gs_data-stock_obj_cal = gs_data-stock_obj_cal + gs_data-inventario_disp_pendiente_rec.
****
*****   Si, el inventario disponible y pendiente por recibir es Mayor al Stock de Seguridad,
*****   debe aplicarse las reglas del paso 4.
****    ELSEIF gs_data-inventario_disp_pendiente_rec GT gs_data-stock_security.
****
*****   Si, el inventario disponible y pendiente por recibir es Mayor al Stock de Seguridad.
*****   Si. El valor es menor que cero entonces se presenta el valor cero en el reporte
*****   Necesidad Reaprov.
****      gs_data-need_reapproval = 0.
****
*****   Necesidad Reaprov. Si es menor a cero entonces cero.
****      IF gs_data-need_reapproval LT 0.
****        gs_data-need_reapproval = 0.
****      ENDIF.
****    ENDIF.


  ENDLOOP.

  IF gt_data IS INITIAL.
    MESSAGE 'No se encontraron datos' TYPE 'S' DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

ENDFORM.



***&---------------------------------------------------------------------*
***& Form GET_DATA
***&---------------------------------------------------------------------*
*FORM GET_DATA.
*
*  DATA:
*    LT_SELSCREEN TYPE TABLE OF RSPARAMS,
*    LS_SELSCREEN TYPE RSPARAMS,
*    LR_DATA      TYPE REF TO DATA.
*
*
*  IF S_VEND IS NOT INITIAL.
*    APPEND_SELSCREEN: 'VEND_OWNER'  'S' 'I' 'BT' S_VEND-LOW S_VEND-HIGH.    "Proveedor
*  ENDIF.
*
*  IF S_NUM IS NOT INITIAL.
*    APPEND_SELSCREEN: 'NUM' 'S' 'I' 'BT' S_NUM-LOW S_NUM-HIGH.    "Contrato de condiciones
*  ENDIF.
*
*  IF S_BUKRS IS NOT INITIAL.
*    APPEND_SELSCREEN: 'BUKRS' 'S' 'I' 'BT' S_BUKRS-LOW S_BUKRS-HIGH.    "Sociedad
*  ENDIF.
*
*  IF S_EKORG IS NOT INITIAL.
*    APPEND_SELSCREEN: 'EKORG' 'S' 'I' 'BT' S_EKORG-LOW S_EKORG-HIGH.    "Organización compras
*  ENDIF.
*
*  IF S_EKGRP IS NOT INITIAL.
*    APPEND_SELSCREEN: 'EKGRP' 'S' 'I' 'BT' S_EKGRP-LOW S_EKGRP-HIGH.    "Organización ventas
*  ENDIF.
*
*  IF S_DATE IS NOT INITIAL.
*    APPEND_SELSCREEN: 'SETTL_DATE' 'S' 'I' 'BT' S_DATE-LOW S_DATE-HIGH.    "Fecha de liquidación
*  ENDIF.
*
*
**  BREAK-POINT.
*  CL_SALV_BS_RUNTIME_INFO=>SET( EXPORTING DISPLAY  = ABAP_FALSE
*                                          METADATA = ABAP_FALSE
*                                          DATA     = ABAP_TRUE ).
*
*  SUBMIT RWB2R_BUSINESS_VOLUME WITH SELECTION-TABLE LT_SELSCREEN AND RETURN EXPORTING LIST TO MEMORY.
**  SUBMIT rwb2r_business_volume AND RETURN EXPORTING LIST TO MEMORY.
*
*  WAIT UP TO 1 SECONDS.
*
*  IF <FS_CONTRACT_DATA> IS ASSIGNED.
*    UNASSIGN <FS_CONTRACT_DATA>.
*  ENDIF.
*
*  TRY.
*      CL_SALV_BS_RUNTIME_INFO=>GET_DATA_REF( IMPORTING R_DATA = LR_DATA ).
*
*      ASSIGN LR_DATA->* TO <FS_CONTRACT_DATA>.
*
*      IF <FS_CONTRACT_DATA> IS ASSIGNED.
*        MOVE-CORRESPONDING <FS_CONTRACT_DATA> TO GT_DATA.
*      ENDIF.
*
*    CATCH CX_SALV_BS_SC_RUNTIME_INFO.
*  ENDTRY.
*
*  CL_SALV_BS_RUNTIME_INFO=>CLEAR_ALL( ).
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**& Form GET_IQ09
**&---------------------------------------------------------------------*
*FORM GET_DATA.
*  BREAK-POINT.
*  DATA: LR_BSTKD      TYPE RANGE OF VBKD-BSTKD,
*        LR_SALESORDER TYPE RANGE OF I_SALESORDER-SALESORDER.
*
*  DATA LT_SD_FLOW TYPE TABLE OF I_SDDOCUMENTMULTILEVELPROCFLOW.
*
*  SELECT * FROM WCOCOH INTO TABLE @DATA(LT_WCOCOH)
*    FOR ALL ENTRIES IN @GT_DATA
*    WHERE NUM EQ @GT_DATA-NUM.
*  IF SY-SUBRC EQ 0.
*
*    LR_BSTKD = VALUE #( FOR WA IN LT_WCOCOH (
*        SIGN   = 'I'
*        OPTION = 'EQ'
*        LOW    = |{ WA-NUM ALPHA = OUT }| ) ).
*
*    IF LR_BSTKD IS NOT INITIAL.
*      SELECT * FROM VBKD INTO TABLE @DATA(LT_VBKD)
*          WHERE BSTKD IN @LR_BSTKD
*            AND POSNR NE '000000'.
*      IF SY-SUBRC EQ 0.
*        SELECT * FROM I_SALESORDER INTO TABLE @DATA(LT_SALESORDER)
*          FOR ALL ENTRIES IN @LT_VBKD
*          WHERE SALESORDER EQ @LT_VBKD-VBELN.
*        IF SY-SUBRC EQ 0.
*
*          LR_SALESORDER = VALUE #( FOR WA2 IN LT_SALESORDER (
*              SIGN   = 'I'
*              OPTION = 'EQ'
*              LOW    = |{ WA2-SALESORDER ALPHA = IN }| ) ).
*
*          SELECT SINGLE *
*            FROM I_SDDOCUMENTMULTILEVELPROCFLOW
*            WHERE PRECEDINGDOCUMENT EQ '0025000187'
*            INTO @DATA(LS_SD_FLOW).
*
*          "BUSCAR ALRTERNATIVA EN vbfa
*
*          SELECT * FROM I_SDDOCUMENTMULTILEVELPROCFLOW INTO TABLE @DATA(LT_MULTILEVELFLOW).
**            FOR ALL ENTRIES IN @lt_salesorder
**            WHERE precedingdocument in @lr_salesorder "@lt_salesorder-salesorder.
**              where precedingdocumentcategory EQ 'C'.
*          IF SY-SUBRC EQ 0.
*            SELECT * FROM I_BILLINGDOCUMENT INTO TABLE @DATA(LT_BILLINGDOCUMENT)
*              FOR ALL ENTRIES IN @LT_MULTILEVELFLOW
*              WHERE BILLINGDOCUMENT EQ @LT_MULTILEVELFLOW-SUBSEQUENTDOCUMENT.
*            IF SY-SUBRC EQ 0.
*              SELECT * FROM P_RO_SAFTBSADBELNR INTO TABLE @DATA(LT_PROSAFTBSADBELNR)
*                FOR ALL ENTRIES IN @LT_BILLINGDOCUMENT
*                WHERE ACCOUNTINGDOCUMENT EQ @LT_BILLINGDOCUMENT-BILLINGDOCUMENT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
*  ENDIF.
*
*
*
*  LOOP AT GT_DATA ASSIGNING FIELD-SYMBOL(<FS_DATA>).
*
*    READ TABLE LT_WCOCOH INTO DATA(LS_WCOCOH) WITH KEY NUM = <FS_DATA>-NUM.
*    IF SY-SUBRC EQ 0.
*
*      MOVE-CORRESPONDING LS_WCOCOH TO <FS_DATA>.
*
*      READ TABLE LT_VBKD INTO DATA(LS_VBKD) WITH KEY BSTKD = LS_WCOCOH-NUM.
*      IF SY-SUBRC EQ 0.
*
*        READ TABLE LT_SALESORDER INTO DATA(LS_SALESORDER) WITH KEY SALESORDER = LS_VBKD-VBELN.
*        IF SY-SUBRC EQ 0.
*          MOVE-CORRESPONDING LS_SALESORDER TO <FS_DATA>.
*
*          READ TABLE LT_MULTILEVELFLOW INTO DATA(LS_MULTILEVELFLOW) WITH KEY PRECEDINGDOCUMENT = LS_SALESORDER-SALESORDER.
*          IF SY-SUBRC EQ 0.
*            MOVE: LS_MULTILEVELFLOW-SUBSEQUENTDOCUMENT TO <FS_DATA>-SUBSEQUENTDOCUMENT.
*
*            READ TABLE LT_BILLINGDOCUMENT INTO DATA(LS_BILLINGDOCUMENT) WITH KEY BILLINGDOCUMENT = LS_MULTILEVELFLOW-SUBSEQUENTDOCUMENT.
*            IF SY-SUBRC EQ 0.
*              MOVE-CORRESPONDING LS_BILLINGDOCUMENT TO <FS_DATA>.
*
*              READ TABLE LT_PROSAFTBSADBELNR INTO DATA(LS_PROSAFTBSADBELNR) WITH KEY ACCOUNTINGDOCUMENT = LS_BILLINGDOCUMENT-BILLINGDOCUMENT.
*              IF SY-SUBRC EQ 0.
*                <FS_DATA>-COMP_STATUS = 'Compensada'.
*              ELSE.
*                <FS_DATA>-COMP_STATUS = 'No Compensada'.
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*
*
*    ENDIF.
*
*
*
*  ENDLOOP.
*
*
*ENDFORM.
*
*
**&---------------------------------------------------------------------*
**& Form UPDATE
**&---------------------------------------------------------------------*
*FORM GET_UPDATE.
**
**  DATA lt_comp_mat TYPE TABLE OF ymqasm01_tb00001.
**
**  SELECT * FROM ymqasm01_tb00001 INTO TABLE lt_comp_mat
**    FOR ALL ENTRIES IN gt_data
**    WHERE matnr EQ gt_data-matnr
**      AND sernr EQ gt_data-sernr.
**  IF sy-subrc EQ 0.
**
**    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
**      READ TABLE lt_comp_mat INTO DATA(ls_comp_mat) WITH KEY matnr = <fs_data>-matnr sernr = <fs_data>-sernr.
**      IF sy-subrc EQ 0.
**        MOVE: ls_comp_mat-smotor  TO <fs_data>-smotor,
**              ls_comp_mat-spoliza TO <fs_data>-spoliza,
**              ls_comp_mat-fpoliza TO <fs_data>-fpoliza.
**      ENDIF.
**    ENDLOOP.
**
**  ENDIF.
*
*ENDFORM.
*
*
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

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-repid
      i_structure_name       = 'ZSTR_REPLANISHMENT' " Pass the local or DDIC structure name
      i_inclname             = sy-repid             " Must be the program/include where structure is defined
    CHANGING
      ct_fieldcat            = gt_slis_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    " Handle errors
  ENDIF.


*  APPEND VALUE #( FIELDNAME = 'LIGHT_IND'
*                  SELTEXT_L = 'Semáforo')
*                  TO GT_SLIS_FIELDCAT.
*
*  APPEND VALUE #( FIELDNAME = 'NUM'
*                  SELTEXT_L = 'Contrato de condición' )
*                  TO GT_SLIS_FIELDCAT.



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
      i_callback_program = sy-repid
*     i_callback_top_of_page   = 'TOP_OF_PAGE'
*     i_callback_pf_status_set = 'PF_STATUS'
*     i_callback_user_command  = 'USER_COMMAND'
      is_layout          = gs_slis_layout
      it_fieldcat        = gt_slis_fieldcat
      it_special_groups  = gt_slis_group
      i_save             = 'X'
    TABLES
      t_outtab           = gt_data.
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
