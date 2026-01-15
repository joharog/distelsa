*&---------------------------------------------------------------------*
*& Include          ZMMR053_F1
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form F_OBT_DAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_obt_dat.
*--- se obtienen los datos de las posiciones de las facturas
  SELECT vkorg_ana werks erdat matnr SUM( fkimg ) vrkme
    INTO TABLE gt_dat_vtas
    FROM vbrp
    WHERE erdat IN s_erdat AND
          matnr IN s_matnr AND
          werks IN s_werks GROUP BY vkorg_ana werks erdat matnr vrkme.
  IF sy-subrc = 0.
    LOOP AT gt_dat_vtas INTO DATA(ls_vtas).
      MOVE-CORRESPONDING ls_vtas TO gs_out.

*--- se busca el registro en la tabla de datos guardados a ver si
*--- para las entradas hay ya documentos de compras creados
      SELECT SINGLE *
        INTO @DATA(ls_zmmtb053)
        FROM zmmtb053
        WHERE vkorg EQ @ls_vtas-vkorg AND
              werks EQ @ls_vtas-werks AND
              erdat EQ @ls_vtas-erdat AND
              matnr EQ @ls_vtas-matnr AND
              fkimg EQ @ls_vtas-fkimg AND
              vrkme EQ @ls_vtas-vrkme.
      IF sy-subrc = 0.
        gs_out-ebeln = ls_zmmtb053-ebeln.
      ENDIF.

      APPEND gs_out TO gt_out.
      CLEAR: gs_out, ls_zmmtb053.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRT_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GV_TESTRUN
*&---------------------------------------------------------------------*
FORM f_crt_po USING pv_testrun.

*--- tablas
  DATA: lt_return	                TYPE STANDARD TABLE OF bapiret2, "
        lt_pocondheader	          TYPE STANDARD TABLE OF bapimepocondheader, "
        lt_pocondheaderx          TYPE STANDARD TABLE OF bapimepocondheaderx, "
        lt_pocond	                TYPE STANDARD TABLE OF bapimepocond, "
        lt_pocondx                TYPE STANDARD TABLE OF bapimepocondx, "
        lt_polimits	              TYPE STANDARD TABLE OF bapiesuhc, "
        lt_pocontractlimits	      TYPE STANDARD TABLE OF bapiesucc, "
        lt_poservices	            TYPE STANDARD TABLE OF bapiesllc, "
        lt_posrvaccessvalues      TYPE STANDARD TABLE OF bapiesklc, "
        lt_poservicestext	        TYPE STANDARD TABLE OF bapieslltx, "
        lt_extensionin            TYPE STANDARD TABLE OF bapiparex, "
        lt_poitem	                TYPE STANDARD TABLE OF bapimepoitem, "
        lt_extensionout	          TYPE STANDARD TABLE OF bapiparex, "
        lt_poexpimpitem	          TYPE STANDARD TABLE OF bapieipo, "
        lt_poexpimpitemx          TYPE STANDARD TABLE OF bapieipox, "
        lt_potextheader	          TYPE STANDARD TABLE OF bapimepotextheader, "
        lt_potextitem	            TYPE STANDARD TABLE OF bapimepotext, "
        lt_allversions            TYPE STANDARD TABLE OF bapimedcm_allversions, "
        lt_popartner              TYPE STANDARD TABLE OF bapiekkop, "
        lt_pocomponents	          TYPE STANDARD TABLE OF bapimepocomponent, "
        lt_pocomponentsx          TYPE STANDARD TABLE OF bapimepocomponentx, "
        lt_poshipping	            TYPE STANDARD TABLE OF bapiitemship, "
        lt_poitemx                TYPE STANDARD TABLE OF bapimepoitemx, "
        lt_poshippingx            TYPE STANDARD TABLE OF bapiitemshipx, "
        lt_poshippingexp          TYPE STANDARD TABLE OF bapimeposhippexp, "
        lt_serialnumber	          TYPE STANDARD TABLE OF bapimeposerialno, "
        lt_serialnumberx          TYPE STANDARD TABLE OF bapimeposerialnox, "
        lt_invplanheader          TYPE STANDARD TABLE OF bapi_invoice_plan_header, "
        lt_invplanheaderx	        TYPE STANDARD TABLE OF bapi_invoice_plan_headerx, "
        lt_invplanitem            TYPE STANDARD TABLE OF bapi_invoice_plan_item, "
        lt_invplanitemx	          TYPE STANDARD TABLE OF bapi_invoice_plan_itemx, "
        lt_poaddrdelivery	        TYPE STANDARD TABLE OF bapimepoaddrdelivery, "
        lt_poschedule	            TYPE STANDARD TABLE OF bapimeposchedule, "
        lt_poschedulex            TYPE STANDARD TABLE OF bapimeposchedulx, "
        lt_poaccount              TYPE STANDARD TABLE OF bapimepoaccount, "
        lt_poaccountx	            TYPE STANDARD TABLE OF bapimepoaccountx, "
        lt_poaccountprofitsegment	TYPE STANDARD TABLE OF bapimepoaccountprofitsegment. "

*--- estructuras
  DATA: ls_poheader	         TYPE bapimepoheader, "
        ls_expheader         TYPE bapimepoheader, "
        ls_poheaderx         TYPE bapimepoheaderx, "
        ls_poaddrvendor	     TYPE bapimepoaddrvendor, "
        ls_exppoexpimpheader TYPE bapieikp, "
        ls_poexpimpheader	   TYPE bapieikp, "
        ls_poexpimpheaderx   TYPE bapieikpx, "
        ls_versions	         TYPE bapimedcm, "
        ls_poitem            LIKE LINE OF lt_poitem,
        ls_poitemx           LIKE LINE OF lt_poitemx,
        ls_poaccount         LIKE LINE OF lt_poaccount,
        ls_poaccountx        LIKE LINE OF lt_poaccountx,
        ls_return            LIKE LINE OF lt_return.

*--- variables
  DATA: lv_exppurchaseorder	 TYPE bapimepoheader-po_number, "
        lv_no_messaging	     TYPE bapiflag-bapiflag, "
        lv_no_message_req	   TYPE bapiflag-bapiflag, "
        lv_no_authority	     TYPE bapiflag-bapiflag, "
        lv_no_price_from_po	 TYPE bapiflag-bapiflag, "
        lv_park_complete     TYPE bapiflag-bapiflag, "
        lv_park_uncomplete   TYPE bapiflag-bapiflag, "
        lv_testrun           TYPE bapiflag-bapiflag, "
        lv_memory_uncomplete TYPE bapiflag-bapiflag, "
        lv_memory_complete   TYPE bapiflag-bapiflag, "
        lv_itnum             TYPE ebelp,
        lv_netpr             TYPE eine-netpr,
        lv_waers             TYPE eine-waers.

*--- validaciones
  LOOP AT gt_out TRANSPORTING NO FIELDS WHERE check EQ 'X' AND ebeln NE space.
  ENDLOOP.
  IF sy-subrc = 0.
    MESSAGE 'Ha seleccionado registro(s) que tiene(n) documento(s) de compras asociado(s)' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  LOOP AT gt_out TRANSPORTING NO FIELDS WHERE check EQ 'X'.
  ENDLOOP.
  IF sy-subrc NE 0.
    MESSAGE 'Debe seleccionar al menos algún registro a procesar' TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*---

*--- se pasa el parametro de si es test o no
  lv_testrun = pv_testrun.

*--- se llenan datos cabecera
*LS_POHEADER
*LS_POHEADERX

  ls_poheader-creat_date = sy-datum.

  ls_poheaderx-comp_code = 'X'.
  ls_poheaderx-doc_type = 'X'.
  ls_poheaderx-creat_date = 'X'.
  ls_poheaderx-vendor = 'X'.
  ls_poheaderx-purch_org = 'X'.
  ls_poheaderx-pur_group = 'X'.
  ls_poheaderx-currency = 'X'.

*--- se pasan los items
*--- items
* LT_POITEM
  lv_itnum = 1.
  LOOP AT gt_out INTO gs_out WHERE check EQ 'X'.

*--- se buscan los datos de mapeo con el material
    SELECT SINGLE *
      INTO @DATA(ls_mmtabmap)
      FROM zmmtbmap053
      WHERE matnr = @gs_out-matnr.

    ls_poheader-comp_code = ls_mmtabmap-bukrs.
    ls_poheader-doc_type = ls_mmtabmap-bsart.
    ls_poheader-purch_org = ls_mmtabmap-ekorg.
    ls_poheader-pur_group = ls_mmtabmap-ekgrp.
    ls_poheader-vendor = ls_mmtabmap-lifnr.

*--- completa con ceros a la izquierda proveedor
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_poheader-vendor
      IMPORTING
        output = ls_poheader-vendor.

    ls_poitem-po_item = lv_itnum.
    ls_poitem-material = gs_out-matnr.
    ls_poitem-plant = gs_out-werks.
    ls_poitem-quantity = gs_out-fkimg.
    ls_poitem-po_unit = gs_out-vrkme.
    ls_poitem-acctasscat = ls_mmtabmap-knttp. "'K'.

*--- se busca el precio neto y moneda
    SELECT SINGLE b~netpr, b~waers
      INTO (@lv_netpr, @lv_waers)
      FROM eina AS a
      INNER JOIN eine AS b
      ON b~infnr = a~infnr
      WHERE matnr = @gs_out-matnr.
    IF sy-subrc = 0.
      ls_poitem-net_price = lv_netpr.
      ls_poheader-currency = lv_waers.
    ENDIF.

*LS_POITEMX
    ls_poitemx-po_item = lv_itnum.
    ls_poitemx-po_itemx = 'X'.
    ls_poitemx-material = 'X'.
    ls_poitemx-plant = 'X'.
    ls_poitemx-quantity = 'X'.
    ls_poitemx-po_unit = 'X'.
    ls_poitemx-acctasscat = 'X'.
    ls_poitemx-net_price = 'X'.


*LS_POACCOUNT
*LS_POACCOUNTX
    ls_poaccount-po_item = lv_itnum.
    ls_poaccount-costcenter = ls_mmtabmap-kostl.
    ls_poaccount-gl_account = ls_mmtabmap-saknr.

    ls_poaccountx-po_item = lv_itnum.
    ls_poaccountx-po_itemx = 'X'.
    ls_poaccountx-costcenter = 'X'.
    ls_poaccountx-gl_account = 'X'.

    APPEND ls_poitem TO lt_poitem.
    APPEND ls_poitemx TO lt_poitemx.
    APPEND ls_poaccount TO lt_poaccount.
    APPEND ls_poaccountx TO lt_poaccountx.

    ADD 1 TO lv_itnum.
  ENDLOOP.

*--- se crea order de compras
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader               = ls_poheader
      no_messaging           = lv_no_messaging
      no_message_req         = lv_no_message_req
      no_authority           = lv_no_authority
      no_price_from_po       = lv_no_price_from_po
      park_complete          = lv_park_complete
      park_uncomplete        = lv_park_uncomplete
      poheaderx              = ls_poheaderx
      poaddrvendor           = ls_poaddrvendor
      testrun                = lv_testrun
      memory_uncomplete      = lv_memory_uncomplete
      memory_complete        = lv_memory_complete
      poexpimpheader         = ls_poexpimpheader
      poexpimpheaderx        = ls_poexpimpheaderx
      versions               = ls_versions
    IMPORTING
      exppurchaseorder       = lv_exppurchaseorder
      expheader              = ls_expheader
      exppoexpimpheader      = ls_exppoexpimpheader
    TABLES
      return                 = lt_return
      pocondheader           = lt_pocondheader
      pocondheaderx          = lt_pocondheaderx
      pocond                 = lt_pocond
      pocondx                = lt_pocondx
      polimits               = lt_polimits
      pocontractlimits       = lt_pocontractlimits
      poservices             = lt_poservices
      posrvaccessvalues      = lt_posrvaccessvalues
      poservicestext         = lt_poservicestext
      extensionin            = lt_extensionin
      poitem                 = lt_poitem
      extensionout           = lt_extensionout
      poexpimpitem           = lt_poexpimpitem
      poexpimpitemx          = lt_poexpimpitemx
      potextheader           = lt_potextheader
      potextitem             = lt_potextitem
      allversions            = lt_allversions
      popartner              = lt_popartner
      pocomponents           = lt_pocomponents
      pocomponentsx          = lt_pocomponentsx
      poshipping             = lt_poshipping
      poitemx                = lt_poitemx
      poshippingx            = lt_poshippingx
      poshippingexp          = lt_poshippingexp
      serialnumber           = lt_serialnumber
      serialnumberx          = lt_serialnumberx
      invplanheader          = lt_invplanheader
      invplanheaderx         = lt_invplanheaderx
      invplanitem            = lt_invplanitem
      invplanitemx           = lt_invplanitemx
      poaddrdelivery         = lt_poaddrdelivery
      poschedule             = lt_poschedule
      poschedulex            = lt_poschedulex
      poaccount              = lt_poaccount
      poaccountprofitsegment = lt_poaccountprofitsegment
      poaccountx             = lt_poaccountx.                 " BAPI_PO_CREATE1

  READ TABLE lt_return INTO ls_return  WITH KEY type = 'S' id = '06' number = '017'.
  IF sy-subrc EQ 0.

*--- se hace commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*--- Creó el documento.
    gs_log-light = '3'.

    DATA(lv_ebeln) = ls_return-message_v2(10).

    MESSAGE ID ls_return-id  TYPE ls_return-type NUMBER ls_return-number
       WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
       INTO gs_log-msg.

    APPEND gs_log TO gt_log.

    PERFORM f_crt_em USING pv_testrun lv_ebeln.

    IF pv_testrun NE 'X'.
      MOVE-CORRESPONDING gs_out TO gs_zmmtb053.
      gs_zmmtb053-ebeln = lv_ebeln.
      INSERT zmmtb053 FROM gs_zmmtb053.

      LOOP AT gt_out INTO gs_out WHERE check EQ 'X'.
        gs_out-ebeln = lv_ebeln.
        MODIFY gt_out FROM gs_out TRANSPORTING ebeln.
      ENDLOOP.

    ENDIF.

  ELSE.

*--- realiza rollback
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*--- hubieron errores
    LOOP AT lt_return INTO ls_return.
      IF ls_return-message_v2 = '$'.
        CONTINUE.
      ENDIF.
      gs_log-light = '1'.
      gs_log-msg = ls_return-message.
      APPEND gs_log TO gt_log.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRT_EM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_crt_em USING pv_testrun p_ebeln.

*--- tablas
  DATA: lt_goodsmvt_item           TYPE STANDARD TABLE OF bapi2017_gm_item_create, "
        lt_goodsmvt_serialnumber   TYPE STANDARD TABLE OF bapi2017_gm_serialnumber, "
        lt_return	                 TYPE STANDARD TABLE OF bapiret2, "
        lt_goodsmvt_serv_part_data TYPE STANDARD TABLE OF /spe/bapi2017_servicepart_data, "
        lt_extensionin             TYPE STANDARD TABLE OF bapiparex. "

*--- estructuras
  DATA: ls_goodsmvt_header  TYPE bapi2017_gm_head_01, "
        ls_goodsmvt_headret	TYPE bapi2017_gm_head_ret, "
        ls_goodsmvt_code    TYPE bapi2017_gm_code, "
        ls_goodsmvt_ref_ewm	TYPE /spe/bapi2017_gm_ref_ewm, "
        ls_goodsmvt_item    LIKE LINE OF lt_goodsmvt_item,
        ls_return           LIKE LINE OF lt_return.

*--- variables
  DATA: lv_materialdocument	TYPE bapi2017_gm_head_ret-mat_doc, "
        lv_testrun          TYPE bapi2017_gm_gen-testrun, "   SPACE
        lv_matdocumentyear  TYPE bapi2017_gm_head_ret-doc_year, "
        lv_po_item          TYPE ebelp.

*--- modo test
  lv_testrun = pv_testrun.

  SELECT SINGLE *
    INTO @DATA(ls_ekko)
    FROM ekko
    WHERE ebeln = @p_ebeln.
  IF sy-subrc = 0.
    SELECT *
      INTO TABLE @DATA(lt_ekpo)
      FROM ekpo
      WHERE ebeln = @p_ebeln.
  ENDIF.

*--- datos cabecera
  ls_goodsmvt_header-pstng_date = sy-datum.
  ls_goodsmvt_header-doc_date = sy-datum.

*--- items
  lv_po_item = 1.
  LOOP AT lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>).
    ls_goodsmvt_item-material = <fs_ekpo>-matnr.
    ls_goodsmvt_item-plant = <fs_ekpo>-werks.
    ls_goodsmvt_item-move_type = '101'.
    ls_goodsmvt_item-entry_qnt = <fs_ekpo>-menge.
    ls_goodsmvt_item-entry_uom = <fs_ekpo>-meins.
    ls_goodsmvt_item-po_number = p_ebeln.
    ls_goodsmvt_item-po_item = lv_po_item.
    ls_goodsmvt_item-mvt_ind = 'B'.
    APPEND ls_goodsmvt_item TO lt_goodsmvt_item.
    CLEAR: <fs_ekpo>, ls_goodsmvt_item.
    ADD 1 TO lv_po_item.
  ENDLOOP.

*--- Asignación de código a transacción p.movimiento mcía.BAPI
  ls_goodsmvt_code-gm_code = '01'.

*--- se llama la bapi para crear la entrada de mercancia - Post goods movements with MB_CREATE_GOODS_MOVEMENT
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header         = ls_goodsmvt_header
      goodsmvt_code           = ls_goodsmvt_code
      testrun                 = lv_testrun
      goodsmvt_ref_ewm        = ls_goodsmvt_ref_ewm
    IMPORTING
      goodsmvt_headret        = ls_goodsmvt_headret
      materialdocument        = lv_materialdocument
      matdocumentyear         = lv_matdocumentyear
    TABLES
      goodsmvt_item           = lt_goodsmvt_item
      goodsmvt_serialnumber   = lt_goodsmvt_serialnumber
      return                  = lt_return
      goodsmvt_serv_part_data = lt_goodsmvt_serv_part_data
      extensionin             = lt_extensionin.              " BAPI_GOODSMVT_CREATE

  IF ls_goodsmvt_headret-mat_doc IS NOT INITIAL AND lt_return[] IS INITIAL.

*--- se hace commit
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*--- Creó el documento.
    CLEAR gs_log.
    gs_log-light = '3'.
    CONCATENATE 'Doc. Material creado bajo el nro'  ls_goodsmvt_headret-mat_doc 'para el año'
    ls_goodsmvt_headret-doc_year  INTO gs_log-msg SEPARATED BY space.

    APPEND gs_log TO gt_log.

  ELSE.

*--- realiza rollback
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

*--- hubieron errores
    LOOP AT lt_return INTO ls_return.
      IF ls_return-message_v2 = '$'.
        CONTINUE.
      ENDIF.
      gs_log-light = '1'.
      gs_log-msg = ls_return-message.
      APPEND gs_log TO gt_log.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECT_ALL_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_select_all_manual .

  DATA ls_itab TYPE ty_out.

  ls_itab-check = gc_char_x.

  MODIFY gt_out FROM ls_itab
                     TRANSPORTING check
                     WHERE check = ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESELECT_ALL_MANUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_deselect_all_manual.

  DATA ls_itab TYPE ty_out.

  CLEAR ls_itab.
  MODIFY gt_out FROM ls_itab
                     TRANSPORTING check
                     WHERE check = gc_char_x.

ENDFORM.
