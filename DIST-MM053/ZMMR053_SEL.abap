*&---------------------------------------------------------------------*
*& Include          ZMMR053_SEL
*&---------------------------------------------------------------------*

TABLES: VBRP.

SELECTION-SCREEN:BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: S_ERDAT FOR VBRP-ERDAT OBLIGATORY,
                  S_MATNR FOR VBRP-MATNR OBLIGATORY,
                  S_WERKS FOR VBRP-WERKS OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

*SELECTION-SCREEN: BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-002.
*  PARAMETERS: P_RD1 RADIOBUTTON GROUP RBG1 USER-COMMAND FCODE DEFAULT 'X',  "reporte de ventas
*              P_RD2 RADIOBUTTON GROUP RBG1.                                 "crear pedido de compras
*SELECTION-SCREEN: END OF BLOCK B2.
*
*PARAMETERS: P_R1 RADIOBUTTON GROUP RBG2 MODIF ID MD1,   "modo test
*            P_R2 RADIOBUTTON GROUP RBG2 MODIF ID MD1.   "modo productivo
*
**--- evento para manejar cambios de la pantalla de selección
*AT SELECTION-SCREEN OUTPUT.
*  LOOP AT SCREEN.
*    IF P_RD1 = 'X'.
*      IF SCREEN-GROUP1 = 'MD1'.
*        SCREEN-ACTIVE = 0.        "se desactiva si P_RD1 se seleciona
*      ENDIF.
*    ELSEIF P_RD2 = 'X'.
*      IF SCREEN-GROUP1 = 'MD1'.   "se activa si P_RD2 se selecciona
*        SCREEN-ACTIVE = 1.
*      ENDIF.
*    ENDIF.
*    MODIFY SCREEN.
*  ENDLOOP.
