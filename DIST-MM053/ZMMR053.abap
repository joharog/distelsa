*&---------------------------------------------------------------------*
*& Report ZMMR053
*&---------------------------------------------------------------------*
*& Descripción: Proceso de tiempo de aire
*& Consultor:   Javier Otaiza MQA
*& Fecha:       05/09/2025
*&---------------------------------------------------------------------*
*& Modificado Por | Fecha Modificación | Descripción Modificación
*&---------------------------------------------------------------------*
*& Johan R.       | 14.01.2026         | Limpiar estructuras
*&---------------------------------------------------------------------*
REPORT ZMMR053.

INCLUDE: ZMMR053_TOP,
         ZMMR053_CL,
         ZMMR053_SEL,
         ZMMR053_F1,
         ZMMR053_F2.

*--- inicio del proceso
START-OF-SELECTION.

*--- se buscan los datos
  PERFORM F_OBT_DAT.

  IF GT_DAT_VTAS[] IS INITIAL.
    WRITE:/ 'No se encontraron coincidencias para los parámetros seleccionados'.
    EXIT.
  ENDIF.

*--- continua si existen datos
*--- si selecciona reporte de ventas
    IF GT_OUT[] IS NOT INITIAL.
      PERFORM F_DSP_RPT.
    ENDIF.

END-OF-SELECTION.
