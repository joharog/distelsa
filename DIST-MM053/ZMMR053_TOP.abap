*&---------------------------------------------------------------------*
*& Include          ZMMR053_TOP
*&---------------------------------------------------------------------*

*--- tablas tipos
TYPES: BEGIN OF TY_OUT,
         CHECK  TYPE CHAR01,           "Check
         EBELN TYPE EKKO-EBELN,        "Doc Compras
         VKORG TYPE VBRP-VKORG_ANA,    "Org Ventas
         WERKS TYPE VBRP-WERKS,        "Centro
         ERDAT TYPE VBRP-ERDAT,        "Fecha
         MATNR TYPE VBRP-MATNR,        "Material
         FKIMG TYPE VBRP-FKIMG,        "Cantidad
         VRKME TYPE VBRP-VRKME,        "Unidad
       END OF TY_OUT,

       BEGIN OF TY_DAT_VTAS,
         VKORG TYPE VBRP-VKORG_ANA,    "Org Ventas
         WERKS TYPE VBRP-WERKS,        "Centro
         ERDAT TYPE VBRP-ERDAT,        "Fecha
         MATNR TYPE VBRP-MATNR,        "Material
         FKIMG TYPE VBRP-FKIMG,        "Cantidad
         VRKME TYPE VBRP-VRKME,        "Unidad
       END OF TY_DAT_VTAS,

       BEGIN OF TY_LOG,
         LIGHT TYPE CHAR1,
         MSG   TYPE STRING,
       END OF TY_LOG.

*--- tablas internas
DATA: GT_DAT_VTAS TYPE STANDARD TABLE OF TY_DAT_VTAS,
      GT_LOG TYPE STANDARD TABLE OF TY_LOG,
      GT_OUT TYPE STANDARD TABLE OF TY_OUT.

*--- estructuras
DATA: GS_DAT_VTAS LIKE LINE OF GT_DAT_VTAS,
      GS_LOG      LIKE LINE OF GT_LOG,
      GS_OUT LIKE LINE OF GT_OUT,
      GS_ZMMTB053 TYPE ZMMTB053.

*--- variables
DATA: GV_TESTRUN TYPE BAPIFLAG-BAPIFLAG.

*--- constantes
DATA: GC_CHAR_X TYPE C VALUE 'X'.
