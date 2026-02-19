FORM USEREXIT_DELETE_DOCUMENT.
*{   INSERT         DS4K905522                                        1

*----------------------------------------------------------------------*
* Definición de objetos para integración vía Web Service (SOAP Proxy)
*----------------------------------------------------------------------*
DATA: lo_entrega       TYPE REF TO zws_co_consulta_entrega, " Instancia del Consumer Proxy
      lo_sys_exception TYPE REF TO cx_ai_system_fault,      " Manejo de errores de comunicación/infraestructura
      ls_request       TYPE zws_consulta_entrega1,          " Estructura de envío (Input Mapping)
      ls_response      TYPE zws_consulta_entrega_response1, " Estructura de recepción (Output Mapping)
      lv_msg           TYPE char255,                        " Buffer para mensajes de usuario
      ls_noti_entrega  TYPE zws_notificar_entrega.          " Estructura para persistencia temporal en memoria

* Breakpoint de usuario para entorno de desarrollo
BREAK mqa_abap3.

*----------------------------------------------------------------------*
* Validación de contexto: Solo procesar durante la modificación de
* Entregas Entrantes (Transacción VL32N)
*----------------------------------------------------------------------*
IF sy-tcode EQ 'VL32N'.

  TRY.
      " Inicialización del objeto Proxy para el consumo del servicio externo
      CREATE OBJECT lo_entrega.

      " Mapeo de parámetros: Se asigna el número de entrega del contexto estándar
      ls_request-consulta_entrega-numero_entrega = cvbup-vbeln.

      " Ejecución de la llamada sincrónica al Web Service
      CALL METHOD lo_entrega->consulta_entrega
        EXPORTING
          input  = ls_request
        IMPORTING
          output = ls_response.

      " Procesamiento de la respuesta del servicio
      IF ls_response IS NOT INITIAL.

        " Validación lógica según el estatus retornado por el servicio externo
        CASE ls_response-consulta_entrega_response-status.

          WHEN 'En Proceso'.
            " Interrupción del flujo: La entrega no puede modificarse en este estado
            lv_msg = |La entrega entrante { ls_request-consulta_entrega-numero_entrega } tiene estatus "En Proceso"|.
            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
            LEAVE TO SCREEN 1000.

          WHEN 'No iniciado'.
            " Borra la entrega

          WHEN OTHERS.
            " Otros status no detiene el borrado

        ENDCASE.

        " Persistencia en Memoria ABAP: Exportamos los datos para su uso en puntos
        " posteriores del proceso (ej. otros Exits o BAdIs durante el SAVE)
        ls_noti_entrega-numero_entrega = ls_request-consulta_entrega-numero_entrega.
        ls_noti_entrega-estado_entrega = ls_response-consulta_entrega_response-status.

        EXPORT ls_noti_entrega TO MEMORY ID 'NOTI_DEL'.

      ENDIF.

    CATCH cx_ai_system_fault INTO lo_sys_exception.
*      lo_sys_exception->if_message~get_text( ).
      " Captura de errores técnicos de conectividad o fallos en el bus de integración
      lv_msg = lo_sys_exception->get_text( ).
      MESSAGE lv_msg TYPE 'I' DISPLAY LIKE 'E'. " Informar al usuario sobre el fallo técnico
  ENDTRY.

ENDIF.
*}   INSERT

ENDFORM.
