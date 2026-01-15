  METHOD if_ex_mb_migo_badi~post_document.

    DATA: ls_mkpf  TYPE mkpf,
          lv_belnr TYPE mblnr,
          lv_gjahr TYPE gjahr.

    " Obtener cabecera del documento
    ls_mkpf = is_mkpf.
    lv_belnr  = ls_mkpf-mblnr.
    lv_gjahr  = ls_mkpf-mjahr.

    LOOP AT it_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>).

      " Verificamos si es un movimiento de anulación
      IF <fs_mseg>-bwart = '102' OR <fs_mseg>-bwart = '352'.

        DELETE FROM ymqasm01_tb00001
          WHERE mblnr = <fs_mseg>-mblnr
            AND matnr = <fs_mseg>-matnr.

      ENDIF.

    ENDLOOP.


    "Envio notifiacion por correo
    CONSTANTS lc_sfname TYPE tdsfname VALUE 'ZMM_ENTMERCANCIA'.

*--- referencias locales de objetos
    DATA: lo_bcs         TYPE REF TO cl_bcs,
          lo_doc_bcs     TYPE REF TO cl_document_bcs,
          lo_recep       TYPE REF TO if_recipient_bcs,
          lo_sapuser_bcs TYPE REF TO cl_sapuser_bcs,
          lo_cx_bcx      TYPE REF TO cx_bcs.

*--- tablas internas locales
    DATA: lt_binary_content TYPE solix_tab,
          lt_text           TYPE bcsy_text,
          lt_details        TYPE tty_we03_details.

*--- estructuras locales
    DATA: ls_docparams    TYPE sfpdocparams,
          ls_outputparams TYPE sfpoutputparams,
          ls_formoutput   TYPE fpformoutput.

*--- local variables
    DATA: lv_bin_filesize  TYPE so_obj_len,
          lv_output_length TYPE i,
          lv_sent_to_all   TYPE os_boolean,
          lv_function_name TYPE fpname,
          lv_string_text   TYPE string,
          lv_subject       TYPE string,
          lv_att_sbj       TYPE sood-objdes,
          lv_ernam         TYPE ekko-ernam,
          lv_smtp_addr     TYPE adr6-smtp_addr.


    SELECT SINGLE bsart FROM ekko INTO @DATA(lv_bsart)
      WHERE ebeln EQ @<fs_mseg>-ebeln. "'4500000083'.

    CASE lv_bsart.
      WHEN 'ZCAD' OR 'ZSER' OR 'ZMOT' OR  'ZPUB' OR 'ZVIA'.
*--- se llama a la función de salida con el nombre del
*--- formulario Adobe
        TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
                i_name     = lc_sfname             " Reemplazar con el nombre de tu formulario Adobe
              IMPORTING
                e_funcname = lv_function_name.
          CATCH cx_root INTO DATA(lo_root).
        ENDTRY.

*--- lleno el control de parámetros, las opciones de salida
*--- y llamo al formulario
*--- control de parámetros - Procesamiento de formulario parámetro de salida
        ls_outputparams-getpdf   = 'X'.
        ls_outputparams-nodialog = 'X'.
        ls_outputparams-preview  = ' '.

*--- se abre un nuevo trabajo de spool
        CALL FUNCTION 'FP_JOB_OPEN'
          CHANGING
            ie_outputparams = ls_outputparams
          EXCEPTIONS
            cancel          = 1
            usage_error     = 2
            system_error    = 3
            internal_error  = 4
            OTHERS          = 5.
        IF sy-subrc <> 0.
        ENDIF.

*ls_docparams-langu = sy-langu.

*--- llamo al formulario a imprimirse
        CALL FUNCTION lv_function_name
          EXPORTING
            /1bcdwb/docparams  = ls_docparams
            mat_nr             = ls_mkpf-mblnr "'5000000474'
            we03_details       = lt_details
          IMPORTING
            /1bcdwb/formoutput = ls_formoutput
          EXCEPTIONS
            usage_error        = 1
            system_error       = 2
            internal_error     = 3
            OTHERS             = 4.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

*--- se cierra el trabajo de spool
        CALL FUNCTION 'FP_JOB_CLOSE'
          EXCEPTIONS
            usage_error    = 1
            system_error   = 2
            internal_error = 3
            OTHERS         = 4.
        IF sy-subrc <> 0.
        ENDIF.


*--- convierto el contenido del pdf a contenido binario
        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = ls_formoutput-pdf
          IMPORTING
            output_length = lv_output_length
          TABLES
            binary_tab    = lt_binary_content.
        IF sy-subrc = 0.
          MOVE lv_output_length TO lv_bin_filesize.
          CONDENSE lv_bin_filesize NO-GAPS.
        ENDIF.

        " DESTEVES AJUSTE POR DUMP
        SELECT SINGLE butxt FROM t001 INTO @DATA(lv_butxt)
          WHERE bukrs = @<fs_mseg>-bukrs. "'1010'.
*        SELECT SINGLE butxt FROM t001 INTO @DATA(lv_butxt)
*          WHERE bukrs = @<fs_mseg>-ebeln. "'1010'.

*--- se prepara el envío por correo electrónico
        TRY.
*--- crear una solicitud de envío persistente
            lo_bcs = cl_bcs=>create_persistent( ).

*--- asunto del correo con la forma:
*--- Nombre de la Sociedad, “Entrada de Mercancía: #documento material, Proveedor
*--- Ejemplo: DISTELSA, Entrada de mercancía 5000000049, FACENCO
*--- Nombre sociedad solicitante


*--- Nombre del proveedor
*--- electrónico respectivo
            SELECT SINGLE name1, adrnr FROM lfa1 INTO ( @DATA(lv_name1), @DATA(lv_adrnr) )
              WHERE lifnr = '0010001578'. "@mseg-lifnr.
            IF lv_adrnr IS NOT INITIAL.
              SELECT * FROM adr6 INTO TABLE @DATA(lt_adr6)
                WHERE addrnumber = @lv_adrnr.
              IF lt_adr6[] IS NOT INITIAL.
                SELECT * FROM adrt INTO TABLE @DATA(lt_adrt)
                  FOR ALL ENTRIES IN @lt_adr6
                  WHERE addrnumber = @lt_adr6-addrnumber
                    AND consnumber = @lt_adr6-consnumber
                    AND remark = 'COMPRAS'.
              ENDIF.
            ENDIF.

**--- completo la cadena del asunto
            lv_subject = | { lv_butxt } | && 'Entrada de Mercancía:' && | { ls_mkpf-mblnr } | && | { lv_name1 } |.


*--- se setea el mensaje en el asunto con la cadena anterior
*--- If need to send more than 50 characters in length as a subject line
*--- Then set message in cl_bcs->set_message_subject
*--- else populate it in method cl_document_bcs=>create_document
            lo_bcs->set_message_subject(
              EXPORTING
                ip_subject = lv_subject ). "CONV STRING( LV_SUBJECT ) ).

*--- se crea el cuerpo/body del email
*--- linea1
            CONCATENATE 'Estimado Proveedor:' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea2
            CONCATENATE ' ' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea3
            lv_string_text = 'Adjunto encontrará copia de la entrada de mercancías, favor revisarla, ya que debe'.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea4
            lv_string_text = ' coincidir con los productos entregados o servicios brindados para su trámite de pago.'.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea5
            CONCATENATE ' ' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea6NATE 'Atentamente,' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea7
            CONCATENATE ' ' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

*--- linea8
            CONCATENATE 'Grupo Distelsa.' cl_abap_char_utilities=>newline INTO lv_string_text.
            APPEND lv_string_text TO lt_text.
            CLEAR lv_string_text.

            lv_att_sbj = lv_subject.

*--- se crea el email - crear documento
            lo_doc_bcs = cl_document_bcs=>create_document(
              i_type    = 'RAW'
              i_text    = lt_text[]
*       I_LENGTH  = '12'
              i_subject = lv_att_sbj ).   "asunto del email - Subject of the Email

*--- se agrega adjunto el documento pdf y se envia -
*--- Add attachment to document and Add document to send request
*--- The internal table “lt_binary_content” contains the content of our attachment.
*     BCS expects document content here e.g. from document upload
*     binary_content = ...
            lv_att_sbj = ls_mkpf-mblnr.
*--- se agrega el archivo adjunto con descripción el nro del pedido
            CALL METHOD lo_doc_bcs->add_attachment
              EXPORTING
                i_attachment_type    = 'PDF'
                i_attachment_size    = lv_bin_filesize
                i_attachment_subject = lv_att_sbj
                i_att_content_hex    = lt_binary_content.

*--- agrega el documento a la solicitud de envio - add document to send request
            CALL METHOD lo_bcs->set_document( lo_doc_bcs ).

*--- se setea el remitente cuando es distinto al usuario del sistema
*--- Set Sender if is different to sy-uname
*    lo_sapuser_bcs = cl_sapuser_bcs=>create( sy-uname ).
*    CALL METHOD lo_bcs->set_sender
*      EXPORTING
*        i_sender = lo_sapuser_bcs.

*--- se agregan los recipientes - Add recipient (e–mail address)
*--- al correo del proveedor
            LOOP AT lt_adr6 ASSIGNING FIELD-SYMBOL(<fsl_adr6>).
              READ TABLE lt_adrt TRANSPORTING NO FIELDS WITH KEY consnumber = <fsl_adr6>-consnumber.
              IF sy-subrc = 0.
                lo_recep = cl_cam_address_bcs=>create_internet_address( <fsl_adr6>-smtp_addr ).
*--- agrego recipientes con respectivos atributos
*--- Add recipient with its respective attributes to send request
                CALL METHOD lo_bcs->add_recipient
                  EXPORTING
                    i_recipient = lo_recep.
*             I_EXPRESS   = 'X'.
              ENDIF.
            ENDLOOP.


*--- se agrega el usuario creador de la solped o del pedido
*--- como recipiente de correo electrónico
            IF lv_smtp_addr IS NOT INITIAL.
              lo_recep = cl_cam_address_bcs=>create_internet_address( lv_smtp_addr ).
*--- agrego recipiente con respectivo atributo
*--- Add recipient with its respective attributes to send request
              CALL METHOD lo_bcs->add_recipient
                EXPORTING
                  i_recipient = lo_recep.
*           I_EXPRESS   = 'X'.
            ENDIF.

            IF lo_recep IS NOT BOUND.
              lo_recep = cl_cam_address_bcs=>create_internet_address( 'prueba@gmail.com' ).
*--- agrego recipientes con respectivos atributos
*--- Add recipient with its respective attributes to send request
              CALL METHOD lo_bcs->add_recipient
                EXPORTING
                  i_recipient = lo_recep.
*           I_EXPRESS   = 'X'.
            ENDIF.


*--- Setear el envio inmediato
*--- Set Send Immediately, you can set this to send  your email immediately
            CALL METHOD lo_bcs->set_send_immediately
              EXPORTING
                i_send_immediately = 'X'.

*--- se envia el email - Send the Email
            CALL METHOD lo_bcs->send(
              EXPORTING
                i_with_error_screen = 'X'
              RECEIVING
                result              = lv_sent_to_all ).

***      IF LV_SENT_TO_ALL IS NOT INITIAL.
***        COMMIT WORK.
***      ENDIF.

*--- manejo de excepciones
          CATCH cx_bcs INTO lo_cx_bcx.
            "Appropriate Exception Handling
*        WRITE: 'Exception:', lo_cx_bcx->error_type.
        ENDTRY.
    ENDCASE.

  ENDMETHOD.
