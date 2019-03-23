*&---------------------------------------------------------------------*
*& Report ZRPCLSTXE
*&---------------------------------------------------------------------*
*& Información: Programa similar al estándar PC_PAYRESULT (RPCLSTRE)
*&              que realiza la lectura de los resultados de nómina del
*&              esquema de paga extra española (cluster XE).
*&---------------------------------------------------------------------*
*& Creado por:  JTD
*& Fecha última versión: 23.03.2019
*&---------------------------------------------------------------------*
REPORT zrpclstxe.

TABLES: pernr.
TABLES: pcl1,
        pcl2.

* Llamada a includes.
INCLUDE rpppxd00.                      "Definición del buffer

DATA: BEGIN OF COMMON PART buffer.
INCLUDE rpppxd10.                      "Definición del buffer
DATA: END OF COMMON PART buffer.

INCLUDE rpppxm00.                      "Instrucciones para el buffer
INCLUDE rpc2cd00.   "Datos específicos: Cluster Directory
INCLUDE rpc2rx00.   "Datos específicos: Cluster nómina general
INCLUDE rpc2ree0.   "Datos específicos: Cluster nómina en España


* Definición de variables, tablas y estructuras del cluster:
CONSTANTS: marcado(1) VALUE 'X',
           c_si(1) VALUE '1',
           c_no(1) VALUE '2',
           c_cancel(1) VALUE 'A'.

*----------------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Local Class to set event
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    METHODS:
*       Double-click control
      handle_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

    METHODS:
*       Hotspot click control
      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION                                                 *
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

*   Handle Double Click
  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row e_column es_row_no.
  ENDMETHOD .                    "handle_double_click

*   Handle Hotspot Click
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING e_row_id
                                       e_column_id
                                       es_row_no.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Local Class to set event
CLASS lcl_event_handler2 DEFINITION.

  PUBLIC SECTION.
    METHODS:
*       Double-click control
      handle_double_click2 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no.

    METHODS:
*       Hotspot click control
      handle_hotspot_click2 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
* CLASS IMPLEMENTATION                                                 *
*----------------------------------------------------------------------*
CLASS lcl_event_handler2 IMPLEMENTATION.

*   Handle Double Click
  METHOD handle_double_click2.
    PERFORM handle_double_click2 USING e_row e_column es_row_no.
  ENDMETHOD .                    "handle_double_click

*   Handle Hotspot Click
  METHOD handle_hotspot_click2.
    PERFORM handle_hotspot_click2 USING e_row_id
                                       e_column_id
                                       es_row_no.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*---------------------------------------------------------------------*
* Tablas, estructuras y variables para el ALV.
*---------------------------------------------------------------------*
TYPES: BEGIN OF t_rgdir,
        pernr   TYPE persno,
        seqnr   TYPE cdseq,
        perpara TYPE char7,
        peren   TYPE char7,
        borrar  TYPE char4,
       END OF t_rgdir.

TYPES: BEGIN OF t_tablas,
         tabla        TYPE char10,
         descripcion  TYPE char40,
         registros(6) TYPE n,
       END OF t_tablas.

DATA: ok_code TYPE syucomm,
      ok_code2 TYPE syucomm,
      p_per(6) TYPE c,
      coment(1) TYPE c,
      g_main_control        TYPE REF TO cl_hr_lstce_main_control,
      g_error_handler       TYPE REF TO cl_hr_lstce_error_handler,
      gt_list TYPE TABLE OF t_rgdir WITH HEADER LINE,
      gt_list2 TYPE TABLE OF t_tablas WITH HEADER LINE.

DATA: gr_event_handler TYPE REF TO lcl_event_handler,
      gr_event_handler2 TYPE REF TO lcl_event_handler2,
      gr_alvgrid TYPE REF TO cl_gui_alv_grid,
      gr_alvgrid2 TYPE REF TO cl_gui_alv_grid,
      gc_custom_control_name TYPE scrfname VALUE 'CC_ALV',
      gc_custom_control_name2 TYPE scrfname VALUE 'CC_ALV2',
      gr_ccontainer TYPE REF TO cl_gui_custom_container,
      gr_ccontainer2 TYPE REF TO cl_gui_custom_container,
      gt_fieldcat TYPE lvc_t_fcat,
      gt_fieldcat2 TYPE lvc_t_fcat,
      gs_layout TYPE lvc_s_layo,
      gs_layout2 TYPE lvc_s_layo.


* ... and a model class object
DATA: gr_table   TYPE REF TO cl_salv_table.
* if you want to display the data table on your own Dynpro, you also need a container
DATA: gr_container TYPE REF TO cl_gui_custom_container.

DATA: g_okcode TYPE syucomm.
DATA nombre_tabla TYPE char12.
DATA descr_tabla(60).

SELECTION-SCREEN BEGIN OF SCREEN 1100 AS SUBSCREEN.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(25) text-002 FOR FIELD s_pernr.
SELECT-OPTIONS s_pernr FOR pernr-pernr NO INTERVALS MATCHCODE OBJECT prem.

SELECTION-SCREEN COMMENT 70(20) text-003 FOR FIELD s_abkrs.
SELECT-OPTIONS s_abkrs FOR pernr-abkrs NO INTERVALS MATCHCODE OBJECT h_t549a.

SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(28) text-005 FOR FIELD p_pbeg.
PARAMETER p_pbeg TYPE begda.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END   OF SCREEN 1100.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1000 OUTPUT.
  SET PF-STATUS 'BOTONES'.
  SET TITLEBAR 'TITULO'.
ENDMODULE.                 " STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  Preparar_alvs  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE preparar_alvs OUTPUT.
  PERFORM preparar_alvs.
ENDMODULE.                 " Preparar_alvs  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1000 INPUT.

  IF s_pernr[] IS INITIAL AND ok_code EQ 'ENTER'.
    MESSAGE i016(rp) WITH text-i01.
  ELSE.
    CASE ok_code.
      WHEN 'VOLVER'.
        PERFORM liberar_memoria.
        LEAVE TO SCREEN 0.
      WHEN 'ATRAS'.
        PERFORM liberar_memoria.
        LEAVE TO SCREEN 0.
      WHEN 'SALIR'.
        PERFORM liberar_memoria.
        LEAVE PROGRAM.
      WHEN 'ENTER'.
        PERFORM seleccionar_registros.
        PERFORM display_alv.
    ENDCASE.
  ENDIF.

ENDMODULE.                 " USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*&      Form  display_alv
*----------------------------------------------------------------------*
FORM display_alv .

  CALL METHOD gr_alvgrid->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

  CALL METHOD gr_alvgrid2->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

ENDFORM.                    " display_alv

*&---------------------------------------------------------------------*
*&      Form  handle_double_click
*&---------------------------------------------------------------------*
FORM handle_double_click USING    i_row TYPE lvc_s_row
                                  i_column TYPE lvc_s_col
                                  is_row_no TYPE lvc_s_roid.


  READ TABLE gt_list INDEX is_row_no-row_id.
  IF sy-subrc = 0.
    CLEAR gt_list2.
    REFRESH gt_list2.

    CLEAR rx-key.
    MOVE gt_list-pernr TO rx-key-pernr.
    MOVE gt_list-seqnr TO rx-key-seqno.
    rp-imp-c2-xe.

*   Relleno la tabla WPBP
    IF wpbp[] IS NOT INITIAL.
      MOVE 'WPBP' TO gt_list2-tabla.
      MOVE 'Puesto tbjo./emol.básicos' TO gt_list2-descripcion.
      DESCRIBE TABLE wpbp LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla RT
    IF rt[] IS NOT INITIAL.
      MOVE 'RT' TO gt_list2-tabla.
      MOVE 'Tabla de resultados' TO gt_list2-descripcion.
      DESCRIBE TABLE rt LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla CRT
    IF crt[] IS NOT INITIAL.
      MOVE 'CRT' TO gt_list2-tabla.
      MOVE 'Tablas resultado acumuladas' TO gt_list2-descripcion.
      DESCRIBE TABLE crt LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla BT
    IF bt[] IS NOT INITIAL.
      MOVE 'BT' TO gt_list2-tabla.
      MOVE 'Transferencia' TO gt_list2-descripcion.
      DESCRIBE TABLE bt LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla AB
    IF ab[] IS NOT INITIAL.
      MOVE 'AB' TO gt_list2-tabla.
      MOVE 'Absentismos' TO gt_list2-descripcion.
      DESCRIBE TABLE ab LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla ABC
    IF abc[] IS NOT INITIAL.
      MOVE 'ABC' TO gt_list2-tabla.
      MOVE 'Acumulación Clases de absentismos' TO gt_list2-descripcion.
      DESCRIBE TABLE abc LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

**   Relleno la tabla Version
*    IF version IS NOT INITIAL.
*      MOVE 'VERSION' TO gt_list2-tabla.
*      MOVE 'Información de creación' TO gt_list2-descripcion.
*      DESCRIBE TABLE version LINES gt_list2-registros.
*      append gt_list2.
*    ENDIF.

*   Relleno la tabla Versc
    IF versc IS NOT INITIAL.
      MOVE 'VERSC' TO gt_list2-tabla.
      MOVE 'Información status de nómina' TO gt_list2-descripcion.
      MOVE 1 TO gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla ST
    IF st[] IS NOT INITIAL.
      MOVE 'ST' TO gt_list2-tabla.
      MOVE 'Impuestos' TO gt_list2-descripcion.
      DESCRIBE TABLE st LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla ST
    IF sv[] IS NOT INITIAL.
      MOVE 'SV' TO gt_list2-tabla.
      MOVE 'Seguridad Social' TO gt_list2-descripcion.
      DESCRIBE TABLE sv LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla SP
    IF sp[] IS NOT INITIAL.
      MOVE 'SP' TO gt_list2-tabla.
      MOVE 'Pagas extraordinarias' TO gt_list2-descripcion.
      DESCRIBE TABLE sp LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

*   Relleno la tabla Garnt
    IF garnt[] IS NOT INITIAL.
      MOVE 'GARNT' TO gt_list2-tabla.
      MOVE 'Retenciones (España)' TO gt_list2-descripcion.
      DESCRIBE TABLE garnt LINES gt_list2-registros.
      APPEND gt_list2.
    ENDIF.

  ENDIF.

  CALL METHOD gr_alvgrid2->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

ENDFORM.                    " handle_double_click
*---------------------------------------------------------------------*
*       FORM handle_hotspot_click                                     *
*---------------------------------------------------------------------*
FORM handle_hotspot_click USING e_row_id TYPE lvc_s_row
                                e_column_id TYPE lvc_s_col
                                es_row_no TYPE lvc_s_roid.

  DATA: titulo TYPE string,
        texto  TYPE string.
  DATA answer  TYPE char1.
  DATA clave   TYPE pclkey.
  DATA error   TYPE flag.

  MOVE text-004 TO titulo.
  CONCATENATE text-006 text-007 text-008 INTO texto SEPARATED BY space.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = titulo
      text_question  = texto
    IMPORTING
      answer         = answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF answer EQ '1'.

    READ TABLE gt_list INDEX e_row_id.
    IF sy-subrc EQ 0.

*     Existe un report estándar que borra registros del cluster XE.
*     Hago un submit a este programa.
*      submit rpudxee0
*              with r_iperm = '01' "Normalmente va a ser mensual.
*              with r_pabrj = gt_list-peren+3(4)
*              with r_pabrp = gt_list-peren(2)
*              with r_pernr = gt_list-pernr
*              with r_test  = space
*              and return.

*   Mejor que hacer el submit, me creo un perform donde utilizo justo
*   lo que necesito del report estándar.
      PERFORM borrar_cluster_xe USING gt_list
                                CHANGING error.

      IF error EQ space.
*       Actualizo la lista del ALV.
        DELETE gt_list INDEX e_row_id.

*       Refresco el listado.
        CALL METHOD gr_alvgrid->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
      ENDIF.

    ENDIF.
  ELSE.
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = text-009
        txt1  = text-010
        txt2  = space.

  ENDIF.

ENDFORM.                    "handle_hotspot_click
*&---------------------------------------------------------------------*
*&      Form  handle_double_click
*&---------------------------------------------------------------------*
FORM handle_double_click2 USING    i_row TYPE lvc_s_row
                                  i_column TYPE lvc_s_col
                                  is_row_no TYPE lvc_s_roid.


  READ TABLE gt_list2 INDEX is_row_no-row_id.
  IF sy-subrc = 0.
    CLEAR nombre_tabla.
    CLEAR descr_tabla.
    MOVE gt_list2-tabla TO nombre_tabla.
    MOVE gt_list2-descripcion TO descr_tabla.
    CALL SCREEN '2000'.
  ENDIF.

  CALL METHOD gr_alvgrid2->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.

ENDFORM.                    " handle_double_click
*---------------------------------------------------------------------*
*       FORM handle_hotspot_click                                     *
*---------------------------------------------------------------------*
FORM handle_hotspot_click2 USING e_row_id TYPE lvc_s_row
                                e_column_id TYPE lvc_s_col
                                es_row_no TYPE lvc_s_roid.

ENDFORM.                    "handle_hotspot_click

*&---------------------------------------------------------------------*
*&      Form  prepare_field_catalog
*&---------------------------------------------------------------------*
FORM prepare_field_catalog CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname  = 'PERNR'.
  ls_fcat-outputlen  = '8'.
  ls_fcat-coltext    = 'NºEmpleado'.
  ls_fcat-seltext    = 'NºEmpleado'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'SEQNR'.
  ls_fcat-outputlen  = '5'.
  ls_fcat-coltext    = 'Secuencia'.
  ls_fcat-seltext    = 'Secuencia'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'PERPARA'.
  ls_fcat-outputlen  = '7'.
  ls_fcat-coltext    = 'Per.Para'.
  ls_fcat-seltext    = 'Per.Para'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'PEREN'.
  ls_fcat-outputlen  = '7'.
  ls_fcat-coltext    = 'Per.En'.
  ls_fcat-seltext    = 'Per.En'.
  APPEND ls_fcat TO pt_fieldcat.

* Opción de Borrado sólo disponible en Desarrollo.
  IF sy-sysid EQ 'ACD'.
    CLEAR ls_fcat.
    ls_fcat-fieldname  = 'BORRAR'.
    ls_fcat-outputlen  = '3'.
    ls_fcat-coltext    = 'Borrar'.
    ls_fcat-seltext    = 'Borrar'.
    ls_fcat-icon       = 'X'.
    ls_fcat-hotspot    = 'X'.
    APPEND ls_fcat TO pt_fieldcat.
  ENDIF.

ENDFORM.                    " prepare_field_catalog
*&---------------------------------------------------------------------*
*&      Form  prepare_layout
*&---------------------------------------------------------------------*
FORM prepare_layout CHANGING ps_layout TYPE lvc_s_layo.

  ps_layout-zebra = 'X'.
  ps_layout-smalltitle = 'X'.

ENDFORM.                    " prepare_layout
*&---------------------------------------------------------------------*
*&      Form  prepare_field_catalog
*&---------------------------------------------------------------------*
FORM prepare_field_catalog2 CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname  = 'TABLA'.
  ls_fcat-outputlen  = '10'.
  ls_fcat-coltext    = 'Nombre'.
  ls_fcat-seltext    = 'Nombre'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'DESCRIPCION'.
  ls_fcat-outputlen  = '60'.
  ls_fcat-coltext    = 'Denominación'.
  ls_fcat-seltext    = 'Denominación'.
  APPEND ls_fcat TO pt_fieldcat.

  CLEAR ls_fcat.
  ls_fcat-fieldname  = 'REGISTROS'.
  ls_fcat-outputlen  = '6'.
  ls_fcat-coltext    = 'Cant.reg'.
  ls_fcat-seltext    = 'Cant.reg.'.
  APPEND ls_fcat TO pt_fieldcat.

ENDFORM.                    " prepare_field_catalog
*&---------------------------------------------------------------------*
*&      Form  prepare_layout
*&---------------------------------------------------------------------*
FORM prepare_layout2 CHANGING ps_layout TYPE lvc_s_layo.


  ps_layout-zebra = 'X'.
  ps_layout-smalltitle = 'X'.

ENDFORM.                    " prepare_layout
*&---------------------------------------------------------------------*
*&      Form  Liberar_memoria
*&---------------------------------------------------------------------*
FORM liberar_memoria .

  CALL METHOD gr_alvgrid->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  CALL METHOD gr_alvgrid2->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  CALL METHOD gr_ccontainer->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

  CALL METHOD gr_ccontainer2->free
    EXCEPTIONS
      cntl_error        = 1
      cntl_system_error = 2
      OTHERS            = 3.

ENDFORM.                    " Liberar_memoria
*&---------------------------------------------------------------------*
*&      Form  seleccionar_registros
*&---------------------------------------------------------------------*
FORM seleccionar_registros .

* Accedo al cluster:
  DATA it_empleados TYPE STANDARD TABLE OF pa0001 WITH HEADER LINE.
  DATA it_pcl2 TYPE TABLE OF pcl2 WITH HEADER LINE.
  DATA var_srtfd TYPE pclkey.
  DATA fecha TYPE char6.

  CLEAR gt_list.
  REFRESH gt_list.

  SELECT * INTO TABLE it_empleados
    FROM pa0001 WHERE pernr IN s_pernr AND
                      abkrs IN s_abkrs AND
                      endda GE p_pbeg.

  SORT it_empleados BY pernr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_empleados COMPARING pernr.

  CLEAR fecha.
  MOVE p_pbeg(6) TO fecha.

  LOOP AT it_empleados.
    CLEAR cd-key.
    MOVE it_empleados-pernr TO cd-key.
    rp-imp-c2-cu.

    LOOP AT dir2 WHERE inper GE fecha AND
                       abkrs IN s_abkrs.
      MOVE it_empleados-pernr TO gt_list-pernr.
      MOVE dir2-seqnr TO gt_list-seqnr.
      CONCATENATE dir2-inper+4(2) '.' dir2-inper(4) INTO gt_list-peren.
      CONCATENATE dir2-fpper+4(2) '.' dir2-fpper(4) INTO gt_list-perpara.
      MOVE '@18@' TO gt_list-borrar.
      APPEND gt_list.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    " seleccionar_registros
*&---------------------------------------------------------------------*
*&      Form  Preparar_alvs
*&---------------------------------------------------------------------*
FORM preparar_alvs .

  IF ( gr_alvgrid IS INITIAL ).

    CREATE OBJECT gr_ccontainer
      EXPORTING
        container_name              = gc_custom_control_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT gr_alvgrid
      EXPORTING
        i_parent          = gr_ccontainer
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM prepare_field_catalog CHANGING gt_fieldcat.
    PERFORM prepare_layout CHANGING gs_layout.

    CALL METHOD gr_alvgrid->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_list[]
        it_fieldcatalog               = gt_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

*   Definimos los eventos.
    CREATE OBJECT gr_event_handler.
    SET HANDLER gr_event_handler->handle_double_click FOR gr_alvgrid.
    SET HANDLER gr_event_handler->handle_hotspot_click FOR gr_alvgrid.

*--------------------------------------------------------------------*
*                     SEGUNDO ALV.
*--------------------------------------------------------------------*
    CREATE OBJECT gr_ccontainer2
      EXPORTING
        container_name              = gc_custom_control_name2
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT gr_alvgrid2
      EXPORTING
        i_parent          = gr_ccontainer2
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM prepare_field_catalog2 CHANGING gt_fieldcat2.
    PERFORM prepare_layout2 CHANGING gs_layout2.

    CALL METHOD gr_alvgrid2->set_table_for_first_display
      CHANGING
        it_outtab                     = gt_list2[]
        it_fieldcatalog               = gt_fieldcat2
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


*   Definimos los eventos.
    CREATE OBJECT gr_event_handler2.
    SET HANDLER gr_event_handler2->handle_double_click2 FOR gr_alvgrid2.
    SET HANDLER gr_event_handler2->handle_hotspot_click2 FOR gr_alvgrid2.

  ENDIF.

ENDFORM.                    " Preparar_alvs
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE g_okcode.
    WHEN 'VOLVER' OR 'ATRAS' OR 'SALIR'.
      CALL METHOD gr_container->free
        EXCEPTIONS
          cntl_error        = 1
          cntl_system_error = 2
          OTHERS            = 3.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
*      call method gr_container->free
*        exceptions
*          cntl_error        = 1
*          cntl_system_error = 2
*          others            = 3.
*      leave program.
  ENDCASE.

  CLEAR g_okcode.

ENDMODULE.                 " USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

  FIELD-SYMBOLS <nombre> TYPE table.
  FIELD-SYMBOLS <nombre2> TYPE any.

  SET PF-STATUS 'BOTONES'.
  SET TITLEBAR 'TITULO'.

  CREATE OBJECT gr_container
    EXPORTING
      container_name = 'CONTAINER'.

  IF NOT ( nombre_tabla EQ 'VERSC' OR nombre_tabla EQ 'VERSION' ).
    CONCATENATE nombre_tabla '[]' INTO nombre_tabla.
    ASSIGN (nombre_tabla) TO <nombre>.
  ELSE.
    ASSIGN (nombre_tabla) TO <nombre2>.

    DATA dref TYPE REF TO data.
    IF nombre_tabla EQ 'VERSC'.
      CREATE DATA dref TYPE STANDARD TABLE OF ('PC202') WITH DEFAULT KEY.
    ELSE.
      CREATE DATA dref TYPE STANDARD TABLE OF ('PC202') WITH DEFAULT KEY.
    ENDIF.

    ASSIGN dref->* TO <nombre>.
    APPEND <nombre2> TO <nombre>.
  ENDIF.

  TRY.
*gr_container->free( ).
      cl_salv_table=>factory(
        EXPORTING r_container = gr_container
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      =    <nombre> ).

    CATCH cx_salv_msg.
  ENDTRY.

  PERFORM display.

ENDMODULE.                 " STATUS_2000  OUTPUT
*&--------------------------------------------------------------------*
*&      Form  set_display_settings
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_display_settings.

  DATA texto TYPE lvc_title.

  DATA:
    ls_display TYPE REF TO cl_salv_display_settings.

  ls_display = gr_table->get_display_settings( ).

  TRY.
      CLEAR texto.
      MOVE descr_tabla TO texto.
      CONCATENATE texto ' - ' gt_list2-tabla INTO texto SEPARATED BY space.
      ls_display->set_list_header( texto ).
      ls_display->set_vertical_lines( abap_false ).
      ls_display->set_horizontal_lines( abap_false ).
      ls_display->set_striped_pattern( abap_true ).
      ls_display->set_list_header_size( cl_salv_display_settings=>c_header_size_small ).
      ls_display->set_suppress_empty_data( abap_true ).
    CATCH cx_no_check.
  ENDTRY.

ENDFORM.                    "set_display_settings

*&--------------------------------------------------------------------*
*&      Form  set_columns
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_columns.

  DATA:
    lr_columns TYPE REF TO cl_salv_columns,  "global columns settings
    lr_column  TYPE REF TO cl_salv_column_table. "individual column setting

  lr_columns = gr_table->get_columns( ).

  lr_columns->set_optimize( abap_true ).

ENDFORM.                    "set_columns

*&--------------------------------------------------------------------*
*&      Form  set_sort
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_sort.

ENDFORM.                    "set_sort

*&--------------------------------------------------------------------*
*&      Form  set_aggregations
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_aggregations.

ENDFORM.                    "set_aggregations

*&--------------------------------------------------------------------*
*&      Form  set_filter
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_filter.

ENDFORM.                    "set_filter

*&--------------------------------------------------------------------*
*&      Form  set_metadata
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM set_metadata .
*§4 depending on the choices made in the lower frame of the selection screen,
*   metadata default settings are changed
  PERFORM set_display_settings.

*   metadata for columns
  PERFORM set_columns.

* sort by company and flight number
  PERFORM set_sort.

* aggregate the paymentsum
  PERFORM set_aggregations.

* filter by currency
  PERFORM set_filter.

ENDFORM.                    " set_metadata

*&---------------------------------------------------------------------*
*&      Form  display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display .

*§4 change the metadata default settings
  PERFORM set_metadata.

*§5 offer the default set of ALV generic funtions
  DATA: lr_functions TYPE REF TO cl_salv_functions_list.

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_default( ).

*... and display the table
  gr_table->display( ).

ENDFORM.                    " display
*&---------------------------------------------------------------------*
*&      Form  BORRAR_CLUSTER_XE
*&---------------------------------------------------------------------*
FORM borrar_cluster_xe  USING    p_gt_list TYPE t_rgdir
                        CHANGING p_error.

  DATA g_bapireturn LIKE bapireturn1.
  DATA iperi TYPE iperi.
  DATA: g_del_dir2 LIKE pc261 OCCURS 0 WITH HEADER LINE,
        g_old_dir2 LIKE pc261 OCCURS 0 WITH HEADER LINE,
        g_new_dir2 LIKE pc261 OCCURS 0 WITH HEADER LINE.

* Borro la variable que controla si se ha producido error.
  CLEAR p_error.

  CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = p_gt_list-pernr
    IMPORTING
      return = g_bapireturn.

  IF g_bapireturn IS NOT INITIAL.
    MOVE 'X' TO p_error.
    MESSAGE i016(rp) WITH text-i04.
  ELSE.
    MOVE p_gt_list-peren+3(4) TO iperi(4).
    MOVE p_gt_list-peren(2)   TO iperi+4(2).
    CALL FUNCTION 'HR_ES_DELETE_XE'
      EXPORTING
        p_test               = space
        p_pernr              = p_gt_list-pernr
        p_iperm              = '01'
        p_inper              = iperi
*       P_NO_AUTHORITY_CHECK = ' '
      TABLES
        p_del_dir2           = g_del_dir2
        p_old_dir2           = g_old_dir2
        p_new_dir2           = g_new_dir2
      EXCEPTIONS
        OTHERS               = 1.

    IF sy-subrc NE 0 OR g_del_dir2[] IS INITIAL.
      MOVE 'X' TO p_error.
      MESSAGE i016(rp) WITH text-i02.
    ELSE.
      CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = p_gt_list-pernr
        IMPORTING
          return = g_bapireturn.
    ENDIF.

    IF NOT g_del_dir2[] IS INITIAL.
      MESSAGE i016(rp) WITH text-i03.
    ENDIF.
  ENDIF.
ENDFORM.                    " BORRAR_CLUSTER_XE
