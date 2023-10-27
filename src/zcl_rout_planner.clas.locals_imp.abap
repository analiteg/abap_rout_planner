CLASS lcl_route DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ls_address_data,
        vaddress  TYPE string,
        latitude  TYPE decfloat34,
        longitude TYPE decfloat34,
      END OF ls_address_data.

    TYPES ty_orders TYPE STANDARD TABLE OF zaorders WITH EMPTY KEY.

    CLASS-METHODS create_instance
      RETURNING VALUE(ro_route) TYPE REF TO lcl_route.

    METHODS create_client
      IMPORTING url           TYPE string
      RETURNING VALUE(result) TYPE REF TO if_web_http_client
      RAISING   cx_static_check.

    METHODS save_orders_into_db
      IMPORTING lt_orders        TYPE STANDARD TABLE
      RETURNING VALUE(rv_status) TYPE string
      RAISING   cx_static_check.

    METHODS get_orders
      RETURNING VALUE(rt_orders) TYPE ty_orders
      RAISING   cx_static_check.

    METHODS get_address
      IMPORTING iv_order         TYPE string
      RETURNING VALUE(rs_result) TYPE ls_address_data
      RAISING   cx_static_check.

    METHODS translit_polish
      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_string) TYPE string
      RAISING   cx_static_check.

    METHODS encode_polish_url
      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_string) TYPE string
      RAISING   cx_static_check.

    METHODS update_orders
      IMPORTING it_orders           TYPE ty_orders
      RETURNING VALUE(rv_tp_status) TYPE string
      RAISING   cx_static_check.

  PRIVATE SECTION.
    CLASS-DATA lo_route TYPE REF TO lcl_route.

    CONSTANTS base_url     TYPE string VALUE 'https://api.geoapify.com/v1/geocode/search?text='.
    CONSTANTS api_key      TYPE string VALUE 'fc1823fd9ff24e1db96dced76209c85d'.
    CONSTANTS content_type TYPE string VALUE 'Content-type'.
    CONSTANTS json_content TYPE string VALUE 'text/xml; charset=UTF-8'.

ENDCLASS.


CLASS lcl_route IMPLEMENTATION.
  METHOD create_instance.
    ro_route = COND #( WHEN lo_route IS BOUND
                       THEN lo_route
                       ELSE NEW lcl_route( )  ).
    lo_route = ro_route.
  ENDMETHOD.

  METHOD save_orders_into_db.
    INSERT zaorders FROM TABLE @lt_orders ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      rv_status = | Inserted | & |{ lines( lt_orders ) }| & | rows|.
    ELSE.
      rv_status = | Error | & |{ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD create_client.
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD.

  METHOD get_address.
    DATA(lv_addr) = iv_order.

    CONDENSE lv_addr.
    lv_addr = encode_polish_url( lv_addr ).
    DATA(url) = |{ base_url }| & |{ lv_addr }| & |&apiKey=| & |{ api_key }|.
    " data(new_url) = cl_http_utility=>if_http_utility~encode_utf8( url ).Possible to use ONLY on non trial platform.

    DATA(client) = create_client( url ).
    DATA(response) = client->execute( if_web_http_client=>get )->get_text( ).
    client->close( ).

    DATA lr_data TYPE REF TO data.

    /ui2/cl_json=>deserialize( EXPORTING json         = response
                                         pretty_name  = /ui2/cl_json=>pretty_mode-user
                                         assoc_arrays = abap_true
                               CHANGING  data         = lr_data ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_data>).
    ASSIGN COMPONENT 'FEATURES' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_features>).
    ASSIGN <fs_features>->* TO FIELD-SYMBOL(<fs_features_table>).

    LOOP AT <fs_features_table> ASSIGNING FIELD-SYMBOL(<fs_features_table_line>).
      IF sy-tabix > 1.
        EXIT.
      ENDIF.
      ASSIGN <fs_features_table_line>->* TO FIELD-SYMBOL(<fs_features_table_line_1>).

    ENDLOOP.

    ASSIGN COMPONENT 'PROPERTIES' OF STRUCTURE <fs_features_table_line_1> TO FIELD-SYMBOL(<fs_properties>).
    ASSIGN <fs_properties>->* TO FIELD-SYMBOL(<fs_prop>).

    ASSIGN COMPONENT 'FORMATTED' OF STRUCTURE <fs_prop> TO FIELD-SYMBOL(<fs_address>).
    ASSIGN <fs_address>->* TO FIELD-SYMBOL(<fs_formatted_address>).

    ASSIGN COMPONENT 'LAT' OF STRUCTURE <fs_prop> TO FIELD-SYMBOL(<fs_lat>).
    ASSIGN <fs_lat>->* TO FIELD-SYMBOL(<fs_latitude>).

    ASSIGN COMPONENT 'LON' OF STRUCTURE <fs_prop> TO FIELD-SYMBOL(<fs_lon>).
    ASSIGN <fs_lon>->* TO FIELD-SYMBOL(<fs_longitude>).

    DATA ls_address TYPE ls_address_data.

    ls_address-vaddress  = <fs_formatted_address>.
    ls_address-latitude  = <fs_latitude>.
    ls_address-longitude = <fs_longitude>.

    rs_result = ls_address.
  ENDMETHOD.

  METHOD translit_polish.
    TYPES:
      BEGIN OF ls_polish_trans,
        psymbol TYPE string,
        usimbol TYPE string,
      END OF ls_polish_trans.

    DATA lt_translit TYPE HASHED TABLE OF ls_polish_trans WITH UNIQUE KEY psymbol.

    lt_translit = VALUE #( ( psymbol   = 'ą' usimbol = 'a' )
                           ( psymbol   = 'Ą' usimbol = 'A' )
                           ( psymbol   = 'ć' usimbol = 'c' )
                           ( psymbol   = 'Ć' usimbol = 'C' )
                           ( psymbol   = 'ę' usimbol = 'e' )
                           ( psymbol   = 'Ę' usimbol = 'E' )
                           ( psymbol   = 'ł' usimbol = 'l' )
                           ( psymbol   = 'Ł' usimbol = 'L' )
                           ( psymbol   = 'ń' usimbol = 'n' )
                           ( psymbol   = 'Ń' usimbol = 'N' )
                           ( psymbol   = 'ó' usimbol = 'o' )
                           ( psymbol   = 'Ó' usimbol = 'O' )
                           ( psymbol   = 'ś' usimbol = 's' )
                           ( psymbol   = 'Ś' usimbol = 'S' )
                           ( psymbol   = 'ż' usimbol = 'z' )
                           ( psymbol   = 'Ż' usimbol = 'Z' )
                           ( psymbol   = 'ź' usimbol = 'z' )
                           ( psymbol   = 'Ź' usimbol = 'Z' ) ).

    DATA(lv_string) = iv_string.
    DATA index    TYPE i.
    DATA char     TYPE c LENGTH 1.
    DATA new_char TYPE c LENGTH 1.
    DATA(length) = strlen( lv_string ).

    WHILE index < length.
      char = lv_string+index(1).
      IF line_exists( lt_translit[ psymbol = char ] ).
        new_char = lt_translit[ psymbol = char ]-usimbol.
        REPLACE char WITH new_char INTO lv_string.
      ENDIF.
      index += 1.
    ENDWHILE.

    rv_string = lv_string.
  ENDMETHOD.

  METHOD encode_polish_url.
    TYPES:
      BEGIN OF ls_polish_trans,
        psymbol TYPE string,
        usimbol TYPE string,
      END OF ls_polish_trans.

    DATA lt_translit TYPE HASHED TABLE OF ls_polish_trans WITH UNIQUE KEY psymbol.

    lt_translit = VALUE #( ( psymbol   = 'ą' usimbol = '%C4%85' )
                           ( psymbol   = 'Ą' usimbol = '%C4%84' )
                           ( psymbol   = 'ć' usimbol = '%C4%87' )
                           ( psymbol   = 'Ć' usimbol = '%C4%86' )
                           ( psymbol   = 'ę' usimbol = '%C4%99' )
                           ( psymbol   = 'Ę' usimbol = '%C4%98' )
                           ( psymbol   = 'ł' usimbol = '%C5%82' )
                           ( psymbol   = 'Ł' usimbol = '%C5%81' )
                           ( psymbol   = 'ń' usimbol = '%C5%84' )
                           ( psymbol   = 'Ń' usimbol = '%C5%83' )
                           ( psymbol   = 'ó' usimbol = '%C3%B3' )
                           ( psymbol   = 'Ó' usimbol = '%C3%93' )
                           ( psymbol   = 'ś' usimbol = '%C5%9B' )
                           ( psymbol   = 'Ś' usimbol = '%C5%9A' )
                           ( psymbol   = 'ż' usimbol = '%C5%BC' )
                           ( psymbol   = 'Ż' usimbol = '%C5%BB' )
                           ( psymbol   = 'ź' usimbol = '%C5%BA' )
                           ( psymbol   = 'Ź' usimbol = '%C5%B9' )
                           ( psymbol   = ' ' usimbol = '%20' ) ).

    DATA(lv_string) = iv_string.
    DATA index    TYPE i.
    DATA char     TYPE c LENGTH 1.
    DATA new_char TYPE string.
    DATA(length) = strlen( lv_string ).

    WHILE index < length.
      char = lv_string+index(1).
      IF line_exists( lt_translit[ psymbol = char ] ).
        new_char = lt_translit[ psymbol = char ]-usimbol.
        length += ( strlen( new_char ) - 1 ).
        REPLACE char WITH new_char INTO lv_string.
      ENDIF.
      index += 1.
    ENDWHILE.

    rv_string = lv_string.
  ENDMETHOD.

  METHOD get_orders.
    DATA rt_return TYPE ty_orders.

    SELECT FROM zaorders
              FIELDS client, uuid, cname, address
              INTO  CORRESPONDING FIELDS OF TABLE @rt_return.
    IF sy-subrc = 0.
      rt_orders = rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD update_orders.

    DATA lt_total_orders TYPE ty_orders.
    DATA ls_full_address TYPE ls_address_data.
    DATA ls_order_line   TYPE LINE OF ty_orders.

    LOOP AT it_orders ASSIGNING FIELD-SYMBOL(<fs_order>).
      ls_full_address = get_address( <fs_order>-address ).
      ls_order_line = CORRESPONDING #( <fs_order> ).
      ls_order_line = CORRESPONDING #( BASE ( ls_order_line ) ls_full_address ).
      APPEND ls_order_line TO lt_total_orders.
    ENDLOOP.

*    MODIFY zaorders FROM TABLE @lt_total_orders.
*    IF sy-subrc <> 0.
*      rv_tp_status = 'Error during insert/update'.
*    ELSE.
*      rv_tp_status = 'Data Updated'.
*    ENDIF.
  ENDMETHOD.
ENDCLASS.
