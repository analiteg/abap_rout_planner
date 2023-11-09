CLASS lcl_route DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ls_address_data,
        vaddress  TYPE string,
        latitude  TYPE decfloat34,
        longitude TYPE decfloat34,
      END OF ls_address_data.

    TYPES:
      BEGIN OF ls_distance,
        del_distance TYPE decfloat16,
        del_time     TYPE decfloat16,
      END OF ls_distance.

    TYPES ty_orders    TYPE STANDARD TABLE OF zaorders WITH  KEY uuid uuid_w.
    TYPES ty_warehouse TYPE STANDARD TABLE OF zawarehouse WITH KEY uuid_w.
    TYPES ty_tarif     TYPE STANDARD TABLE OF zatarif WITH KEY min_distance max_distance.

    CLASS-METHODS create_instance
      RETURNING VALUE(ro_route) TYPE REF TO lcl_route.

    METHODS create_client
      IMPORTING url           TYPE string
      RETURNING VALUE(result) TYPE REF TO if_web_http_client
      RAISING   cx_static_check.

    METHODS save_orders_data_into_db
      IMPORTING lt_orders        TYPE STANDARD TABLE
      RETURNING VALUE(rv_status) TYPE string
      RAISING   cx_static_check.

    METHODS save_warehouses_data_into_db
      IMPORTING lt_warehouse     TYPE STANDARD TABLE
      RETURNING VALUE(rv_status) TYPE string
      RAISING   cx_static_check.

    METHODS save_tarifs_data_into_db
      IMPORTING lt_tarifs        TYPE STANDARD TABLE
      RETURNING VALUE(rv_status) TYPE string
      RAISING   cx_static_check.

    METHODS get_orders
      RETURNING VALUE(rt_orders) TYPE ty_orders
      RAISING   cx_static_check.

    METHODS get_warehouses
      RETURNING VALUE(rt_warehouse) TYPE ty_warehouse
      RAISING   cx_static_check.

    METHODS get_tarifs
      RETURNING VALUE(rt_tarif) TYPE ty_tarif
      RAISING   cx_static_check.

    METHODS get_geo_data
      IMPORTING iv_order         TYPE string
      RETURNING VALUE(rs_result) TYPE ls_address_data
      RAISING   cx_static_check.

    METHODS get_rout_data
      IMPORTING is_order         TYPE LINE OF ty_orders
                is_warehouse     TYPE LINE OF ty_warehouse
      RETURNING VALUE(rs_result) TYPE ls_distance
      RAISING   cx_static_check.

    METHODS update_orders_data
      IMPORTING it_orders           TYPE ty_orders
      RETURNING VALUE(rv_tp_status) TYPE string
      RAISING   cx_static_check.

    METHODS encode_polish_url
      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_string) TYPE string
      RAISING   cx_static_check.

    METHODS get_optimal_delivery_rout
*      IMPORTING iv_string        TYPE string
      RETURNING VALUE(rv_string) TYPE string
      RAISING   cx_static_check.

  PRIVATE SECTION.
    CLASS-DATA lo_route TYPE REF TO lcl_route.

    CONSTANTS base_url     TYPE string VALUE 'https://api.geoapify.com/v1/geocode/search?text='.
    CONSTANTS route_url    TYPE string VALUE 'https://api.geoapify.com/v1/routing?waypoints='.
    CONSTANTS api_key      TYPE string VALUE 'fc1823fd9ff24e1db96dced76209c85d'.
    CONSTANTS content_type TYPE string VALUE 'Content-type'.
    CONSTANTS json_content TYPE string VALUE 'text/xml; charset=UTF-8'.
    CONSTANTS json_content2 TYPE string VALUE 'application/json'.

ENDCLASS.


CLASS lcl_route IMPLEMENTATION.
  METHOD create_instance.
    ro_route = COND #( WHEN lo_route IS BOUND
                       THEN lo_route
                       ELSE NEW lcl_route( )  ).
    lo_route = ro_route.
  ENDMETHOD.

  METHOD save_orders_data_into_db.
    INSERT zaorders FROM TABLE @lt_orders ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      rv_status = | Inserted | & |{ lines( lt_orders ) }| & | rows|.
    ELSE.
      rv_status = | Error | & |{ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD save_warehouses_data_into_db.
    INSERT zawarehouse FROM TABLE @lt_warehouse ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      rv_status = | Inserted | & |{ lines( lt_warehouse ) }| & | rows|.
    ELSE.
      rv_status = | Error | & |{ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD save_tarifs_data_into_db.
    INSERT zatarif FROM TABLE @lt_tarifs ACCEPTING DUPLICATE KEYS.
    IF sy-subrc = 0.
      rv_status = | Inserted | & |{ lines( lt_tarifs ) }| & | rows|.
    ELSE.
      rv_status = | Error | & |{ sy-subrc }|.
    ENDIF.
  ENDMETHOD.

  METHOD create_client.
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
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

  METHOD get_geo_data.
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

  METHOD get_orders.
    DATA rt_return TYPE ty_orders.

    SELECT FROM zaorders
              FIELDS *
              INTO  CORRESPONDING FIELDS OF TABLE @rt_return.
    IF sy-subrc = 0.
      rt_orders = rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD get_warehouses.
    DATA rt_return TYPE ty_warehouse.

    SELECT FROM zawarehouse
              FIELDS client, uuid_w, address, longitude,latitude
              INTO  CORRESPONDING FIELDS OF TABLE @rt_return.
    IF sy-subrc = 0.
      rt_warehouse = rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD get_tarifs.
    DATA rt_return TYPE ty_tarif.

    SELECT FROM zatarif
              FIELDS *
              INTO  CORRESPONDING FIELDS OF TABLE @rt_return.
    IF sy-subrc = 0.
      rt_tarif = rt_return.
    ENDIF.
  ENDMETHOD.

  METHOD get_rout_data.
    DATA(ls_order) = is_order.
    DATA(ls_warehouse) = is_warehouse.

    DATA(url) = |{ route_url }| &
    |{ CONV string( ls_warehouse-latitude )  }| & |%2C| &
    |{ CONV string( ls_warehouse-longitude ) }| & |%7C| &
    |{ CONV string( ls_order-latitude ) }| & |%2C| &
    |{ CONV string( ls_order-longitude ) }| &
    |&mode=drive| & |&apiKey=| & |{ api_key }|.

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

    ASSIGN COMPONENT 'DISTANCE' OF STRUCTURE <fs_prop> TO FIELD-SYMBOL(<fs_distance>).
    ASSIGN <fs_distance>->* TO FIELD-SYMBOL(<fs_formatted_distance>).

    ASSIGN COMPONENT 'TIME' OF STRUCTURE <fs_prop> TO FIELD-SYMBOL(<fs_time>).
    ASSIGN <fs_time>->* TO FIELD-SYMBOL(<fs_formatted_time>).

    DATA ls_distance TYPE ls_distance.
    ls_distance-del_distance = <fs_formatted_distance>.
    ls_distance-del_time     = <fs_formatted_time>.

    rs_result = ls_distance.
  ENDMETHOD.

  METHOD update_orders_data.
    DATA lt_total_orders_step_1 TYPE ty_orders.
    DATA lt_total_orders_step_2 TYPE ty_orders.
    DATA lt_total_orders_step_3 TYPE ty_orders.
    DATA ls_full_address        TYPE ls_address_data.
    DATA ls_order_line          TYPE LINE OF ty_orders.

    LOOP AT it_orders ASSIGNING FIELD-SYMBOL(<fs_order_step_1>).
      ls_full_address = get_geo_data( <fs_order_step_1>-address ).
      ls_order_line = CORRESPONDING #( <fs_order_step_1> ).
      ls_order_line = CORRESPONDING #( BASE ( ls_order_line ) ls_full_address ).
      APPEND ls_order_line TO lt_total_orders_step_1.
    ENDLOOP.

    DATA lt_warehouse TYPE ty_warehouse.
    lt_warehouse = get_warehouses( ).

    LOOP AT lt_total_orders_step_1 ASSIGNING FIELD-SYMBOL(<fs_order_step_2>).
      DATA(ls_warehouse) = lt_warehouse[ uuid_w = <fs_order_step_2>-uuid_w ].
      DATA(ls_full) = get_rout_data( is_order = <fs_order_step_2> is_warehouse = ls_warehouse ).
      ls_order_line = CORRESPONDING #( <fs_order_step_2> ).
      ls_order_line = CORRESPONDING #( BASE ( ls_order_line ) ls_full ).
      APPEND ls_order_line TO lt_total_orders_step_2.
    ENDLOOP.

    DATA lt_tarif TYPE SORTED TABLE OF zatarif WITH NON-UNIQUE KEY min_distance max_distance.
    lt_tarif = get_tarifs( ).

    LOOP AT lt_total_orders_step_2 ASSIGNING FIELD-SYMBOL(<fs_order_step_3>).
      DATA(ls_tarif) = FILTER #( lt_tarif USING KEY primary_key WHERE min_distance < CONV #( <fs_order_step_3>-del_distance ) AND max_distance > CONV #( <fs_order_step_3>-del_distance ) ).
      ls_order_line = CORRESPONDING #( <fs_order_step_3> ).
      ls_order_line = CORRESPONDING #( BASE ( ls_order_line ) ls_tarif[ 1 ] ).
      ls_order_line-del_cost = ls_tarif[ 1 ]-zone_tarif * ( ls_order_line-del_distance ) / 1000.

      APPEND ls_order_line TO lt_total_orders_step_3.

    ENDLOOP.

    MODIFY zaorders FROM TABLE @lt_total_orders_step_3.
    IF sy-subrc <> 0.
      rv_tp_status = 'Error during insert/update'.
    ELSE.
      rv_tp_status = 'Data Updated'.
    ENDIF.
  ENDMETHOD.

  METHOD get_optimal_delivery_rout.
    SELECT
    zo~uuid,
    zo~longitude AS olongitude,
    zo~latitude AS olatitude,
    zw~uuid_w,
    zw~longitude AS wlongitude,
    zw~latitude AS wlatitude
    FROM zaorders AS zo
              INNER JOIN zawarehouse AS zw ON zo~uuid_w = zw~uuid_w
              " TODO: variable is assigned but never used (ABAP cleaner)
              INTO   TABLE @DATA(rt_return).

    CONSTANTS base_route_url     TYPE string VALUE 'https://api.geoapify.com/v1/routeplanner?apiKey=fc1823fd9ff24e1db96dced76209c85d'.




    TYPES : BEGIN OF ty_timew,
              time_wind TYPE STANDARD TABLE OF i WITH EMPTY KEY,
            END OF ty_timew.


    TYPES : BEGIN OF ty_agents,
              start_location TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY,
              time_windows   TYPE STANDARD TABLE OF ty_timew WITH EMPTY KEY,
            END OF ty_agents.

    TYPES : BEGIN OF ty_pickup,
              location_index TYPE i,
              duration       TYPE i,
            END OF ty_pickup.

    TYPES : BEGIN OF ty_delivery,
              location TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY,
              duration TYPE i,
            END OF ty_delivery.

    TYPES : BEGIN OF ty_shipments,
              id       TYPE string,
              delivery TYPE ty_delivery,
              pickup   TYPE ty_pickup,
            END OF ty_shipments.

    TYPES : BEGIN OF ty_locations,
              id       TYPE string,
              location TYPE STANDARD TABLE OF decfloat34 WITH EMPTY KEY,
            END OF ty_locations.

    TYPES : BEGIN OF ty_body,
              mode      TYPE string,
              agents    TYPE STANDARD TABLE OF ty_agents WITH EMPTY KEY,
              shipments TYPE STANDARD TABLE OF ty_shipments WITH EMPTY KEY,
              locations TYPE STANDARD TABLE OF ty_locations WITH EMPTY KEY,
            END OF ty_body.

    " Agents
    DATA agents          TYPE STANDARD TABLE OF ty_agents WITH EMPTY KEY.


    agents = VALUE #( ( start_location = VALUE #( ( CONV #( '23.19756076017041' ) ) ( CONV #( '53.14327315' ) ) )   time_windows = VALUE #( ( time_wind = VALUE #( ( 0 ) ( 10800  ) ) ) ) )
                      ( start_location = VALUE #( ( CONV #( '23.19756076017045' ) ) ( CONV #( '53.14327319' ) ) )   time_windows = VALUE #( ( time_wind = VALUE #( ( 0 ) ( 10800  ) ) ) ) )
    ).


    " Locations warehouse
    DATA locations TYPE STANDARD TABLE OF ty_locations WITH KEY id.
    DATA(warehouse) = get_warehouses( ).
    LOOP AT warehouse ASSIGNING FIELD-SYMBOL(<fs_warehouse>).
      locations = VALUE #( BASE locations ( id = <fs_warehouse>-uuid_w  location = VALUE #( (  <fs_warehouse>-longitude )  (  <fs_warehouse>-latitude ) ) ) ).
    ENDLOOP.


    " Shipments
    DATA shipments     TYPE STANDARD TABLE OF ty_shipments WITH EMPTY KEY.
    DATA(orders) = get_orders( ).
    LOOP AT orders ASSIGNING FIELD-SYMBOL(<fs_orders>).
      shipments = VALUE #( BASE shipments ( id = <fs_orders>-uuid pickup = VALUE #( location_index =  line_index( locations[ id = <fs_orders>-uuid_w ]  ) - 1   duration = 120  )
                                            delivery = VALUE #(  location = VALUE #( (  <fs_orders>-longitude  ) (  <fs_orders>-latitude  ) )  duration = 120 ) ) ).
    ENDLOOP.


    " Body
    DATA body TYPE  ty_body.
    body = VALUE #(  mode = 'drive' agents = agents shipments = shipments locations = locations  ).



    DATA(lv_json) = /ui2/cl_json=>serialize( data = body  pretty_name = /ui2/cl_json=>pretty_mode-low_case ).


    DATA(char) = '{"time_wind":[0,10800]}'.
    DATA(new_char) = '[0,10800]'.

    REPLACE ALL OCCURRENCES OF char IN lv_json WITH new_char.
*    REPLACE char WITH new_char INTO lv_json.



    DATA(url) = |{ base_route_url }|.
    DATA(client) = create_client( url ).
    DATA(req) = client->get_http_request(  ).
    req->set_text( lv_json ).
    req->set_header_field( i_name = content_type i_value = json_content2 ).

    DATA(result) = client->execute( if_web_http_client=>post )->get_text(  ).
    client->close(  ).


    DATA lr_data TYPE REF TO data.

    /ui2/cl_json=>deserialize( EXPORTING json         = result
                                         pretty_name  = /ui2/cl_json=>pretty_mode-user
                                         assoc_arrays = abap_true
                               CHANGING  data         = lr_data ).

    ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_data>).
    ASSIGN COMPONENT 'FEATURES' OF STRUCTURE <fs_data> TO FIELD-SYMBOL(<fs_features>).
    ASSIGN <fs_features>->* TO FIELD-SYMBOL(<fs_features_table>).

    LOOP AT <fs_features_table> ASSIGNING FIELD-SYMBOL(<fs_features_table_line>).
      ASSIGN <fs_features_table_line>->* TO FIELD-SYMBOL(<fs_features_table_line_1>).
      ASSIGN COMPONENT 'GEOMETRY'   OF STRUCTURE <fs_features_table_line_1> TO FIELD-SYMBOL(<fs_geometry>).
      ASSIGN COMPONENT 'PROPERTIES' OF STRUCTURE <fs_features_table_line_1> TO FIELD-SYMBOL(<fs_properties>).

      ASSIGN <fs_geometry>->* TO FIELD-SYMBOL(<fs_geometry_values>).
      ASSIGN <fs_properties>->* TO FIELD-SYMBOL(<fs_properties_values>).

      ASSIGN COMPONENT 'AGENT_INDEX' OF STRUCTURE <fs_properties_values> TO FIELD-SYMBOL(<fs_properties_agent_index>).
      ASSIGN COMPONENT 'DISTANCE'    OF STRUCTURE <fs_properties_values> TO FIELD-SYMBOL(<fs_properties_distance>).
      ASSIGN COMPONENT 'MODE'        OF STRUCTURE <fs_properties_values> TO FIELD-SYMBOL(<fs_properties_mode>).
      ASSIGN COMPONENT 'START_TIME'  OF STRUCTURE <fs_properties_values> TO FIELD-SYMBOL(<fs_properties_start_time>).
      ASSIGN COMPONENT 'END_TIME'    OF STRUCTURE <fs_properties_values> TO FIELD-SYMBOL(<fs_properties_end_time>).









    ENDLOOP.











  ENDMETHOD.
ENDCLASS.
