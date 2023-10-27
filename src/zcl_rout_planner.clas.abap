CLASS zcl_rout_planner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.


CLASS zcl_rout_planner IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    " Step 1 - Create Instance (singleton)
    DATA(mo_route) = lcl_route=>create_instance( ).

*    " Step 2 - Create Table Of Delivery Points
*    DATA(system_uuid) = cl_uuid_factory=>create_system_uuid( ).
*
*    DATA lt_orders TYPE STANDARD TABLE OF zaorders.
*    lt_orders = VALUE #(
*       ( cname   = 'Szkoła Podstawowa Nr 1 im. Juliusza Słowackiego w Białymstoku'
*          address = 'ul. Juliusza Słowackiego 4 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 5 im. Władysława Broniewskiego w Białymstoku'
*          address = 'ul. Kamienna 15 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 6 im. Jarosława Iwaszkiewicza w Białymstoku'
*          address = 'ul. Wesoła 11A Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 8 im. Świętego Kazimierza Królewicza w Białymstoku'
*          address = 'ul. Jesienna 8 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname = 'Szkoła Podstawowa Nr 9 im. 42 Pułku Piechoty w Białymstoku' address = 'ul. Legionowa 7 Białystok' uuid = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 12 im. Zygmunta Glogera w Białymstoku'
*          address = 'ul. Komisji Edukacji Narodowej 1A Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 14 im. Kazimierza Pułaskiego Zespół Szkolno - Przedszkolny Nr 2 w Białymstoku'
*          address = 'ul. Kazimierza Pułaskiego 25 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 15 im Marii Skłodowskiej-Curie w Białymstoku'
*          address = 'ul. Władysława Broniewskiego 1 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname = 'Szkoła Podstawowa Nr 19 im. Mieszka I w Białymstoku ' address = 'ul. Mieszka I 18 Białystok' uuid = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 21 im. Marszałka Józefa Piłsudskiego w Białymstoku'
*          address = 'ul Polowa 7/1 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 22 Zespół Szkół Ogólnokształcących Mistrzostwa Sportowego Nr 1 im. Marii Konopnickiej w Białymstoku'
*          address = 'ul. Marii Konopnickiej 3 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 24 z Oddziałami Dwujęzycznymi im. 2 Korpusu Polskich Sił Zbrojnych na Zachodzie w Białymstoku'
*          address = 'ul. Antoniuk Fabryczny 5/7 Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 26 im. Stanisława Staszica w Białymstoku'
*          address = 'ul. Radzymińska 11  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 28 im. Konstantego Ildefonsa Gałczyńskiego w Białymstoku'
*          address = 'ul. Warmińska 55  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 29 im. Synów Pułku Zespół Szkół Ogólnokształcących Mistrzostwa Sportowego Nr 2 w Białymstoku'
*          address = 'ul. Promienna 13A   Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 32 im. mjr Henryka Dobrzańskiego ps. „Hubal” Zespół Szkół Ogólnokształcących Mistrzostwa Sportowego Nr 3 „Pietrasze” w Białymstoku'
*          address = 'ul. Pietrasze 29   Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 34 im. gen. Józefa Zachariasza Bema w Białymstoku'
*          address = 'ul. Pogodna 12  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 37 im. Kazimierza Górskiego w Białymstoku'
*          address = 'ul. Jaworowa 8  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 38 im. Wiesława Kazaneckiego Zespół Szkolno - Przedszkolny Nr 4 w Białymstoku'
*          address = 'ul. Porzeczkowa 11  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 47 im. Jana Klemensa Branickiego w Białymstoku'
*          address = 'ul. Palmowa 28  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 48 im. króla Stefana Batorego Zespół Szkolno - Przedszkolny Nr 5 w Białymstoku'
*          address = 'ul. Magnoliowa 13  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) )
*        ( cname   = 'Szkoła Podstawowa Nr 52 im. dr Ireny Białówny w Białymstoku'
*          address = 'ul. Dojlidy Górne 48  Białystok'
*          uuid    = system_uuid->create_uuid_x16( ) ) ).

    " Step 3 - Save Orders Into DB

*    TRY.
*        out->write( mo_route->save_orders_into_db( lt_orders ) ).
*      CATCH cx_root INTO DATA(exc).
*        out->write( exc->get_text( ) ).
*    ENDTRY.


    TRY.
        out->write( mo_route->update_orders( mo_route->get_orders(  ) ) ).
      CATCH cx_root INTO DATA(exc).
        out->write( exc->get_text( ) ).
    ENDTRY.







  ENDMETHOD.
ENDCLASS.
