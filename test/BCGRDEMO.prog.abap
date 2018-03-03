************************************************************************
*                                                                      *
*          Demo-Programm für erweiterten Graphikdialog                 *
*                                                                      *
*    ( angepaßt an die ab Release 2.1 geänderten Bezugsdatümer )       *
*                                                                      *
************************************************************************


REPORT BCGRDEMO.

*** Definitionen für GRAPH_HIERARCHY_MENUE ***

DATA: BEGIN OF BAUM OCCURS 1,
        NID(2),                       "// Knoten-ID
        PNID(2),                      "// Knoten-ID des Vorgängers
        NTXT(15),                     "// Knotentext
        ALTER TYPE P,                 "// Kriterium 1
        GEHALT TYPE P,                "// Kriterium 2
        KTAGE TYPE P,                 "// Kriterium 3 (Krankentage)
      END OF BAUM.

DATA: BEGIN OF MENU OCCURS 1,
        C(30),
      END OF MENU.

DATA: NODE_ID(2), MENUEPKT, HIERSTAT, HIERMTYP, RWNID(6), RBUFF(80).


*** Definitionen für TEXT_MATRIX ***

DATA: BEGIN OF TEXTTAB OCCURS 1,
        ZEILE(100),
      END OF TEXTTAB.

DATA: TEMATITL(25) VALUE 'persönliche Daten',
      TEMASTAT.


*** Definitionen für STAT_GRAPH_REF ***

DATA: BEGIN OF SDATA OCCURS 1,
        P TYPE P,
      END OF SDATA.

DATA: BEGIN OF SVALS OCCURS 1,
        C(4),
      END OF SVALS.

DATA: BEGIN OF SOPTS OCCURS 1,
        C(80),
      END OF SOPTS.

DATA: STATTITL(40) VALUE 'Gehaltsentwicklung',
      U_TITL(40),
      STATSTAT.


*** Definitionen für GRAPH_MATRIX_2D ***

DATA: BEGIN OF DATTAB2D OCCURS 1,
        NAME(15),
        JAN TYPE P,   FEB TYPE P,   MAR TYPE P,
        APR TYPE P,   MAI TYPE P,   JUN TYPE P,   JUL_DEZ,
      END OF DATTAB2D.

DATA: BEGIN OF OPTTAB2D OCCURS 1,
        C(20),
      END OF OPTTAB2D.

DATA: BEGIN OF SPTITL OCCURS 1,
        C(20),
      END OF SPTITL.

DATA: BG2DTITL(40) VALUE 'Überstunden 1992  -  ',
      ZLNR, BG2DSTAT.


*** Definitionen für GRAPH_MATRIX_3D ***

DATA: BEGIN OF DATTAB3D OCCURS 1,
        NAME(15),
        Q1 TYPE P,  Q2 TYPE P,  Q3 TYPE P,  Q4 TYPE P,  GESAMT TYPE P,
      END OF DATTAB3D.

DATA: BEGIN OF OPTTAB3D OCCURS 1,
        C(20),
      END OF OPTTAB3D.

DATA: BG3DSTAT.


*** Definitionen für GANTT_DIAGRAMM ***

DATA: BEGIN OF ELEM OCCURS 1,
        BEG TYPE P,   BGT TYPE T,   DUR TYPE P,
        TXT(40),   COL(40),   POS(40),   ESZ(40),
      END OF ELEM.

DATA: BEGIN OF ITEM OCCURS 1,
        ZLTITL(20),
        PPZAHL TYPE P,
      END OF ITEM.

DATA:  T TYPE T,  D TYPE D,  B TYPE D VALUE '18991230',
       GANTSTAT,  GANTMTYP.


************************************************************************

*** Daten für GRAPH_HIERARCHY_MENUE ***

BAUM-NID    = '0'.
BAUM-PNID   = SPACE.
BAUM-NTXT   = 'Chef'.
BAUM-ALTER  = 62.
BAUM-GEHALT = 30.
BAUM-KTAGE  = 0.
APPEND BAUM.

BAUM-NID    = '11'.
BAUM-PNID   = '0'.
BAUM-NTXT   = 'Abt.Leiter A'.
BAUM-ALTER  = 53.
BAUM-GEHALT = 12.
BAUM-KTAGE  = 6.
APPEND BAUM.

BAUM-NID    = '12'.
BAUM-PNID   = '0'.
BAUM-NTXT   = 'Abt.Leiter B'.
BAUM-ALTER  = 61.
BAUM-GEHALT = 13.
BAUM-KTAGE  = 1.
APPEND BAUM.

BAUM-NID    = '21'.
BAUM-PNID   = '11'.
BAUM-NTXT   = 'Mitarbeiter A1'.
BAUM-ALTER  = 40.
BAUM-GEHALT = 8.
BAUM-KTAGE  = 13.
APPEND BAUM.

BAUM-NID    = '22'.
BAUM-PNID   = '11'.
BAUM-NTXT   = 'Mitarbeiter A2'.
BAUM-ALTER  = 32.
BAUM-GEHALT = 6.
BAUM-KTAGE  = 4.
APPEND BAUM.

BAUM-NID    = '23'.
BAUM-PNID   = '12'.
BAUM-NTXT   = 'Mitarbeiter B1'.
BAUM-ALTER  = 51.
BAUM-GEHALT = 9.
BAUM-KTAGE  = 12.
APPEND BAUM.

BAUM-NID    = '24'.
BAUM-PNID   = '12'.
BAUM-NTXT   = 'Mitarbeiter B2'.
BAUM-ALTER  = 35.
BAUM-GEHALT = 7.
BAUM-KTAGE  = 14.
APPEND BAUM.

BAUM-NID    = '25'.
BAUM-PNID   = '12'.
BAUM-NTXT   = 'Mitarbeiter B3'.
BAUM-ALTER  = 25.
BAUM-GEHALT = 5.
BAUM-KTAGE  = 2.
APPEND BAUM.

MENU = '*****    Anzeigeauswahl    ***'.  APPEND MENU.
MENU = 'persönliche Daten'.               APPEND MENU.
MENU = 'Gehaltsentwicklung'.              APPEND MENU.
MENU = 'Überstunden  92'.                 APPEND MENU.
MENU = 'Überstundenübersicht  91'.        APPEND MENU.
MENU = 'Urlaubsübersicht  92'.            APPEND MENU.
MENU = 'alle Daten'.                      APPEND MENU.


*** Daten für STAT_GRAPH_REF ***

SVALS-C = '1985'.             APPEND SVALS.
SVALS-C = '1986'.             APPEND SVALS.
SVALS-C = '1987'.             APPEND SVALS.
SVALS-C = '1988'.             APPEND SVALS.
SVALS-C = '1989'.             APPEND SVALS.
SVALS-C = '1990'.             APPEND SVALS.
SVALS-C = '1991'.             APPEND SVALS.
SVALS-C = '1992'.             APPEND SVALS.

SOPTS-C = '$8'.               APPEND SOPTS.
SOPTS-C = 'COLOR=4'.          APPEND SOPTS.
SOPTS-C = 'C_ART=0'.          APPEND SOPTS.
SOPTS-C = 'THICK=1'.          APPEND SOPTS.
SOPTS-C = 'LTEXT=Gehaltskurve'.             APPEND SOPTS.
SOPTS-C = 'DTEXT=Gehaltsentwicklung in den letzten 8 Jahren'.
                                            APPEND SOPTS.

*** Daten für GRAPH_MATRIX_2D ***

DATTAB2D-NAME  = 'Abt.Leiter A'.
DATTAB2D-JAN   = 20.
DATTAB2D-FEB   = 15.
DATTAB2D-MAR   = 18.
DATTAB2D-APR   = 25.
DATTAB2D-MAI   = 10.
DATTAB2D-JUN   = 22.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Abt.Leiter B'.
DATTAB2D-JAN   = 5.
DATTAB2D-FEB   = 8.
DATTAB2D-MAR   = 10.
DATTAB2D-APR   = 7.
DATTAB2D-MAI   = 9.
DATTAB2D-JUN   = 18.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Mitarbeiter A1'.
DATTAB2D-JAN   = 2.
DATTAB2D-FEB   = 1.
DATTAB2D-MAR   = 3.
DATTAB2D-APR   = 3.
DATTAB2D-MAI   = 6.
DATTAB2D-JUN   = 7.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Mitarbeiter A2'.
DATTAB2D-JAN   = 6.
DATTAB2D-FEB   = 7.
DATTAB2D-MAR   = 3.
DATTAB2D-APR   = 10.
DATTAB2D-MAI   = 12.
DATTAB2D-JUN   = 8.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Mitarbeiter B1'.
DATTAB2D-JAN   = 2.
DATTAB2D-FEB   = 0.
DATTAB2D-MAR   = 1.
DATTAB2D-APR   = 3.
DATTAB2D-MAI   = 5.
DATTAB2D-JUN   = 6.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Mitarbeiter B2'.
DATTAB2D-JAN   = 1.
DATTAB2D-FEB   = 3.
DATTAB2D-MAR   = 7.
DATTAB2D-APR   = 0.
DATTAB2D-MAI   = 7.
DATTAB2D-JUN   = 10.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.
DATTAB2D-NAME  = 'Mitarbeiter B3'.
DATTAB2D-JAN   = 5.
DATTAB2D-FEB   = 2.
DATTAB2D-MAR   = 1.
DATTAB2D-APR   = 8.
DATTAB2D-MAI   = 0.
DATTAB2D-JUN   = 13.
DATTAB2D-JUL_DEZ = 0.
APPEND DATTAB2D.

OPTTAB2D  = 'P2TYPE = VB'.      APPEND OPTTAB2D.
OPTTAB2D  = 'TISIZE = 2'.       APPEND OPTTAB2D.
OPTTAB2D  = 'CLBACK = X'.       APPEND OPTTAB2D.

SPTITL  = 'Januar'.           APPEND SPTITL.
SPTITL  = 'Februar'.          APPEND SPTITL.
SPTITL  = 'März'.             APPEND SPTITL.
SPTITL  = 'April'.            APPEND SPTITL.
SPTITL  = 'Mai'.              APPEND SPTITL.
SPTITL  = 'Juni'.             APPEND SPTITL.
SPTITL  = 'Jul-Dez'.          APPEND SPTITL.


*** Daten für GRAPH_MATRIX_3D ***

DATTAB3D-NAME = 'Abt.Leiter A'.
DATTAB3D-Q1   =  40.
DATTAB3D-Q2   =  35.
DATTAB3D-Q3   =  50.
DATTAB3D-Q4   =  60.
DATTAB3D-GESAMT = 185.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Abt.Leiter B'.
DATTAB3D-Q1   =  25.
DATTAB3D-Q2   =  20.
DATTAB3D-Q3   =  35.
DATTAB3D-Q4   =  42.
DATTAB3D-GESAMT = 122.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Mitarbeiter A1'.
DATTAB3D-Q1   =  15.
DATTAB3D-Q2   =  20.
DATTAB3D-Q3   =  12.
DATTAB3D-Q4   =  25.
DATTAB3D-GESAMT = 72.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Mitarbeiter A2'.
DATTAB3D-Q1   =  10.
DATTAB3D-Q2   =  20.
DATTAB3D-Q3   =  25.
DATTAB3D-Q4   =  13.
DATTAB3D-GESAMT = 68.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Mitarbeiter B1'.
DATTAB3D-Q1   =  9.
DATTAB3D-Q2   =  14.
DATTAB3D-Q3   =  10.
DATTAB3D-Q4   =  18.
DATTAB3D-GESAMT = 51.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Mitarbeiter B2'.
DATTAB3D-Q1   =  0.
DATTAB3D-Q2   =  5.
DATTAB3D-Q3   =  8.
DATTAB3D-Q4   =  4.
DATTAB3D-GESAMT = 17.
APPEND DATTAB3D.
DATTAB3D-NAME = 'Mitarbeiter B3'.
DATTAB3D-Q1   =  4.
DATTAB3D-Q2   =  9.
DATTAB3D-Q3   =  12.
DATTAB3D-Q4   =  10.
DATTAB3D-GESAMT = 35.
APPEND DATTAB3D.

OPTTAB3D  = 'FIFRST = 3D'.      APPEND OPTTAB3D.
OPTTAB3D  = 'P3TYPE = TO'.      APPEND OPTTAB3D.
OPTTAB3D  = 'P3CTYP = SI'.      APPEND OPTTAB3D.
OPTTAB3D  = 'TISIZE = 2'.       APPEND OPTTAB3D.
OPTTAB3D  = 'CLBACK = X'.       APPEND OPTTAB3D.


*** Daten für GANTT_DIAGRAMM ***

ITEM-ZLTITL  = 'Abt.Leiter A'.
ITEM-PPZAHL  = 3.
APPEND ITEM.

D = '19920301'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  5 * 86400.
ELEM-TXT  =  'SET ETEXT 5 Tage'.
ELEM-COL  =  'SET BAKGR RED'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.

D = '19920511'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  15 * 86400.
ELEM-TXT  =  'SET ETEXT 15 Tage'.
ELEM-COL  =  'SET BAKGR RED'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.

D = '19920923'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  8 * 86400.
ELEM-TXT  =  'SET ETEXT 8 Tage'.
ELEM-COL  =  'SET BAKGR RED'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Abt.Leiter B'.
ITEM-PPZAHL  = 2.
APPEND ITEM.

D = '19920612'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  20 * 86400.
ELEM-TXT  =  'SET ETEXT 20 Tage'.
ELEM-COL  =  'SET BAKGR BLUE'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.

D = '19920719'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  7 * 86400.
ELEM-TXT  =  'SET ETEXT 7 Tage'.
ELEM-COL  =  'SET BAKGR BLUE'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Mitarbeiter A1'.
ITEM-PPZAHL  = 1.
APPEND ITEM.

D = '19920712'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  25 * 86400.
ELEM-TXT  =  'SET ETEXT 25 Tage'.
ELEM-COL  =  'SET BAKGR GREEN'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Mitarbeiter A2'.
ITEM-PPZAHL  = 1.
APPEND ITEM.

D = '19920705'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  22 * 86400.
ELEM-TXT  =  'SET ETEXT 22 Tage'.
ELEM-COL  =  'SET BAKGR RED'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Mitarbeiter B1'.
ITEM-PPZAHL  = 1.
APPEND ITEM.

D = '19920505'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  25 * 86400.
ELEM-TXT  =  'SET ETEXT 25 Tage'.
ELEM-COL  =  'SET BAKGR BLUE'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Mitarbeiter B2'.
ITEM-PPZAHL  = 1.
APPEND ITEM.

D = '19920725'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  21 * 86400.
ELEM-TXT  =  'SET ETEXT 21 Tage'.
ELEM-COL  =  'SET BAKGR GREEN'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


ITEM-ZLTITL  = 'Mitarbeiter B3'.
ITEM-PPZAHL  = 1.
APPEND ITEM.

D = '19920801'.
T = '000000'.
ELEM-BEG  =  ( D - B ) * 86400  +  T.
ELEM-BGT  =  T.
ELEM-DUR  =  18 * 86400.
ELEM-TXT  =  'SET ETEXT 18 Tage'.
ELEM-COL  =  'SET BAKGR RED'.
ELEM-POS  =  'SET PLACE CENTRE'.
ELEM-ESZ  =  'SET HEGHT 15'.
APPEND ELEM.


************************************************************************

*----------------------------------------------------------------------*
*     Dialogschleife                                                   *
*----------------------------------------------------------------------*

HIERSTAT = SPACE.
TEMASTAT = '2'.
STATSTAT = '2'.
BG2DSTAT = '2'.
BG3DSTAT = '2'.
GANTSTAT = '2'.

DO.
  CALL FUNCTION 'GRAPH_HIERARCHY_MENUE'
       EXPORTING
          STAT  =  HIERSTAT
          WINID = 'HIER01'
          SUPER = 'X'
          TTEXT = 'Firmenstruktur'        "// Titeltext
          BOXLN = '12'                    "// Knotenbreite
         WINPOS = '2'                     "// Fensterposition
         WINSZX = '75'                    "// Fensterbreite
         WINSZY = '37'                    "// Fensterhöhe
          NTEXT = 'BLACK'                 "// Knotentextfarbe
          VTXT1 = 'Alter'                 "// Kriterium 1
          VALD1 = 0                       "//--------------------------
          COL11 = 'YELLOW'                "//
          VAL11 = 30                      "//  Schwellwerte (VALxy)
          COL12 = 'GREEN'                 "//
          VAL12 = 45                      "//  und  Farbenset (COLxy)
          COL13 = 'DARKYELLOW'            "//
          VAL13 = 60                      "//  für  Kriterium 1
          COL14 = 'RED'                   "//
          VALU1 = 100                     "//--------------------------
          VTXT2 = 'Gehalt'                "// Kriterium 2
          VALD2 = 0                       "//--------------------------
          COL21 = 'GREEN'                 "//
          VAL21 = 8                       "//  Schwellwerte (VALxy)
          COL22 = 'DARKYELLOW'            "//
          VAL22 = 12                      "//  und  Farbenset (COLxy)
          COL23 = 'YELLOW'                "//
          VAL23 = 20                      "//  für Kriterium 2
          COL24 = 'RED'                   "//
          VALU2 = 50                      "//--------------------------
          VTXT3 = 'Krankentage'           "// Kriterium 3
          VALD3 = 0                       "//--------------------------
          COL31 = 'GREEN'                 "//  Schwellwerte (VALxy)
          VAL31 = 5                       "//
          COL32 = 'YELLOW'                "//  und  Farbenset (COLxy)
          VAL32 = 10                      "//
          COL33 = 'RED'                   "//  für Kriterium 3
          VALU3 = 50                      "//--------------------------
       IMPORTING
          M_TYP = HIERMTYP
          RWNID = RWNID
          RBUFF = RBUFF
          NODES = NODE_ID
          MENNR = MENUEPKT
       TABLES
          DATA  = BAUM
          MENU  = MENU.

  HIERSTAT = '4'.


  CASE HIERMTYP.

    WHEN ' '.   " kann hier nicht sein !
    WHEN 'D'.   EXIT.
    WHEN 'Q'.   EXIT.

    WHEN '0'.          "// Es wird hier stets das Schließen des
      CASE RWNID.      "// entsprechenden Fremdfensters angenommen.
        WHEN 'TEMA01'.     TEMASTAT = '2'.
        WHEN 'STAT01'.     STATSTAT = '2'.
        WHEN 'BUSG02'.     BG2DSTAT = '2'.
        WHEN 'BUSG03'.     BG3DSTAT = '2'.
        WHEN 'GANT01'.     GANTSTAT = '2'.
*         CALL FUNCTION 'GANTT_DIAGRAMM'
*                EXPORTING
*                  STAT  = 'R'         "/ Status 'R' kann momentan beim
*                  INBUF = RBUFF       "/ FB GANTT_DIAGRAMM noch nicht
*                IMPORTING             "/ benutzt werden.  Daher muß
*                  M_TYP = GANTMTYP    "/ dieser Block vorläufig ausge-
*                TABLES                "/ sternt werden (sonst würde
*                  ITEM  = ITEM        "/ hier GANTMTYP falsch gesetzt).
*                  ELEM  = ELEM.
*         IF  GANTMTYP = 'Q'.    GANTSTAT = '2'.
*         ELSE.  SKIP 2.
*                WRITE: /2
*                  'Es wurden Modifikationen in der Urlaubsplanung',
*                  'vorgenommen !'.
*         ENDIF.
      ENDCASE.

    WHEN 'I'.
      IF  NODE_ID = 0.
        REFRESH TEXTTAB.
        TEXTTAB = '**********************************'.  APPEND TEXTTAB.
        TEXTTAB = '*                                *'.  APPEND TEXTTAB.
        TEXTTAB = '*   ACHTUNG !!!                  *'.  APPEND TEXTTAB.
        TEXTTAB = '*                                *'.  APPEND TEXTTAB.
        TEXTTAB = '*   Gewünschte Datenselektion    *'.  APPEND TEXTTAB.
        TEXTTAB = '*   nicht erlaubt !              *'.  APPEND TEXTTAB.
        TEXTTAB = '*                                *'.  APPEND TEXTTAB.
        TEXTTAB = '*   Bitte einen anderen Knoten   *'.  APPEND TEXTTAB.
        TEXTTAB = '*   wählen !                     *'.  APPEND TEXTTAB.
        TEXTTAB = '*                                *'.  APPEND TEXTTAB.
        TEXTTAB = '**********************************'.  APPEND TEXTTAB.
        CALL FUNCTION 'TEXT_MATRIX'
               EXPORTING
                  STAT  = TEMASTAT
                  WINID = 'TEMA01'
                  CSIZE = '37'
                  RSIZE = '11'
               TABLES
                  DATA = TEXTTAB.
        TEMASTAT = '5'.
      ELSE.
        CASE MENUEPKT.
          WHEN '1'.   " persönliche Daten im Textfenster "
            REFRESH TEXTTAB.
            CASE NODE_ID.
              WHEN '11'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*      Abt.Leiter A        *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  22.09.1957 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.07.1983 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/15067 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '12'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*      Abt.Leiter B        *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  31.12.1955 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.01.1978 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/12345 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '21'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter A1       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  01.04.1959 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.04.1984 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06351/110  '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '22'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter A2       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  24.12.1956 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.06.1985 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06374/999  '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '23'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B1       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  11.11.1950 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.07.1975 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06321/6974 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '24'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B2       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  29.02.1952 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.10.1972 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/11111 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '25'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B3       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  01.01.1960 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.10.1980 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06351/4711 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
            ENDCASE.
            CALL FUNCTION 'TEXT_MATRIX'
                   EXPORTING
                      STAT  = TEMASTAT
                      WINID = 'TEMA01'
                      TTEXT = TEMATITL
                      CSIZE = '31'
                      RSIZE = '12'
                   TABLES
                      DATA  = TEXTTAB.
            TEMASTAT = '5'.
          WHEN '2'.   " Gehaltsentwicklung als Statistikgraphik "
            REFRESH SDATA.
            CASE NODE_ID.
              WHEN '11'.
                SDATA-P = 8000.       APPEND SDATA.
                SDATA-P = 8500.       APPEND SDATA.
                SDATA-P = 9200.       APPEND SDATA.
                SDATA-P = 10200.      APPEND SDATA.
                SDATA-P = 11500.      APPEND SDATA.
                SDATA-P = 12000.      APPEND SDATA.
                SDATA-P = 12900.      APPEND SDATA.
                SDATA-P = 14000.      APPEND SDATA.
                U_TITL  = 'Abteilungsleiter A'.
              WHEN '12'.
                SDATA-P = 9000.       APPEND SDATA.
                SDATA-P = 9600.       APPEND SDATA.
                SDATA-P = 10000.      APPEND SDATA.
                SDATA-P = 10900.      APPEND SDATA.
                SDATA-P = 11600.      APPEND SDATA.
                SDATA-P = 12300.      APPEND SDATA.
                SDATA-P = 13000.      APPEND SDATA.
                SDATA-P = 13800.      APPEND SDATA.
                U_TITL  = 'Abteilungsleiter B'.
              WHEN '21'.
                SDATA-P = 4000.       APPEND SDATA.
                SDATA-P = 4500.       APPEND SDATA.
                SDATA-P = 4900.       APPEND SDATA.
                SDATA-P = 5500.       APPEND SDATA.
                SDATA-P = 6100.       APPEND SDATA.
                SDATA-P = 6700.       APPEND SDATA.
                SDATA-P = 7200.       APPEND SDATA.
                SDATA-P = 7900.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter A1'.
              WHEN '22'.
                SDATA-P = 4300.       APPEND SDATA.
                SDATA-P = 4800.       APPEND SDATA.
                SDATA-P = 5200.       APPEND SDATA.
                SDATA-P = 5800.       APPEND SDATA.
                SDATA-P = 6600.       APPEND SDATA.
                SDATA-P = 7000.       APPEND SDATA.
                SDATA-P = 7500.       APPEND SDATA.
                SDATA-P = 8100.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter A2'.
              WHEN '23'.
                SDATA-P = 4500.       APPEND SDATA.
                SDATA-P = 4900.       APPEND SDATA.
                SDATA-P = 5500.       APPEND SDATA.
                SDATA-P = 6000.       APPEND SDATA.
                SDATA-P = 6400.       APPEND SDATA.
                SDATA-P = 7100.       APPEND SDATA.
                SDATA-P = 7800.       APPEND SDATA.
                SDATA-P = 8800.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B1'.
              WHEN '24'.
                SDATA-P = 5000.       APPEND SDATA.
                SDATA-P = 5700.       APPEND SDATA.
                SDATA-P = 6000.       APPEND SDATA.
                SDATA-P = 6400.       APPEND SDATA.
                SDATA-P = 6900.       APPEND SDATA.
                SDATA-P = 7500.       APPEND SDATA.
                SDATA-P = 7900.       APPEND SDATA.
                SDATA-P = 8700.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B2'.
              WHEN '25'.
                SDATA-P = 4700.       APPEND SDATA.
                SDATA-P = 5100.       APPEND SDATA.
                SDATA-P = 5700.       APPEND SDATA.
                SDATA-P = 6200.       APPEND SDATA.
                SDATA-P = 6900.       APPEND SDATA.
                SDATA-P = 7300.       APPEND SDATA.
                SDATA-P = 7800.       APPEND SDATA.
                SDATA-P = 8500.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B3'.
            ENDCASE.
            CALL FUNCTION 'STAT_GRAPH_REF'
                   EXPORTING
                      STAT  = STATSTAT
                      WINID = 'STAT01'
                      TTEXT = STATTITL
                      UTEXT = U_TITL
                      TAXIS = 'Kalenderjahr'
                      ATYPE = 'VAL'
                   TABLES
                      DATA  = SDATA
                      VALS  = SVALS
                      OPTS  = SOPTS.
            STATSTAT = '5'.
          WHEN '3'.    " Überstundenauflistung von '92 in 2D-Graphik "
            CASE NODE_ID.
              WHEN '11'.     ZLNR = '1'.    WRITE 'A'  TO BG2DTITL+21.
              WHEN '12'.     ZLNR = '2'.    WRITE 'B'  TO BG2DTITL+21.
              WHEN '21'.     ZLNR = '3'.    WRITE 'A1' TO BG2DTITL+21.
              WHEN '22'.     ZLNR = '4'.    WRITE 'A2' TO BG2DTITL+21.
              WHEN '23'.     ZLNR = '5'.    WRITE 'B1' TO BG2DTITL+21.
              WHEN '24'.     ZLNR = '6'.    WRITE 'B2' TO BG2DTITL+21.
              WHEN '25'.     ZLNR = '7'.    WRITE 'B3' TO BG2DTITL+21.
            ENDCASE.
            CALL FUNCTION 'GRAPH_MATRIX_2D'
                   EXPORTING
                      STAT  = BG2DSTAT
                      WINID = 'BUSG02'
                      NROW  = ZLNR
                      TITL  = BG2DTITL
                      VALT  = 'Stunden'
                   TABLES
                      DATA  = DATTAB2D
                      OPTS  = OPTTAB2D
                      TCOL  = SPTITL.
            BG2DSTAT = '5'.
          WHEN '4'.   " Überstundenübersicht '91 in 3D-Graphik "
            CALL FUNCTION 'GRAPH_MATRIX_3D'
                   EXPORTING
                      STAT  = BG3DSTAT
                      WINID = 'BUSG03'
                      DIM1  = 'Quartal/Gesamt'
                      DIM2  = 'Mitarbeiter'
                      COL1  = '1. Quartal'
                      COL2  = '2. Quartal'
                      COL3  = '3. Quartal'
                      COL4  = '4. Quartal'
                      COL5  = 'Gesamt'
                      TITL  = 'Überstundenjahresübersicht 1991'
                      VALT  = 'Stunden'
                   TABLES
                      DATA  = DATTAB3D
                      OPTS  = OPTTAB3D.
            BG3DSTAT = '5'.
          WHEN '5'.   " Urlaubsübersicht '92 als Gantt-Diagramm "
            CALL FUNCTION 'GANTT_DIAGRAMM'
                   EXPORTING
                      STAT  = GANTSTAT
                      WINID = 'GANT01'
                    WHEADER = 'Urlaubsplaner'
                     TTITLE = 'Urlaubsübersicht 1992'
                      TTEXT = 'Mitarbeiter'
                      TUNIT = 'D'
                      MODIF = 'ON'
                   TABLES
                      ITEM  = ITEM
                      ELEM  = ELEM.
            GANTSTAT = '5'.
          WHEN '6'.   " alle Graphikfenster von '1' bis '5' anzeigen "
*         " persönliche Daten im Textfenster "
            REFRESH TEXTTAB.
            CASE NODE_ID.
              WHEN '11'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*      Abt.Leiter A        *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  22.09.1957 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.07.1983 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/15067 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '12'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*      Abt.Leiter B        *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  31.12.1955 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.01.1978 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/12345 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '21'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter A1       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  01.04.1959 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.04.1984 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06351/110  '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '22'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter A2       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  24.12.1956 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.06.1985 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  verheiratet'.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06374/999  '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '23'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B1       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  11.11.1950 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  01.07.1975 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06321/6974 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '24'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B2       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  29.02.1952 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.10.1972 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  0631/11111 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
              WHEN '25'.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '*     Mitarbeiter B3       *'.APPEND TEXTTAB.
                TEXTTAB = '*                          *'.APPEND TEXTTAB.
                TEXTTAB = '****************************'.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
                TEXTTAB = ' Geburtsdatum :  01.01.1960 '.APPEND TEXTTAB.
                TEXTTAB = ' Eintritt     :  15.10.1980 '.APPEND TEXTTAB.
                TEXTTAB = ' Geschlecht   :  männlich   '.APPEND TEXTTAB.
                TEXTTAB = ' Familienstand:  ledig      '.APPEND TEXTTAB.
                TEXTTAB = ' Tel.(privat) :  06351/4711 '.APPEND TEXTTAB.
                TEXTTAB = '                            '.APPEND TEXTTAB.
            ENDCASE.
            CALL FUNCTION 'TEXT_MATRIX'
                   EXPORTING
                      STAT  = TEMASTAT
                      WINID = 'TEMA01'
                      TTEXT = TEMATITL
                      CSIZE = '31'
                      RSIZE = '12'
                   TABLES
                      DATA  = TEXTTAB.
            TEMASTAT = '5'.
*         " Gehaltsentwicklung als Statistikgraphik "
            REFRESH SDATA.
            CASE NODE_ID.
              WHEN '11'.
                SDATA-P = 8000.       APPEND SDATA.
                SDATA-P = 8500.       APPEND SDATA.
                SDATA-P = 9200.       APPEND SDATA.
                SDATA-P = 10200.      APPEND SDATA.
                SDATA-P = 11500.      APPEND SDATA.
                SDATA-P = 12000.      APPEND SDATA.
                SDATA-P = 12900.      APPEND SDATA.
                SDATA-P = 14000.      APPEND SDATA.
                U_TITL  = 'Abteilungsleiter A'.
              WHEN '12'.
                SDATA-P = 9000.       APPEND SDATA.
                SDATA-P = 9600.       APPEND SDATA.
                SDATA-P = 10000.      APPEND SDATA.
                SDATA-P = 10900.      APPEND SDATA.
                SDATA-P = 11600.      APPEND SDATA.
                SDATA-P = 12300.      APPEND SDATA.
                SDATA-P = 13000.      APPEND SDATA.
                SDATA-P = 13800.      APPEND SDATA.
                U_TITL  = 'Abteilungsleiter B'.
              WHEN '21'.
                SDATA-P = 4000.       APPEND SDATA.
                SDATA-P = 4500.       APPEND SDATA.
                SDATA-P = 4900.       APPEND SDATA.
                SDATA-P = 5500.       APPEND SDATA.
                SDATA-P = 6100.       APPEND SDATA.
                SDATA-P = 6700.       APPEND SDATA.
                SDATA-P = 7200.       APPEND SDATA.
                SDATA-P = 7900.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter A1'.
              WHEN '22'.
                SDATA-P = 4300.       APPEND SDATA.
                SDATA-P = 4800.       APPEND SDATA.
                SDATA-P = 5200.       APPEND SDATA.
                SDATA-P = 5800.       APPEND SDATA.
                SDATA-P = 6600.       APPEND SDATA.
                SDATA-P = 7000.       APPEND SDATA.
                SDATA-P = 7500.       APPEND SDATA.
                SDATA-P = 8100.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter A2'.
              WHEN '23'.
                SDATA-P = 4500.       APPEND SDATA.
                SDATA-P = 4900.       APPEND SDATA.
                SDATA-P = 5500.       APPEND SDATA.
                SDATA-P = 6000.       APPEND SDATA.
                SDATA-P = 6400.       APPEND SDATA.
                SDATA-P = 7100.       APPEND SDATA.
                SDATA-P = 7800.       APPEND SDATA.
                SDATA-P = 8800.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B1'.
              WHEN '24'.
                SDATA-P = 5000.       APPEND SDATA.
                SDATA-P = 5700.       APPEND SDATA.
                SDATA-P = 6000.       APPEND SDATA.
                SDATA-P = 6400.       APPEND SDATA.
                SDATA-P = 6900.       APPEND SDATA.
                SDATA-P = 7500.       APPEND SDATA.
                SDATA-P = 7900.       APPEND SDATA.
                SDATA-P = 8700.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B2'.
              WHEN '25'.
                SDATA-P = 4700.       APPEND SDATA.
                SDATA-P = 5100.       APPEND SDATA.
                SDATA-P = 5700.       APPEND SDATA.
                SDATA-P = 6200.       APPEND SDATA.
                SDATA-P = 6900.       APPEND SDATA.
                SDATA-P = 7300.       APPEND SDATA.
                SDATA-P = 7800.       APPEND SDATA.
                SDATA-P = 8500.       APPEND SDATA.
                U_TITL  = 'Mitarbeiter B3'.
            ENDCASE.
            CALL FUNCTION 'STAT_GRAPH_REF'
                   EXPORTING
                      STAT  = STATSTAT
                      WINID = 'STAT01'
                      TTEXT = STATTITL
                      UTEXT = U_TITL
                      TAXIS = 'Kalenderjahr'
                      ATYPE = 'VAL'
                   TABLES
                      DATA  = SDATA
                      VALS  = SVALS
                      OPTS  = SOPTS.
            STATSTAT = '5'.
*         " Überstundenauflistung von '92 in 2D-Graphik "
            CASE NODE_ID.
              WHEN '11'.     ZLNR = '1'.    WRITE 'A'  TO BG2DTITL+21.
              WHEN '12'.     ZLNR = '2'.    WRITE 'B'  TO BG2DTITL+21.
              WHEN '21'.     ZLNR = '3'.    WRITE 'A1' TO BG2DTITL+21.
              WHEN '22'.     ZLNR = '4'.    WRITE 'A2' TO BG2DTITL+21.
              WHEN '23'.     ZLNR = '5'.    WRITE 'B1' TO BG2DTITL+21.
              WHEN '24'.     ZLNR = '6'.    WRITE 'B2' TO BG2DTITL+21.
              WHEN '25'.     ZLNR = '7'.    WRITE 'B3' TO BG2DTITL+21.
            ENDCASE.
            CALL FUNCTION 'GRAPH_MATRIX_2D'
                   EXPORTING
                      STAT  = BG2DSTAT
                      WINID = 'BUSG02'
                      NROW  = ZLNR
                      TITL  = BG2DTITL
                      VALT  = 'Stunden'
                   TABLES
                      DATA  = DATTAB2D
                      OPTS  = OPTTAB2D
                      TCOL  = SPTITL.
            BG2DSTAT = '5'.
*         " Überstundenübersicht '91 in 3D-Graphik "
            CALL FUNCTION 'GRAPH_MATRIX_3D'
                   EXPORTING
                      STAT  = BG3DSTAT
                      WINID = 'BUSG03'
                      DIM1  = 'Quartal/Gesamt'
                      DIM2  = 'Mitarbeiter'
                      COL1  = '1. Quartal'
                      COL2  = '2. Quartal'
                      COL3  = '3. Quartal'
                      COL4  = '4. Quartal'
                      COL5  = 'Gesamt'
                      TITL  = 'Überstundenjahresübersicht 1991'
                      VALT  = 'Stunden'
                   TABLES
                      DATA  = DATTAB3D
                      OPTS  = OPTTAB3D.
            BG3DSTAT = '5'.
*         " Urlaubsübersicht '92 als Gantt-Diagramm "
            CALL FUNCTION 'GANTT_DIAGRAMM'
                   EXPORTING
                      STAT  = GANTSTAT
                      WINID = 'GANT01'
                    WHEADER = 'Urlaubsplaner'
                     TTITLE = 'Urlaubsübersicht 1992'
                      TTEXT = 'Mitarbeiter'
                      TUNIT = 'D'
                      MODIF = 'ON'
                   TABLES
                      ITEM  = ITEM
                      ELEM  = ELEM.
            GANTSTAT = '5'.
        ENDCASE.
      ENDIF.
  ENDCASE.
ENDDO.

CALL FUNCTION 'GRAPH_DIALOG'   EXPORTING   CLOSE  = 'X'.
