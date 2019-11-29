/**************************************************************************/
/* Prog:  xxmf_mtto02.p                                                   */
/* Autor: (0103042) Jonathan Lopez Moreno                                 */
/* Fecha: 24 de Abril del 2018                                            */
/*                                                                        */
/* Desc: Mantenimiento de precios.                                        */
/*                                                                        */
/* Proyecto: Costos ABS & ABC Proy(768).                                  */
/**************************************************************************/

{mfdtitle.i}


DEFINE VARIABLE vc_anio AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_mes  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_resp AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_prod AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_msg  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vc_ar   AS CHARACTER NO-UNDO.
DEFINE VARIABLE entidad AS CHARACTER NO-UNDO INITIAL "9000".

DEF VAR vi_prod     AS INT  INITIAL 0.
DEF VAR row_browser AS INT  INITIAL 1.
DEF VAR row         AS INT  INITIAL 0.
DEF VAR vd_tncomp   AS DEC  INITIAL 0.
DEF VAR vc_recid    AS RECID.
DEF VAR vConfirma   AS LOG  NO-UNDO FORMAT "Si/No".


DEF TEMP-TABLE t-informacion NO-UNDO
    FIELD t-articulo  AS CHAR
    FIELD t-desc      AS CHAR
    FIELD t-proveedor AS CHAR
    FIELD t-nombre    AS CHAR
    FIELD t-resp      AS CHAR
    FIELD t-anio      AS CHAR
    FIELD t-mes       AS CHAR
    FIELD t-precio    AS CHAR
    FIELD t-compra    AS CHAR
    FIELD t-ar        AS CHAR.


DEFINE QUERY q1 FOR t-informacion.
DEFINE BROWSE b1 QUERY q1 DISPLAY
t-articulo  LABEL "Art"       WIDTH 7
t-desc      LABEL "Desc"      WIDTH 8
t-proveedor LABEL "Prov"      WIDTH 8
t-resp      LABEL "Resp"      WIDTH 8
t-anio      LABEL "A#o"       WIDTH 4
t-mes       LABEL "Mes"       WIDTH 3
t-precio    LABEL "Precio"    WIDTH 8
t-compra    LABEL "Comp"      WIDTH 8
t-ar        LABEL "AR"        WIDTH 8
WITH 12 DOWN WIDTH 75 SEPARATORS OVERLAY.
FORM b1
  WITH NO-LABELS CENTERED ROW 6 NO-BOX NO-UNDERLINE OVERLAY FRAME B.



ANIO:
REPEAT:

    HIDE FRAME B.

    ASSIGN
      vc_anio = ""
      vc_mes  = ""
      vc_resp = "".

    SET vc_anio LABEL "A#o"  FORMAT "x(4)"
        vc_mes  LABEL "Mes"  FORMAt "x(2)"
        vc_resp LABEL "Resp" FORMAT "x(7)"
        WITH FRAME a SIDE-LABELS WIDTH 80 ATTR-SPACE
        TITLE COLOR normal " Precios Definitivos ".


    IF vc_anio = "" THEN vc_anio = STRING(YEAR(TODAY)).
    IF vc_mes  = "" THEN vc_mes  = STRING(MONTH(TODAY)).


    EMPTY TEMP-TABLE t-informacion.
    FOR EACH xxtabla_misc WHERE xxtabla_domain = global_domain
                            AND xxtabla_idsist = entidad
                            BY xxtabla_key2.


                   FIND xxpm_mstr WHERE xxpm_domain  = global_domain
                                    AND xxpm_entidad = entidad
                                    AND xxpm_anio    = INT(vc_anio)
                                    AND xxpm_mes     = INT(vc_mes)
                                    AND xxpm_prov    = xxtabla_prog
                                    AND xxpm_part    = xxtabla_key1
                                    NO-LOCK NO-ERROR.
                   IF AVAILABLE xxpm_mstr THEN xxtabla__chr01 = STRING(xxpm_precio).
                   ELSE xxtabla__chr01 = "0".
                   xxtabla_key3 = vc_anio.
                   xxtabla_key4 = vc_mes.



                   IF vc_resp <> "" THEN DO:
                             IF vc_resp <> xxtabla_key2 THEN NEXT.
                   END.
                   CREATE t-informacion.
                      ASSIGN
                         t-proveedor = xxtabla_prog
                         t-articulo  = xxtabla_key1
                         t-resp      = xxtabla_key2
                         t-anio      = xxtabla_key3
                         t-mes       = xxtabla_key4.

                      IF xxtabla__chr01 = "" THEN t-precio = "0".
                      ELSE t-precio = xxtabla__chr01.

                      FIND pt_mstr WHERE pt_domain = global_domain
                                     AND pt_part   = xxtabla_key1
                                     NO-LOCK NO-ERROR.
                      IF AVAILABLE pt_mstr THEN t-desc = pt_desc1.

                      vc_ar = "NO".
                      vd_tncomp = 0.
                      FIND glc_cal WHERE glc_domain = global_domain
                                     AND glc_year   = INT(vc_anio)
                                     AND glc_per    = INT(vc_mes)
                                     NO-LOCK NO-ERROR.
                      IF AVAILABLE glc_cal THEN DO:
                          FOR EACH  prh_hist WHERE prh_domain    = global_domain
                                               AND prh_vend      = xxtabla_misc.xxtabla_prog
                                               AND prh_part      = xxtabla_misc.xxtabla_key1
                                               AND prh_rcp_date >= glc_start
                                               AND prh_rcp_date <= glc_end
                                               AND prh_type = ""
                                               USE-INDEX prh_vend
                                               NO-LOCK:
                                    vc_ar = "SI".
                                    /*vd_tncomp = vd_tncomp + (prh_rcvd * prh_um_conv).*/
                                    vd_tncomp = vd_tncomp + prh_rcvd.
                          END.
                      END. /*glc_cal*/
                      ASSIGN
                         t-ar     = vc_ar
                         t-compra = STRING(vd_tncomp).

    END. /*xxtabla_misc*/




    BROWSER_1:
    REPEAT:

         ON b OF b1 DO:

             UPDATE vc_prod LABEL "Buscar articulo"
              WITH FRAME searchProd ROW 17 CENTERED OVERLAY SIDE-LABELS.

             vi_prod = 0.
             FOR EACH t-informacion NO-LOCK.
                  vi_prod = vi_prod + 1.
                  IF t-articulo = vc_prod THEN DO:
                      vc_prod = "SI".
                     LEAVE.
                  END.
             END.
             IF vc_prod = "SI" THEN DO:
                row_browser = vi_prod.
                row = vi_prod.
                DO TRANSACTION:
                    OPEN QUERY q1 FOR EACH t-informacion NO-LOCK.
                         b1:SET-REPOSITIONED-ROW(row_browser).
                    REPOSITION q1 TO ROW row.
                END.
             END.
             ELSE DO:
                vc_msg ="El articulo no existe".
                {pxmsg.i &msgtext=vc_msg &errorlevel=3 &PAUSEAFTER=true}
             END.

         END.

         ON CURSOR-UP OF b1 DO:
           IF row_browser > 1 THEN row_browser = row_browser - 1.
         END.
         ON CURSOR-DOWN OF b1 DO:
           IF row_browser < b1:DOWN THEN row_browser = row_browser + 1.
         END.
         ON ENTER OF b1 DO:
            DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
                vc_recid = RECID(t-informacion).
                row = current-result-row("q1").

                FIND FIRST t-informacion WHERE recid(t-informacion) = vc_recid
                NO-ERROR.
                IF AVAILABLE t-informacion THEN DO:

                    IF t-resp = global_userid THEN DO:
                      UPDATE t-precio LABEL "Nuevo Precio"
                      WITH FRAME fprecio ROW 17 CENTERED OVERLAY SIDE-LABELS.
                    END.
                    ELSE DO:
                      STATUS INPUT
                      "Usuario no autorizado para actualizar precio.".
                    END.

                END.

            END.

            DO TRANSACTION:
                OPEN QUERY q1 FOR EACH t-informacion NO-LOCK.
                b1:SET-REPOSITIONED-ROW(row_browser).
                REPOSITION q1 TO ROW row.
            END.

         END.

         STATUS INPUT
         "Flechas para navegar. Enter para seleccionar.".

         OPEN QUERY q1 FOR EACH t-informacion NO-LOCK.
              UPDATE b1 WITH FRAME B NO-UNDERLINE.
         CLOSE QUERY q1.

         IF LASTKEY = 301 OR keyfunc(lastkey) = "GO" THEN DO: /*F5*/
            vc_msg = "Seguro que desea actualizar los precios?".
            {xxmsg.i &MSGTEXT=vc_msg &ERRORLEVEL=1 &CONFIRM=vConfirma}

            IF vConfirma = TRUE THEN DO:

                 FOR EACH t-informacion NO-LOCK.

                       IF t-resp <> global_userid THEN NEXT.

                       FIND xxpm_mstr WHERE xxpm_domain  = global_domain
                                        AND xxpm_entidad = entidad
                                        AND xxpm_anio    = INT(t-anio)
                                        AND xxpm_mes     = INT(t-mes)
                                        AND xxpm_prov    = t-proveedor
                                        AND xxpm_part    = t-articulo
                                        EXCLUSIVE-LOCK NO-ERROR.
                       IF NOT AVAILABLE xxpm_mstr THEN DO:
                           CREATE xxpm_mstr.
                              ASSIGN
                                 xxpm_domain  = global_domain
                                 xxpm_entidad = entidad
                                 xxpm_anio    = INT(t-anio)
                                 xxpm_mes     = INT(t-mes)
                                 xxpm_prov    = t-proveedor
                                 xxpm_part    = t-articulo
                                 xxpm__dte01  = TODAY
                                 xxpm_precio  = DEC(t-precio).
                       END.
                       ELSE DO:
                           ASSIGN
                              xxpm_precio = DEC(t-precio)
                              xxpm__dte01 = TODAY.
                       END.


                 END.


                 STATUS INPUT
                 "Precios actualizados correctamente!.".
                 NEXT ANIO.

            END.

         END.

    END. /*REPEAT BROSER_1*/


END. /*REPEAT: ANIO*/