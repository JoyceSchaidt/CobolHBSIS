       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS007P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: HBSIS007P                                       *
      * ANALISTA.....: JOYCE SCHAIDT                                   *
      * DATA.........: 23/04/2019                                      *
      * OBJETIVO.....: REALIZAR DISTRIBUI플O DE CLIENTES X VENDEDORES  *
      * ARQUIVOS.....:                                                 *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *----------------------------------------------------------------*
           SELECT ARQ-CLIENTE        ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-CLIENTE-HBSIS002C
                       ALTERNATE RECORD KEY IS CNPJ-HBSIS002C
                       ALTERNATE RECORD KEY IS RAZAO-SOCIAL-HBSIS002C
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-CLI.

           SELECT ARQ-VENDEDOR       ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-VENDEDOR-HBSIS004C
                       ALTERNATE RECORD KEY IS CPF-HBSIS004C
                       ALTERNATE RECORD KEY IS NOME-VEND-HBSIS004C
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-VEN.

           SELECT ARQ-DISTRIBUICAO   ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-DIS.
                                
      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqCliente'.
       COPY "HBSIS002C.CPY".
       
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqVendedor'.
       COPY "HBSIS004C.CPY".
       
       FD  ARQ-DISTRIBUICAO
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqDistribuicao'.
       COPY "HBSIS007C.CPY".
       
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-CLI            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-VEN            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-DIS            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-REL            PIC  X(002)         VALUE "00".

       01  WS-MENOR-DISTANCIA          PIC  9(009)V9(002)  VALUE       
           999999999.
       01  WS-CALC-DISTANCIA           PIC  9(009)V9(002)  VALUE ZEROS.
       01  WS-LAT-CLI                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LAT-VEN                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LON-CLI                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LON-VEN                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-DLA                      PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-DLO                      PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-A                        PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-C                        PIC S9(003)V9(008)  VALUE ZEROS.
      
      *----------------------------------------------------------------*
      * AREAS DE COMUNICA플O COM OUTROS PROGRAMAS                      *
      *----------------------------------------------------------------*
       01  WS-HBSIS008                 PIC  X(009)         VALUE
           'HBSIS008P'.
           
       COPY HBSIS008L.
      
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS007L.    
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS007L.
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
       
           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 3000-FINALIZA
           
           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZA플O                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.
       
           MOVE ZEROS                  TO COD-RETORNO-HBSIS007L
           MOVE "DISTRIBUICAO REALIZADA COM SUCESSO" 
                                       TO MSG-RETORNO-HBSIS007L
           
           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
           
           PERFORM 2100-OPEN-ARQ-CLIENTE
           PERFORM 2200-OPEN-ARQ-DISTRIBUICAO
           
           PERFORM 2300-LER-ARQ-CLIENTE
           
           PERFORM 2400-TRATA-CLIENTE UNTIL 
                   WS-FL-STATUS-CLI   NOT EQUAL "00"
           
           PERFORM 2500-CLOSE-ARQ-CLIENTE
           PERFORM 2700-CLOSE-ARQ-DISTRIBUICAO
           
           PERFORM 2800-GERA-RELATORIO
           
           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO CLIENTE                                    *
      *----------------------------------------------------------------*
       2100-OPEN-ARQ-CLIENTE           SECTION.

           OPEN INPUT ARQ-CLIENTE                                       
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO DISTRIBUICAO                               *
      *----------------------------------------------------------------*
       2200-OPEN-ARQ-DISTRIBUICAO      SECTION.

           OPEN OUTPUT ARQ-DISTRIBUICAO                                 
           
           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE DISTRIBUICAO" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LEITURA DE ARQUIVO CLIENTE                                     *
      *----------------------------------------------------------------*
       2300-LER-ARQ-CLIENTE            SECTION.

           READ ARQ-CLIENTE NEXT                                        
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS OR '10' 
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NA LEITURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EFETUA DISTRIBUICAO                                            *
      *----------------------------------------------------------------*
       2400-TRATA-CLIENTE              SECTION.
       
           MOVE COD-CLIENTE-HBSIS002C  TO COD-CLIENTE-HBSIS007C
           MOVE RAZAO-SOCIAL-HBSIS002C TO RAZAO-SOCIAL-HBSIS007C

           PERFORM 2410-OPEN-ARQ-VENDEDOR
           PERFORM 2420-LER-ARQ-VENDEDOR
           
           PERFORM 2430-TRATA-VENDEDOR UNTIL WS-FL-STATUS-VEN           
                                       NOT EQUAL "00"
           
           MOVE  WS-MENOR-DISTANCIA    TO DISTANCIA-HBSIS007C           
           MOVE  999999999             TO WS-MENOR-DISTANCIA            
           WRITE ARQ-HBSIS007C
           
           PERFORM 2600-CLOSE-ARQ-VENDEDOR
           
           PERFORM 2300-LER-ARQ-CLIENTE.

       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO VENDEDOR                                   *
      *----------------------------------------------------------------*
       2410-OPEN-ARQ-VENDEDOR      SECTION.

           OPEN INPUT ARQ-VENDEDOR                                      
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE VENDEDOR - 7" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2410-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LEITURA DE ARQUIVO VENDEDOR                                    *
      *----------------------------------------------------------------*
       2420-LER-ARQ-VENDEDOR            SECTION.

           READ ARQ-VENDEDOR NEXT                                       
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS OR '10' 
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NA LEITURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2420-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TRATA VENDEDOR                                                 *
      *----------------------------------------------------------------*
       2430-TRATA-VENDEDOR             SECTION.
       
           COMPUTE WS-LAT-CLI = LATITUDE-CLI-HBSIS002C
                              * FUNCTION PI
                              / 180
       
           COMPUTE WS-LAT-VEN = LATITUDE-VEND-HBSIS004C                 
                              * FUNCTION PI
                              / 180

           COMPUTE WS-LON-CLI = LONGITUDE-CLI-HBSIS002C                 
                              * FUNCTION PI
                              / 180
                              
           COMPUTE WS-LON-VEN = LONGITUDE-VEND-HBSIS004C                
                              * FUNCTION PI
                              / 180

           COMPUTE WS-DLA = WS-LAT-VEN - (WS-LAT-CLI) 

           COMPUTE WS-DLO = WS-LON-VEN - (WS-LON-CLI)                   

           COMPUTE WS-A = FUNCTION SIN(WS-DLA / 2)
                        * FUNCTION SIN(WS-DLA / 2)
                        + FUNCTION COS(WS-LAT-CLI)
                        * FUNCTION COS(WS-LAT-VEN)
                        * FUNCTION SIN(WS-DLO / 2)
                        * FUNCTION SIN(WS-DLO / 2)
           
           COMPUTE WS-C = 2 * FUNCTION ATAN(FUNCTION SQRT(WS-A) /
                                            FUNCTION SQRT(1 - WS-A))

           COMPUTE WS-CALC-DISTANCIA = 6731 * WS-C * 1000     
                                                    
           IF WS-CALC-DISTANCIA         LESS WS-MENOR-DISTANCIA         
              MOVE WS-CALC-DISTANCIA    TO WS-MENOR-DISTANCIA           
              MOVE COD-VENDEDOR-HBSIS004C     
                                        TO COD-VENDEDOR-HBSIS007C
              MOVE NOME-VEND-HBSIS004C  TO NOME-VEND-HBSIS007C
           END-IF
           
           PERFORM 2420-LER-ARQ-VENDEDOR
           
           .
       2430-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2500-CLOSE-ARQ-CLIENTE          SECTION.

           CLOSE ARQ-CLIENTE                                            
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO CLIENTE" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2600-CLOSE-ARQ-VENDEDOR         SECTION.

           CLOSE ARQ-VENDEDOR                                           
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO VENDEDOR" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2700-CLOSE-ARQ-DISTRIBUICAO     SECTION.

           CLOSE ARQ-DISTRIBUICAO                                       
           
           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-HBSIS007L
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DISTRIBUICAO - 7" 
                                       TO MSG-RETORNO-HBSIS007L
              PERFORM 3000-FINALIZA
           END-IF

           .
       2700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GERA RELATORIO                                                 *
      *----------------------------------------------------------------*
       2800-GERA-RELATORIO             SECTION.

           CALL WS-HBSIS008            USING HBSIS008L                  
           
           .
       2800-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZA플O                                          *
      *----------------------------------------------------------------*
       3000-FINALIZA                   SECTION.

           GOBACK
           
           .
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA HBSIS008P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS007P.
      *----------------------------------------------------------------*