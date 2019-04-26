       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     HBSIS001P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: HBSIS001                                        *
      * ANALISTA.....: JOYCE SCHAIDT                                   *
      * DATA.........: 23/04/2019                                      *
      * OBJETIVO.....: MENU INICIAL CARTEIRA CLIENTES AMBEV            *
      * ARQUIVOS.....:                                                 *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  WS-OPCAO                    PIC  9(001)         VALUE ZEROS.
       01  WS-CAD-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-REL-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-EXE-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CLI-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-VEN-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CONFIRMA                 PIC  X(001)         VALUE ZEROS.
       01  WS-LINHA-BRANCO             PIC  X(078)         VALUE SPACES.
       01  WS-MSG-ERRO                 PIC  X(050)         VALUE
           "FAVOR DIGITAR UMA OPCAO VALIDA".
      * 
       01  WS-CODIGO-CLI               PIC  9(007)         VALUE ZEROS.
       01  WS-CNPJ-CLI                 PIC  9(014)         VALUE ZEROS.
       01  WS-RAZAO-SOCIAL             PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-CLI             PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-CLI            PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-NOME-ARQ-CLI             PIC  X(020)         VALUE SPACES.
      *
       01  WS-CODIGO-VEND              PIC  9(003)         VALUE ZEROS.
       01  WS-CPF-VEND                 PIC  9(011)         VALUE ZEROS.
       01  WS-NOME-VEND                PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-VEND            PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-VEND           PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-NOME-ARQ-VEND            PIC  X(020)         VALUE SPACES.
      *
       01  WS-RELC-TIPO-ORD            PIC  X(001)         VALUE SPACES.
       01  WS-RELC-TIPO-CLA            PIC  X(001)         VALUE SPACES.
       01  WS-RELC-COD-CLI             PIC  9(007)         VALUE ZEROS.
       01  WS-RELC-RAZ-SOC             PIC  X(040)         VALUE SPACES.
       01  WS-RELC-COD-VEND            PIC  9(003)         VALUE ZEROS.
      *
       01  WS-RELV-TIPO-ORD            PIC  X(001)         VALUE SPACES.
       01  WS-RELV-TIPO-CLA            PIC  X(001)         VALUE SPACES.
       01  WS-RELV-COD-VEND            PIC  9(003)         VALUE ZEROS.
       01  WS-RELV-NOME-VEND           PIC  X(040)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      * AREAS DE COMUNICAÇÃO COM OUTROS PROGRAMAS                      *
      *----------------------------------------------------------------*
       01  WS-HBSIS002                 PIC  X(009)         VALUE
           'HBSIS002P'.
       01  WS-HBSIS004                 PIC  X(009)         VALUE
           'HBSIS004P'.
       01  WS-HBSIS005                 PIC  X(009)         VALUE
           'HBSIS005P'.
       01  WS-HBSIS006                 PIC  X(009)         VALUE
           'HBSIS006P'.
       01  WS-HBSIS007                 PIC  X(009)         VALUE
           'HBSIS007P'.

       COPY HBSIS002L.
       COPY HBSIS004L.
       COPY HBSIS005L.
       COPY HBSIS006L.
       COPY HBSIS007L.
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
       01  TELA-MENU.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU PRINCIPAL".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - CADASTROS".
           05  LINE 09 COL 10          VALUE "2 - RELATORIOS".
           05  LINE 10 COL 10          VALUE "3 - EXECUTAR".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "DIGITE A OPCAO DESEJADA E TECLE ENTER: ".
           05  LINE 15 COL 49,         PIC  9(001)
                                       TO WS-OPCAO.
                                       
       01  TELA-CADASTRO.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU DE CADASTROS".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - CADASTRO DE CLIENTE".
           05  LINE 09 COL 10          VALUE "2 - CADASTRO DE VENDEDOR".
           05  LINE 10 COL 10          VALUE 
           "3 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE  
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49,         PIC  9(001) 
                                       TO WS-CAD-OPCAO.
                                       
       01  TELA-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CADASTRO DE CLIENTES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - INCLUIR CLIENTE".
           05  LINE 09 COL 10          VALUE "2 - ALTERAR CLIENTE".
           05  LINE 10 COL 10          VALUE "3 - EXCLUIR CLIENTE".
           05  LINE 11 COL 10          VALUE "4 - IMPORTAR CLIENTE".
           05  LINE 12 COL 10          VALUE 
           "5 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 15 COL 10          VALUE 
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49          PIC  9(001)
                                       TO WS-CLI-OPCAO.

       01  TELA-INSERIR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "INCLUIR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC  ZZZZZZ9 
                                       TO WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       TO WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       TO WS-RAZAO-SOCIAL.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LATITUDE-CLI.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LONGITUDE-CLI.
           05  LINE 15 COL 10          VALUE 
              "CONFIRMA A INCLUSAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 48,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-BUSCAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CONSULTAR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC  ZZZZZZ9 
                                       TO WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "OU CNPJ.......:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       TO WS-CNPJ-CLI.
           05  LINE 15 COL 10          VALUE 
           "DIGITE OS DADOS E TECLE ENTER:".

       01  TELA-ALTERAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".   
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "ALTERAR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC ZZZZZZ9 FROM WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) FROM WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       USING WS-RAZAO-SOCIAL AUTO.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LATITUDE-CLI AUTO.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LONGITUDE-CLI AUTO.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A ALTERACAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-EXCLUIR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".   
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "EXCLUIR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC ZZZZZZ9 
                                       FROM WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       FROM WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       FROM WS-RAZAO-SOCIAL.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LATITUDE-CLI.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LONGITUDE-CLI.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A EXCLUSAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 48,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-IMPORTAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".   
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "IMPORTAR CLIENTE".        
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 07 COL 10          VALUE 
           "DIGITE O NOME DO ARQUIVO DE IMPORTACAO:".
           05  LINE 08 COL 25,         PIC  X(020) 
                                       TO WS-NOME-ARQ-CLI.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A IMPORTACAO?(S/N):".
           05  LINE 15 COL 30,         PIC  X(001) 
                                       TO WS-CONFIRMA.
                                       
       01  TELA-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".   
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CADASTRO DE VENDEDORES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - INCLUIR VENDEDOR".
           05  LINE 09 COL 10          VALUE "2 - ALTERAR VENDEDOR".
           05  LINE 10 COL 10          VALUE "3 - EXCLUIR VENDEDOR".    
           05  LINE 11 COL 10          VALUE "4 - IMPORTAR VENDEDOR".   
           05  LINE 12 COL 10          VALUE 
           "5 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 15 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49          PIC  9(001)
                                       TO WS-VEN-OPCAO.

       01  TELA-INSERIR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE VENDEDORES". 
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "INCLUIR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC  ZZ9 
                                       TO WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       TO WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       TO WS-NOME-VEND.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LATITUDE-VEND.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LONGITUDE-VEND.
           05  LINE 15 COL 10          VALUE
              "CONFIRMA A INCLUSAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-BUSCAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE VENDEDORES". 
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CONSULTAR VENDEDOR".      
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC  ZZ9 
                                       TO WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "OU CPF.........:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       TO WS-CPF-VEND.
           05  LINE 15 COL 10          VALUE
           "DIGITE OS DADOS E TECLE ENTER:".

       01  TELA-ALTERAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE VENDEDORES".  
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "ALTERAR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC ZZ9 FROM WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) FROM WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       USING WS-NOME-VEND AUTO.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LATITUDE-VEND AUTO.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LONGITUDE-VEND AUTO.
           05  LINE 15 COL 10          VALUE
           "CONFIRMA A ALTERACAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 50,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-EXCLUIR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE VENDEDORES".  
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "EXCLUIR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC ZZ9 
                                       FROM WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       FROM WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       FROM WS-NOME-VEND.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LATITUDE-VEND.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LONGITUDE-VEND.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A EXCLUSAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  TELA-IMPORTAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE VENDEDORES".  
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "IMPORTAR VENDEDOR".       
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 07 COL 10          VALUE 
           "DIGITE O NOME DO ARQUIVO DE IMPORTACAO:".
           05  LINE 08 COL 25,         PIC  X(020) 
                                       TO WS-NOME-ARQ-VEND.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A IMPORTACAO?(S/N):".
           05  LINE 15 COL 30,         PIC  X(001) 
                                       TO WS-CONFIRMA.
       
       01  TELA-RELATORIO.
           05  BLANK SCREEN.
           05 LINE 01 COL 1           VALUE
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 1           VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU DE RELATORIOS".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - RELATORIO DE CLIENTE".
           05  LINE 09 COL 10          VALUE 
           "2 - RELATORIO DE VENDEDOR".
           05  LINE 10 COL 10          VALUE 
           "3 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.

       01  TELA-RELAT-CLIENTES.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 1           VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "RELATORIO DE CLIENTES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO".
	       05  LINE 07 COL 10          VALUE 
           "(A)ASCENDENTE (D)DECRESCENTE.: ".
           05  LINE 07 COL 41,         PIC  X(001)
                                       TO WS-RELC-TIPO-ORD.
           05  LINE 09 COL 10          VALUE "TIPO DE CLASSIFICACAO".
           05  LINE 10 COL 10          VALUE
           "(C)COD CLIENTE (R)RAZ SOCIAL.: ".
           05  LINE 10 COL 41,         PIC  X(001)
                                       TO WS-RELC-TIPO-CLA.
           05  LINE 12 COL 10          VALUE "CODIGO CLIENTE.: ".
           05  LINE 12 COL 28,         PIC  ZZZZZZ9 
                                       TO WS-RELC-COD-CLI.
           05  LINE 13 COL 10          VALUE "RAZAO SOCIAL...: ".
           05  LINE 13 COL 28,         PIC  X(040) 
                                       TO WS-RELC-RAZ-SOC.
           05  LINE 14 COL 10          VALUE "CODIGO VENDEDOR: ".
           05  LINE 14 COL 28,         PIC  ZZ9 
                                       TO WS-RELC-COD-VEND.
           05  LINE 16 COL 10          VALUE 
           "1-GERAR RELATORIO   2-VOLTAR   9-SAIR DO SISTEMA".
           05  LINE 17 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 17 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.

       01  TELA-RELAT-VENDEDORES.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".   
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 1           VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "RELATORIO DE VENDEDORES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO".
	       05  LINE 07 COL 10          VALUE 
           "(A)ASCENDENTE (D)DECRESCENTE.....: ".
           05  LINE 07 COL 45,         PIC  X(001)
                                       TO WS-RELV-TIPO-ORD.
           05  LINE 09 COL 10          VALUE "TIPO DE CLASSIFICACAO".
           05  LINE 10 COL 10          VALUE
           "(C)COD VENDEDOR (N)NOME VENDEDOR.: ".
           05  LINE 10 COL 45,         PIC  X(001)
                                       TO WS-RELV-TIPO-CLA.
           05  LINE 12 COL 10          VALUE "CODIGO VENDEDOR: ".
           05  LINE 12 COL 28,         PIC  ZZ9 
                                       TO WS-RELV-COD-VEND.
           05  LINE 13 COL 10          VALUE "NOME VENDEDOR..: ".
           05  LINE 13 COL 28,         PIC  X(040) 
                                       TO WS-RELV-NOME-VEND.
           05  LINE 15 COL 10          VALUE 
           "1-GERAR RELATORIO   2-VOLTAR   9-SAIR DO SISTEMA".
           05  LINE 16 COL 10          VALUE
           "DIGITE A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 16 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.
                                       
       01  TELA-EXECUTAR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 02 COL 01          VALUE "*".
           05  LINE 02 COL 21          VALUE "CARTEIRA DE CLIENTES".
           05  LINE 02 COL 58          VALUE "*".
           05  LINE 03 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU EXECUTAR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE
           "1 - EXECUTAR DISTRIBUICAO DE CLIENTE".
           05  LINE 09 COL 10          VALUE 
           "2 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 10 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "DIGITE A OPCAO DESEJADA E TECLE ENTER".
           05  LINE 15 COL 49          PIC  9(001) 
                                       TO WS-EXE-OPCAO.
       
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
       
           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 9000-FINALIZA
           
           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZAÇÃO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.
       
            PERFORM 3000-LIMPA-CAMPOS-CLI
            PERFORM 4000-LIMPA-CAMPOS-VEN
       
            .       
       1000-EXIT.
           EXIT.                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
       
           DISPLAY TELA-MENU
           ACCEPT  TELA-MENU
       
           EVALUATE WS-OPCAO
               WHEN 1
                    PERFORM 2100-CADASTROS 
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 3
                    PERFORM 2300-EXECUTAR
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2000-PROCESSA
           END-EVALUATE
           
           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TELA DE CADASTRAMENTO                                          *
      *----------------------------------------------------------------*
       2100-CADASTROS                  SECTION.
       
           DISPLAY TELA-CADASTRO
           ACCEPT  TELA-CADASTRO
       
           EVALUATE WS-CAD-OPCAO
               WHEN 1
                    PERFORM 2110-CAD-CLIENTE
               WHEN 2
                    PERFORM 2120-CAD-VENDEDOR
               WHEN 3
                    PERFORM 1000-INICIALIZA
                    PERFORM 2000-PROCESSA
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2100-CADASTROS
           END-EVALUATE

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE CADASTRAMENTO DE CLIENTES                            *
      *----------------------------------------------------------------*
       2110-CAD-CLIENTE                SECTION.
       
           DISPLAY TELA-CLIENTE
           ACCEPT  TELA-CLIENTE
       
           EVALUATE WS-CLI-OPCAO
               WHEN 1
                    PERFORM 2111-INSERIR-CLIENTE
               WHEN 2
                    PERFORM 2112-ALTERAR-CLIENTE                        
               WHEN 3
                    PERFORM 2113-EXCLUIR-CLIENTE                       
               WHEN 4
      *              PERFORM 2114-IMPORTAR-CLIENTE                      
               WHEN 5
                    PERFORM 3000-LIMPA-CAMPOS-CLI
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2110-CAD-CLIENTE
           END-EVALUATE

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INSERÇÃO DE CLIENTES                                 *
      *----------------------------------------------------------------*
       2111-INSERIR-CLIENTE            SECTION.
           
           PERFORM 3000-LIMPA-CAMPOS-CLI
           
           DISPLAY TELA-INSERIR-CLIENTE
           ACCEPT  TELA-INSERIR-CLIENTE
       
           IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
              MOVE 1                   TO COD-FUNCAO-HBSIS002
              PERFORM 5000-CARREGA-DADOS-CLIENTE                        
              PERFORM 6000-CHAMA-HBSIS002
              DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
              DISPLAY MSG-RETORNO-HBSIS002
                                       AT 1820
              STOP ' '
              PERFORM 3000-LIMPA-CAMPOS-CLI
              PERFORM 2000-PROCESSA
           ELSE
              IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                 PERFORM 3000-LIMPA-CAMPOS-CLI
                 PERFORM 2110-CAD-CLIENTE
              ELSE
                 DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                 DISPLAY WS-MSG-ERRO   AT 1820
                 STOP ' '
                 PERFORM 3000-LIMPA-CAMPOS-CLI
                 PERFORM 2110-CAD-CLIENTE
              END-IF
           END-IF

           .
       2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALTERAÇAO DE CLIENTES                                *
      *----------------------------------------------------------------*
       2112-ALTERAR-CLIENTE            SECTION.
       
           PERFORM 3000-LIMPA-CAMPOS-CLI
           
           DISPLAY TELA-BUSCAR-CLIENTE
           ACCEPT  TELA-BUSCAR-CLIENTE
      
           MOVE 0                     TO COD-FUNCAO-HBSIS002       
           PERFORM 5000-CARREGA-DADOS-CLIENTE
           PERFORM 6000-CHAMA-HBSIS002
            
           EVALUATE COD-RETORNO-HBSIS002
               WHEN ZEROS
                    MOVE COD-CLIENTE-HBSIS002
                                      TO WS-CODIGO-CLI
                    MOVE CNPJ-HBSIS002
                                      TO WS-CNPJ-CLI
                    DISPLAY TELA-ALTERAR-CLIENTE
                    ACCEPT  TELA-ALTERAR-CLIENTE
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 2          TO COD-FUNCAO-HBSIS002
                       PERFORM 5000-CARREGA-DADOS-CLIENTE               
                       PERFORM 6000-CHAMA-HBSIS002
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-HBSIS002            
                                       AT 1820
                       STOP ' '
                       PERFORM 3000-LIMPA-CAMPOS-CLI           
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 3000-LIMPA-CAMPOS-CLI
                          PERFORM 2110-CAD-CLIENTE
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2110-CAD-CLIENTE
                       END-IF
                    END-IF
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-HBSIS002                       
                                       AT 1820
                    STOP ' '
                    PERFORM 3000-LIMPA-CAMPOS-CLI
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXCLUSÃO DE CLIENTES                                           *
      *----------------------------------------------------------------*
       2113-EXCLUIR-CLIENTE            SECTION.
           
           PERFORM 3000-LIMPA-CAMPOS-CLI
           
           DISPLAY TELA-BUSCAR-CLIENTE
           ACCEPT  TELA-BUSCAR-CLIENTE
      
           MOVE 0                      TO COD-FUNCAO-HBSIS002     
           PERFORM 5000-CARREGA-DADOS-CLIENTE
           PERFORM 6000-CHAMA-HBSIS002
            
           EVALUATE COD-RETORNO-HBSIS002
               WHEN ZEROS
                    MOVE COD-CLIENTE-HBSIS002
                                       TO WS-CODIGO-CLI
                    MOVE CNPJ-HBSIS002
                                       TO WS-CNPJ-CLI
                    MOVE LATITUDE-CLI-HBSIS002                          
                                       TO WS-LATITUDE-CLI
                    MOVE LONGITUDE-CLI-HBSIS002                      
                                       TO WS-LONGITUDE-CLI
                    MOVE RAZAO-SOCIAL-HBSIS002  
                                       TO WS-RAZAO-SOCIAL
                    DISPLAY TELA-EXCLUIR-CLIENTE
                    ACCEPT  TELA-EXCLUIR-CLIENTE
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 3          TO COD-FUNCAO-HBSIS002           
                       PERFORM 6000-CHAMA-HBSIS002
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-HBSIS002
                                       AT 1820
                       STOP ' '
                       PERFORM 3000-LIMPA-CAMPOS-CLI
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 3000-LIMPA-CAMPOS-CLI
                          PERFORM 2110-CAD-CLIENTE
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2110-CAD-CLIENTE
                       END-IF
                   END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-HBSIS002                       
                                       AT 1820
                    STOP ' '
                    PERFORM 3000-LIMPA-CAMPOS-CLI
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * IMPORTAÇÃO DE CLIENTES                                         *
      *----------------------------------------------------------------*
       2114-IMPORTAR-CLIENTE            SECTION.
       
           PERFORM 3000-LIMPA-CAMPOS-CLI
           
           DISPLAY TELA-IMPORTAR-CLIENTE
           ACCEPT  TELA-IMPORTAR-CLIENTE
       
           .
       2114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE CADASTRAMENTO DE VENDEDORES                          *
      *----------------------------------------------------------------*
       2120-CAD-VENDEDOR               SECTION.
       
           DISPLAY TELA-VENDEDOR
           ACCEPT  TELA-VENDEDOR
       
           EVALUATE WS-VEN-OPCAO
               WHEN 1
                    PERFORM 2121-INSERIR-VENDEDOR
               WHEN 2
                    PERFORM 2122-ALTERAR-VENDEDOR                       
               WHEN 3
                    PERFORM 2123-EXCLUIR-VENDEDOR                      
               WHEN 4
      *              PERFORM 2124-IMPORTAR-VENDEDOR                     
               WHEN 5
                    PERFORM 4000-LIMPA-CAMPOS-VEN
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2120-CAD-VENDEDOR
           END-EVALUATE

           .
       2120-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INSERÇÃO DE VENDEDORES                                         *
      *----------------------------------------------------------------*
       2121-INSERIR-VENDEDOR           SECTION.
           
           PERFORM 4000-LIMPA-CAMPOS-VEN
           
           DISPLAY TELA-INSERIR-VENDEDOR
           ACCEPT  TELA-INSERIR-VENDEDOR
       
           IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
              MOVE 1                   TO COD-FUNCAO-HBSIS004
              PERFORM 7000-CARREGA-DADOS-VENDEDOR                 
              PERFORM 8000-CHAMA-HBSIS004
              DISPLAY WS-LINHA-BRANCO  AT 1802
              DISPLAY MSG-RETORNO-HBSIS004
                                       AT 1820
              STOP ' '
              PERFORM 4000-LIMPA-CAMPOS-VEN
              PERFORM 2000-PROCESSA
           ELSE
              IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                 PERFORM 4000-LIMPA-CAMPOS-VEN
                 PERFORM 2120-CAD-VENDEDOR
              ELSE
                 DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                 DISPLAY WS-MSG-ERRO   AT 1820
                 STOP ' '
                 PERFORM 4000-LIMPA-CAMPOS-VEN
                 PERFORM 2120-CAD-VENDEDOR
              END-IF
           END-IF

           .
       2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ALTERAÇAO DE VENDEDORES                                        *
      *----------------------------------------------------------------*
       2122-ALTERAR-VENDEDOR           SECTION.
       
           PERFORM 4000-LIMPA-CAMPOS-VEN
           
           DISPLAY TELA-BUSCAR-VENDEDOR
           ACCEPT  TELA-BUSCAR-VENDEDOR
      
           MOVE 0                     TO COD-FUNCAO-HBSIS004       
           PERFORM 7000-CARREGA-DADOS-VENDEDOR                          
           PERFORM 8000-CHAMA-HBSIS004
            
           EVALUATE COD-RETORNO-HBSIS004
               WHEN ZEROS
                    MOVE COD-VENDEDOR-HBSIS004
                                       TO WS-CODIGO-VEND
                    MOVE CPF-HBSIS004  TO WS-CPF-VEND
                    DISPLAY TELA-ALTERAR-VENDEDOR
                    ACCEPT  TELA-ALTERAR-VENDEDOR
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 2          TO COD-FUNCAO-HBSIS004
                       PERFORM 7000-CARREGA-DADOS-VENDEDOR              
                       PERFORM 8000-CHAMA-HBSIS004
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-HBSIS004            
                                       AT 1820
                       STOP ' '
                       PERFORM 4000-LIMPA-CAMPOS-VEN
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 4000-LIMPA-CAMPOS-VEN
                          PERFORM 2120-CAD-VENDEDOR
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2120-CAD-VENDEDOR
                       END-IF
                    END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-HBSIS004                       
                                       AT 1820
                    STOP ' '
                    PERFORM 4000-LIMPA-CAMPOS-VEN
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXCLUSÃO DE VENDEDORES                                         *
      *----------------------------------------------------------------*
       2123-EXCLUIR-VENDEDOR           SECTION.
           
           PERFORM 4000-LIMPA-CAMPOS-VEN
                     
           DISPLAY TELA-BUSCAR-VENDEDOR
           ACCEPT  TELA-BUSCAR-VENDEDOR
      
           MOVE 0                      TO COD-FUNCAO-HBSIS004     
           PERFORM 7000-CARREGA-DADOS-VENDEDOR                          
           PERFORM 8000-CHAMA-HBSIS004
            
           EVALUATE COD-RETORNO-HBSIS004
               WHEN ZEROS
                    MOVE COD-VENDEDOR-HBSIS004
                                       TO WS-CODIGO-VEND
                    MOVE CPF-HBSIS004
                                       TO WS-CPF-VEND
                    MOVE LATITUDE-VEN-HBSIS004                         
                                       TO WS-LATITUDE-VEND
                    MOVE LONGITUDE-VEN-HBSIS004                      
                                       TO WS-LONGITUDE-VEND
                    MOVE NOME-VENDEDOR-HBSIS004  
                                       TO WS-NOME-VEND
                    DISPLAY TELA-EXCLUIR-VENDEDOR
                    ACCEPT  TELA-EXCLUIR-VENDEDOR
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 3          TO COD-FUNCAO-HBSIS004           
                       PERFORM 8000-CHAMA-HBSIS004
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-HBSIS004
                                       AT 1820
                       STOP ' '
                       PERFORM 4000-LIMPA-CAMPOS-VEN
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 4000-LIMPA-CAMPOS-VEN
                          PERFORM 2120-CAD-VENDEDOR
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2120-CAD-VENDEDOR
                       END-IF
                    END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-HBSIS004                       
                                       AT 1820
                    STOP ' '
                    PERFORM 4000-LIMPA-CAMPOS-VEN
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * IMPORTAÇÃO DE VENDEDORES                                       *
      *----------------------------------------------------------------*
       2124-IMPORTAR-VENDEDOR          SECTION.
       
           PERFORM 4000-LIMPA-CAMPOS-VEN
                    
           DISPLAY TELA-IMPORTAR-VENDEDOR
           ACCEPT  TELA-IMPORTAR-VENDEDOR
                       
           .
       1114-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE RELATORIOS                                           *
      *----------------------------------------------------------------*
       2200-RELATORIOS                 SECTION.
       
           DISPLAY TELA-RELATORIO
           ACCEPT  TELA-RELATORIO
       
           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2210-RELAT-CLIENTE 
               WHEN 2
                    PERFORM 2220-RELAT-VENDEDOR
               WHEN 3
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO RELATORIO DE CLIENTES                             *
      *----------------------------------------------------------------*
       2210-RELAT-CLIENTE              SECTION.
       
           MOVE ZEROS                  TO WS-RELC-COD-CLI
                                          WS-RELC-COD-VEND
           MOVE SPACES                 TO WS-RELC-TIPO-ORD
                                          WS-RELC-TIPO-CLA
                                          WS-RELC-RAZ-SOC

           DISPLAY TELA-RELAT-CLIENTES
           ACCEPT  TELA-RELAT-CLIENTES

           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2211-CHAMA-HBSIS005
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-HBSIS005L
                                       AT 1920
                    STOP ' '
                    PERFORM 2210-RELAT-CLIENTE
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2210-RELAT-CLIENTE
           END-EVALUATE

           .
       2210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    GERACAO DO RELATORIO DE CLIENTES                            *
      *----------------------------------------------------------------*
       2211-CHAMA-HBSIS005             SECTION.

           MOVE WS-RELC-TIPO-ORD       TO TIPO-ORD-HBSIS005L 
           MOVE WS-RELC-TIPO-CLA       TO TIPO-CLA-HBSIS005L
           MOVE WS-RELC-COD-CLI        TO COD-CLI-HBSIS005L
           MOVE WS-RELC-RAZ-SOC        TO RAZ-SOC-HBSIS005L
           MOVE WS-RELC-COD-VEND       TO COD-VEND-HBSIS005L
           
           CALL WS-HBSIS005            USING HBSIS005L

           .
       2211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO RELATORIO DE VENDEDORES                           *
      *----------------------------------------------------------------*
       2220-RELAT-VENDEDOR             SECTION.
       
           MOVE ZEROS                  TO WS-RELV-COD-VEND
           MOVE SPACES                 TO WS-RELV-TIPO-ORD
                                          WS-RELV-TIPO-CLA
                                          WS-RELV-NOME-VEND

           DISPLAY TELA-RELAT-VENDEDORES
           ACCEPT  TELA-RELAT-VENDEDORES
               
           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2211-CHAMA-HBSIS006
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-HBSIS006L
                                       AT 1920
                    STOP ' '
                    PERFORM 2220-RELAT-VENDEDOR
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY WS-MSG-ERRO
                                       AT 1920
                    STOP ' '
                    PERFORM 2220-RELAT-VENDEDOR
           END-EVALUATE

           .
       2220-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO GERACAO DO RELATORIO DE VENDEDORES                *
      *----------------------------------------------------------------*
       2211-CHAMA-HBSIS006             SECTION.

           MOVE WS-RELV-TIPO-ORD       TO TIPO-ORD-HBSIS006L
           MOVE WS-RELV-TIPO-CLA       TO TIPO-CLA-HBSIS006L
           MOVE WS-RELV-COD-VEND       TO COD-VEND-HBSIS006L
           MOVE WS-RELV-NOME-VEND      TO NOME-VEND-HBSIS006L

           CALL WS-HBSIS006            USING HBSIS006L

           .
       2221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXECUÇÃO                                             *
      *----------------------------------------------------------------*
       2300-EXECUTAR                   SECTION.
       
           DISPLAY TELA-EXECUTAR
           ACCEPT  TELA-EXECUTAR
       
           EVALUATE WS-EXE-OPCAO
               WHEN 1
                    PERFORM 2310-CHAMA-HBSIS007
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-HBSIS007L
                                       AT 1920
                    STOP ' '
                    PERFORM 2300-EXECUTAR
               WHEN 2
                    PERFORM 2000-PROCESSA
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY WS-MSG-ERRO
                                       AT 1920
                    STOP ' '
                    PERFORM 2300-EXECUTAR
           END-EVALUATE

           .
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    EFETUA DISTRIBUICAO CLIENTE X VENDEDOR                      *
      *----------------------------------------------------------------*
       2310-CHAMA-HBSIS007             SECTION.

           CALL WS-HBSIS007            USING HBSIS007L

           .
       2310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INICIALIZAÇÃO DE VARIAVEIS CLIENTE                             *
      *----------------------------------------------------------------*
       3000-LIMPA-CAMPOS-CLI           SECTION.
       
           MOVE ZEROS                  TO WS-CODIGO-CLI            
              
                                          WS-CNPJ-CLI                 
                                          WS-LATITUDE-CLI
                                          WS-LONGITUDE-CLI
           MOVE SPACES                 TO WS-RAZAO-SOCIAL

           .     
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INICIALIZAÇÃO DE VARIAVEIS VENDEDOR                            *
      *----------------------------------------------------------------*
       4000-LIMPA-CAMPOS-VEN           SECTION.
       
           MOVE ZEROS                  TO WS-CODIGO-VEND               
                                          WS-CPF-VEND                 
                                          WS-LATITUDE-VEND
                                          WS-LONGITUDE-VEND
           MOVE SPACES                 TO WS-NOME-VEND

           .     
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CARREGAMENTO DE DADOS DO CLIENTE                               *
      *----------------------------------------------------------------*
       5000-CARREGA-DADOS-CLIENTE      SECTION.                         
           
           MOVE WS-CODIGO-CLI          TO COD-CLIENTE-HBSIS002
           MOVE WS-CNPJ-CLI            TO CNPJ-HBSIS002
           MOVE WS-LATITUDE-CLI        TO LATITUDE-CLI-HBSIS002
           MOVE WS-LONGITUDE-CLI       TO LONGITUDE-CLI-HBSIS002
           MOVE WS-RAZAO-SOCIAL        TO RAZAO-SOCIAL-HBSIS002
           MOVE WS-NOME-ARQ-CLI        TO NOME-ARQ-CLI-HBSIS002         
     
           .
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ACESSAR PROGRAMA HBSIS002P                                     *
      *----------------------------------------------------------------*
       6000-CHAMA-HBSIS002             SECTION.

           CALL WS-HBSIS002            USING HBSIS002L. 
           
           . 
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CARREGAMENTO DE DADOS VENDEDOR                                 *
      *----------------------------------------------------------------*
       7000-CARREGA-DADOS-VENDEDOR     SECTION.                         
           
           MOVE WS-CODIGO-VEND         TO COD-VENDEDOR-HBSIS004
           MOVE WS-CPF-VEND            TO CPF-HBSIS004
           MOVE WS-LATITUDE-VEND       TO LATITUDE-VEN-HBSIS004
           MOVE WS-LONGITUDE-VEND      TO LONGITUDE-VEN-HBSIS004
           MOVE WS-NOME-VEND           TO NOME-VENDEDOR-HBSIS004
           MOVE WS-NOME-ARQ-VEND       TO NOME-ARQ-VEN-HBSIS004        
     
           .
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR PROGRAMA HBSIS004P                         *
      *----------------------------------------------------------------*
       8000-CHAMA-HBSIS004             SECTION.

           CALL WS-HBSIS004            USING HBSIS004L. 
           
           . 
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZAÇÃO                                          *
      *----------------------------------------------------------------*
       9000-FINALIZA                   SECTION.

           GOBACK.       
     
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA HBSIS001P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     HBSIS001P.
      *----------------------------------------------------------------*