
      //--------------------------------------------------------------------
      //
      // PURPOSE: Card Authorization (either Debit or Credit) with
      //          GetBeyond.
      //
      // M A I N T E N A N C E   L O G
      // -----------------------------
      // R119
      // BJE 08/11/2022: Increase Dimension of Items array to 200 from 20.
      // BJE 08/24/2022: Removed line to Clear Token.  Token is being used
      //                 as both input and output in parameters.
      // BJE 09/24/2022: Removed line to Clear Token.  Token is being used
      // lah 09/07/2022: incease size of ponum                         sed
      // lah/BJE 09/22   Level 2 processing
      // lah     09/20/22 Add AccountZip
      // BJE 04/12/2023  Increase size of field "Output" to 32766
      // BJE 04/12/2023  Add parameter "RESPONSE" to check if a response has
      //                 been received from the API. If not, send a message
      //                 back to calling program.
      //
      // R118
      // BJE 12/21/2021: Created
      // BJE 05/03/2022: Totally Reorganized for simpler reading
      //
      //--------------------------------------------------------------------
       Ctl-Opt Option(*SrcStmt) Dftactgrp(*NO) Bnddir('QC2LE');

       // Prototypes
       Dcl-Pr Entry   Extpgm('AR471');
         CCNo         Char(16)    CONST;
         ExpDate      Char(6)     CONST;
         #Amount      Char(10)    CONST;
         TransType    Char(15)    CONST;
         HolderTyp    Char(1)     CONST;
         PONum        Char(30);
         CustAcct     Char(7)     CONST;
         InvoiceNum   Char(8)     CONST;
         TaxAmount    Char(10)    CONST;
         TaxIndicator Char(1)     CONST;
         AccountZip   Char(12)    CONST;
         ItemCount    Char(3)     CONST;
         #Items       Like(Items) DIM(200) CONST;
         AuthCode     Char(6);
         AuthMessage  Char(50);
         Token        Char(22);
         ReferenceNo  Char(10);
       End-Pr;

       Dcl-Pi Entry;
         CCNo         Char(16)    CONST;
         ExpDate      Char(6)     CONST;
         #Amount      Char(10)    CONST;
         TransType    Char(15)    CONST;
         HolderTyp    Char(1)     CONST;
         PONum        Char(30);
         CustAcct     Char(7)     CONST;
         InvoiceNum   Char(8)     CONST;
         TaxAmount    Char(10)    CONST;
         TaxIndicator Char(1)     CONST;
         AccountZip   Char(12)    CONST;
         ItemCount    Char(3)     CONST;
         #Items       Like(Items) DIM(200) CONST;
         AuthCode     Char(6);
         AuthMessage  Char(50);
         Token        Char(22);
         ReferenceNo  Char(10);
       End-Pi;

       Dcl-Pr Call_AR471C  Extpgm('AR471C');
         GBPRDTST     Char(1);
         Response     Char(1);
       End-Pr;

       Dcl-Pr ENCODE64 Extpgm('ENCODE64') End-Pr;
       Dcl-Pr DECODE64 Extpgm('DECODE64') End-Pr;


       // Prototypes for IFS access
       Dcl-Pr    mkdir     Int(10) extproc('mkdir');
         path              Pointer value options(*trim:*STRING);
         mode              Int(10) value;
       End-Pr;

       Dcl-Pr    openf     Int(10)  extproc('open');
         path              Pointer  value options(*STRING);
         oflag             Int(10)  value;
         mode              Uns(10)  value options(*NOPASS);
         codepage          Uns(10)  value options(*NOPASS);
       End-Pr;

       Dcl-Pr    readf    Int(10)  extproc('read');
          fildes           Int(10)  value;
          buf              Pointer  value;
          nbyte            Uns(10)  value;
       End-Pr;

       Dcl-Pr    writef    Int(10)  extproc('write');
          fildes           Int(10)  value;
          buf              Pointer  value;
          nbyte            Uns(10)  value;
       End-Pr;

       Dcl-Pr    closef    Int(10)  extproc('close');
         fildes            Int(10)  value;
       End-Pr;

       Dcl-Pr    unlink    Int(10)  extproc('unlink');
         path              Pointer  value options(*STRING);
       End-Pr;


       // Files
       Dcl-F Oeopnol0  Keyed;
       Dcl-F Gbcreden  Keyed;

       // Global Constants
       Dcl-C CRLF            x'0d25';
       Dcl-C TAB             x'05';
       Dcl-C NULL            x'00';
       Dcl-C ##YES           '1';
       Dcl-C ##NO            '0';
       Dcl-C #MAXITEMS       30;

       // Constants for IFS Creation
       Dcl-C o_RDONLY        1;
       Dcl-C o_WRONLY        2;
       Dcl-C o_CREAT         8;
       Dcl-C o_TRUNC         64;
       Dcl-C o_CODEPAGE      8388608;
       Dcl-C o_TEXTDATA      16777216;
       Dcl-C o_TEXT_CREAT    33554432;

       // Constants for IFS authority
       Dcl-C RW              6;
       Dcl-C R               4;
       Dcl-C OWNER           64;
       Dcl-C GROUP           8;

       // Local Data Area
     D               EUDS                  EXTNAME(LDADS)

       // Local Data Structures

       Dcl-DS Items  DIM(200) Qualified;
         ItemCode       Char(5);
         Description    Char(35);
         Qty            Zoned(10:3);
         UnitCost       Zoned(9:4);
         Uom            Char(3);
         TotalAmt       Zoned(9:2);
       End-DS;

       // Global Variables
       Dcl-S Directory       Char(30);

       // ------------------------------------------------------------------------------------------
       // Mainline Routine
       // ------------------------------------------------------------------------------------------

         // Clear Output Parameters
         Clear AuthCode;
         Clear AuthMessage;
         // Clear Token;  Removed 8/24/2022 by BJE
         Clear ReferenceNo;

         // Get Items Parameter
         Items = #Items;

         // Make directory for user if needed
         Directory = '/mw4ifs/web/' + %trim(Ldausr);
         mkdir(Directory:511);

         Main();

         *INLR = ##YES;
         Return;

       //----------------------------------------------------------------
       //  Main Procedure
       //----------------------------------------------------------------
       Dcl-Proc Main;

         // Declare Local Varaibles
         Dcl-S Data            Char(1000000);
         Dcl-S bUser           Char(10);
         Dcl-S Output          Char(1000000);
         Dcl-S Response        Char(1);

         // Get User and Password for API
         bUser=*blanks;
         Chain (Ldaco#:Ldausr) Gbcreden;
         If not %found (Gbcreden);
           Chain (Ldaco#:bUser) Gbcreden;
         Endif;

         // Create Request in  XML format
         Data = CreateXMLRequest(GBUSER:GBUSERPW:GBMERCH:GBMERCHA);

         // Create XML Envelope to use in SOAP Protocol
         WriteXMLEnvelope(Data);

         // Build configuration file for CURL cmdline
         BuildCommandLine();

         // Run CL program for CURL command
         call_AR471C(GBPRDTST:Response);

         // If a response has been received from the API,
         // process the XML data
         If Response = 'Y';

           // Extract Process Result from XML Response
           Data = ExtractResult();

           // Get Authorization Code
           GetAuthCode(Data);

           // Get Authorization Message
           GetAuthMessage(Data);

           // Get Token for Credit Card
           GetToken(Data);

           // Get Authorization Reference (aka GatewayTransID)
           GetAuthReference(Data);

         // If no response received, send back error message
         Else;
           AuthMessage = 'No response received from server. Try again later.';
         Endif;

         // Delete Stream Files
         DeleteStreamFiles();

        End-Proc;
       //----------------------------------------------------------------
       //  Create Request in XML format
       //----------------------------------------------------------------
       Dcl-Proc CreateXMLRequest;

         // Declare Procedure Interface
         Dcl-Pi  CreateXMLRequest   Char(1000000);
           GBUSER      Char(50);
           GBUSERPW    Char(50);
           GBMERCH     Char(15);
           GBMERCHA    Char(15);
         End-Pi;

         // Declare Local Variables
         Dcl-S data            Char(1000000);
         Dcl-S i               Int(3);
         Dcl-S Amount          Char(10);
         Dcl-S RequestDateTime Char(14);
         Dcl-S UnitCostRnd     Packed(7:2);
         Dcl-S UnitCostAmt     Packed(9:0);
         Dcl-S UnitCostChr     Char(9);
         Dcl-S TotalAmtInt     Packed(11:0);

         // Format Amount
         Amount = %char(%int(%dec(#Amount:9:2)*100));

         // Format Request Date and Time
         RequestDateTime = %char(%timestamp:*ISO0);

         data = '<?xml version="1.0" encoding="utf-16"?>';
         data = %trim(data) + '<requestHeader>';
         data = %trim(data) + '<ClientIdentifier>SOAP</ClientIdentifier>';
         data = %trim(data) + '<TransactionID>1000</TransactionID>';
         data = %trim(data) + '<RequestType>004</RequestType>';
         data = %trim(data) + '<RequestDateTime>' + RequestDateTime
                          + '</RequestDateTime>';
         data = %trim(data) + '<User>' + %trim(GBUSER) + '</User>';
         data = %trim(data) + '<Password>' + %trim(GBUSERPW) + '</Password>';
         data = %trim(data) + '<requestMessage>';
         data = %trim(data) + '<MerchantCode>' + %trim(GBMERCH)
                            +'</MerchantCode>';
         data = %trim(data) + '<MerchantAccountCode>' + %trim(GBMERCHA)
                            +  '</MerchantAccountCode>';

         // If Token not passed, use the actual Card Number
         If Token = *blanks;
           data = %trim(data) + '<PaymentAccountNumber>' + CCNo
                            + '</PaymentAccountNumber>';
         Else;
           data = %trim(data) + '<Token>' + %trim(Token) + '</Token>';
         Endif;

<        data = %trim(data) + '<ExpirationDate>' + %trim(ExpDate)
<                          + '</ExpirationDate>';
<        data = %trim(data) + '<Amount>' + %trim(Amount) + '</Amount>';
<        data = %trim(data) + '<TransactionType>' + %trim(TransType)
                           + '</TransactionType>';
<        data = %trim(data) + '<TransIndustryType>RE</TransIndustryType>';
<        data = %trim(data) + '<AcctType>R</AcctType>';
<        data = %trim(data) + '<HolderType>' + HolderTyp
                            + '</HolderType>';
<        data = %trim(data) + '<InvoiceNum>' + %trim(InvoiceNum) +
                              '</InvoiceNum>';
<      //  data = %trim(data) + '<InvoiceNum>' + 'INV100'  +
       //                     '</InvoiceNum>';
         FormatPONum(); // Remove special characters
<        data = %trim(data) + '<PONum>' + %trim(PONum) + '</PONum>';
<        data = %trim(data) + '<CustomerAccountCode>' + %trim(CustAcct) +
                              '</CustomerAccountCode>';
<        data = %trim(data) + '<AccountZip>' + %trim(AccountZip) +
                              '</AccountZip>';
<        data = %trim(data) + '<TaxAmount>' + %trim(TaxAmount) +
                              '</TaxAmount>';
<        data = %trim(data) + '<TaxIndicator>' + %trim(TaxIndicator) +
                              '</TaxIndicator>';
<        data = %trim(data) + '<LocalTaxAmount>' + %trim(TaxAmount) +
                              '</LocalTaxAmount>';
<        data = %trim(data) + '<LocalTaxIndicator>' + %trim(TaxIndicator) +
                              '</LocalTaxIndicator>';
<        data = %trim(data) + '<NationalTaxAmount>' + %trim(TaxAmount) +
                              '</NationalTaxAmount>';
<        data = %trim(data) + '<NationalTaxIndicator>' + %trim(TaxIndicator) +
                              '</NationalTaxIndicator>';
         data = %trim(data) + '<ItemCount>' + %trim(ItemCount) + '</ItemCount>';

         For i = 1 to %int(ItemCount);  // Item Count
           //  data = %trim(data) + '<Item>';
           data = %trim(data) + '<ItemCode>' + %trim(Items(i).ItemCode);
           data = %trim(data) + '</ItemCode>';

           // Format Item Description
           FormatItemDesc(Items(i).Description);
           data = %trim(data) + '<ItemDescription>' +
                  %trim(Items(i).Description);
           data = %trim(data) + '</ItemDescription>';

           data = %trim(data) + '<ItemQuantity>';
           data = %trim(data) + %trim(%editc(Items(i).Qty:'3'));
           data = %trim(data) + '</ItemQuantity>';
           UnitCostRnd = (Items(i).UnitCost + .005); // Round Unit Cost to 2 decimals
           UnitCostAmt = UnitCostRnd * 100; // Remove Decimal Point
           data = %trim(data) + '<ItemUnitCostAmt>' +
                  %trim(%char(UnitCostAmt)  );
           data = %trim(data) + '</ItemUnitCostAmt>';
           data = %trim(data) + '<ItemUnitMeasure>' + %trim(Items(i).Uom);
           data = %trim(data) + '</ItemUnitMeasure>';
           data = %trim(data) + '<ItemTotalAmount>';
           TotalAmtInt = Items(i).TotalAmt * 100;
           data = %trim(data) + %trim(%char(TotalAmtInt));
           data = %trim(data) + '</ItemTotalAmount>';
             // data = %trim(data) + '</Item>';
         Endfor;

<        data = %trim(data) + '</requestMessage>';
<        data = %trim(data) + '</requestHeader>';

         Return data;

       End-Proc;

       //----------------------------------------------------------------
       //  Format Item Description                                      -
       //----------------------------------------------------------------
       Dcl-Proc FormatItemDesc;

         // Declare Procedure Interface
         Dcl-Pi FormatItemDesc;
           ItemDesc       Char(35);
         End-Pi;

         ItemDesc = %scanrpl('&':'&amp;':ItemDesc);

       End-Proc;

       //----------------------------------------------------------------
       //  Format PO Number                                             -
       //----------------------------------------------------------------
       Dcl-Proc FormatPONum;

         // Declare Local Variables
         Dcl-S i     Int(3);

         PONum = %scanrpl(' ':'-':%trim(PONum));
         For i = 1 to %len(%trim(PONum));
           If (%subst(PONum:i:1) < '0' or
              %subst(PONum:i:1) > '9') and
              (%subst(PONum:i:1) < 'A' or
              %subst(PONum:i:1) > 'Z') and
              (%subst(PONum:i:1) < 'a' or
              %subst(PONum:i:1) > 'z') and
              %subst(PONum:i:1) <> '-';
              PONum = %replace('':PONum:i:1);
           Endif;
         Endfor;

       End-Proc;

       //----------------------------------------------------------------
       //  Write XML envelope (xmlinput.txt)
       //----------------------------------------------------------------
       Dcl-Proc WriteXMLEnvelope;

         // Declare Procedure Interface
         Dcl-Pi  WriteXMLEnvelope;
           Data    Char(1000000);
         End-Pi;

         // Declare Local Variables
         Dcl-S path            Char(50);
         Dcl-S fd              Int(10);
         Dcl-S Top             Char(300);
         Dcl-S Bottom          Char(200);

         // Encode Data to BASE64
         EncodeData(Data);

         // path = XML Envelope with RequestMessage in the center
         path  = '/mw4ifs/web/' + %trim(Ldausr) + '/xmlinput.txt';
         fd = openf(%trim(path):O_CREAT+O_WRONLY+O_CODEPAGE:
                 RW*OWNER + RW*GROUP + R:819);

         // File needs to be closed and then reopened for data to be PC readable
         closef(fd);
         fd = openf(%trim(path):O_TEXTDATA+O_WRONLY);

         // Write Top part of xmlinput.txt
         Top = '<soapenv:Envelope xmlns:soapenv=' +
         '"http://schemas.xmlsoap.org/soap/envelope/" ' + CRLF;
         Top  = %trim(Top) + 'xmlns:req=' +
         '"http://bridgepaynetsecuretx.com/requesthandler">' + CRLF;
         Top = %trim(Top) +  '  <soapenv:Header/>' + CRLF;
         Top = %trim(Top) +  '  <soapenv:Body>' + CRLF ;
         Top = %trim(Top) +  '  <req:ProcessRequest>' + CRLF ;
         Top = %trim(Top) +  '  <req:requestMsg>' + CRLF;
         Top = %trim(Top);
         writef(fd: %addr(Top): %len(%trim(Top)));

         // Write RequestMessage
         Data = %trim(Data) + CRLF;
         writef(fd: %addr(Data): %len(%trim(Data)));

         // Write Bottom part of xmlinput.txt
         Bottom =  '  </req:requestMsg>' + CRLF;
         Bottom = %trimr(Bottom) + '  </req:ProcessRequest>' + CRLF;
         Bottom = %trim(Bottom)  + '  </soapenv:Body>' + CRLF;
         Bottom = %trim(Bottom)  + '  </soapenv:Envelope> ';
         Bottom = %trim(Bottom);
         writef(fd: %addr(Bottom): %len(%trim(Bottom)));
         closef(fd);

       End-Proc;

       //----------------------------------------------------------------
       //  Encode Raw Data into a Request in BASE64 format
       //----------------------------------------------------------------
       Dcl-Proc EncodeData;

         // Declare Procedure Interface
         Dcl-Pi   EncodeData;
           Data      Char(1000000);
         End-Pi;

         // Declare Local Variables
         Dcl-S path        Char(50);
         Dcl-S fd          Int(10);

         // Open file for Writing
         path = '/mw4ifs/web/' + %trim(Ldausr) + '/EncodeInput.txt';
         fd = openf(%trim(path):O_CREAT+O_WRONLY+O_CODEPAGE:
                 RW*OWNER + RW*GROUP + R:819);

         // File needs to be closed and then reopened for data to be PC readable
         closef(fd);
         fd = openf(%trim(path):O_TEXTDATA+O_WRONLY);

         // Write Data to text file as input for Encoding
         writef(fd:%addr(Data):%len(%trim(Data)));
         closef(fd);

         // Call CL Program ENCODE64
         ENCODE64();

         // Open text file for reading
         path = '/mw4ifs/web/' + %trim(Ldausr) + '/EncodeOutput2.txt';
         fd = openf(%trim(path):o_RDONLY+o_TEXTDATA);

         // Read Data from text file as output from Encoding
         readf(fd: %addr(Data): %size(Data));
         closef(fd);

       End-Proc;

        //----------------------------------------------------------------
       //  Build Command Line for CURL command
       //----------------------------------------------------------------
       Dcl-Proc BuildCommandLine;

         // Declare Local Variables
         Dcl-S handle      Int(10);
         Dcl-S handlepath  Char(50);
         Dcl-S Data        Char(1000000);

         If GBPRDTST = 'P';
           handlepath  = '/mw4ifs/web/' + %trim(Ldausr) + '/cmdlineP.txt';
         Else;
           handlepath  = '/mw4ifs/web/' + %trim(Ldausr) + '/cmdlineT.txt';
         Endif;

         handle = openf(%trim(handlepath):O_CREAT+O_WRONLY+O_CODEPAGE:
                RW*OWNER + RW*GROUP + R:819);

         // File needs to be closed and then reopened for data to be PC readable
         closef(handle);

         handle = openf(%trim(handlepath):O_TEXTDATA+O_WRONLY);

         If GBPRDTST = 'P';
           data = 'url = "https://api.getbeyondpay.com/PaymentService/'
               + 'RequestHandler.svc"';
         Else;
           data = 'url = "https://api-test.getbeyondpay.com/PaymentService/'
               + 'RequestHandler.svc"';
         Endif;

         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = '--insecure';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = 'header = "accept: application/xml"';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = 'header = "Content-Type: text/xml; charset=utf-8"';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = 'header = "SOAPAction: http://bridgepaynetsecuretx.com/' +
            'requesthandler/IRequestHandler/ProcessRequest"';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = '-d @/mw4ifs/web/' + %trim(Ldausr) + '/xmlinput.txt';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         data = '-o "/mw4ifs/web/' + %trim(Ldausr) + '/response.txt"';
         data = %trim(data) + CRLF;
         writef(handle: %addr(data): %len(%trim(data)));

         closef(handle);

       End-Proc;

       //----------------------------------------------------------------
       //  Extract out Process Result from Response from Web Service
       //----------------------------------------------------------------
       Dcl-Proc ExtractResult;

         // Declare Procedure Interface
         Dcl-Pi ExtractResult  Char(1000000) End-Pi;

         // Declare Local Variables
         Dcl-S Data            Char(1000000);
         Dcl-S Start           Int(10);
         Dcl-S End             Int(10);
         Dcl-S Output          Char(32766);

         exec sql select * into :Output from qtemp.ccresponse;

         Data = Output;

         Start = %scan('<ProcessRequestResult>':Data);
         End   = %scan('</ProcessRequestResult>':Data);
         If Start > 0 and End > 0;
           Start += 22;
           Data  = %subst(Data:Start:End-Start);
         Endif;

         // Decode Data from BASE64
         DecodeData(Data);

         Return Data;

       End-Proc;

       //----------------------------------------------------------------
       //  Decode Response from BASE64 format back into Raw Data
       //----------------------------------------------------------------
       Dcl-Proc DecodeData;

         // Declare Procedure Interface
         Dcl-Pi   DecodeData;
           Data  Char(1000000);
         End-Pi;

         // Declare Local Variables
         Dcl-S path        Char(50);
         Dcl-S fd          Int(10);

         // Write to Input text file for php script Decode.php
         path = '/mw4ifs/web/' + %trim(Ldausr) + '/DecodeInput.txt';

         fd = openf(%trim(path):O_CREAT+O_WRONLY+O_CODEPAGE:
                 RW*OWNER + RW*GROUP + R:819);
         // File needs to be closed and then reopened for data to be PC readable
         closef(fd);
         fd = openf(%trim(path):O_TEXTDATA+O_WRONLY);
         writef(fd: %addr(Data): %len(%trim(Data)));
         closef(fd);

         // Run CL Program for Decoding
         DECODE64();

         // Read from Output text file from php script Decode.php
         path = '/mw4ifs/web/' + %trim(Ldausr) + '/DecodeOutput2.txt';
         fd = openf(%trim(path):o_RDONLY+o_TEXTDATA);
         readf(fd: %addr(Data): %size(Data));
         closef(fd);

       End-Proc;

       //----------------------------------------------------------------
       //  Get Authorization Code from Result
       //----------------------------------------------------------------
       Dcl-Proc GetAuthCode;

         // Declare Procedure Interface
         Dcl-Pi GetAuthCode;
           Data      Char(1000000) CONST;
         End-Pi;

         // Declare Local Variables
         Dcl-S Start     Int(10);
         Dcl-S End       Int(10);

         Start = %scan('<AuthorizationCode>':data);
         End   = %scan('</AuthorizationCode>':data);
         Start += 19;

         If End > Start;
           AuthCode = %subst(Data:Start:End-Start);
         Endif;

       End-Proc;

       //----------------------------------------------------------------
       //  Get Authorization Message from Result
       //----------------------------------------------------------------
       Dcl-Proc GetAuthMessage;

         // Declare Procedure Interface
         Dcl-Pi GetAuthMessage;
           Data      Char(1000000) CONST;
         End-Pi;

         // Declare Local Variables
         Dcl-S Start     Int(10);
         Dcl-S End       Int(10);

         Start = %scan('<ResponseDescription>':Data);
         End   = %scan('</ResponseDescription>':Data);
         Start += 21;

         If End > Start;
           AuthMessage = %subst(Data:Start:End-Start);
         Endif;

         // Replace 'Successful Request' with Approval Code Message
         If %trim(AuthMessage) = 'Successful Request';
           Start = %scan('<InternalMessage>':Data);
           End   = %scan('</InternalMessage>':Data);
           Start += 17;

           If End > Start;
             AuthMessage = %subst(data:start:end-start);
           Endif;

         Endif;

       End-Proc;

       //----------------------------------------------------------------
       //  Get Token from Result
       //----------------------------------------------------------------
       Dcl-Proc GetToken;

         // Declare Procedure Interface
         Dcl-Pi GetToken;
           Data      Char(1000000) CONST;
         End-Pi;

         // Declare Local Variables
         Dcl-S Start     Int(10);
         Dcl-S End       Int(10);

         // Return Token for Credit Card
         Start = %scan('<Token>':Data);
         End   = %scan('</Token>':Data);
         Start += 7;

         If End > Start;
           Token = %subst(Data:Start:End-Start);
         Endif;

       End-Proc;

       //----------------------------------------------------------------
       //  Get Authorization Reference from Result
       //----------------------------------------------------------------
       Dcl-Proc GetAuthReference;

         // Declare Procedure Interface
         Dcl-Pi GetAuthReference;
           Data      Char(1000000) CONST;
         End-Pi;

         // Declare Local Variables
         Dcl-S Start     Int(10);
         Dcl-S End       Int(10);

         // Return Authorization Reference (GatewayTransID in Result)
         Start = %scan('<GatewayTransID>':Data);
         End   = %scan('</GatewayTransID>':Data);
         Start += 16;

         If End > Start;
           ReferenceNo = %subst(data:start:end-start);
         Endif;

       End-Proc;

       //----------------------------------------------------------------
       //  Delete IFS Stream files
       //----------------------------------------------------------------
       Dcl-Proc DeleteStreamFiles;

         // Delete Stream Files
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/cmdlineP.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/cmdlineT.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/xmlinput.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/EncodeInput.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/EncodeOutput.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/EncodeOutput2.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/DecodeInput.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/DecodeOutput.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/DecodeOutput2.txt');
         unlink ('/mw4ifs/web/' + %trim(Ldausr) + '/response.txt');

       End-Proc;