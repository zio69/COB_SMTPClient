      *****************************************************************
      *                                                               *
      *   Losely based on IBM sample EZA6SOPC (changed to IPV4)       *
      *   This subroutine allocates a socket on the address passed by *
      *   caller then sends an e-mail (body and recipients also       *
      *   passed by caller). Then it deallocates the socket and       *
      *   returns to caller                                           *
      *                                                               *
      * EZA6SOPC Copyright: Licensed Materials - Property of IBM      *
      * This routine: I wrote it - anyone can use it (and hopefully   *
      * impress one's bosses!)                                        *
      *                                                               *
      * SHOULD work in CICS as well as batch - but when I last tried  *
      * I had some strange S0C4 in CICS. That was over 10 ago, though.*
      *                                                               *
      *****************************************************************

       Identification Division.
       Program-id. COBSMTPC initial.
       Environment Division.
       Data Division.
       Working-storage Section.
      *---------------------------------------------------------------*
      * Socket interface function codes                               *
      *---------------------------------------------------------------*
       01  soket-functions.
           02 soket-accept          pic x(16) value 'ACCEPT          '.
           02 soket-bind            pic x(16) value 'BIND            '.
           02 soket-close           pic x(16) value 'CLOSE           '.
           02 soket-connect         pic x(16) value 'CONNECT         '.
           02 soket-fcntl           pic x(16) value 'FCNTL           '.
           02 soket-freeaddrinfo    pic x(16) value 'FREEADDRINFO    '.
           02 soket-getaddrinfo     pic x(16) value 'GETADDRINFO     '.
           02 soket-getclientid     pic x(16) value 'GETCLIENTID     '.
           02 soket-gethostbyaddr   pic x(16) value 'GETHOSTBYADDR   '.
           02 soket-gethostbyname   pic x(16) value 'GETHOSTBYNAME   '.
           02 soket-gethostid       pic x(16) value 'GETHOSTID       '.
           02 soket-gethostname     pic x(16) value 'GETHOSTNAME     '.
           02 soket-getnameinfo     pic x(16) value 'GETNAMEINFO     '.
           02 soket-getpeername     pic x(16) value 'GETPEERNAME     '.
           02 soket-getsockname     pic x(16) value 'GETSOCKNAME     '.
           02 soket-getsockopt      pic x(16) value 'GETSOCKOPT      '.
           02 soket-givesocket      pic x(16) value 'GIVESOCKET      '.
           02 soket-initapi         pic x(16) value 'INITAPI         '.
           02 soket-ioctl           pic x(16) value 'IOCTL           '.
           02 soket-listen          pic x(16) value 'LISTEN          '.
           02 soket-ntop            pic x(16) value 'NTOP            '.
           02 soket-pton            pic x(16) value 'PTON            '.
           02 soket-read            pic x(16) value 'READ            '.
           02 soket-recv            pic x(16) value 'RECV            '.
           02 soket-recvfrom        pic x(16) value 'RECVFROM        '.
           02 soket-select          pic x(16) value 'SELECT          '.
           02 soket-send            pic x(16) value 'SEND            '.
           02 soket-sendto          pic x(16) value 'SENDTO          '.
           02 soket-setsockopt      pic x(16) value 'SETSOCKOPT      '.
           02 soket-shutdown        pic x(16) value 'SHUTDOWN        '.
           02 soket-socket          pic x(16) value 'SOCKET          '.
           02 soket-takesocket      pic x(16) value 'TAKESOCKET      '.
           02 soket-termapi         pic x(16) value 'TERMAPI         '.
           02 soket-write           pic x(16) value 'WRITE           '.
      *---------------------------------------------------------------*
      * Work variables                                                *
      *---------------------------------------------------------------*
       01  command                        pic 9(8) binary value zero.
       01  reqarg                         pic 9(8) binary value zero.
       01  errno                          pic 9(8) binary value zero.
       01  retcode                        pic s9(8) binary value zero.
       01  index-counter                  pic 9(8) binary value zero.
       01  buffer-element.
           05  buffer-element-nbr         pic 9(5).
           05  filler                     pic x(3) value space.
       01  server-ipaddr-dotted           pic x(15) value space.
       01  client-ipaddr-dotted           pic x(15) value space.
       01  close-server                   pic 9(8) Binary value zero.
           88  close-server-down          value 1.
       01  Connect-Flag                   pic x value space.
           88 CONNECTED                         value 'Y'.
       01  Client-Server-Flag             pic x value space.
           88 CLIENTS                           value 'C'.
           88 SERVERS                           value 'S'.
       01  Terminate-Options              pic x value space.
           88 Opened-API                        value 'A'.
           88 Opened-Socket                     value 'S'.
       01  timer-accum                    pic 9(8) Binary value zero.
       01  timer-interval                 pic 9(8) Binary value 2000.
       01  Cur-time.
           02  Hour                       pic 9(2).
           02  Minute                     pic 9(2).
           02  Second                     pic 9(2).
           02  Hund-Sec                   pic 9(2).
       77  Failure                        Pic S9(8) comp.
      *---------------------------------------------------------------*
      * Variables used for the INITAPI call                           *
      *---------------------------------------------------------------*
       01  maxsoc-fwd                     pic 9(8) Binary.
       01  maxsoc-rdf redefines maxsoc-fwd.
           02 filler                      pic x(2).
           02 maxsoc                      pic 9(4) Binary.
       01  initapi-ident.
           05  tcpname                    pic x(8) Value 'TCPIP  '.
           05  asname                     pic x(8) Value space.
       01  subtask                        pic x(8) value 'COBSMTPC'.
       01  maxsno                         pic 9(8) Binary Value 1.
      *---------------------------------------------------------------*
      * Variables used by the SHUTDOWN Call                           *
      *---------------------------------------------------------------*
       01  how                            pic 9(8) Binary.
      *---------------------------------------------------------------*
      * Variables returned by the GETCLIENTID Call                    *
      *---------------------------------------------------------------*
       01  clientid.
           05  clientid-domain            pic 9(8) Binary value 19.
           05  clientid-name              pic x(8) value space.
           05  clientid-task              pic x(8) value space.
           05  filler                     pic x(20) value low-value.
      *---------------------------------------------------------------*
      * Variables returned by the GETNAMEINFO Call                    *
      *---------------------------------------------------------------*
       01  name-len                       pic 9(8) binary.
       01  host-name                      pic x(255).
       01  host-name-len                  pic 9(8) binary.
       01  service-name                   pic x(32).
       01  service-name-len               pic 9(8) binary.
       01  name-info-flags                pic 9(8) binary value 0.
       01  ni-nofqdn                      pic 9(8) binary value 1.
       01  ni-numerichost                 pic 9(8) binary value 2.
       01  ni-namereqd                    pic 9(8) binary value 4.
       01  ni-numericserver               pic 9(8) binary value 8.
       01  ni-dgram                       pic 9(8) binary value 16.
      *---------------------------------------------------------------*
      * Variables used for the SOCKET call                            *
      *---------------------------------------------------------------*
       01  AF-INET                        pic 9(8) Binary Value 2.
       01  AF-INET6                       pic 9(8) Binary Value 19.
       01  SOCK-STREAM                    pic 9(8) Binary Value 1.
       01  SOCK-DATAGRAM                  pic 9(8) Binary Value 2.
       01  SOCK-RAW                       pic 9(8) Binary Value 3.
       01  IPPROTO-IP                     pic 9(8) Binary Value zero.
       01  IPPROTO-TCP                    pic 9(8) Binary Value 6.
       01  IPPROTO-UDP                    pic 9(8) Binary Value 17.
       01  IPPROTO-IPV6                   pic 9(8) Binary Value 41.
       01  socket-descriptor              pic 9(4) Binary Value zero.
      *---------------------------------------------------------------*
      * Server socket address structure                               *
      *---------------------------------------------------------------*
       01  server-socket-address.
           05  server-afinet              pic 9(4) Binary Value 2.
           05  server-port                pic 9(4) Binary Value 0.
           05  server-ipaddr              pic 9(8) Binary Value zero.
           05                             pic X(8).
       01  NBYTE                  PIC 9(8)  COMP value 256.
       01  BUF                    PIC X(1024).
      *---------------------------------------------------------------*
      * Variables used by the BIND Call                               *
      *---------------------------------------------------------------*
       01  client-socket-address.
           05  client-family              pic 9(4) Binary Value 2.
           05  client-port                pic 9(4) Binary Value 1031.
           05  client-ipaddr              pic 9(8) Binary Value zero.
           05                             pic X(8).
      *---------------------------------------------------------------*
      * Buffer and length fields for send operation                   *
      *---------------------------------------------------------------*
       01  send-request-length            pic 9(8) Binary value zero.
       01  send-buffer.
           05  send-buffer-total          pic x(4000) value space.
           05  closedown-message redefines send-buffer-total.
               10  closedown-id           pic x(8).
               10  filler                 pic x(3992).
           05  send-buffer-seq redefines send-buffer-total
                                          pic x(8) occurs 500 times.
      *---------------------------------------------------------------*
      * Variables used for the NTOP/PTON call                         *
      *---------------------------------------------------------------*
       01  IN6ADDR-ANY                    pic x(45)
                               value '::'.
       01  IN6ADDR-LOOPBACK               pic x(45)
                               value '::1'.
       01  presentable-addr               pic x(45) value spaces.
       01  presentable-addr-len           pic 9(4) Binary value 45.
       01  numeric-addr.
           05 filler                      pic 9(16) Binary Value 0.
           05 filler                      pic 9(16) Binary Value 0.
      *---------------------------------------------------------------*
      * Buffer and length fields for recv operation                   *
      *---------------------------------------------------------------*
       01  read-request-length            pic 9(8) Binary value zero.
       01  read-buffer                    pic x(4000) value space.
      *---------------------------------------------------------------*
      * Other fields for send and reccfrom operation                  *
      *---------------------------------------------------------------*
       01  send-flag                      pic 9(8) Binary value zero.
       01  recv-flag                      pic 9(8) Binary value zero.
      *---------------------------------------------------------------*
      * Error message for socket interface errors                     *
      *---------------------------------------------------------------*
       01  ezaerror-msg.
           05  filler                     pic x(9) Value 'Function='.
           05  ezaerror-function          pic x(16) Value space.
           05  filler                     pic x value ' '.
           05  filler                     pic x(8) Value 'Retcode='.
           05  ezaerror-retcode           pic ---99.
           05  filler                     pic x value ' '.
           05  filler                     pic x(9) Value 'Errorno='.
           05  ezaerror-errno             pic zzz99.
           05  filler                     pic x value ' '.
           05  ezaerror-text              pic x(50) value ' '.
      *---------------------------------------------------------------*
      * From here on: my stuff!                                       *
      *---------------------------------------------------------------*
       01  temp-sender                    pic x(62).
       01  lgt-sender                     pic 9(4) binary.
       01  complex-sender                 pic x.
       01  MIME-Header.
           05 h1                          pic x(80) value
             'MIME-Version: 1.0'.
           05 h2                          pic x(80) value
             'Content-type: multipart/mixed;'.
           05 h2a                         pic x(80) value
             ' boundary="_COBSMTPC_boundary_0x69f2abcdq47jh_"'.
           05 h3                          pic x(80) value
             'This message requires an HTML compatible mail client.'.
           05 h4                          pic x(80) value
             '--_COBSMTPC_boundary_0x69f2abcdq47jh_'.
           05 h5                          pic x(80) value
             'Content-type: text/html'.
       01  redefines MIME-Header.
           05  Header-line occurs 6       pic x(80).
       01  ws-carbon-copy.
           05  occurs 10.
               10  work-cc                pic x(62).
               10  lgt-cc                 pic 9(4) binary.
               10  complex-cc             pic x.
       01  ws-recipients.
           05  occurs 10.
               10  work-recipient         pic x(62).
               10  lgt-recipient          pic 9(4) binary.
               10  complex-recipient pic x.

       01  ix                             pic 9(4) binary.
       01  iy                             pic 9(4) binary.
       01  ib                             pic 9(8) binary.
       01  my-pointer                     usage is pointer.
      *
      * feel free to increase if you need huge attachments!
      * if memory is a concern, you may have this area allocated by 
      * LE services and hence move it to linkage section.
      * 
       01  base64-area.
           03 b64-char                    pic x(1) occurs 3000000.

       01  temp-32b                       pic 9(8) comp-5.
       01  redefines temp-32b.
           03                             pic x.
           03  temp-24b                   pic x(3).

       01  base64-bits.
           03 b64-bits                    pic x occurs 24.

       01  bits.
           03  bit                        pic x occurs 8.

       01  i1                             pic 9(4) binary.
       01  i2                             pic 9(4) binary.
       01  i64                            pic 9(8) binary.
       01  num-bytes-rounded              pic 9(8) binary.
       01  temp                           pic 9(8) binary.
       01  remaindr                          pic 9(8) binary.
       01  tab64                          pic x(64) value
           'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567
      -    '89+/'.
       01  asc3                           pic x(3).
       01  temp3                          pic x(3).
       77  EBCDIC-CCSID                   PIC 9(4) BINARY VALUE 1140.
       77  ASCII-CCSID                    PIC 9(4) BINARY VALUE 819.
       77  num-byte-zd                    PIC 9(8).
     *
       Linkage Section.
     *
       copy cpysmtpc.
      *01  param.
      *        
      *    05  ipv4-address               pic x(15).
      *    mandatory: dotted i.p address i.e. 192.168.000.001 
      *
      *    05  port                       pic 9(03).
      *    mandatory: usually 021
      *    
      *    05  helo                       pic x(40).
      *    can be empty
      *        string presented to SMTP server on HELO command
      *        default COBSMTPC if <= spaces                             
      *        you may want to pass the name of the calling program
      *        
      *    05  sender                     pic x(60).
      *    mandatory: a valid address the SMTP server will accept
      *    in doubt ask your mail administrator
      *    
      *    05  recipients.
      *        10  recipient occurs 10    pic x(60).
      *    mandatory: at least one! 
      *    can be "plain" i.e. wylecoyote@acme.com
      *    or "fancy" i.e. Wile E. Coyote <wylecoyote@acme.com>
      *    
      *    05  carbon-copy.
      *        10  cc        occurs 10    pic x(60).
      *    can be empty. same format as recipients
      *    
      *    05  html                       pic x(01).
      *    can be empty. if set to '1' you can use HTML in the mail body
      *    
      *    05  subject                    pic x(80).
      *    mandatory. whatever you like. be polite and professional
      *    
      *    05  rc-client                   pic 9(02).
      *    output - caller should test for 0 - anything else means trouble
      *    should be the last parameter but we've got an ODO....
      *    
      *    05  num-rows                  pic 9(04) binary.
      *    mandatory anything from 1 to 32767.
      *    HOWEVER it MUST be the amount of rows in the following ODO
      *    Otherwise this routine will be roaming in the storage of the caller
      *    or even further, if you know what I mean.
      *    WHICH is far from ideal - my humble two cents.
      
      *    05  txt-msg.
      *        10  msg-row               pic x(128)
      *            occurs 500 depending on num-rows.
      *    mandatory, at least 1 occurrence. row can be longer if needed        
      *            
      *=============================================*
      *= attachments area - picture is meaningless =*
      *=============================================*
       01  attachment1                    pic x(32000).
       01  attachment2                    pic x(32000).
       01  attachment3                    pic x(32000).
       01  attachment4                    pic x(32000).
       01  attachment5                    pic x(32000).
      *=============================================*
      *= attachment dummy-section                  =*
      *=============================================*
       01  attachment-ds.
           03 Content-Type                pic x(80).
           03 Content-name                pic x(80).
           03 Content-Transfer-Encode     pic x(01).
           03 Translate-to-ASCII          pic x(01).
           03 Content-Transfer-Encoding   pic x(80).
           03 Content-row-length          pic 9(04) binary.
           03 Content-num-bytes           pic 9(08) binary.
           03 Content-area.
              05 content-byte             pic x
                   occurs 2000000 depending on Content-num-bytes.
      *=============================================*
      *= what's inside the attachment areas        =*
      *=============================================*
      *    03 Content-Type                pic x(80).
      *    a valid MIME Content-Type.
      *    i.e. text/html or application/pdf or whatever. 
      *    you created the attachment, I reckon you know what it is!
      *    
      *    03 Content-name                pic x(80).
      *    attachment name. like 'booya.txt'
      *
      *    03 Content-Transfer-Encode     pic x(01).
      *    Y/y/1 means you want BASE64 encoding. SHOULD not be necessary
      *    for text/plain or xml but sometimes SMTP server gets lost if there
      *    are multiple cr+lf. I suggest you use it, unless it's a tiny text 
      *    file.
      *
      *    03 Translate-to-ASCII          pic x(01).
      *    if your content is already in ASCII then N 
      *    if it's some EBCDIC flavor then definitely Y/y/1
      *
      *    03 Content-Transfer-Encoding   pic x(80).
      *    if you asked for base64, then this field will contain "Base64"
      *    on return.
      *    if you took care yourself of encoding the content, specifiy the
      *    encoding you used. i.e. 7bit/Base64/Quoted-Printable            
      *
      *    03 Content-row-length          pic 9(04) binary.
      *    number of bytes before this routine inserts a CRLF (there's no 
      *    LRECL on PCs)
      *    for instance, you should insert 132/133 for classic printouts
      *         or 80 if you're attaching a source
      *    it will return 76 if you asked for base64
      *    
      *    03 Content-num-bytes           pic 9(08) binary.
      *    length of the content. MUST be precise! Otherwise, again, this 
      *    routine will roam freely in the caller's storage. And possibly 
      *    beyond, until you get a S0C4
      *    
      *    03 Content-area.
      *       05 content-byte             pic x
      *          occurs 2000000 depending on Content-num-bytes.
      *    your attachment. 
      *    note for rooks: since this is an ODO (occurs depending on)
      *    the 2 millions are totaly meaningless - it's the
      *    previous field that defines how many bytes will be attached.
      *    I could have written any number.
      *    please note: since CoBOL does not allow programmers to loop 
      *    through linkage section items, if you need more than 5 attachments
      *    you need to define futher areas in linkage section and process
      *    them individually (see below)
      *    I would have gone for an occurs depending on, but since there's 
      *    one already that's a no-no.
      *    If you wish, you may go for an array of pointers and loop through
      *    the array, like:
      * 01  Array-of-pointers.
      *     03 number-of-pointers         pic 9(4) binary. 
      *     03 attach-pointer        usage is pointer 
      *        occurs 1 depending on number-of-pointers.
      *        
      *     if number-of-pointers greater 1
      *        perform varying your-index from 1 by 1
      *          until your-index greater number-of-pointers
      *          set address of attachment-ds to attach-pointer(your-index)
      *          perform send-attachment thru ex-send-attachment
      *        end perform
      *     end-if.
      *
      *     The reason why I did not use pointers is....(old) cobol programmers
      *     are somewhat allergic to pointers. I've tried many times to explain
      *     that they are not evil/wizardry/magic - with little success.
      *     So for the sake of maintenance I didn't go for them.
      *     
      *     If you like the idea but you're not sure how, in the caller when
      *     you want to add an attachment:
      *     add 1 to number-of-pointers
      *     set attach-pointer(number-of-pointers) to address of your-attachment
      *     
      *
      *=============================================*
       Procedure Division using param-COBSMTPC
                                attachment1
                                attachment2
                                attachment3
                                attachment4
                                attachment5.
      *=============================================*

            move port                   to     server-port

            Perform Initialize-API      thru   Initialize-API-Exit
            Perform Get-Client-ID       thru   Get-Client-ID-Exit
            Perform Sockets-Descriptor  thru   Sockets-Descriptor-Exit
            Perform Presentation-To-Numeric
               thru Presentation-To-Numeric-Exit
            Perform CONNECT-Socket      thru   CONNECT-Socket-Exit
      *     Perform FCNTL               thru   FCNTL-Exit.            
            Perform Numeric-TO-Presentation
               thru Numeric-To-Presentation-Exit
      *
            perform compose-email       thru   compose-email-exit
      *
            Perform Shutdown-Send       thru   Shutdown-Send-Exit
            Perform Shutdown-Receive    thru   shutdown-Receive-Exit
            Perform Close-Socket        thru   Exit-Now
            .

      *---------------------------------------------------------------*
      * Initialize socket API                                         *
      *---------------------------------------------------------------*
       Initialize-API.
           Move soket-initapi to ezaerror-function.
           Call 'EZASOKET' using soket-initapi maxsoc initapi-ident
                                 subtask maxsno errno retcode.
           Move 'Initapi failed' to ezaerror-text.
           If retcode < 0
              move 12 to failure
           end-if
           Perform Return-Code-Check  thru Return-Code-Exit.
           Move 'A' to Terminate-Options.
       Initialize-API-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Let us see the client-id                                      *
      *---------------------------------------------------------------*
       Get-Client-ID.
            Move soket-getclientid to ezaerror-function.
            Call 'EZASOKET' using soket-getclientid clientid errno
                                  retcode.
      *     Display 'Our client ID = ' clientid-name ' ' clientid-task.
            Move 'Getclientid failed' to ezaerror-text.
            If retcode < 0
               move 24 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
            Move 'C' to client-server-flag.
       Get-Client-ID-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Get us a stream socket descriptor                             *
      *---------------------------------------------------------------*
       Sockets-Descriptor.
            Move soket-socket to ezaerror-function.
            Call 'EZASOKET' using soket-socket AF-INET SOCK-STREAM
                IPPROTO-IP errno retcode.
            Move 'Socket call failed' to ezaerror-text.
            If retcode < 0
               move 60 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
            Move 'S' to Terminate-Options.
            Move retcode to socket-descriptor.
       Sockets-Descriptor-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Use PTON to create an IP address to bind to.                  *
      *---------------------------------------------------------------*
       Presentation-To-Numeric.
            move soket-pton to ezaerror-function.
            move ipv4-address   to presentable-addr.
            Call 'EZASOKET' using soket-pton AF-INET
               presentable-addr presentable-addr-len
               numeric-addr
               errno retcode.
            Move 'PTON call failed' to ezaerror-text.
            If retcode < 0
               move 24 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
            move numeric-addr to server-ipaddr.
       Presentation-To-Numeric-Exit.
           Exit.

      *---------------------------------------------------------------*
      * CONNECT                                                       *
      *---------------------------------------------------------------*
       Connect-Socket.
            Move space to Connect-Flag.
            Move zeros to errno retcode.
            move soket-connect to ezaerror-function.
            CALL 'EZASOKET' USING SOKET-CONNECT socket-descriptor
                              server-socket-address errno retcode.
            Move 'Connection call failed' to ezaerror-text.
            If retcode < 0
               move 24 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
            If retcode = 0
               Move 'Y' to Connect-Flag
            end-if.
       Connect-Socket-Exit.
           Exit.

      *------------------------------------------------------------------*
      * FCNTL - sets the socket to nonblocking (for debug under xpediter)*
      *------------------------------------------------------------------*
       FCNTL-Socket.
            Move zeros to errno retcode.
            move soket-fcntl to ezaerror-function.
            move +4 to command reqarg
            CALL 'EZASOKET' USING SOKET-FCNTL socket-descriptor
                              command reqarg     errno retcode.
            Move 'FCNTL call failed' to ezaerror-text.
            If retcode < 0
               move 24 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
       FCNTL-Socket-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Use NTOP to display the IP address.                           *
      *---------------------------------------------------------------*
       Numeric-To-Presentation.
           move soket-ntop to ezaerror-function.
           move server-ipaddr to numeric-addr.
           move soket-ntop to ezaerror-function.
           Call 'EZASOKET' using soket-ntop AF-INET
              numeric-addr
              presentable-addr presentable-addr-len
              errno retcode.
      *    Display 'Presentable address = ' presentable-addr.
           Move 'NTOP call failed' to ezaerror-text.
           If retcode < 0
               move 24 to failure
            end-if
           Perform Return-Code-Check thru Return-Code-Exit.
       Numeric-TO-Presentation-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Use GETNAMEINFO to get the host and service names             *
      *---------------------------------------------------------------*
       Get-Name-Information.
           move 28 to name-len.
           move 255 to host-name-len.
           move 32 to service-name-len.
           move ni-namereqd to name-info-flags.
           move soket-getnameinfo to ezaerror-function.
           Call 'EZASOKET' using soket-getnameinfo
              server-socket-address name-len
              host-name host-name-len
              service-name service-name-len
              name-info-flags
              errno retcode.
TEST***    Display 'Host name = ' host-name.
TEST***    Display 'Service = ' service-name.
           Move 'Getaddrinfo call failed' to ezaerror-text.
           If retcode < 0
               move 24 to failure
            end-if
           Perform Return-Code-Check thru Return-Code-Exit.
       Get-Name-Information-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Write a message to the server                                 *
      *---------------------------------------------------------------*
       Write-Message.
TEST***     display '>>> ' buf(1:nbyte)
            add 1 to nbyte
            move x'0D25' to buf(nbyte:2)
            add 1 to nbyte
            inspect buf(1:nbyte) replacing all low-values by spaces
            call 'EZACIC04' using buf nbyte
            Move soket-write to ezaerror-function.
            Call 'EZASOKET' using soket-write socket-descriptor
                nbyte buf
                errno retcode.
            Move 'Write call failed' to ezaerror-text.
            If retcode < 0
               move 84 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
       Write-Message-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Shutdown to pipe                                              *
      *---------------------------------------------------------------*
       Shutdown-Send.
            Move soket-shutdown to ezaerror-function.
            move 1 to how.
            Call 'EZASOKET' using soket-shutdown socket-descriptor
                how
                errno retcode.
            Move 'Shutdown call failed' to ezaerror-text.
            If retcode < 0
               move 99 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
       Shutdown-Send-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Read a message from the server.                               *
      *---------------------------------------------------------------*
       Read-Message.
            Move soket-recv to ezaerror-function.
            Move spaces to buf.
            move +0 to recv-flag
            Call 'EZASOKET' using soket-recv socket-descriptor
                  recv-flag
                  nbyte buf
                  errno retcode.
            If retcode < 0
               Move 'recv call failed' to ezaerror-text
               move 120 to failure
               Perform Return-Code-Check thru Return-Code-Exit
            end-if.
            move retcode to nbyte
            call 'EZACIC05' using buf nbyte.
test***     display '<<< ' buf(1:nbyte).
       Read-Message-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Shutdown receive pipe                                         *
      *---------------------------------------------------------------*
       Shutdown-Receive.
            Move soket-shutdown to ezaerror-function.
            move 0 to how.
            Call 'EZASOKET' using soket-shutdown socket-descriptor
                how
                errno retcode.
            Move 'Shutdown call failed' to ezaerror-text.
            If retcode < 0
               move 99 to failure
            end-if
            Perform Return-Code-Check thru Return-Code-Exit.
       Shutdown-Receive-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Close socket                                                  *
      *---------------------------------------------------------------*
       Close-Socket.
             Move soket-close to ezaerror-function.
             Call 'EZASOKET' using soket-close socket-descriptor
                                   errno retcode.
             Move 'Close call failed' to ezaerror-text.
             If retcode < 0 move 132 to failure
                perform write-ezaerror-msg thru
                        write-ezaerror-msg-exit
             end-if.
      *      Accept Cur-Time from TIME.
      *      Display Cur-Time ' COBSMTPC: ' ezaerror-function
      *                   ' RETCODE=' RETCODE ' ERRNO= ' ERRNO.
       Close-Socket-Exit.
           Exit.

      *---------------------------------------------------------------*
      * Terminate socket API                                          *
      *---------------------------------------------------------------*
       exit-term-api.
           Call 'EZASOKET' using soket-termapi.
      *    ACCEPT cur-time from TIME.
      *    Display cur-time '  COBSMTPC:  TERMAPI '
      *            ' RETCODE= ' RETCODE ' ERRNO= ' ERRNO.

      *---------------------------------------------------------------*
      * Terminate program                                             *
      *---------------------------------------------------------------*
       exit-now.
      *    Move failure to return-code.
           Move failure to rc-client.
           Goback.

      *---------------------------------------------------------------*
      * Subroutine.                                                   *
      * -----------                                                   *
      * Write out an error message                                    *
      *---------------------------------------------------------------*
       write-ezaerror-msg.
           Move errno to ezaerror-errno.
           Move retcode to ezaerror-retcode.
           Display ezaerror-msg.
       write-ezaerror-msg-exit.
           Exit.

      *---------------------------------------------------------------*
      * Check Return Code after each Socket Call                      *
      *---------------------------------------------------------------*
       Return-Code-Check.
      *     Accept Cur-Time from TIME.
      *     Display Cur-Time ' COBSMTPC: ' ezaerror-function
      *                           ' RETCODE=' RETCODE ' ERRNO= ' ERRNO.
            IF RETCODE < 0
               Perform Write-ezaerror-msg thru write-ezaerror-msg-exit
               Move zeros to errno retcode
               IF Opened-Socket
                  Go to Close-Socket
               ELSE
                   IF Opened-API
                      Go to exit-term-api
                   ELSE
                      Go to exit-now
                  end-if
               end-if
            end-if
            Move zeros to errno retcode.
       Return-Code-Exit.
           Exit.

       compose-email.
      * 
      *     scan recipients-cc
      *
            perform varying iy from 1 by 1
                      until iy greater 10
                if recipient(iy) greater spaces
                   perform Handle-recipients
                      thru Handle-recipients-exit
                end-if
                if cc(iy) greater spaces
                   perform Handle-cc
                      thru Handle-cc-exit
                end-if
            end-perform.
      **
      *     scan sender - checking for Name <name@org.xxx> address syntax*
      *
            perform varying ix from 1 by 1
                      until ix greater 60
                         or sender(ix:1) equal '<'
            end-perform
            if ix not greater 60
      *
      *        found a < - likely already formatted        
      *        
               move sender(ix:) to temp-sender
               move 's' to complex-sender
            else
      *
      *        not found - let's enclose address in <>        
      *            
               move '<' to temp-sender(1:1)
               move sender to temp-sender(2:)
               move 'n' to complex-sender
            end-if
            perform varying ix from 62 by -1
                      until ix less 1
                         or temp-sender(ix:1) not equal space
            end-perform
      *
      *     caller fogot the closing >??? No problemo.
      *
            if temp-sender(ix:1) not equal '>'
               add 1 to ix
               move '>' to temp-sender(ix:1)
            end-if
            move ix to lgt-sender
      **
      *
      *     let's get server's welcome message
      *     should begin with 2xx
      *
            Perform Read-Message       thru   Read-Message-Exit.
      *     display 'host smtp says ' buf
      
            if buf(1:1) not equal '2'
               display 'welcome server SMTP: ' buf
               move 55 to failure
               move -1 to retcode
               Perform Return-Code-Check thru Return-Code-Exit
            end-if
      *
      * HELO - let's introduce ourselves
      *
            Move 'HELO ' to buf.
      *      
      * caller provided a custom HELO string
      *      
            if helo greater spaces
               move helo       to buf(6:)
               perform varying ix from 46 by -1
                         until buf(ix:1) not equal space
               end-perform
               move ix to nbyte
            else
      * standard HELO string      
               move 'COBSMTPC' to buf(6:)
               move 13 to nbyte
            end-if
      *      
      * send message to server and get its reply. reply should begin with 2xx
      * 
            Perform Write-Message      thru   Write-Message-Exit.
            move 100 to nbyte
            Perform Read-Message       thru   Read-Message-Exit.
      *     display 'host smtp says ' buf
            if buf(1:1) not equal '2'
               display 'HELO reply: ' buf
               move 55 to failure
               move -1 to retcode
               Perform Return-Code-Check thru Return-Code-Exit
            end-if
      
      *  from here on, standard SMTP exchange 
      *  every string must be carefully inspected to establish its actual length
      *  beacause traling binary zeros can irk some receiving email clients. 
      *  Cannot they, Apple? 
            
      *
      * mail from
      *
            Move 'MAIL FROM:' to buf
            move temp-sender to buf(11:)
            compute nbyte = lgt-sender + 11
            Perform Write-Message      thru   Write-Message-Exit.
            move 100 to nbyte
            Perform Read-Message       thru   Read-Message-Exit.
      *     display 'smtp host says ' buf
            if buf(1:1) not equal '2'
               display 'risposta MAIL FROM: ' buf
               move 55 to failure
               move -1 to retcode
               Perform Return-Code-Check thru Return-Code-Exit
            end-if
      *
      * rcpt to
      *
            perform varying ix from 1 by 1
                      until ix greater 10
               if recipient(ix) greater spaces
                  Move 'RCPT TO:' to buf
                  move work-recipient(ix) to buf(9:)
                  compute nbyte = 8 + lgt-recipient(ix)
                  Perform Write-Message thru  Write-Message-Exit
                  move 100 to nbyte
                  Perform Read-Message  thru   Read-Message-Exit
      *           display 'smtp host says ' buf
                  if buf(1:1) not equal '2'
                     display 'answer to RCPT TO: ' buf
                     move 55 to failure
                     move -1 to retcode
                     Perform Return-Code-Check thru Return-Code-Exit
                  end-if
               end-if
            end-perform.
      *
      * rcpt to - cc
      *
            perform varying ix from 1 by 1
                      until ix greater 10
               if cc(ix) greater spaces
                  Move 'RCPT TO:' to buf
                  move work-cc(ix) to buf(9:)
                  compute nbyte = 8 + lgt-cc(ix)
                  Perform Write-Message thru  Write-Message-Exit
                  move 100 to nbyte
                  Perform Read-Message thru   Read-Message-Exit
      *           display 'smtp host says ' buf
                  if buf(1:1) not equal '2'
                     display 'risposta RCPT TO: ' buf
                     move 55 to failure
                     move -1 to retcode
                     Perform Return-Code-Check thru Return-Code-Exit
                  end-if
               end-if
            end-perform.
      *
      * DATA
      *
            Move 'DATA' to buf
            move 4 to nbyte
            Perform Write-Message      thru   Write-Message-Exit.
            move 100 to nbyte
            Perform Read-Message       thru   Read-Message-Exit.
      *     display 'smtp host says ' buf
            if buf(1:1) not equal '3'
               display 'risposta DATA: ' buf
               move 55 to failure
               move -1 to retcode
               Perform Return-Code-Check thru Return-Code-Exit
            end-if
      *
      * BODY
      *
            Move 'FROM: '  to buf

            if complex-sender = 's'
               move sender to buf(7:)
               move 66 to nbyte
            else
               move temp-sender to buf(7:)
               compute nbyte = 6 + lgt-sender
            end-if
            Perform Write-Message      thru   Write-Message-Exit.
      *
            perform varying ix from 1 by 1
                      until ix greater 10
               if recipient(ix) greater spaces
                 Move 'TO: ' to buf
                 if complex-recipient(ix) = 's'
                    move recipient(ix) to buf(5:)
                    move 64 to nbyte
                 else
                    move work-recipient(ix) to buf(5:)
                    compute nbyte = 4 + lgt-recipient(ix)
                 end-if
                 Perform Write-Message thru   Write-Message-Exit
               end-if
            end-perform
      *
            perform varying ix from 1 by 1
                      until ix greater 5
               if cc(ix) greater spaces
                  Move 'CC: ' to buf
                  if complex-cc(ix) = 's'
                     move cc(ix) to buf(5:)
                     move 64 to nbyte
                  else
                     move work-cc(ix) to buf(5:)
                     compute nbyte = 4 + lgt-cc(ix)
                  end-if
                  Perform Write-Message thru  Write-Message-Exit
               end-if
            end-perform
      *
            Move 'subject: ' to buf
            move subject to buf(10:)
            perform varying ix from 80 by -1
                      until ix not greater zero
                         or subject(ix:1) greater space
            end-perform
            compute nbyte = 9 + ix

            Perform Write-Message      thru   Write-Message-Exit.
      *
      *     MIME header if caller required it
      *
            if html equal '1'
               perform varying ix from 1 by 1
                         until ix greater 6
                   move Header-line(ix)   to buf
                   move 80                to nbyte
                   Perform Write-Message      thru   Write-Message-Exit
               end-perform
            end-if
      *
      *     extra CRLF required by Exchange servers
      *     considering the CRLF of the last line that makes 2 CRLF in a row
      *     but it looks like they're needed...!
      *     And other servers seem not to mind. Which is good
      *
            move x'0D25' to buf
            move 2 to nbyte
            Perform Write-Message      thru   Write-Message-Exit.
      *
            perform varying ix from 1 by 1
                      until ix greater num-rows
                move msg-row(ix) to buf
                move length of msg-row(ix) to nbyte
                Perform Write-Message      thru   Write-Message-Exit
            end-perform
      *
      *     I have prepared for a maximum of 5 attachents. 
      *     When I wrote this there was no way to loop through all input 
      *     parameters - or possibly I'm not aware that there was one.
      *     Whatever the case, if you need more than 5 you either add what you
      *     need in LINKAGE SECTION + USING + here OR find a way to make it 
      *     dynamic with a loop. In ASM one just has to scan the memory pointed
      *     by R1.... in cobol, dunno. 
      *     
      *
            set my-pointer to address of attachment1.
            if my-pointer not equal nulls
               set address of attachment-ds to my-pointer
               perform send-attachment thru ex-send-attachment
            end-if
            set my-pointer to address of attachment2.
            if my-pointer not equal nulls
               set address of attachment-ds to my-pointer
               perform send-attachment thru ex-send-attachment
            end-if
            set my-pointer to address of attachment3.
            if my-pointer not equal nulls
               set address of attachment-ds to my-pointer
               perform send-attachment thru ex-send-attachment
            end-if
            set my-pointer to address of attachment4.
            if my-pointer not equal nulls
               set address of attachment-ds to my-pointer
               perform send-attachment thru ex-send-attachment
            end-if
            set my-pointer to address of attachment5.
            if my-pointer not equal nulls
               set address of attachment-ds to my-pointer
               perform send-attachment thru ex-send-attachment
            end-if
      *
      *   send '.' followed by CRLF for End Of Message
      *
            Move '.'                                    to buf
            move 1  to nbyte
            Perform Write-Message      thru   Write-Message-Exit.
            move 100 to nbyte
            Perform Read-Message       thru   Read-Message-Exit.
      *     display 'smtp host says ' buf
            if buf(1:1) not equal '2'
               display 'reply to DATA: ' buf
               move 55 to failure
               move -1 to retcode
               Perform Return-Code-Check thru Return-Code-Exit
            end-if
      *
      *   QUIT
      *
            Move 'QUIT'                                 to buf
            move 4  to nbyte
            Perform Write-Message      thru   Write-Message-Exit.
            move 100 to nbyte
            Perform Read-Message       thru   Read-Message-Exit.
      *
      *     there should be no answer to quit 
      *     if it's there, we kindly ignore it assuming everything was good
      *
       compose-email-Exit.
           Exit.
      *
       Handle-recipients.
           perform varying ix from 1 by 1
                     until ix greater 60
                        or recipient(iY) (ix:1) equal '<'
           end-perform
           if ix not greater 60
              move recipient(iy) (ix:) to work-recipient(iy)
              move 's' to complex-recipient(iy)
           else
              move '<' to work-recipient(iy) (1:1)
              move recipient(iy) to work-recipient(iy) (2:)
              move 'n' to complex-recipient(iy)
           end-if
           perform varying ix from 62 by -1
                     until ix less 1
                        or work-recipient(iy) (ix:1) not = space
           end-perform
           if work-recipient(iy) (ix:1) not equal '>'
              add 1 to ix
              move '>' to work-recipient(iy) (ix:1)
           end-if
           move ix to lgt-recipient(iy).
       Handle-recipients-Exit.
           Exit.
      *
       Handle-cc.
            perform varying ix from 1 by 1
                      until ix greater 60
                         or cc(iy) (ix:1) equal '<'
            end-perform
            if ix not greater 60
               move cc(iy) (ix:) to work-cc(iy)
               move 's' to complex-cc(iy)
            else
               move '<' to work-cc(iy) (1:1)
               move cc(iy) to work-cc(iy) (2:)
               move 'n' to complex-cc(iy)
            end-if
            perform varying ix from 62 by -1
                      until ix less 1
                         or work-cc(iy) (ix:1) not equal space
            end-perform
            if work-cc(iy) (ix:1) not equal '>'
               add 1 to ix
               move '>' to work-cc(iy) (ix:1)
            end-if
            move ix to lgt-cc(iy).
       Handle-cc-Exit.
           Exit.
      *
       send-attachment.
      * 
      *    if caller requested encoding, attachment is transformed
      *    using base64
      *
           if i64 greater zero
      *
      *    means this is not the 1st attachment - gotta clean the work-area!
      *
              move spaces to base64-area(1:i64)
           end-if
           if Content-Transfer-Encode equal 'Y' or 'y' or '1'
              move +76 to content-row-length
              move 'base64' to Content-Transfer-Encoding
              perform base64 thru ex-base64
           end-if
           move 37 to nbyte
           move '--_COBSMTPC_boundary_0x69f2abcdq47jh_' to buf
           Perform Write-Message thru  Write-Message-Exit
           if content-name greater spaces
              move 'Content-Type: ' to buf
              move content-type to buf(15:)
              perform varying iy from 94 by -1
                        until buf(iy:1) greater space
              end-perform
              add 1 to iy
              move '; name="' to buf(iy:8)
              add 8 to iy
              move content-name to buf(iY:)
              perform varying iy from 94 by -1
                        until buf(iy:1) greater space
              end-perform
              add 1 to iy
              move '"' to buf(iY:1)
              move iy to nbyte
              Perform Write-Message thru  Write-Message-Exit
              move 'Content-Disposition: attachment; filename="' to buf
              move content-name to buf(44:)
              perform varying iy from 94 by -1
                        until buf(iy:1) greater space
              end-perform
              add 1 to iy
              move '"' to buf(iY:1)
              add 1 to iy
              move '; size=' to buf(iY:7)
              add 7 to iy
              move Content-num-bytes to num-byte-zd
              move num-byte-zd to buf(iy:8)
              add 7 to iy
              move iy to nbyte
              Perform Write-Message thru  Write-Message-Exit
           end-if.
           if Content-Transfer-Encoding greater spaces
              move 'Content-Transfer-Encoding:' to buf
              move Content-Transfer-Encoding to buf(28:)
              perform varying iy from 64 by -1
                        until buf(iy:1) greater spaces
              end-perform
              add 1 to iy
              move x'0d25' to buf(iy:2)
              add 1 to iy giving nbyte
              Perform Write-Message   thru   Write-Message-Exit
           end-if.
           if Content-Transfer-Encode equal 'Y' or 'y' or '1'
              perform varying ib from 1 by Content-row-length
                        until ib greater i64
                  add content-row-length to ib giving iy
                  if iy greater i64
                     compute iy = i64 - ib + 1
                     move base64-area(ib:iy) to buf
                     add 1 to iy
                     move x'0d25' to buf(iy:2)
                     add 1 to iy giving nbyte
                  else
                     move base64-area(ib:Content-row-length) to buf
                  end-if
                  move Content-row-length to nbyte
                  Perform Write-Message   thru   Write-Message-Exit
              end-perform
           else
              perform varying ib from 1 by Content-row-length
                        until ib greater Content-num-bytes
                  add content-row-length to ib giving iy
                  if iy greater i64
                     compute iy = i64 - ib + 1
                     move content-area(ib:iy) to buf
                     add 1 to iy
                     move x'0d25' to buf(iy:2)
                     add 1 to iy giving nbyte
                  else
                     move content-area(ib:Content-row-length) to buf
                     move Content-row-length to nbyte
                  end-if
                  Perform Write-Message   thru   Write-Message-Exit
              end-perform
           end-if.
           move 39 to nbyte
           move '--_COBSMTPC_boundary_0x69f2abcdq47jh_--' to buf
           Perform Write-Message thru  Write-Message-Exit.
       ex-send-attachment.
           Exit.
      *
      *    base64 coding    3 bytes are exploded in 4 6-bits groups
      *                     BINARY value of each of these groups
      *                     becomes an index to an array of 64 characters 
      *                     containing characters that are valid for any
      *                     and every codepage
      *                     each row contains up to 76 chars
      *                     in this case we cannot rely on ebcdic/ascii
      *                     translatio by z/OS tcp service (we need ascii codes 
      *                     to compute the indexes!!), hence the string
      *                     to be encoded is translated to ascii BEFORE coding
      *                     to base64
      *
       base64.
           move 0 to i64.
           divide Content-num-bytes by 3 giving temp remainder remaindr
           compute num-bytes-rounded = temp * 3.
           perform varying ib from 1 by 3
                     until ib greater num-bytes-rounded
               move zero to temp-32b
               if translate-to-ASCII equal '1' or 'y' or 'Y'
                  move function display-of(function national-of
                       (content-area(ib:3) EBCDIC-CCSID),
                        ASCII-CCSID) to asc3
                  move asc3 to temp-24b
               else
                  move content-area(ib:3) to temp-24b
               end-if
               perform 3bytes-to-24bits thru ex-3bytes-to-24bits
               move '00' to bits
               move base64-bits(1:6) to bits(3:6)
               perform encode thru ex-encode
               move base64-bits(7:6) to bits(3:6)
               perform encode thru ex-encode
               move base64-bits(13:6) to bits(3:6)
               perform encode thru ex-encode
               move base64-bits(19:6) to bits(3:6)
               perform encode thru ex-encode
           end-perform.
           if remaindr greater zero
               move zero to temp-32b
               move low-values to temp3
               move content-area(ib:remaindr) to temp3(1:remaindr)
               if translate-to-ASCII equal '1' or 'y' or 'Y'
                  move function display-of(function national-of
                       (temp3 EBCDIC-CCSID),
                        ASCII-CCSID) to asc3
                  move asc3 to temp-24b
               else
                  move content-area(ib:1) to temp-24b
               end-if
               perform 3bytes-to-24bits thru ex-3bytes-to-24bits
               move base64-bits(1:6) to bits(3:6)
               perform encode thru ex-encode
               move base64-bits(7:6) to bits(3:6)
               perform encode thru ex-encode
               if remaindr = 2
                  move base64-bits(13:6) to bits(3:6)
                  perform encode thru ex-encode
               end-if
           end-if.
       ex-base64.
           Exit.
      *
       3bytes-to-24bits.
           move all zeroes to base64-bits.
           if temp-32b greater 8388607
              move '1' to b64-bits(1)
              subtract 8388608 from temp-32b
           end-if.
           if temp-32b greater 4194303
              move '1' to b64-bits(2)
              subtract 4194304 from temp-32b
           end-if.
           if temp-32b greater 2097151
              move '1' to b64-bits(3)
              subtract 2097152 from temp-32b
           end-if.
           if temp-32b greater 1048575
              move '1' to b64-bits(4)
              subtract 1048576 from temp-32b
           end-if.
           if temp-32b greater 524287
              move '1' to b64-bits(5)
              subtract 524288 from temp-32b
           end-if.
           if temp-32b greater 262143
              move '1' to b64-bits(6)
              subtract 262144 from temp-32b
           end-if.
           if temp-32b greater 131071
              move '1' to b64-bits(7)
              subtract 131072 from temp-32b
           end-if.
           if temp-32b greater 65535
              move '1' to b64-bits(8)
              subtract 65536 from temp-32b
           end-if.
           if temp-32b greater 32767
              move '1' to b64-bits(9)
              subtract 32768 from temp-32b
           end-if.
           if temp-32b greater 16383
              move '1' to b64-bits(10)
              subtract 16384 from temp-32b
           end-if.
           if temp-32b greater 8191
              move '1' to b64-bits(11)
              subtract 8192 from temp-32b
           end-if.
           if temp-32b greater 4095
              move '1' to b64-bits(12)
              subtract 4096 from temp-32b
           end-if.
           if temp-32b greater 2047
              move '1' to b64-bits(13)
              subtract 2048 from temp-32b
           end-if.
           if temp-32b greater 1023
              move '1' to b64-bits(14)
              subtract 1024 from temp-32b
           end-if.
           if temp-32b greater 511
              move '1' to b64-bits(15)
              subtract 512 from temp-32b
           end-if.
           if temp-32b greater 255
              move '1' to b64-bits(16)
              subtract 256 from temp-32b
           end-if.
           if temp-32b greater 127
              move '1' to b64-bits(17)
              subtract 128 from temp-32b
           end-if.
           if temp-32b greater 63
              move '1' to b64-bits(18)
              subtract 64 from temp-32b
           end-if.
           if temp-32b greater 31
              move '1' to b64-bits(19)
              subtract 32 from temp-32b
           end-if.
           if temp-32b greater 15
              move '1' to b64-bits(20)
              subtract 16 from temp-32b
           end-if.
           if temp-32b greater 7
              move '1' to b64-bits(21)
              subtract 8 from temp-32b
           end-if.
           if temp-32b greater 3
              move '1' to b64-bits(22)
              subtract 4 from temp-32b
           end-if.
           if temp-32b greater 1
              move '1' to b64-bits(23)
              subtract 2 from temp-32b
           end-if.
           if temp-32b greater zero
              move '1' to b64-bits(24)
           end-if.
       ex-3bytes-to-24bits.
           Exit.
      *
       encode.
           move zero to i2
           if bit(8) equal '1'
              add 1 to i2
           end-if
           if bit(7) equal '1'
              add 2 to i2
           end-if
           if bit(6) equal '1'
              add 4 to i2
           end-if
           if bit(5) equal '1'
              add 8 to i2
           end-if
           if bit(4) equal '1'
              add 16 to i2
           end-if
           if bit(3) equal '1'
              add 32 to i2
           end-if
           add 1 to i2
           add 1 to i64.
           move tab64(i2:1) to b64-char(i64).
       ex-encode.
           Exit.
