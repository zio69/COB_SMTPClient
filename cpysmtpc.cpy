       01  param-cobsmtpc.                                           
           05  ipv4-address               pic x(15).                 
           05  port                       pic 9(03).                 
           05  helo                       pic x(40).                 
           05  sender                     pic x(60).                 
           05  recipients.                                          
               10  recipient    occurs 10 pic x(60).                 
           05  carbon-copy.                                          
               10  cc           occurs 10 pic x(60).                 
           05  html                       pic x(01).                 
           05  subject                    pic x(80).                 
           05  rc-client                  pic 9(02).                 
           05  num-rows                   pic 9(04) binary.          
           05  txt-msg.                                              
               10  msg-row                pic x(128)                 
                   occurs 2500 depending on num-rows.               
