Kylix XML-RPC PDF CONVERSION DAEMON
Copyright Clifford E. Baeseman <codepunk@codepunk.com>

The daemon forking code in this example was 
ported by Tony Caduto from the free pascal project.

This example daemon uses linux commands to convert
text to a pdf document and return it. A client example
for windows is in the examples/delphi/client directory.

The daemon exposes one xmlrpc method
conversion.text2pdf it takes base64 text as a parameter and returns
a base64 pdf document.

conversion.text2pdf
signature base64 (base64)

To launch the daemon modify the startup script called
startpdfd

then start the daemon using the script
./startpdfd


Compiling
This requires Indy 9.0
Compile with conditional INDY9 
or you will get errors

Version
 release 0.1 6, Dec 2001
 initial example release

 issues
 daemon is hard wired to port 8012
 storage is hard wired to /opt for temp
 file storage 
 introspect is not enabled
 linked libs included in this package
 
 